# 20_set_sim.R

source(here("scripts/10_get_env.R"))

# -------------------------------------------------------------------------

generate_incidence_matrix <- function(n_topics,
                                      agents_per_topic,
                                      betabin_alpha,
                                      betabin_beta,
                                      ...) {
  if (n_topics == 1) {
    return(Matrix::Matrix(
      1,
      nrow = agents_per_topic,
      ncol = 1,
      sparse = TRUE
    ))
  }
  
  affiliation_total <- n_topics * agents_per_topic
  
  # p_beta <- rbeta(n = affiliation_total,
  #                 shape1 = betabin_alpha,
  #                 shape2 = betabin_beta)
  # affiliation_pool <- 1 + rbinom(n = affiliation_total, size = n_topics - 1, prob = p_beta)
  
  affiliation_pool <- 1 + extraDistr::rbbinom(
    n = affiliation_total,
    size = n_topics - 1,
    alpha = betabin_alpha,
    beta = betabin_beta
  )
  
  affiliation_sample <- which.max(cumsum(affiliation_pool) >= affiliation_total)
  
  topics_per_agent <- affiliation_pool[1:affiliation_sample]
  topics_per_agent[affiliation_sample] <- affiliation_total - sum(topics_per_agent[-affiliation_sample])
  
  n_agents <- length(topics_per_agent)
  
  row_idx <- rep(seq_len(n_agents), times = topics_per_agent)
  col_idx <- ((seq_along(row_idx) - 1) %% n_topics) + 1
  
  mat <- Matrix::sparseMatrix(
    i = row_idx,
    j = col_idx,
    x = 1,
    dims = c(n_agents, n_topics)
  )
  
  inc_mat <- tryCatch({
    suppressWarnings({
      # mat_shuffle <- vegan::permatswap(
      #   mat,
      #   fixedmar = "both",
      #   method = "quasiswap",
      #   mtype = "prab",
      #   times = 1,
      #   burnin = 0
      # )
      # mat_shuffle$perm[[1]]
      
      shuffled_mat <- backbone::fastball(mat)
      Matrix::Matrix(shuffled_mat, sparse = TRUE)
    })
  }, error = function(e) {
    mat
  }) %>%
    Matrix::Matrix(sparse = TRUE)
  
  return(inc_mat)
}

# -------------------------------------------------------------------------

generate_adjacency_matrix <- function(n_agents,
                                      avg_degree,
                                      lognormal_mu,
                                      lognormal_sigma,
                                      ...) {
  fitness_scores <- rlnorm(n_agents, meanlog = lognormal_mu, sdlog = lognormal_sigma)
  total_edges <- round((n_agents * avg_degree) / 2)
  
  adj_mat <- igraph::sample_fitness(
    no.of.edges = total_edges,
    fitness.out = fitness_scores,
    fitness.in  = NULL,
    loops       = FALSE,
    multiple    = FALSE
  ) %>%
    igraph::as_adjacency_matrix(sparse = TRUE)
  
  return(adj_mat)
}

# -------------------------------------------------------------------------

run_diffusion <- function(network_mat,
                          affiliation_mat,
                          initial_adoption,
                          time_steps = 1000,
                          ...) {
  affiliation_mat <- as(affiliation_mat, "dgCMatrix")
  network_mat <- as(network_mat, "dgCMatrix")
  
  n_agents <- nrow(affiliation_mat)
  eps <- .Machine$double.eps
  
  threshold_mat <- affiliation_mat
  threshold_mat@x <- runif(length(threshold_mat@x), eps, 1 - eps)
  
  active_threshold <- rowSums(threshold_mat)
  active_threshold[active_threshold == 0] <- Inf
  
  influence_mat <- affiliation_mat
  influence_mat@x <- runif(length(influence_mat@x), eps, 1 - eps)
  
  active_influence_mat <- influence_mat
  
  current_adoption <- as.numeric(rbinom(n_agents, 1, initial_adoption))
  current_steps <- 0
  
  repeat {
    current_steps <- current_steps + 1
    previous_adoption <- current_adoption
    
    # --- topic level linear threshold model ---
    active_influence_mat@x <- influence_mat@x * current_adoption[influence_mat@i + 1]
    
    active_neighbor_influence <- network_mat %*% active_influence_mat
    active_neighbor_influence_sum <- rowSums(active_neighbor_influence * affiliation_mat)
    
    active_neighbor_count <- as.numeric(network_mat %*% current_adoption)
    active_neighbor_count[active_neighbor_count == 0] <- 1
    
    active_neighbor_influence_rescale <- active_neighbor_influence_sum / active_neighbor_count
    
    current_adoption <- (active_neighbor_influence_rescale >= active_threshold |
                           current_adoption == 1)
    
    if (all(current_adoption == previous_adoption) ||
        current_steps >= time_steps)
      break
  }
  
  # network_g <- igraph::graph_from_adjacency_matrix(network_mat, mode = "undirected")
  potential_density <- length(network_mat@i) / (n_agents * (n_agents - 1))
  
  realized_density <- 0
  adopters_index <- which(current_adoption)
  adopters_count <- length(adopters_index)
  if (adopters_count > 1) {
    sub_mat <- network_mat[adopters_index, adopters_index, drop = FALSE]
    realized_density <- length(sub_mat@i) / (adopters_count * (adopters_count - 1))
  }
  
  return(
    tibble(
      agents_count       = n_agents,
      adopters_count     = adopters_count,
      adopters_prop      = adopters_count / agents_count,
      density_potential  = potential_density,
      density_realized   = realized_density,
      density_ratio      = density_realized / density_potential
      # clustering_coeff = igraph::transitivity(network_g, type = "global"),
      # giant_comp_size  = max(igraph::components(network_g)$csize) / n_agents,
      # avg_path_length  = igraph::mean_distance(network_g, directed = FALSE)
    )
  )
}


# -------------------------------------------------------------------------

run_simulation <- function(...) {
  aff_mat <- generate_incidence_matrix(...)
  net_mat <- generate_adjacency_matrix(n_agents = nrow(aff_mat), ...)
  
  run_diffusion(network_mat = net_mat, affiliation_mat = aff_mat, ...)
}

# -------------------------------------------------------------------------


iterate_simulation <- function(n_iterations, ...) {
  ls <- vector("list", n_iterations)
  
  for (i in seq_len(n_iterations)) {
    ls[[i]] <- run_simulation(...)
    
    if (i %% 10 == 0)
      gc(full = TRUE)
  }
  
  df <- list_rbind(ls)
  
  df_summary <- df %>%
    summarise(across(everything(), list(
      mean = \(x) mean(x, na.rm = TRUE),
      sd = \(x) sd(x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}"))
  
  rm(ls, df)
  return(df_summary)
}


# iterate_simulation <- function(n_iterations, ...) {
#   df <- map(seq_len(n_iterations), \(i) {
#     res <- run_simulation(...)
#     gc()
#     return(res)
#   }) %>%
#     list_rbind()
#
#   df_summary <- df %>%
#     summarise(across(everything(), list(
#       mean = \(x) mean(x, na.rm = TRUE),
#       sd   = \(x) sd(x, na.rm = TRUE)
#     ), .names = "{.col}_{.fn}"))
#
#   return(df_summary)
# }