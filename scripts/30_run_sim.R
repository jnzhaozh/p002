# 30_run_sim.R

source(here("scripts/10_get_env.R"))
source(here("scripts/20_set_sim.R"))

betabins <- tibble(
  topic_factor = c("Right-Skewed", "Symmetric", "Left-Skewed"),
  betabin_alpha = c(1.0, 4.0, 4.0),
  betabin_beta  = c(4.0, 4.0, 1.0)
)

lognormals <- tibble(
  density_factor = c("Sparse", "Standard", "Dense"),
  avg_degree = c(3, 6, 12)
) %>%
  reframe(
    degree_factor = c("Homogeneous", "Heterogeneous", "Highly-Heterogeneous"),
    lognormal_sigma = c(0.01, 0.40, 0.80),
    lognormal_mu    = log(avg_degree) - (lognormal_sigma^2 / 2),
    .by = c(density_factor, avg_degree)
  )

simulation_params <- tidyr::expand_grid(
  n_iterations     = 100,
  n_topics         = 1:10,
  agents_per_topic = 1000,
  initial_adoption = 0.02,
  betabins,
  lognormals
)

# -------------------------------------------------------------------------

gc()
plan(sequential)
# plan(multisession, workers = parallel::detectCores(logical = FALSE) * 0.5)
plan(multisession, workers = 2)

simulation_results <- simulation_params %>%
  mutate(df = future_pmap(
    .l = pick(
      n_iterations,
      n_topics,
      agents_per_topic,
      betabin_alpha,
      betabin_beta,
      avg_degree,
      lognormal_mu,
      lognormal_sigma,
      initial_adoption
    ),
    .f        = iterate_simulation,
    .options  = furrr_options(seed = 15),
    .progress = TRUE
  )) %>%
  tidyr::unnest_wider(df)

plan(sequential)
gc()

# this_saveRDS(simulation_results)

#
# lm(final_adoption_prop_mean ~ n_topics + betabin_shape + lognormal_shape,
#    data = simulation_results) %>%
#   summary()