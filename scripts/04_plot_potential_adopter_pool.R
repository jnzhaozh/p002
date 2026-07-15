source(here::here("scripts/01_setup_project.R"))

simulation_results <- readRDS(here::here("results/simulation_results.rds"))

# -------------------------------------------------------------------------

data_expansion <- simulation_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(
    topic_factor = factor(
      topic_factor,
      levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
      labels = c("Narrow", "Moderate", "Broad")
    )
  )

p_expansion <- data_expansion %>%
  ggplot(aes(x = n_topics, y = agents_count_mean, color = topic_factor)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(
    name = "Number of information topics",
    breaks = 1:10
  ) +
  scale_y_continuous(
    name = "Number of potential adopters",
    limits = c(0, NA),
    labels = scales::comma_format(),
  ) +
  ggsci::scale_color_d3(
    name = "Distribution of individual attention",
    guide = guide_legend(nrow = 3)
  )

p_expansion

# -------------------------------------------------------------------------

data_composition <- simulation_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(
    topic_factor = factor(
      topic_factor,
      levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
      labels = c("Narrow", "Moderate", "Broad")
    )
  ) %>%
  distinct(
    n_topics,
    topic_factor,
    betabin_alpha,
    betabin_beta,
    agents_count_mean
  ) %>%
  reframe(k = seq_len(n_topics), .by = everything()) %>%
  mutate(
    prob = dbbinom(
      x = k - 1,
      size = n_topics - 1,
      alpha = betabin_alpha,
      beta = betabin_beta
    ),
    expected_agents = prob * agents_count_mean
  )


p_composition <- levels(data_composition$topic_factor) %>%
  map(., \(topic) {
    p <- data_composition %>%
      filter(topic_factor == topic) %>%
      ggplot(aes(x = n_topics, y = k, color = topic_factor)) +
      geom_point(aes(size = expected_agents)) +

      scale_x_continuous(
        name = switch(topic, "Moderate" = "Number of information topics"),
        breaks = 1:10
      ) +
      scale_y_continuous(
        name = switch(topic, "Narrow" = "Level of individual attention"),
        breaks = 1:10,
        labels = switch(topic, "Narrow" = waiver())
      ) +
      scale_size_area(
        name = "Number of potential adopters",
        max_size = 6,
        breaks = c(100, 500, 1000),
        labels = scales::comma_format(),
        guide = switch(
          topic,
          "Broad" = guide_legend(reverse = TRUE, nrow = 3, order = 2),
          "none"
        )
      ) +
      ggsci::scale_color_d3(
        limits = topics,
        guide = "none"
      ) +
      labs(title = topic)

    return(p)
  }) %>%
  wrap_plots(ncol = 3)

p_composition

# -------------------------------------------------------------------------

p_potential_adopter_pool <- (p_expansion / p_composition) +
  plot_layout(
    heights = c(1, 1.15),
    axes = "collect",
    guides = "collect"
  ) &
  plot_annotation(tag_levels = list(c("A", "B", "", ""))) &
  this_theme(base_size = 12)

p_potential_adopter_pool

# this_ggsave(p_potential_adopter_pool, width = 9, height = 8)
