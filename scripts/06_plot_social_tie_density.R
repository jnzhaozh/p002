source(here::here("scripts/01_setup_project.R"))

simulation_results <- readRDS(here::here("results/simulation_results.rds"))

# -------------------------------------------------------------------------

data_social_tie_density <- simulation_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(
    topic_factor = factor(
      topic_factor,
      levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
      labels = c("Narrow", "Moderate", "Broad")
    )
  )

# -------------------------------------------------------------------------

p_potential <- data_social_tie_density %>%
  ggplot(aes(x = n_topics, y = density_potential_mean, color = topic_factor)) +
  geom_errorbar(
    aes(
      ymin = pmax(density_potential_mean - density_potential_sd, 0),
      ymax = density_potential_mean + density_potential_sd
    ),
    width = 0.15,
    linewidth = 0.2,
    alpha = 0.75,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(
    name = NULL,
    labels = NULL,
    breaks = 1:10
  ) +
  scale_y_continuous(
    name = "Social-tie density\n(potential adopters)",
    n.breaks = 4,
    labels = scales::label_number(accuracy = 0.001)
  ) +
  ggsci::scale_color_d3(
    guide = "none"
  )


p_realized <- data_social_tie_density %>%
  ggplot(aes(x = n_topics, y = density_realized_mean, color = topic_factor)) +
  geom_errorbar(
    aes(
      ymin = pmax(density_realized_mean - density_realized_sd, 0),
      ymax = density_realized_mean + density_realized_sd
    ),
    width = 0.15,
    linewidth = 0.2,
    alpha = 0.75,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(
    name = NULL,
    labels = NULL,
    breaks = 1:10
  ) +
  scale_y_continuous(
    name = "Social-tie density\n(realized adopters)",
    n.breaks = 4,
    labels = scales::label_number(accuracy = 0.001)
  ) +
  ggsci::scale_color_d3(
    guide = "none"
  )

p_ratio <- data_social_tie_density %>%
  ggplot(aes(x = n_topics, y = density_ratio_mean, color = topic_factor)) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "black",
    alpha = 0.8
  ) +
  geom_errorbar(
    aes(
      ymin = pmax(density_ratio_mean - density_ratio_sd, 0),
      ymax = density_ratio_mean + density_ratio_sd
    ),
    width = 0.15,
    linewidth = 0.2,
    alpha = 0.75,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(
    name = "Number of information topics",
    breaks = 1:10
  ) +
  scale_y_continuous(
    name = "Social-tie density ratio\n(realized / potential)",
    limits = c(0, NA),
    n.breaks = 4
  ) +
  ggsci::scale_color_d3(
    name = "Distribution of individual attention",
    guide = guide_legend(nrow = 3)
  )

p_social_tie_density <- (p_potential / p_realized / p_ratio) +
  plot_annotation(tag_levels = "A") &
  this_theme(base_size = 12)

p_social_tie_density

this_ggsave(p_social_tie_density, width = 9, height = 9.5)
