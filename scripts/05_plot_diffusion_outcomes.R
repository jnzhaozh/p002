source(here::here("scripts/01_setup_project.R"))

simulation_results <- readRDS(here::here("results/simulation_results.rds"))

# -------------------------------------------------------------------------

data_diffusion_outcomes <- simulation_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(
    topic_factor = factor(
      topic_factor,
      levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
      labels = c("Narrow", "Moderate", "Broad")
    )
  )

# -------------------------------------------------------------------------

p_diffusion_outcomes <- data_diffusion_outcomes %>%
  ggplot(aes(x = n_topics, y = adopters_prop_mean, color = topic_factor)) +
  geom_hline(
    yintercept = 0.02,
    linetype = "dashed",
    color = "black",
    alpha = 0.8
  ) +
  annotate(
    "text",
    x = 1,
    y = 0.02,
    label = "Initial adoption: 2%",
    hjust = 0,
    vjust = -0.5,
    size = 3.5,
    fontface = "italic",
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin = adopters_prop_mean - adopters_prop_sd,
      ymax = adopters_prop_mean + adopters_prop_sd
    ),
    width = 0.15,
    linewidth = 0.2,
    alpha = 0.75,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = adopters_count_mean)) +
  scale_x_continuous(
    name = "Number of information topics",
    breaks = 1:10
  ) +
  scale_y_continuous(
    name = "Final adoption (%)",
    labels = scales::label_percent(),
    limits = c(0, NA)
  ) +
  ggsci::scale_color_d3(
    name = "Distribution of individual attention",
    guide = guide_legend(nrow = 3, order = 1)
  ) +
  scale_size_area(
    name = "Number of final adopters",
    max_size = 4,
    breaks = c(200, 350, 500),
    guide = guide_legend(reverse = TRUE, nrow = 3, order = 2)
  ) +
  this_theme(base_size = 12)

p_diffusion_outcomes

# this_ggsave(p_diffusion_outcomes, width = 9, height = 6.5)
