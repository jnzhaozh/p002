# 42_viz_adoption.R

source(here("scripts/10_get_env.R"))

sim_results <- readRDS("results/simulation_results.rds")

adotpion_results <- sim_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(topic_factor = factor(
    topic_factor,
    levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
    labels = c("Narrow", "Moderate", "Broad")
  ))

# -------------------------------------------------------------------------


p_adoption <- adotpion_results %>%
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
    label = "Initial Adoption: 2%",
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
    width = 0.2
  ) +
  geom_line(linewidth = 1) +
  geom_point(aes(size = adopters_count_mean)) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA)) +
  ggsci::scale_color_d3() +
  scale_size_area(max_size = 4, breaks = c(200, 350, 500)) +
  guides(color = guide_legend(nrow = 3, order = 1),
         size = guide_legend(
           nrow = 3,
           order = 2,
           reverse = TRUE
         )) +
  labs(x = "Number of Information Topics",
       y = "Final Adoption (%)",
       color = "Distribution of Individual Attention",
       size = "Number of Individuals") +
  this_theme()

p_adoption

this_ggsave(p_adoption, width = 22, height = 18)
