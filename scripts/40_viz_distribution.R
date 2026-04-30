# 40_viz_distribution.R

source(here("scripts/10_get_env.R"))

sim_results <- readRDS("results/simulation_results.rds")

# -------------------------------------------------------------------------


population_results <- sim_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(topic_factor = factor(
    topic_factor,
    levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
    labels = c("Narrow", "Moderate", "Broad")
  ))


p_population <- population_results %>%
  ggplot(aes(x = n_topics, y = agents_count_mean, color = topic_factor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 5) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  ggsci::scale_color_d3() +
  guides(color = guide_legend(nrow = 3)) +
  labs(
    # title = ""
    x = "Number of Information Topics",
    y = "Number of Potential Adopters",
    color = "Distribution of Individual Attention"
  ) +
  this_theme()

p_population


# -------------------------------------------------------------------------

affiliation_results <- sim_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(topic_factor = factor(
    topic_factor,
    levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
    labels = c("Narrow", "Moderate", "Broad")
  )) %>%
  distinct(n_topics,
           topic_factor,
           betabin_alpha,
           betabin_beta,
           agents_count_mean) %>%
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

p_affiliation <- affiliation_results %>%
  ggplot(aes(x = n_topics, y = k, color = topic_factor)) +
  geom_point(aes(size = expected_agents)) +
  facet_wrap( ~ topic_factor) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(breaks = 1:10) +
  scale_size_area(
    max_size = 6,
    breaks = c(100, 500, 1000),
    labels = scales::comma_format()
  ) +
  ggsci::scale_color_d3() +
  guides(color = "none", size = guide_legend(reverse = TRUE, nrow = 3)) +
  labs(
    # title = ""
    x = "Number of Information Topics",
    y = "Level of Individual Attention",
    size = "Number of Individuals"
  ) +
  this_theme()


# -------------------------------------------------------------------------


p_distribution <- (p_population / p_affiliation) +
  plot_layout(heights = c(1, 1), guides = "collect") +
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold"))

p_distribution

this_ggsave(p_distribution, width = 22, height = 20)
