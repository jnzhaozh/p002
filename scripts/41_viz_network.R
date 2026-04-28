# 41_viz_network.R

source(here::here("scripts/10_get_env.R"))

sim_results <- readRDS("results/simulation_results.rds")

network_results <- sim_results %>%
  filter(density_factor == "Standard", degree_factor == "Homogeneous") %>%
  mutate(topic_factor = factor(
    topic_factor,
    levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
    labels = c("Narrow", "Moderate", "Broad")
  ))

# -------------------------------------------------------------------------


p_ponteial <- network_results %>%
  ggplot(aes(x = n_topics, y = density_potential_mean, color = topic_factor)) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  scale_x_continuous(breaks = 1:10, labels = NULL) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
  ggsci::scale_color_d3() +
  guides(color = "none") +
  labs(x = NULL, y = "Network Density\n(Potential Adopters)") +
  this_theme()


p_realized <- network_results %>%
  ggplot(aes(x = n_topics, y = density_realized_mean, color = topic_factor)) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 1:10, labels = NULL) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
  ggsci::scale_color_d3() +
  guides(color = "none") +
  labs(x = NULL, y = "Network Density\n(Realized Adopters)") +
  this_theme()

p_efficiency <- network_results %>%
  ggplot(aes(x = n_topics, y = density_ratio_mean, color = topic_factor)) +
  geom_line(linewidth = 0.8) +
  geom_point() +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    color = "black",
    alpha = 0.8
  ) +
  scale_x_continuous(breaks = 1:10) +
  scale_y_continuous(limits = c(0, NA)) +
  ggsci::scale_color_d3() +
  guides(color = guide_legend(nrow = 3)) +
  labs(
    # title = ""
    x = "Number of Information Topics",
    y = "Density Ratio\n(Realized / Potential)",
    color = "Distribution of Individual Attention"
  ) +
  this_theme()

p_network <- (p_ponteial / p_realized / p_efficiency) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'A') &
  theme(legend.position = "bottom",
        plot.tag = element_text(face = "bold"))

p_network

this_ggsave(p_network, width = 22, height = 24)



# -------------------------------------------------------------------------

#
# p_density <- network_results %>%
#   ggplot(aes(x = n_topics, color = topic_factor, fill = topic_factor)) +
#   geom_ribbon(
#     aes(ymin = density_potential_mean, ymax = density_realized_mean),
#     alpha = 0.1,
#     color = NA
#   ) +
#   geom_line(
#     aes(y = density_potential_mean, linetype = "Potential Adopters"),
#     linewidth = 0.8,
#     alpha = 0.5,
#   ) +
#   geom_point(aes(y = density_potential_mean), alpha = 0.5) +
#   geom_line(aes(y = density_realized_mean, linetype = "Realized Adopters"),
#             linewidth = 1) +
#   geom_point(aes(y = density_realized_mean)) +
#   scale_x_continuous(breaks = 1:10) +
#   scale_y_continuous(n.breaks = 8,
#                      labels = scales::label_number(accuracy = 0.001)) +
#   scale_color_d3() +
#   scale_fill_d3() +
#   # scale_shape_manual(values = c(18, 17, 15)) +
#   scale_linetype_manual(values = c(
#     "Potential Adopters" = "dashed",
#     "Realized Adopters" = "solid"
#   )) +
#   labs(
#     x = "Topic Span of Information (n)",
#     y = "Network Density",
#     linetype = "Type of Adopters",
#     color = "Distribution of Topic Affiliation"
#   ) +
#   guides(
#     color = guide_legend(
#       nrow = 3,
#       order = 1,
#       override.aes = list(linetype = 0)
#     ),
#     fill = "none",
#     linetype = guide_legend(
#       nrow = 2,
#       order =  2,
#       override.aes = list(shape = 16)
#     ),
#   ) +
#   this_theme()
#
# p_density
