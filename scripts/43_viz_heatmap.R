# 43_viz_heatmap.R

source(here("scripts/10_get_env.R"))

sim_results <- readRDS("results/simulation_results.rds")

# -------------------------------------------------------------------------

heatmap_data <- sim_results %>%
  filter(n_topics %in% c(1, 5, 10)) %>%
  summarise(
    score = mean(adopters_prop_mean),
    .by = c(n_topics, density_factor, degree_factor, topic_factor)
  ) %>%
  mutate(
    # topic_label = paste0("Topic Span: ", n_topics) %>%
    #   reorder(n_topics),
    topic_label = case_when(
      n_topics == 1 ~ "1 Topic",
      n_topics == 5 ~ "5 Topics",
      n_topics == 10 ~ "10 Topics"
    ) %>%
      factor(levels = c("1 Topic", "5 Topics", "10 Topics")),
    topic_factor = factor(
      topic_factor,
      levels = c("Right-Skewed", "Symmetric", "Left-Skewed"),
      labels = c("Narrow", "Moderate", "Broad")
    ),
    density_factor = factor(
      density_factor,
      levels = c("Sparse", "Standard", "Dense"),
      labels = c("3", "6", "12")
    ),
    degree_factor  = factor(
      degree_factor,
      levels = c("Homogeneous", "Heterogeneous", "Highly-Heterogeneous"),
      labels = c("0.01", "0.40", "0.80")
    )
  )

p_heatmap <- heatmap_data %>%
  ggplot(aes(x = degree_factor, y = density_factor, fill = score * 100)) +
  geom_raster() +
  geom_text(aes(label = round(score * 100)),
            color = "white",
            fontface = "bold") +
  # facet_wrap( ~ topic_factor) +
  facet_grid(topic_label ~ topic_factor) +
  ggsci::scale_color_npg() +
  labs(
    x = TeX("Degree Heterogeneity ($sigma$)"),
    y = TeX("Average Degree ($\\bar{k}$)"),
    fill = "Final Adoption (%)"
    # title = ""
  ) +
  this_theme()

p_heatmap

this_ggsave(p_heatmap, width = 22, height = 24)
