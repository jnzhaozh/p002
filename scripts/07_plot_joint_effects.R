source(here::here("scripts/01_setup_project.R"))

simulation_results <- readRDS(here::here("results/simulation_results.rds"))

# -------------------------------------------------------------------------

data_joint_effects <- simulation_results %>%
  filter(n_topics %in% c(1, 5, 10)) %>%
  summarise(
    score = mean(adopters_prop_mean),
    .by = c(n_topics, density_factor, degree_factor, topic_factor)
  ) %>%
  mutate(
    # topic_label = paste0("Topic Span: ", n_topics) %>%
    #   reorder(n_topics),
    topic_label = case_when(
      n_topics == 1 ~ "1 topic",
      n_topics == 5 ~ "5 topics",
      n_topics == 10 ~ "10 topics"
    ) %>%
      factor(levels = c("1 topic", "5 topics", "10 topics")),

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

    degree_factor = factor(
      degree_factor,
      levels = c("Homogeneous", "Heterogeneous", "Highly-Heterogeneous"),
      labels = c("0.01", "0.4", "0.8")
    )
  )

p_joint_effects <- data_joint_effects %>%
  ggplot(aes(x = degree_factor, y = density_factor, fill = score * 100)) +
  geom_tile(color = "white", linewidth = 0.1) +
  geom_text(
    aes(
      label = round(score * 100),
      color = if_else(score * 100 >= 30, "white", "black")
    ),
    fontface = "bold",
    show.legend = FALSE
  ) +
  facet_grid(topic_label ~ topic_factor) +
  scale_x_discrete(name = TeX("Degree heterogeneity ($\\sigma$)")) +
  scale_y_discrete(name = TeX("Average degree ($\\bar{k}$)")) +
  scale_fill_gradientn(
    name = "Final adoption (%)",
    breaks = c(10, 30, 60),
    labels = c("10", "30", "60"),
    colours = c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
    guide = guide_colorbar(
      barwidth = unit(3, "in"),
      barheight = unit(0.2, "in")
    )
  ) +
  scale_color_identity() +
  coord_cartesian(expand = FALSE) +
  this_theme(base_size = 12)

p_joint_effects

this_ggsave(p_joint_effects, width = 9, height = 9.5)
