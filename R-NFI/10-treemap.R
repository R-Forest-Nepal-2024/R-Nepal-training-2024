

tree |> 
  filter(plot_id_new == "128-51-1") |>
  ggplot() +
  geom_point(aes(x = tree_distance, y = tree_azimuth))

gg <- tree |> 
  filter(plot_id_new == "128-51-1") |>
  ggplot() +
  geom_vline(xintercept = c(0, 4, 8, 15, 20), colour = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = c(0, 90, 180, 270), colour = "grey80", linewidth = 0.2) +
  geom_point(aes(x = tree_distance, y = tree_azimuth, color = tree_dbh_class1, size = tree_dbh_class1)) +
  scale_x_continuous(breaks = c(0, 4, 8, 15, 20), limits = c(0, 20)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(
    x = "", 
    y = "",
    color = "",
    size = "",
    subtitle = paste0("tree maps for plot: ", "128-51-1")
  )
print(gg)
