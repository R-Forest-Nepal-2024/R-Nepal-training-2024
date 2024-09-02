
message("hello")



tmp_tree1 <- tree |>
  ## Keep crown class 1:6 and 13:14 
  ## others are deadwood, stumps and climbers
  filter(tree_crown_class %in% c(1:6, 13:14)) |>
  left_join(plot_E, by = "plot_id_new") |>
  mutate(
    ## height model from Chave et al. 2014
    tree_height_model = exp(0.893 - plot_envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    ## model RSE: 0.243, t_alpha: 1.96 (for CI 95%)
    tree_height_ciupper = tree_height_model * exp(1.96 * 0.243),
    tree_height_cilower = tree_height_model * exp(-1.96 * 0.243),
    ## Height correction for lean trees (FRA data analysis manual)
    tree_height_corr = if_else(
      tree_base_distance > 0, 
      sqrt(tree_total_height^2 + tree_base_distance^2),
      tree_total_height
    ),
    ## Final height
    tree_height_final = case_when(
      tree_crown_class == 6                   ~ tree_total_height, 
      is.na(tree_total_height)                ~ tree_height_model,
      tree_total_height == 0                  ~ tree_height_model,
      tree_total_height > tree_height_ciupper ~ tree_height_model,
      tree_total_height < tree_height_cilower ~ tree_height_model,
      TRUE ~ tree_height_corr
    )
  )

summary(tmp_tree1$tree_height_model)

## !! EX
## + Make a basic ggplot of 'tree_dbh' and 'tree_height_model'
## + Make same plot with color based on 'plot_id_new' and geometry line
##   - add to ggplot: theme(legend.position = "none")
## !!
ggplot(tmp_tree1) +
  geom_point(aes(x = tree_dbh, y = tree_height_model))

ggplot(tmp_tree1) +
  geom_line(
    aes(x = tree_dbh, y = tree_height_model, color = plot_id_new)
    ) +
  theme(legend.position = "none")


gg <- tmp_tree1 |>
  filter(plot_id_new == "106-61-6") |> ## "106-61-6" "150-43-1"
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = tree_total_height)) +
  geom_line(aes(y = tree_height_model), col = "darkred") +
  geom_line(aes(y = tree_height_ciupper), col = "lightblue") +
  geom_line(aes(y = tree_height_cilower), col = "lightblue") +
  geom_point(aes(y = tree_height_final), col = "red", alpha = 0.5) +
  labs(
    x = "Tree DBH (cm)",
    y = "Tree totla height (m)",
    caption = "Model in red, \nObservations in black"
  )
print(gg)

ggplot(tmp_tree1) +
  geom_point(
    aes(x = tree_dbh, y = tree_total_height)
    ) +
  geom_point(
    aes(x = tree_dbh, y = tree_height_final), 
    color = "lightgreen"
    )





