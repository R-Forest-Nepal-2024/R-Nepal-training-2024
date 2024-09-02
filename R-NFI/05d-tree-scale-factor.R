
message("Now doing 05d")

tree_agb <- tmp_tree3 |>
  mutate(
    tree_scale_factor = 10000 / (pi * plot_radius^2),
    tree_agb_ha = tree_agb * tree_scale_factor / 1000, ## conv kg to ton
    tree_ba_ha  = (pi * tree_dbh^2) / 40000 * tree_scale_factor,
    tree_count_ha = tree_scale_factor
  )

table(tree_agb$tree_scale_factor, useNA = "ifany")

## Remove temporary objects
rm(list = str_subset(ls(), "tmp_"))
