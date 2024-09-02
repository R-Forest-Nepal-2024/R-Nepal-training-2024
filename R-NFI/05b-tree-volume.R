
message("hello again")

## Isolate default values
eq_terai <- eq |> filter(tree_species_code == "OTHERS-1")
eq_hill  <- eq |> filter(tree_species_code == "OTHERS-2")

## Get physiographic region
tmp_plot <- plot |> select(plot_id_new, plot_physio)

## Calculations
tmp_tree2 <- tmp_tree1 |>
  left_join(tmp_plot, by = "plot_id_new") |>
  mutate(
    tree_species_code = as.character(tree_species_code)
    ) |>
  left_join(eq, by = "tree_species_code") |>
  mutate(
    tree_stem_volume = case_when(
      !is.na(stem_a) ~ exp(
        stem_a + stem_b*log(tree_dbh) + stem_c*log(tree_height_final)
        ) / 1000,
      plot_physio == "Terai" ~ exp(
        eq_terai$stem_a + eq_terai$stem_b*log(tree_dbh) + eq_terai$stem_c*log(tree_height_final)
      ) / 1000,
      plot_physio == "Churia" ~ exp(
        eq_terai$stem_a + eq_terai$stem_b*log(tree_dbh) + eq_terai$stem_c*log(tree_height_final)
      ) / 1000,
      TRUE ~ exp(
        eq_hill$stem_a + eq_hill$stem_b*log(tree_dbh) + eq_hill$stem_c*log(tree_height_final)
      ) / 1000
    )
  )

summary(tmp_tree2$stem_a)
summary(tmp_tree2$tree_stem_volume)

ggplot(tmp_tree2) +
  geom_point(aes(x = tree_dbh, y = tree_stem_volume)) +
  facet_wrap(~plot_physio)



  