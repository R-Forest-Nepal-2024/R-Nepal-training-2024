

## Make equation values for missing species
eq_tarai <- eq |> filter(species_name_combi == "Miscellaneous in Terai")
eq_hill <- eq |> filter(species_name_combi == "Miscellaneous in Hill")

## Prepare Physio for tree level
tmp_plot <- plot |> select(plot_id_new, plot_physio)


##
## Add corrected H ######
##

## !!! Missing borken top correction
tmp_tree <- tree |>
  left_join(tmp_plot, by = "plot_id_new") |>
  mutate(
    tree_total_height_corr = case_when(
      tree_base_distance > 0 ~ sqrt(tree_total_height^2 + tree_base_distance^2),
      TRUE ~ tree_total_height
    )
  )


##
## Add H model ######
##
tmp_tree2 <- tmp_tree |>
  left_join(plot_E, by = "plot_id_new") |>
  mutate(
    tree_height_model = exp(0.893 - plot_envir_stress + 0.760 * log(tree_dbh) - 0.0340 * (log(tree_dbh))^2),
    tree_height_ciupper = tree_height_model * exp(1.96 * 0.243),
    tree_height_cilower = tree_height_model * exp(-1.96 * 0.243),
    tree_total_height_all = case_when(
      is.na(tree_total_height_corr) ~ tree_height_model,
      tree_total_height_corr == 0   ~ tree_height_model,
      tree_total_height_corr > tree_height_ciupper ~ tree_height_model,
      tree_total_height_corr < tree_height_cilower ~ tree_height_model,
      TRUE ~ tree_total_height_corr
     )
  )


## Plot with highest number of trees measured
tt <- tree |> 
  group_by(plot_id_new) |>
  summarise(
    count_tree = n()
  ) |>
  arrange(desc(count_tree))

vec_plot_manytrees <- tt |> 
  slice_head(n = 1) |>
  pull(plot_id_new)

gg <- tmp_tree2 |> 
  filter(!is.na(tree_total_height_corr), tree_total_height_corr != 0, plot_id_new == "106-61-6") |>
  ggplot() +
  geom_segment(aes(x = tree_dbh, xend = tree_dbh, y = tree_total_height_corr, yend = tree_total_height_all), alpha = 0.4) +
  geom_point(aes(x = tree_dbh, y = tree_total_height_corr)) +
  geom_point(aes(x = tree_dbh, y = tree_total_height_all), color = "darkred") +
  geom_line(aes(x = tree_dbh, y = tree_height_model), col = "darkred") +
  geom_line(aes(x = tree_dbh, y = tree_height_ciupper), col = "lightgreen") +
  geom_line(aes(x = tree_dbh, y = tree_height_cilower), col = "lightgreen") +
  labs(
    x = "Tree DBH (cm)",
    y = "Tree total height (m)",
    caption = "red line: model, green line: CI\n dot: correct (red) and measured (black) height"
  )
print(gg)

gg <- tmp_tree2 |> 
  filter(!is.na(tree_total_height_corr), tree_total_height_corr != 0) |>
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_total_height_corr)) +
  geom_point(aes(x = tree_dbh, y = tree_height_model, color = plot_physio)) +
  theme(legend.position = "none") +
  facet_wrap(~plot_physio) +
  labs(
    x = "Tree DBH (cm)",
    y = "Tree total height (m)",
    caption = "Measured height (black) and modeled height (color)"
  )
print(gg)
ggsave(
  gg, filename = "results/H-DBH measured and Chave 2014.png",
  width = 15, height = 12, units = "cm", dpi = 300
  )

gg <- tmp_tree2 |> 
  filter(!is.na(tree_total_height_corr), tree_total_height_corr != 0) |>
  ggplot() +
  geom_point(aes(x = tree_dbh, y = tree_total_height_corr)) +
  geom_point(aes(x = tree_dbh, y = tree_total_height_all, color = plot_physio)) +
  theme(legend.position = "none") +
  facet_wrap(~plot_physio) +
  labs(
    x = "Tree DBH (cm)",
    y = "Tree total height (m)",
    caption = "Measured height (black) and corrected height (color)"
  )
print(gg)
ggsave(
  gg, filename = "results/H-DBH corrected and measured.png",
  width = 15, height = 12, units = "cm", dpi = 300
)


## 
## Volume ######
##
eq2 <- eq |> select(tree_species_code, stem_a, stem_b, stem_c, density)


tmp_tree3 <- tmp_tree2 |>
  mutate(tree_species_code = as.character(tree_species_code)) |>
  left_join(eq2, by = join_by(tree_species_code)) |>
  mutate(
    stem_a = case_when(
      is.na(stem_a) & plot_physio %in% c("Terai", "Churia") ~ eq_tarai$stem_a,
      is.na(stem_a) & !(plot_physio %in% c("Terai", "Churia")) ~ eq_hill$stem_a,
      TRUE ~ stem_a
    ),
    stem_b = case_when(
      is.na(stem_b) & plot_physio %in% c("Terai", "Churia") ~ eq_tarai$stem_b,
      is.na(stem_b) & !(plot_physio %in% c("Terai", "Churia")) ~ eq_hill$stem_b,
      TRUE ~ stem_b
    ),
    stem_c = case_when(
      is.na(stem_c) & plot_physio %in% c("Terai", "Churia") ~ eq_tarai$stem_c,
      is.na(stem_c) & !(plot_physio %in% c("Terai", "Churia")) ~ eq_hill$stem_c,
      TRUE ~ stem_c
    ),
    tree_stem_volume = exp(stem_a + stem_b * log(tree_dbh) + stem_c * log(tree_total_height_all)) / 1000
  )

table(tmp_tree3$plot_physio, useNA = "ifany")
summary(tmp_tree3$tree_stem_volume)



##
##  Biomass ######
##

tmp_tree4 <- tmp_tree3 |>
  mutate(
    tree_wd = case_when(
      is.na(density) & plot_physio %in% c("Terai", "Churia") ~ eq_tarai$density,
      is.na(density) & !(plot_physio %in% c("Terai", "Churia")) ~ eq_hill$density,
      TRUE ~ density
    ),
    tree_agb = tree_stem_volume * tree_wd, ## Just stem for now
    tree_ba = pi * (tree_dbh/200)^2
  )

ggplot(tmp_tree4) +
  geom_point(aes(x = tree_dbh, y = tree_agb)) +
  facet_wrap(~plot_physio)



##
## SCALE FACTOR ######
##

tmp_tree5 <- tmp_tree4 |>
  mutate(
    plot_radius = case_when(
      tree_dbh < 10 ~ 4,
      tree_dbh < 20 ~ 8,
      tree_dbh < 30 ~ 15,
      tree_dbh >= 30 ~ 20,
      TRUE ~ NA_integer_
    ),
    tree_scale_factor = 10000 / (pi * plot_radius^2)
  )

## 
## FINAL TABLE ######
##

tree_agb <- tmp_tree5 |>
  mutate(
    tree_agb_ha = tree_agb * tree_scale_factor / 1000, ## convert kg to tons
    tree_ba_ha = tree_ba * tree_scale_factor,
    tree_count_ha = tree_scale_factor
    )

rm(list = str_subset(ls(), "tmp_"))
