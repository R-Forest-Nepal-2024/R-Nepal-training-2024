
leaf_agb_plot <- NFI_ceo |>
  group_by(leaf_class_txt, leaf_class, lu_code) |>
  summarise(
    leaf_plot_count = n(),
    leaf_agb_mean = mean(bio_mass_ad_second),
    leaf_agb_sd   = sd(bio_mass_ad_second),
    .groups = "drop"
  ) |>
  mutate(
    leaf_agb_se = leaf_agb_sd / sqrt(leaf_plot_count),
    leaf_agb_me = leaf_agb_se * qt(0.95, leaf_plot_count -1 ),
    leaf_agb_ciperc = round(leaf_agb_me / leaf_agb_mean * 100, 0)
  )

cstock_leaf <- leaf_agb_plot |>
  mutate(
    type = "land use",
    desc = leaf_class_txt,
    id = lu_code,
    value = leaf_agb_mean,
    se = leaf_agb_se, 
    pdf = "Normal",
    unit = "t/ha"
    ) |>
  select(type, desc, id, value, se, pdf, unit)

cstock_init

cstock <- bind_rows(cstock_init, cstock_leaf) |>
  arrange(type)

