
AD_trans <- AD_ceo |>
  group_by(disturbance_type, strata_name, lu_initial, lu_final, transition) |>
  summarise(
    trans_plot_count_nih = n(),
    .groups = "drop"
  ) |>
  left_join(AD_strata, by = "strata_name") |>
  mutate(
    trans_prop_pih = trans_plot_count_nih / strata_plot_count,
    trans_area_aih = trans_prop_pih * strata_area_ha,
    trans_var_vih  = strata_area_ha^2 / (strata_plot_count - 1) * trans_prop_pih * (1 - trans_prop_pih)
  )

AD_trans

table(AD_trans$transition)

AD_trans2 <- AD_trans |> 
  group_by(region, transition, lu_initial, lu_final) |>
  summarise(
    trans_plot_count = sum(trans_plot_count_nih),
    trans_area = sum(trans_area_aih),
    trans_var  = sum(trans_var_vih, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    trans_se = sqrt(trans_var),
    trans_ci = qnorm(0.95) * trans_se,
    trans_ci_perc = trans_ci / trans_area,
    redd_activity = case_when(
      transition == "Anthropogenic-Dense Forest-Thin Forest"                         ~ "forest degradation",
      transition == "Anthropogenic-Dense Forest-Other Land"                          ~ "deforestation",
      transition == "Anthropogenic-Dense Forest-Settlements"                         ~ "deforestation",
      transition == "Anthropogenic-Dense Forest-Unshaded Cropland (TCC 10% or less)" ~ "deforestation",
      transition == "Anthropogenic-Thin Forest-Other Land"                          ~ "deforestation",
      transition == "Anthropogenic-Thin Forest-Settlements"                         ~ "deforestation",
      transition == "Anthropogenic-Thin Forest-Unshaded Cropland (TCC 10% or less)" ~ "deforestation",
      transition == "Anthropogenic-Secondary Forest-Other Land"                          ~ "deforestation",
      transition == "Anthropogenic-Secondary Forest-Settlements"                         ~ "deforestation",
      transition == "Anthropogenic-Secondary Forest-Unshaded Cropland (TCC 10% or less)" ~ "deforestation",
      TRUE ~ "Natural change or other activity"
    )
  ) |>
  left_join(lu_code, by = join_by(lu_initial == lu_name)) |>
  left_join(lu_code, by = join_by(lu_final == lu_name), suffix = c("_initial", "_final")) |>
  mutate(
    lu_id_initial = if_else(lu_id_initial %in% c("FD", "FT"), paste0(region, "-", lu_id_initial), lu_id_initial),
    lu_id_final = if_else(lu_id_final %in% c("FD", "FT"), paste0(region, "-", lu_id_final), lu_id_final)
    )

table(AD_trans2$lu_id_initial, AD_trans2$lu_id_final)

write_csv(AD_trans2, "results/AD_trans2.csv")

## Emissions only, filter transitions from REDD+ activities linked to emissions
AD_trans_E <- AD_trans2 |>
  filter(redd_activity %in% c("deforestation", "forest degradation")) |>
  mutate(trans_id = paste0(lu_id_initial, "-", lu_id_final)) |>
  select(
    redd_activity, trans_id, lu_id_initial, lu_id_final, trans_plot_count, 
    trans_area, trans_se
    ) |>
  arrange(redd_activity)
AD_trans_E

write_csv(AD_trans_E, "results/AD-emission-transitions.csv")


# AD_DG <- AD_trans2 |>
#   filter(redd_activity == "forest degradation")
# 
# AD_DF <- AD_trans2 |>
#   filter(redd_activity == "deforestation")
