
##
## Emissions based on averages ######
##
tmp_cstock <- cstock |> 
  filter(type == "land use") |>
  select(lu_id = id, agb = value)
  
RS <- cstock |> filter(id == 'RS') |> pull(value)
CF <- cstock |> filter(id == 'CF') |> pull(value)
RP <- cstock |> filter(id == 'RP') |> pull(value)

E_trans <- AD_trans2 |>
  select(redd_activity, lu_id_initial, lu_id_final, trans_AD = trans_area) |>
  filter(redd_activity %in% c("deforestation", "forest degradation")) |> 
  left_join(tmp_cstock, by = join_by(lu_id_initial == lu_id)) |>
  left_join(tmp_cstock, by = join_by(lu_id_final == lu_id), suffix = c("_initial", "_final")) |>
  mutate(
    trans_EF = (agb_initial - agb_final) * (1 + RS) * CF * 44/12,
    trans_E  = trans_AD * trans_EF
  ) |>
  arrange(redd_activity)
E_trans

E_activity <- E_trans |>
  group_by(redd_activity) |>
  summarise(E = sum(trans_E)) |>
  mutate(E_mean = E / RP)
E_activity

FREL <- sum(E_activity$E_mean)
FREL
