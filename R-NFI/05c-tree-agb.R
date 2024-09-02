
message("here is 05c")

tmp_tree3 <- tmp_tree2 |>
  mutate(
    tree_agb_stem = case_when(
      !is.na(density)                       ~ tree_stem_volume * density,
      plot_physio %in% c("Terai", "Churia") ~ tree_stem_volume * eq_terai$density,
      TRUE                                  ~ tree_stem_volume * eq_hill$density
    ),
    ratio_branch_s = case_when(
      !is.na(branch_s)                      ~ branch_s,
      plot_physio %in% c("Terai", "Churia") ~ eq_terai$branch_s,
      TRUE                                  ~ eq_hill$branch_s
    ),
    ratio_branch_m = case_when(
      !is.na(branch_m)                      ~ branch_m,
      plot_physio %in% c("Terai", "Churia") ~ eq_terai$branch_m,
      TRUE                                  ~ eq_hill$branch_m
    ),
    ratio_branch_l = case_when(
      !is.na(branch_l)                      ~ branch_l,
      plot_physio %in% c("Terai", "Churia") ~ eq_terai$branch_l,
      TRUE                                  ~ eq_hill$branch_l
    ),
    ratio_foliage_s = case_when(
      !is.na(foliage_s)                      ~ foliage_s,
      plot_physio %in% c("Terai", "Churia") ~ eq_terai$foliage_s,
      TRUE                                  ~ eq_hill$foliage_s
    ),
    ratio_foliage_m = case_when(
      !is.na(foliage_m)                      ~ foliage_m,
      plot_physio %in% c("Terai", "Churia") ~ eq_terai$foliage_m,
      TRUE                                  ~ eq_hill$foliage_m
    ),
    ratio_foliage_l = case_when(
      !is.na(foliage_l)                      ~ foliage_l,
      plot_physio %in% c("Terai", "Churia") ~ eq_terai$foliage_l,
      TRUE                                  ~ eq_hill$foliage_l
    ),
    ratio_branch = case_when(
      tree_dbh < 10 ~ ratio_branch_s,
      tree_dbh < 40 ~ ((tree_dbh - 10) * ratio_branch_m + (40 - tree_dbh) * ratio_branch_s) / 30,
      tree_dbh < 70 ~ ((tree_dbh - 40) * ratio_branch_l + (70 - tree_dbh) * ratio_branch_m) / 30,
      TRUE          ~ ratio_branch_l
    ),
    ratio_foliage = case_when(
      tree_dbh < 10 ~ ratio_foliage_s,
      tree_dbh < 40 ~ ((tree_dbh - 10) * ratio_foliage_m + (40 - tree_dbh) * ratio_foliage_s) / 30,
      tree_dbh < 70 ~ ((tree_dbh - 40) * ratio_foliage_l + (70 - tree_dbh) * ratio_foliage_m) / 30,
      TRUE          ~ ratio_foliage_l
    ),
    tree_agb = tree_agb_stem * (1 + ratio_branch + ratio_foliage)
  )

summary(tmp_tree3$tree_agb_stem)
summary(tmp_tree3$ratio_branch)
summary(tmp_tree3$tree_agb)
