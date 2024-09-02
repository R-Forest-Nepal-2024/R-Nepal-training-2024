
##
## Calc strata area based on pixel count ######
##

AD_strata <- AD_strata_init |>
  mutate(
    strata_area_ha = (strata_size_pixel - mca_pixel) * 30^2 / 10000
  )

##
## AD: Add transitions, and land use based on disturbance type ######
##

table(AD_ceo_init$t2_disturbance_type_subcat, useNA = "ifany")
table(AD_ceo_init$Non.forest.land.use.type.in.2021, useNA = "ifany")
table(AD_ceo_init$Nat.Anth.2017.2021., useNA = "ifany")
table(AD_ceo_init$DegType2017_2021, useNA = "ifany")

AD_ceo <- AD_ceo_init |>
  select(
    strata_name      = FinalStrata_split_substrata_readable2,
    disturbance_type = t2_disturbance_type_subcat, 
    t0_sample_count  = Number.of.tree.covered.samples..2011.2012., 
    t1_sample_count  = t1_numbertrees, 
    t2_sample_count  = t2_numbertrees, 
    lu_final_nf      = Non.forest.land.use.type.in.2021,
    df_nat_anth      = Nat.Anth.2017.2021.,
    dg_nat_anth      = DegType2017_2021
    ) |>
  mutate(
    lu_initial = case_when(
      disturbance_type == "permanent forest loss" & t0_sample_count >= 6 ~ "Dense Forest",
      disturbance_type == "permanent forest loss" & t0_sample_count < 6  ~ "Thin Forest",
      disturbance_type == "stable forest"         & t1_sample_count >= 6 ~ "Dense Forest",
      disturbance_type == "stable forest"         & t1_sample_count < 6  ~ "Thin Forest",
      disturbance_type == "forest degradation"    & t1_sample_count >= 6 ~ "Dense Forest",
      disturbance_type == "forest degradation"    & t1_sample_count < 6  ~ "Thin Forest",
      disturbance_type == "natural secondary forest loss"                ~ "Secondary Forest",
      disturbance_type == "natural secondary forest gain"                ~ "Non Forest",
      disturbance_type == "shaded cropland gain"                         ~ "Non Forest",
      disturbance_type == "stable non forest"                            ~ "Non Forest",
    ),
    lu_final = case_when(
      disturbance_type == "permanent forest loss"                        ~ lu_final_nf,
      disturbance_type == "stable forest"         & t2_sample_count >= 6 ~ "Dense Forest",
      disturbance_type == "stable forest"         & t2_sample_count < 6  ~ "Thin Forest",
      disturbance_type == "forest degradation"    & t2_sample_count >= 6 ~ "Dense Forest",
      disturbance_type == "forest degradation"    & t2_sample_count < 6  ~ "Thin Forest",
      disturbance_type == "natural secondary forest loss"                ~ lu_final_nf,
      disturbance_type == "natural secondary forest gain"                ~ "Secondary Forest",
      disturbance_type == "shaded cropland gain"                         ~ "Shaded Cropland",
      disturbance_type == "stable non forest"                            ~ "Non Forest",
    ),
    is_natural = case_when(
      !is.na(dg_nat_anth) & disturbance_type == "forest degradation" ~ dg_nat_anth,
      !is.na(df_nat_anth)  ~ df_nat_anth,
      TRUE ~ NA_character_
    ),
    transition = if_else(
      is.na(is_natural), 
      paste0(lu_initial, "-", lu_final), 
      paste0(is_natural, "-", lu_initial, "-", lu_final)
    )
  )

table(AD_ceo$is_natural, useNA = "ifany")

test <- AD_ceo |>
  filter(
    is_natural == "Anthropogenic",
    lu_initial == "Dense Forest", 
    lu_final == "Thin Forest"
    )

## 
## EF: add LEAF Class ######
## 

table(NFI_ceo_init$Phy_Name, useNA = "ifany")
table(NFI_ceo_init$pl_phy_name, useNA = "ifany")
table(NFI_ceo_init$pl_phy_name, NFI_ceo_init$Phy_Name, useNA = "ifany")
table(NFI_ceo_init$t2_disturbance_type_subcat, useNA = "ifany")
summary(NFI_ceo_init$t2_numbertrees)

NFI_ceo <- NFI_ceo_init |>
  filter(!is.na(t2_numbertrees)) |>
  ## In German calculation, plot: 472a is missing from summary table
  ## Removing here to be consistent
  filter(plotid_unique != "472a") |>
  mutate(
    ## Correction missing Phy_Name for 1 plot
    Phy_Name = if_else(Plot_id == "104-57-1", "Middle Mountain", Phy_Name),
    forest_density = if_else(t2_numbertrees < 6, "thin", "dense"),
    leaf_region = if_else(Phy_Name == "High Mountain & High Himal", "R2-HH", "R1-MST"),
    leaf_class = case_when(
      leaf_region == "R1-MST" & forest_density == "dense" ~ 1,
      leaf_region == "R1-MST" & forest_density == "thin"  ~ 2,
      leaf_region == "R2-HH" & forest_density == "dense" ~ 3,
      leaf_region == "R2-HH" & forest_density == "thin"  ~ 4,
      ),
    leaf_class_txt = paste0(leaf_region, "-", forest_density),
    lu_code = case_when(
      leaf_region == "R1-MST" & forest_density == "dense" ~ "RG1-FD",
      leaf_region == "R1-MST" & forest_density == "thin"  ~ "RG1-FT",
      leaf_region == "R2-HH" & forest_density == "dense" ~ "RG2-FD",
      leaf_region == "R2-HH" & forest_density == "thin"  ~ "RG2-FT",
    ),
  )

table(NFI_ceo$leaf_class_txt, useNA = "ifany")


## Check one plot too many compared to German table
# vec_RG2_FT_XL <- c(
#   "75-61-2", "80-64-6", "83-61-2", "119-58-6", "150-43-6", "81-61-1", 
#   "110-62-2", "118-62-3", "118-62-6", "146-46-6", "154-41-4", "155-38-5"
#   )
# 
# check <- NFI_ceo |>
#   filter(lu_code == "RG2-FT", !(pl_plot_id...20 %in% vec_RG2_FT_XL)) 
