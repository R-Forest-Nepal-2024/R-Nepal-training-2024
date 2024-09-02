

##
## PLOT LEVEL ######
##

tmp_plot <- tree_agb |>
  group_by(plot_id_new) |>
  summarise(
    plot_ntree = n(),
    plot_ntree_ha = sum(tree_count_ha),
    plot_ba_ha = sum(tree_ba_ha),
    plot_agb_ha = sum(tree_agb_ha)
  )

plot_agb <- plot |>
  left_join(plot_E, by = "plot_id_new") |>
  left_join(tmp_plot, by = "plot_id_new") |>
  mutate(
    cluster_nplot_target = if_else(plot_physio == "Terai", 4, 6) 
  ) |>
  filter(!is.na(plot_agb_ha))

## Check
gg <- ggplot(plot_agb) +
  geom_point(aes(x = plot_ba_ha, y = plot_agb_ha)) +
  labs(
    x = "Basal area (m2)",
    y = "AGB (t/ha)",
    subtitle = "Plot level aboveground biomass vs basal area"
  )
print(gg)

ggplot(plot_agb) +
  geom_boxplot(aes(x = plot_physio, y = plot_agb_ha)) +
  guides(x = guide_axis(n.dodge = 2))

##
## Compare plot level with CEONFI ######
##
tmp_ceonfi <- ceonfi |>
  select(
    plot_id_new, t2_numbertrees, bio_mass_ad_second
  )

table(plot_agb$plot_physio)

plot_agb2 <- plot_agb |>
  left_join(tmp_ceonfi, by = "plot_id_new") |>
  mutate(
    forest_density = if_else(t2_numbertrees < 6, "thin", "dense"),
    plot_physio2 = if_else(
      plot_physio %in% c("Terai", "Churia", "Middle Mountain"),
      plot_physio,
      "HM_HH"
    ),
    leaf_region = if_else(plot_physio2 == "HM_HH","R2-HH","R1-MST"),
    leaf_class = case_when(
      leaf_region == "R1-MST" & forest_density == "thin"  ~ 1,
      leaf_region == "R1-MST" & forest_density == "dense" ~ 2,
      leaf_region == "R2-HH"  & forest_density == "thin"  ~ 3,
      leaf_region == "R2-HH"  & forest_density == "dense" ~ 4,
      TRUE ~ NA_integer_
    ),
    leaf_class_txt = paste0(leaf_region, "-", forest_density)
  )

table(plot_agb2$leaf_class_txt, useNA = "ifany")

gg <- ggplot(plot_agb2) +
  geom_abline(intercept = 0, slope = 1, col = "darkred") +
  geom_point(aes(
    x = bio_mass_ad_second, 
    y = plot_agb_ha,
    color = plot_physio2
    ), size = 0.3) +
  facet_wrap((~plot_physio2)) +
  theme(legend.position = "none") +
  labs(
    x = "Plot AGB provided for LEAF area",
    y = "Plot AGB recalculated",
    caption = "HM_HH: High Mountain & High Himal"
  )
print(gg)

## !! EX
## + Make graph of 'plot_agb_ha' against 'leaf_class_txt' 
##   with geometry boxplot
## + Same graph with 'bio_mass_ad_second' instead of 'plot_agb_ha'
## !!
ggplot(plot_agb2) +
  geom_boxplot(aes(
    x = leaf_class_txt, y = plot_agb_ha
    )) +
  guides(x = guide_axis(n.dodge = 2))

ggplot(plot_agb2) +
  geom_boxplot(aes(
    x = leaf_class_txt, y = bio_mass_ad_second
  )) +
  guides(x = guide_axis(n.dodge = 2))

## + Check the NAs ######
test <- plot_agb2 |> 
  filter(is.na(t2_numbertrees))

test$plot_id_new

test2 <- plot |>
  filter(plot_id_new %in% test$plot_id_new)

test3 <- plot_agb2 |>
  filter(plot_id_new %in% test$plot_id_new)

summary(ceonfi$t2_numbertrees)
test4 <- ceonfi |>
  filter(plot_id_new %in% test$plot_id_new)

table(plot_agb2$leaf_class, useNA = "ifany")
table(plot_agb2$forest_density, useNA = "ifany")

## FINAL PLOT TABLE ######
plot_agb3 <- plot_agb2 |> filter(!is.na(t2_numbertrees))

## COMMENT:
## + 3 plots in CEONFI don't have t2_numbertrees 
## + 2 plots on plot_agb don't equivalent in CEONFI


##
## CLUSTER ######
##

## Cluster level LEAF class ######
## Identify the clusters that have more than 
## 1 LEAF class (region x thin/dense)
test_cluster <- plot_agb3 |>
  select(cluster_id_new, leaf_class_txt) |>
  distinct()

test_cluster2 <- test_cluster |>
  group_by(cluster_id_new) |>
  summarise(cluster_dup = n())

table(test_cluster2$cluster_dup, useNA = "ifany")

vec_dup <- test_cluster2 |>
  filter(cluster_dup > 1) |>
  pull(cluster_id_new)

test_cluster3 <- plot_agb3 |>
  filter(cluster_id_new %in% vec_dup)

## For now: removing clusters with more than 
## 1 LEAF class
plot_agb4 <- plot_agb3 |> 
  filter(!(cluster_id_new %in% vec_dup))

## Missing info on inaccessible plot. 
## Assuming that all missing plots are due to non-forest 
## with no tree (cstock = 0)

tmp_cluster <- plot_agb4 |>
  group_by(cluster_id_new, plot_physio2, leaf_class_txt, leaf_class) |>
  summarise(
    plot_count_final = n(),
    cluster_ntree_ha = sum(plot_ntree_ha / cluster_nplot_target),
    cluster_ba_ha    = sum(plot_ba_ha / cluster_nplot_target),
    cluster_agb_ha   = sum(plot_agb_ha / cluster_nplot_target),
    .groups = "drop"
  )

cluster_agb <- cluster |>
  left_join(tmp_cluster, by = "cluster_id_new") |>
  filter(!is.na(cluster_agb_ha)) |>
  rename(cluster_physio2 = plot_physio2)

sf_cluster_agb <- sf_cluster |>
  left_join(tmp_cluster, by = "cluster_id_new") |>
  filter(!is.na(cluster_agb_ha)) |>
  rename(cluster_physio2 = plot_physio2)

table(cluster_agb$leaf_class_txt, useNA = 'ifany')

## Checks
ggplot(cluster_agb) +
  geom_point(aes(
    x = cluster_ba_ha, 
    y = cluster_agb_ha,
    color = leaf_class_txt
    ), size = 0.4) +
  facet_wrap(~cluster_physio2)
 
ggplot(cluster_agb) +
  geom_boxplot(aes(x = cluster_physio2, cluster_agb_ha))

gg <- ggplot() +
  geom_sf(data = sf_nepal, color = "darkred", linewidth = 0.6) +
  geom_sf(data = sf_cluster_agb, aes(color = cluster_agb_ha), size = 0.6) +
  scale_color_viridis_c() +
  theme(legend.position = "bottom") +
  labs(
    color = "AGB (t/ha)"
  ) + 
  facet_wrap(~cluster_physio2)
print(gg)


##
## Forest type / Physiographic region
## 
table(cluster_agb$cluster_physio2, useNA = "ifany")
table(cluster_agb$leaf_class_txt, useNA = "ifany")

physio_agb <- cluster_agb |>
  group_by(cluster_physio2) |>
  summarise(
    cluster_count = n(),
    physio_agb_mean = mean(cluster_agb_ha),
    physio_agb_sd = sd(cluster_agb_ha)
  ) |>
  mutate(
    physio_agb_se = physio_agb_sd / sqrt(cluster_count),
    physio_agb_me = physio_agb_se * qt(0.95, cluster_count-1),
    physio_agb_ciperc = round(physio_agb_me / physio_agb_mean * 100, 0)
  )


gg <- ggplot(physio_agb, aes(x = cluster_physio2)) +
  geom_col(aes(y = physio_agb_mean, fill = cluster_physio2)) +
  geom_errorbar(aes(
    ymin = physio_agb_mean - physio_agb_me,
    ymax = physio_agb_mean + physio_agb_me, 
    ), width = 0.6) +
  geom_label(aes(y = 5, label = cluster_count)) +
  theme(legend.position = "none") +
  labs(
    x = "",
    y = "AGB (t/ha)",
    caption = "Number of clusters in label."
    
  )
print(gg)

##!!EX 
## + Reproduce the average AGB +/- CI for 'leaf_class_txt" 
##   based on 'cluster_agb' 
## + Reproduce the average AGB +/- CI for the LEAF Classes 
##   based on 'ceonfi' plot level
##   - From 'ceonfi' table, reconstruct 'leaf_class_txt'
##   - Aggregate to 'leaf_class_txt' using 'bio_mass_ad_second'
##!!
leaf_agb <- cluster_agb |>
  group_by(leaf_class_txt) |>
  summarise(
    cluster_count = n(),
    leaf_agb_mean = mean(cluster_agb_ha),
    leaf_agb_sd = sd(cluster_agb_ha)
  ) |>
  mutate(
    leaf_agb_se = leaf_agb_sd / sqrt(cluster_count),
    leaf_agb_me = leaf_agb_se * qt(0.95, cluster_count-1),
    leaf_agb_ciperc = round(leaf_agb_me / leaf_agb_mean * 100, 0)
  )

gg <- ggplot(leaf_agb, aes(x = leaf_class_txt)) +
  geom_col(aes(y = leaf_agb_mean, fill = leaf_class_txt)) +
  geom_errorbar(aes(
    ymin = leaf_agb_mean - leaf_agb_me,
    ymax = leaf_agb_mean + leaf_agb_me, 
  ), width = 0.6) +
  geom_label(aes(y = 5, label = cluster_count)) +
  theme(legend.position = "none") +
  labs(
    x = "",
    y = "AGB (t/ha)",
    caption = "Number of clusters in label."
  )
print(gg)

## Reconstruct LEAF class AGB same as TRESS Registration document
table(ceonfi$Phy_Name, useNA = "ifany")
summary(ceonfi$t2_numbertrees)

ceonfi_agb <- ceonfi |>
  filter(!is.na(t2_numbertrees)) |>
  select(plot_id_new, Phy_Name, t2_numbertrees, bio_mass_ad_second) |>
  mutate(
    forest_density = if_else(t2_numbertrees < 6 , "thin", "dense"),
    leaf_region = if_else(
      Phy_Name == "High Mountain & High Himal", "R2-HH", "R1-MST"
        ),
    leaf_class = case_when(
      leaf_region == "R1-MST" & forest_density == "thin"  ~ 1,
      leaf_region == "R1-MST" & forest_density == "dense" ~ 2,
      leaf_region == "R2-HH"  & forest_density == "thin"  ~ 3,
      leaf_region == "R2-HH"  & forest_density == "dense" ~ 4,
      TRUE ~ NA_integer_
    ),
    leaf_class_txt = paste0(leaf_region, "-", forest_density)
  )

ggplot(ceonfi_agb) +
  geom_boxplot(aes(x = leaf_class_txt, y = bio_mass_ad_second))

leaf_agb_plot <- ceonfi_agb |>
  group_by(leaf_class_txt, leaf_class) |>
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
 
  
  
