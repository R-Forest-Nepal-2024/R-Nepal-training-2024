
## EQUATIONS 
## Naming convention:
## + column_name OR ColumnName
## + start column name with attribute level
eq <- eq_init |>
  rename(tree_species_name = Species_Name, tree_species_code = species_code, species_name_combi = Species_name) |>
  rename_with(str_to_lower)



##
## TREE - Preparation #######
##

tmp_tree <- tree_init |> 
  mutate(
    cluster_id_new = case_when(
      col < 10  ~ paste0("00", col, "-", row), 
      col < 100 ~ paste0("0", col, "-", row), 
      TRUE ~ paste0(col, "-", row)
    ),
    plot_id_new = paste0(cluster_id_new, "-", plot_numbe),
    tree_id_new = case_when(
      tree_numbe < 10  ~ paste0("00", plot_id_new, "-", tree_numbe),
      tree_numbe < 100 ~ paste0("0", plot_id_new, "-", tree_numbe),
      TRUE ~ paste0(plot_id_new, "-", tree_numbe)
    ),
    ## CORRECTION
    Physiograh = if_else(is.na(Physiograh), "High Mountain", Physiograh)
  )

##
## TREE #######
##

tree <- tmp_tree |>
  select(
    cluster_id_new, plot_id_new, tree_id_new, 
    tree_fid = FID_tree_d, tree_fid2 = S_N_, plot_id = Plot_id, plot_col = col, 
    plot_row = row, plot_no = plot_numbe, tree_no = tree_numbe, tree_distance = distance, 
    tree_azimuth = bearing, tree_species_code = Species_Co, tree_species_scientific_name = Species_na,
    tree_dbh = DBH, tree_quality = quality_cl, tree_crown_class = crown_clas, tree_sample_code = sample_tre, 
    tree_total_height = height, tree_base_distance = Ht_base, tree_bole_height = height_cro
  ) |>
  mutate(
    ## Plot radius
    plot_radius = case_when(
      tree_dbh < 10 ~ 4,
      tree_dbh < 20 ~ 8,
      tree_dbh < 30 ~ 15,
      tree_dbh >= 30 ~ 20,
      TRUE ~ NA_integer_
    ),
    ## DBH class according to plot radius
    tree_dbh_class1 = case_when(
      tree_dbh < 10 ~ "05-09",
      tree_dbh < 20 ~ "10-19",
      tree_dbh < 30 ~ "20-29",
      tree_dbh >= 30 ~ "30+",
      TRUE ~ NA_character_
    )
  )

##
## PLOT ######
##

length(unique(tmp_tree$plot_id_new))
plot <- tmp_tree |>
  select(
    cluster_id_new, plot_id_new,
    plot_id = Plot_id, plot_col = col, plot_row = row, plot_no = plot_numbe,
    plot_gps_zone = zone, plot_gps_x = easting, plot_gps_y = northing, 
    plot_gps_lon = lon, plot_gps_lat = lat, admin_municipality = vdc_munici, 
    admin_forest_name = forest_nam, plot_azimuth = bearing_to, 
    plot_distance = distance_t, plot_azimuth1 = bearing__1, plot_distance1 = distance_1,
    plot_aspect = aspect, plot_slope = slope, plot_altitude = altitude, 
    plot_topography = macro_topo, forest_status = forest_sta, 
    plot_accessibility = reachabili, plot_fao_landuse = fao_landus, 
    plot_lrmp_landuse = lrmp_landu, plot_forest_management = management, 
    plot_forest_type = forest_typ, plot_forest_origin = origin, 
    plot_crown_cover = crown_cove, plot_developement = developmen, 
    plot_selected = selected_f, plot_physio = Physiograh, 
    admin_province = Province, admin_district = DISTRICT, 
    admin_municipality_new = GaPa_NaPa, plot_ward_no = WARD_No    
  ) |>
  distinct()



## Check 
check <- length(unique(tmp_tree$plot_id_new)) == nrow(plot)
check
message("plot table has correct number of plots: ", check)

## Find duplicate
vec_plot_id <- plot |>
  group_by(plot_id_new) |>
  summarise(count = n()) |>
  filter(count > 1) |>
  pull(plot_id_new)
vec_plot_id

tt <- tmp_tree |> filter(plot_id_new %in% vec_plot_id)

##
## CLUSTER ######
##

length(unique(plot$cluster_id_new))

## + Get number of plots per cluster ####
tmp_cluster1 <- plot |> 
  group_by(cluster_id_new, plot_physio) |>
  summarise(
    plot_count = n(), 
    .groups = "drop"
  ) |>
  rename(cluster_physio = plot_physio)

## Checks
table(tmp_cluster1$plot_count)

# vec_cluster_id <- tmp_cluster1 |> filter(plot_count > 6) |> pull(cluster_id_new)
# tt <- tree |> filter(cluster_id_new %in% vec_cluster_id)

## + Get coordinate of cluster center ####
## ++ Add Berhmann meter projection
sf_plot <- plot |>
  mutate(lon = plot_gps_lon, lat = plot_gps_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform("ESRI:54017")

tmp_plot <- sf_plot |>
  mutate(
    plot_gps_x_behrmann = st_coordinates(sf_plot)[,1],
    plot_gps_y_behrmann = st_coordinates(sf_plot)[,2],
    ) |>
  as_tibble() |>
  select(-geometry)

tmp_cluster2 <- tmp_plot |>
  mutate(
    cluster_gps_xx = case_when(
      plot_no %in% 1:3 ~ plot_gps_x_behrmann + 150,
      plot_no %in% 4:6 ~ plot_gps_x_behrmann - 150,
      TRUE ~ NA_integer_
    ),
    cluster_gps_yy = case_when(
      plot_no %in% c(1, 4) ~ plot_gps_y_behrmann + 150,
      plot_no %in% c(2, 5) ~ plot_gps_y_behrmann,
      plot_no %in% c(3, 6) ~ plot_gps_y_behrmann - 150,
      TRUE ~ NA_integer_
    ),
  ) |>
  group_by(cluster_id_new) |>
  summarise(
    cluster_gps_x = mean(cluster_gps_xx),
    cluster_gps_y = mean(cluster_gps_yy)
  )

summary(tmp_cluster2$cluster_gps_x)
summary(tmp_cluster2$cluster_gps_y)

## Reconvert to lat/lon
tmp_sf_cluster <- tmp_cluster2 |>
  mutate(cluster_gps_xx = cluster_gps_x, cluster_gps_yy = cluster_gps_y) |>
  st_as_sf(coords = c("cluster_gps_xx", "cluster_gps_yy"), crs = "ESRI:54017") |>
  st_transform(crs = 4326) 

tmp_cluster3 <- tmp_sf_cluster |>
  mutate(
    cluster_gps_lon = st_coordinates(tmp_sf_cluster)[,1],
    cluster_gps_lat = st_coordinates(tmp_sf_cluster)[,2],
  ) |>
  as_tibble() |>
  select(-geometry)

cluster <- tmp_cluster1 |>
  left_join(tmp_cluster3, by = "cluster_id_new")


##
## CEO NFI ######
##

## Plot ID should be 3numbers - 2 numbers - 1 number
## If plot_col < 100, need to add an extra 0 for sorting purpose

ceonfi <- ceonfi_init |>
  mutate(
    plot_col = as.numeric(str_extract(Plot_id, "[^-]+")),
    plot_id_new = if_else(
      plot_col < 100, paste0("0", Plot_id), Plot_id
    )
  ) |>
  select(plot_id_new, everything())


## Save tables ######
write_csv(tree, "data/tree.csv")
write_csv(plot, "data/plot.csv")
write_csv(cluster, "data/cluster.csv")
write_csv(ceonfi, "data/ceonfi.csv")


## Clean R environment ######
rm(list = str_subset(ls(), "tmp"))


