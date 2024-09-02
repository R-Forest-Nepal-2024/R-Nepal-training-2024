
## Create spatial 'plot' table
sf_plot <- plot |>
  mutate(lon = plot_gps_lon, lat = plot_gps_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

ggplot() +
  geom_sf(data = sf_plot)

## Create spatial 'cluster' table
sf_cluster <- cluster |>
  mutate(lon = cluster_gps_lon, lat = cluster_gps_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 


## Extract environmental stress value if needed

if ("plot_E.csv" %in% list.files("data")) {
  
  plot_E <- read_csv("data/plot_E.csv", show_col_types = FALSE)
  
} else {
  
  tmp_plot <- terra::extract(rs_E, vect(sf_plot))
  tmp_plot
  
  plot_E <- bind_cols(sf_plot, tmp_plot) |> 
    select(plot_id_new, plot_envir_stress = E)
  
  write_csv(plot_E, "data/plot_E.csv")
  
  rm(tmp_plot)
}

## Check E at plot level
sf_plot_E <- plot |>
  left_join(plot_E, by = "plot_id_new") |>
  mutate(lon = plot_gps_lon, lat = plot_gps_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

gg <- ggplot() +
  geom_sf(data = sf_nepal, fill = NA, linewidth = 0.6) +
  geom_sf(data = sf_plot_E, aes(color = plot_envir_stress), alpha = 0.4) +
  scale_color_viridis_c() +
  labs(
    color = "E",
    subtitle = "Plot environmental stress",
    caption = "E from Chave et al. 2014"
  )
print(gg)

