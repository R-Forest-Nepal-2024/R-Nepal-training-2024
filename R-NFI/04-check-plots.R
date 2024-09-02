
## PLOT LEVEL - mapping ######

##  Check plots in one cluster
sf_plot |>
  filter(plot_row == 64, plot_col == 34) |>
  ggplot() +
  geom_sf() +
  geom_sf_text(aes(label = plot_no))

## Interactive map
## Check basemaps here: https://leaflet-extras.github.io/leaflet-providers/preview/
tmap_mode("view")
tm_basemap(c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap", "Esri.WorldImagery", "OpenStreetMap")) +
  tm_shape(sf_nepal) +
  tm_borders(col = "red") +
  tm_shape(sf_plot) +
  tm_symbols(col = "plot_physio") +
  tm_scale_bar()

## CLUSTER LEVEL ######
gg <- ggplot() +
  geom_sf(data = sf_nepal, fill = NA) +
  geom_sf(data = sf_cluster, aes(color = cluster_physio)) +
  theme(legend.position = "bottom") +
  labs(
    subtitle = "Cluster location map",
    color = ""
  )
print(gg)


