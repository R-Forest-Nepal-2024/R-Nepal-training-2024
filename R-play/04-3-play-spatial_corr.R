
## Objective: Create maps with spatial data

## Steps:
## + load and install packages if necessary
## + Load vector and raster data 
## + Load tables and convert to spatial data
## + map cluster location (ggplot)
## + map plot location (interactive)
## + extract raster info at plot location 


## 01 load libraries ######
if (!require(sf)) install.packages("sf")
if (!require(terra)) install.packages("terra")
if (!require(tmap)) install.packages("tmap")
if (!require(tmap)) install.packages("tidyverse")

library(sf)
library(tmap)
library(terra)
library(tidyverse)

## 02 load data ######
## Load tree and plot data

plot <- read_csv("data/plot.csv")
cluster <- read_csv("data/cluster.csv")

sf_nepal <- read_sf("data-spatial/gadm41_NPL_0.json")
rs_E <- rast("data-spatial/E.bil")

st_crs(sf_nepal)

## 03 Convert to spatial ######

sf_cluster <- cluster |> 
  mutate(lon = cluster_gps_lon, lat = cluster_gps_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
sf_cluster


## !! EX
## Convert 'plot' to spatial (call it 'sf_plot') 
## !!
sf_plot <- plot |>
  mutate(lon = plot_gps_lon, lat = plot_gps_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


## 04 Map cluster location ######

## Same graph different code
ggplot(sf_cluster) +
  geom_sf()

sf_cluster |>
  ggplot() +
  geom_sf()

ggplot() +
  geom_sf(data = sf_cluster)

## order matters
ggplot() +
  geom_sf(data = sf_cluster) +
  geom_sf(data = sf_nepal)

ggplot() +
  geom_sf(data = sf_nepal) +
  geom_sf(data = sf_cluster)

## colors or not
ggplot() +
  geom_sf(data = sf_nepal, fill = NA, color = "darkred", linewidth = 0.6) +
  geom_sf(data = sf_cluster , aes(color = as.character(plot_count)))


## 05 interactive map ######
## Guide on tmap: https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html#hello-world
## Check basemaps here: https://leaflet-extras.github.io/leaflet-providers/preview/

## Basic
tmap_mode("view")
tm_shape(sf_plot) +
  tm_symbols(col = "plot_physio")

## Intermediate
tm_basemap(
  c("Esri.WorldGrayCanvas", "Esri.WorldTopoMap", "Esri.WorldImagery", "OpenStreetMap")
  ) +
  tm_shape(sf_nepal) +
  tm_borders(col = "red") +
  tm_shape(sf_plot) +
  tm_symbols(col = "plot_physio") +
  tm_scale_bar()


## 06 extract raster info  ######
plot(rs_E)

tmp_plot <- terra::extract(rs_E, vect(sf_plot))

tmp_plot2 <- sf_plot |>
  mutate(plot_E = tmp_plot$E) |>
  as_tibble() |>
  select(plot_id_new, plot_E)

plot2 <- plot |> left_join(tmp_plot2, by = "plot_id_new")

summary(plot2$plot_E)


## !! EX
## + Create 'sf_plot2' by making plot2 spatial
## + Make a map with country boundaries and plot location 
##   with color based on `plot_E`
## !!
sf_plot2 <- plot2 |>
  mutate(lon = plot_gps_lon, lat = plot_gps_lat) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

gg <- ggplot() +
  geom_sf(
    data = sf_nepal, fill = "lightpink",
    color = "darkgreen", linewidth = 0.6
    ) +
  geom_sf(data = sf_plot2, aes(color = plot_E)) +
  scale_color_viridis_c() +
  theme_bw()
print(gg)

ggsave(
  gg, filename = "results/gg-plot-E.png",
  width = 15, height = 12, units = "cm", dpi = 300
)

write_sf(sf_plot2, "results/sf_plot2.geojson")
