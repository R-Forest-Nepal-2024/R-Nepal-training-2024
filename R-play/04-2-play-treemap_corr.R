
## Objective: create tree location map

## Steps:
## + Find plots with most trees 
## + Filter one plot
## + Make treemap with coord_polar() 


## 01 load libraries ######
library(tidyverse)

## 02 load data ######
## Load tree and plot data
tree <- read_csv("data/tree.csv")

## 03 main code ######

##
## 3.1 Find plots with most trees ####
##

## Use group_by(), summarise(), arrange() and slice_*()
tmp_plot <- tree |>
  group_by(plot_id_new) |>
  summarise(
    count_tree = n()
  )

tmp_plot2 <- tmp_plot |>
  arrange(desc(count_tree)) |>
  slice_head(n = 3)

vec_plot <- tmp_plot2 |> pull(plot_id_new)

##
## 3.2 Filter one plot ####
##
vec_plotid <- vec_plot[3]
vec_plotid 

tt <- tree |> 
  filter(plot_id_new == vec_plotid)

##
## 3.3 Make treemap with coord_polar() ####
##

## + Step 1: basic plot
## + Step 2: coord_polar()
## + Step 3: Add scales
## + Step 4: final plot
## + Step 5: save plot

## 3.3.1 basic plot ####
ggplot(tt) +
  geom_point(aes(x = tree_distance, y = tree_azimuth))

## 3.3.2 Add coord_polar() ####
ggplot(tt) +
  geom_text(aes(x = tree_distance, y = tree_azimuth, label = tree_azimuth)) +
  coord_polar(theta = "y")

## 3.3.3 Add scales
ggplot(tt) +
  geom_text(aes(x = tree_distance, y = tree_azimuth, label = tree_azimuth)) +
  scale_x_continuous(breaks = c(0, 4, 8, 15, 20), limits = c(0, 20)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y")


## 3.3.4 Final graph
gg <- ggplot(tt) +
  geom_point(
    aes(
      x = tree_distance, 
      y = tree_azimuth, 
      color = tree_dbh_class1, 
      size = tree_dbh_class1
      )) +
  scale_x_continuous(breaks = c(0, 4, 8, 15, 20), limits = c(0, 20)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  coord_polar(theta = "y") +
  theme_bw() + 
  theme(panel.grid = element_line(colour = "grey80")) +
  labs(
    x = "Plot radius (m)", 
    y = "",
    color = "DBH class",
    size = "DBH class",
    subtitle = paste0("Tree location map for plot: ", vec_plotid)
  )
print(gg)

## 3.3.5 Save
ggsave(
  gg, filename = paste0("results/treemap-", vec_plotid, ".png"),
  width = 15, height = 12, units = "cm", dpi = 300
)


## !! EX
## + Create a save the tree location maps for the 2 other plots in vec_plot
## !!
 
vec_plotid <- vec_plot[3]
vec_plotid 

tt <- tree |> filter(plot_id_new == vec_plotid)

gg <- ggplot(tt) +
  geom_vline(xintercept = c(0, 4, 8, 15, 20), colour = "grey80", linewidth = 0.2) +
  geom_hline(yintercept = c(0, 90, 180, 270), colour = "grey80", linewidth = 0.2) +
  geom_point(
    aes(
      x = tree_distance, 
      y = tree_azimuth, 
      color = tree_dbh_class1, 
      size = tree_dbh_class1
    )) +
  scale_x_continuous(breaks = c(0, 4, 8, 15, 20), limits = c(0, 20)) +
  scale_y_continuous(breaks = c(0, 90, 180, 270), labels = c("N", "E", "S", "W"), limits = c(0, 360)) +
  scale_size_discrete() +
  coord_polar(theta = "y") +
  theme_bw() + 
  #theme(panel.grid = element_line(colour = "grey80")) +
  theme(panel.grid = element_blank()) +
  labs(
    x = "Plot radius (m)", 
    y = "",
    color = "DBH class",
    size = "DBH class",
    subtitle = paste0("Tree location map for plot: ", vec_plotid)
  )

print(gg)

ggsave(
  gg, filename = paste0("results/treemap-", vec_plotid, ".png"),
  width = 15, height = 12, units = "cm", dpi = 300
)


## For plot 2 check
# source("R/04-1-play-tree_corr.R", local = TRUE)
# 
# sort(plot_tocheck)
# 
# tmp_tree <- tree |>
#   filter(plot_id_new %in% plot_tocheck)
# 
# plot_tocheck2 <- tmp_tree |>
#   group_by(plot_id_new) |>
#   summarise(tree_count = n()) |>
#   arrange(desc(tree_count)) |>
#   slice_head(n = 5) |>
#   pull(plot_id_new)
# plot_tocheck2
# 
# test <- tt4 |> filter(plot_id_new == "063-64-3")
