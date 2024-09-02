
## Objective: Check the data (basic, HD, nested level)

## 01 Load Libraries ######
library(tidyverse)

## 02 Load data ######
tree <- read_csv("data/tree.csv")
plot <- read_csv("data/plot.csv")

## 03 main code ######

##
## 3.1 Check the tree data ####
##

## 3.1.1 basic check ####
names(tree)
summary(tree)
table(tree$tree_dbh_class1, useNA = "ifany")
nrow(tree)


tree2 <- tree
tree2$plot_radius <- ifelse(
  tree2$tree_dbh < 10, 4, ifelse(
    tree2$tree_dbh < 20, 8, ifelse(
      tree2$tree_dbh < 30, 15, 20
    )))

table(tree2$plot_radius)

tree3 <- tree |>
  mutate(
  plot_radius = case_when(
    tree_dbh < 10 ~ 4,
    tree_dbh < 20 ~ 8,
    tree_dbh < 30 ~ 15,
    TRUE ~ 20
  )
)
table(tree3$plot_radius, useNA = "ifany")

## 3.1.2 HD graph ####
tt <- tree |> 
  filter(
    !is.na(tree_total_height), 
    tree_total_height != 0
  )

## Filtering by sample code keeps height == 0
# tt2 <- tree |> filter(tree_sample_code > 0)
# summary(tt2$tree_total_height)
# 
# test <- tt2 |> filter(tree_total_height == 0)
# table(test$tree_sample_code)

ggplot(tt) + 
  geom_point(
    aes(x = tree_dbh, y = tree_total_height), 
    alpha = 0.3, size = 0.3
    )

ggplot(tt) + 
  geom_point(
    aes(
      x = tree_dbh, 
      y = tree_total_height, 
      color = tree_crown_class
      )
    )

ggplot(tt) + 
  geom_point(
    aes(
      x = tree_dbh, 
      y = tree_total_height, 
      color = as.character(tree_crown_class)
      )
    )

## !! EX 
## + Create the object 'tt2' from 'tt' and use filter() 
##   to keep only:
##   + crown class not 10
##   + height >= 2 (to remove stumps)
## + make a HD graphs with 'tt2', change the color based on `tree_dbh_class1`.
## !! 
tt2 <- tt |>
  filter(
    tree_crown_class != 10,
    tree_total_height >= 2
    )

ggplot(tt2) + 
  geom_point(
    aes(
      x = tree_dbh, 
      y = tree_total_height, 
      color = tree_dbh_class1
    ),
    size = 0.2, alpha = 0.5
  )


## 3.1.3 Check tree in correct plot nested level

## Base R style 
tt3 <- tree
tt3$tree_lvl1_check <- ifelse(
  tt3$tree_dbh < 10 & tt3$tree_distance <= 4, 0, ifelse(
    tt3$tree_dbh < 10 & tt3$tree_distance > 4, 1, NA
  ))

table(tt3$tree_lvl1_check, useNA = "ifany")

## tidyverse style
tt3 <- tree |>
  mutate(
    tree_level1_check = case_when(
      tree_dbh < 10 & tree_distance <= 4 ~ 0,
      tree_dbh < 10 & tree_distance > 4  ~ 1,
      TRUE ~ NA_integer_
    )
  )
table(tt3$tree_level1_check, useNA = "ifany")


## !! EX
## + Create 'tt4' from 'tt3' and use mutate() and case_when()
##   to calculate `tree_level2_check`, `tree_level3_check` and 
##   `tree_level4_check`
## + create 'tt5' with the outliers filtered out
## !!

tt4 <- tt3 |>
  mutate(
    tree_level2_check = case_when(
      tree_dbh < 20 & tree_distance <= 8 ~ 0,
      tree_dbh < 20 & tree_distance > 8  ~ 1,
      TRUE ~ NA_integer_
    ),
    tree_level3_check = case_when(
      tree_dbh < 30 & tree_distance <= 15 ~ 0,
      tree_dbh < 30 & tree_distance > 15  ~ 1,
      TRUE ~ NA_integer_
    ),
    tree_level4_check = case_when(
      tree_dbh >= 30 & tree_distance <= 20 ~ 0,
      tree_dbh >= 30 & tree_distance > 20  ~ 1,
      TRUE ~ NA_integer_
    ),
    tree_level_check = case_when(
      tree_dbh < 10 & tree_distance > 4 ~ 1,
      tree_dbh < 20 & tree_distance > 8 ~ 1,
      tree_dbh < 30 & tree_distance > 15 ~ 1,
      tree_dbh >= 30 & tree_distance > 20 ~ 1,
      TRUE ~ 0
    )
  )

table(tt4$tree_level1_check)
table(tt4$tree_level2_check)
table(tt4$tree_level3_check)
table(tt4$tree_level4_check)
table(tt4$tree_level_check)

plot_tocheck <- tt4 |>
  filter(tree_level_check == 1) |>
  pull(plot_id_new) |>
  unique()

plot_tocheck
nrow(plot)
length(plot_tocheck)
nrow(plot) - length(plot_tocheck)

## Save the final table
write_csv(tt4, "results/tt4.csv")

gg <- ggplot(tt4) +
  geom_point(
    aes(
      x = tree_dbh,
      y = tree_total_height,
      color = as.character(tree_level4_check)
      ),
    size = 0.3, alpha = 0.6
  ) +
  theme_bw() +
  labs(
    x = "Tree DBH (cm)",
    y = "Tree total height (m)",
    color = "Flag lvl4",
    title = "Tree height-diameter",
    subtitle = "Tree data from LEAF area",
    caption = "NFI Nepal 2022"
  )
gg
print(gg)

ggsave(
  gg, filename = "results/gg-HD.png",
  width = 15, height = 12, units = "cm", dpi = 300
)
