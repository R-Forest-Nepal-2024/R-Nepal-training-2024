

##
## Tables ######
##

## + TREE DATA ######
tree_init <- read_xlsx(
  "data-source/LEAF/tree_data2016_21_LEAF.xlsx", 
  sheet = "Sheet1",
  guess_max = 1000
  )

tree_init
names(tree_init)
summary(tree_init)

length(unique(tree_init$FID_tree_d))
length(unique(tree_init$S_N_))

length(unique(tree_init$tree_numbe))

length(unique(tree_init$Plot_id))

## + EQUATIONS ######
eq_init <- read_csv(file.path(path_leaf, "Equations.csv"))
# eq_init <- read_csv("data-source/LEAF/Equations.csv")


## CEO NFI essential
ceonfi_init <- read_csv("data/CEONFI-fromHermann-NFI2Time-sep25.csv")

##
## SPATIAL ######
##

## + NEPAL Country boundaries ######
## Get from GADM.org
sf_nepal <- read_sf("data-spatial/gadm41_NPL_0.json")

## + Environmental stress from Chave et al. 2014 ######

## Download E from BIOMASS Package if needed
# download.file(
#   url = "https://github.com/umr-amap/BIOMASS/raw/master/data-raw/climate_variable/E.zip",
#   destfile = "data-spatial/E.zip"
# )
# unzip(zipfile = "data-spatial/E.zip", exdir = "data-source/spatial")

rs_E <- terra::rast("data-spatial/E.bil")


## Identify MCA Samples from GIS (Arun) - to be removed for analysis
plot_MCA <- "093-42-6"
