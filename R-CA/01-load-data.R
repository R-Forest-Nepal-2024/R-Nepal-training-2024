
## Reading initial CEO data from Excel ######
## => Recommend to switch to the initial CSV file from R script 
AD_ceo_init <- read_xlsx(
  file.path(path_calc, "Nepal_LEAF_AD_tool_FREL-MCA.xlsx"),
  sheet = "CompiledData_CEO_GEE-ART-TREES-", 
  skip = 1, 
  guess_max = 1760, 
  na = "NA"
  )

table(AD_ceo_init$FinalStrata_split_substrata_readable2, useNA = "ifany")


AD_strata_init <- read_csv(file.path(path_calc, "leaf-strata-weight.csv"))
AD_strata_init

## Reading CEO NFI from Excel ######
# NFI_ceo_init <- read_xlsx(
#   file.path(path_calc, "NFI_2Time_With_CEO_Sep01_GOV_Sep-25-2023.xlsx"),
#   sheet = "Summary", range = "BE2:BS1092", na = "NA"
# )

NFI_ceo_init <- read_xlsx(
  file.path(path_calc, "NFI_2Time_With_CEO_Sep01_GOV_Sep-25-2023.xlsx"),
  sheet = "CompiledData_CEO_GEE-ART-TREES-", range = "A1:EC1091",
  na = "NA", guess_max = 1000
)

table(NFI_ceo_init$t2_numbertrees, useNA = "ifany")


## Create Cstock not from NFI ######
cstock_init <- tibble(
  type  = c(rep("land use", 4), rep("default value", 2), "time period"),
  desc  = c(
    "grassland", "other land", "settlements", "unshaded cropland",
    "RS", "CF", "reference period"
    ),
  id = c("G", "O", "S", "U", "RS", "CF", "RP"),
  value = c(3.97, 39.95, 3.97, 48.31, 0.44,  0.47, 5),
  se    = c(6.17, 45.12, 6.17, 38.32, 	0.184, 0.0120, NA),
  pdf   = c(rep("Normal", 6), NA), 
  unit = c(rep("t/ha", 4), "dimensionless", "dimensionless", "year")
)
cstock_init


## LU code list ######
table(AD_ceo_init$Non.forest.land.use.type.in.2021)

lu_code <- tibble(
  lu_name = c(
    "Dense Forest", "Thin Forest", "Secondary Forest", 
    "Shaded Cropland", "Non Forest",  "Grasslands", "Other Land", 
    "Settlements", "Unshaded Cropland (TCC 10% or less)"
  ),
  lu_id = c(
    "FD", "FT", "FS", "SC", "NF", "G", "O", "S", "U"),
  lu_no = 1:9
)
