
## Load packages, install if necessary
load_pkg <- function(.pkg_name){
  if (!require(.pkg_name, character.only = TRUE, quietly = TRUE)) {
    install.packages(.pkg_name, dep =TRUE)
    library(.pkg_name, character.only = TRUE, quietly = TRUE)
  }
}

load_pkg("tictoc")

source("R-CA/00-setup.R")

source("R-CA/01-load-data.R")

source("R-CA/02-harmo-data.R")

source("R-CA/03-calc-AD.R")

source("R-CA/04-calc-cstock.R")

source("R-CA/05-calc-E.R")

source("R-CA/06-Uncertainty-E-corr.R")
