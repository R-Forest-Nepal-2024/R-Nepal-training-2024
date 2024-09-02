library(tictoc)

tic()

source("R-NFI/00-setup.R", local = TRUE)

source("R-NFI/01-load-data.R", local = TRUE)

source("R-NFI/02-harmo-data.R", local = TRUE)

source("R-NFI/03-spatial-data.R", local = TRUE)

# source("R-NFI/04-check-plots.R", local = TRUE)

# source("R-NFI/05-tree-agb.R", local = TRUE)

source("R-NFI/05a-tree-height.R", local = TRUE)
source("R-NFI/05b-tree-volume.R", local = TRUE)
source("R-NFI/05c-tree-agb.R", local = TRUE)
source("R-NFI/05d-tree-scale-factor.R", local = TRUE)

source("R-NFI/06-agg-agb.R", local = TRUE)

source("R-NFI/10-treemap.R", local = TRUE)

toc()