

## Libraries
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(readxl)) install.packages("readxl")
if (!require(sf)) install.packages("sf")
if (!require(terra)) install.packages("terra")
if (!require(tmap)) install.packages("tmap")
if (!require(lmfor)) install.packages("lmfor")

library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(lmfor)

theme_set(theme_bw())

## Paths
path_leaf <- "data-source/LEAF"

