install.packages("sf")
library(sf)
library(tidyverse)
library(raster)
library(marmap)

USA_crit_hab = st_read('data/North_Atlantic_Right_Whale_Critical_Habitat', "North_Atlantic_Right_Whale_Critical_Habitat")
