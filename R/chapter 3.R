library(sf)
library(raster)
library(dplyr)
library(stringr)
library(tidyr)
library(spData)

# 3.2
methods(class = "sf")

dim(world)
nrow(world)
ncol(world)

world_df = st_drop_geometry(world)
class(world_df)






