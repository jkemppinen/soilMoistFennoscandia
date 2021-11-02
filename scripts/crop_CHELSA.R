library(raster)
library(sf)
library(tidyverse)

r <- raster("data/CHELSA_bio10_12.tif")

p <- st_read("data/area_polygons_all.gpkg")

r <- crop(r, st_bbox(p)+c(-20,-10,5,5))
plot(r)

writeRaster(r, "data/CHELSA_bio10_12.tif", overwrite = T)
