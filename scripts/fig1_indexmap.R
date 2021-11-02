library(raster)
library(sf)
library(pals)

# bring the data files
your_map1  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/chelsa/CHELSA_bio10_12.tif")
your_points <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/gis/centroid_points_all.gpkg")
your_polygon <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/gis/area_polygons_all.gpkg")

# crop
e <- extent(5, 45, 55, 72)
your_map2 <- crop(your_map1, e)

# set 1000 m / y limit
your_map2[your_map2 > 700] <- 700

#plot
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_indexmap.pdf")

plot (your_map2, col = rev(ocean.haline(100)))
plot(st_geometry(your_points), pch= 15, cex = 0.25, add=T)
plot(st_geometry(your_polygon), add=T)

dev.off()
