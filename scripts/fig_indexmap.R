library(raster)
library(sf)
library(pals)

# bring the data files
#your_map1  <- raster ("data/CHELSA_bio10_12.tif")
your_map1  <- raster ("data/ERA_precipitation_2020.tif")

your_points <- st_read("data/centroid_points_all.gpkg")
your_polygon <- st_read("data/area_polygons_all.gpkg")

# crop
e <- extent(5, 45, 55, 72)
your_map2 <- crop(your_map1, e)

# set 1000 m / y limit
#your_map2[your_map2 > 600] <- 600

#plot
dev.off()
pdf(file="fig/fig_indexmap.pdf")

plot (your_map2, col = rev(parula(100)),zlim=c(150,700))
plot(st_geometry(your_points), pch= 15, cex = 0.25, add=T)
plot(st_geometry(your_polygon), add=T, lwd=10)

dev.off()

hist(your_map2)
