library(raster)
library(sf)
library(pals)
library(tidyverse)

# bring dem files
your_map_a  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/rasters/dem10_kilpis.tif")
your_map_b  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/rasters/dem2_rastigaisa.tif")
your_map_c  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/rasters/dem2_varrio.tif")
your_map_d  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/rasters/dem2_tiilikka.tif")
your_map_e  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/rasters/dem2_pisa.tif")
your_map_f  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/rasters/dem2_hyytiala.tif")
your_map_g  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/rasters/dem2_karkali.tif")

your_map1  <- raster ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/chelsa/CHELSA_bio10_12.tif")

e <- extent(5, 45, 55, 72)
your_map2 <- crop(your_map1, e)

plot(your_map2)

your_map3 <- projectRaster(your_map2, crs = as.character(crs(your_map_a)))
your_map4 <- projectRaster(your_map2, crs = as.character(crs(your_map_b)))

# bring logger point files and logger soil moisture files
your_points <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/gis/points_all.gpkg")

your_sm  <- read.csv ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/all_data_monthly.csv")
your_sm %>% rename(id = site) %>% 
  mutate(id = as.character(id)) -> your_sm

your_sm %>% group_by(id_code, area, id) %>% 
  summarise(moist_mean = mean(moist_mean, na.rm = T)) -> your_sm

your_points <- left_join(your_points, your_sm)

summary(your_points$moist_mean)

your_points_a <- your_points %>% st_transform(as.character(crs(your_map_a)))
your_points_b <- your_points %>% st_transform(as.character(crs(your_map_b)))
your_points_c <- your_points %>% st_transform(as.character(crs(your_map_c)))
your_points_d <- your_points %>% st_transform(as.character(crs(your_map_d)))
your_points_e <- your_points %>% st_transform(as.character(crs(your_map_e)))
your_points_f <- your_points %>% st_transform(as.character(crs(your_map_f)))
your_points_g <- your_points %>% st_transform(as.character(crs(your_map_g)))

# bring study area polygon files
your_polygon <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/gis/area_polygons_all.gpkg")

your_polygon_a <- your_polygon %>% st_transform(as.character(crs(your_map_a)))
your_polygon_b <- your_polygon %>% st_transform(as.character(crs(your_map_b)))
your_polygon_c <- your_polygon %>% st_transform(as.character(crs(your_map_c)))
your_polygon_d <- your_polygon %>% st_transform(as.character(crs(your_map_d)))
your_polygon_e <- your_polygon %>% st_transform(as.character(crs(your_map_e)))
your_polygon_f <- your_polygon %>% st_transform(as.character(crs(your_map_f)))
your_polygon_g <- your_polygon %>% st_transform(as.character(crs(your_map_g)))

# bring waterbodies polygon files
your_water_a_1 <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_saana.gpkg")
your_water_a_2 <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_malla.gpkg")
your_water_a_3 <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_ailakka.gpkg")
your_water_b <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_rastigaisa.gpkg")
your_water_c <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_varrio.gpkg")
your_water_d <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_tiilikka.gpkg")
your_water_e <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_pisa.gpkg")
your_water_f <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_hyytiala.gpkg")
your_water_g <- st_read("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/water_polygons/waterbodies_karkali.gpkg")

your_water_a_1 <- your_water_a_1 %>% st_transform(as.character(crs(your_map_a)))
your_water_a_2 <- your_water_a_2 %>% st_transform(as.character(crs(your_map_a)))
your_water_a_3 <- your_water_a_3 %>% st_transform(as.character(crs(your_map_a)))
your_water_b <- your_water_b %>% st_transform(as.character(crs(your_map_b)))
your_water_c <- your_water_c %>% st_transform(as.character(crs(your_map_c)))
your_water_d <- your_water_d %>% st_transform(as.character(crs(your_map_d)))
your_water_e <- your_water_e %>% st_transform(as.character(crs(your_map_e)))
your_water_f <- your_water_f %>% st_transform(as.character(crs(your_map_f)))
your_water_g <- your_water_g %>% st_transform(as.character(crs(your_map_g)))

# colors
ocean.speed (10)
map_ramp <- colorRampPalette(c( "#FFFDCD", "#EBDC90", "#D0C054", "#A8AC23", "#789A0E", "#468715", "#1D7126", "#0D582B"))
hillshade_ramp <- colorRampPalette(c("#00000000","#FFFFFF00"))

# hillshade
slope_a <- terrain(your_map_a, opt='slope')
aspect_a <- terrain(your_map_a, opt='aspect')
hill_a <- hillShade(slope_a, aspect_a, 40, 270)

slope_b <- terrain(your_map_b, opt='slope')
aspect_b <- terrain(your_map_b, opt='aspect')
hill_b <- hillShade(slope_b, aspect_b, 40, 270)

slope_c <- terrain(your_map_c, opt='slope')
aspect_c <- terrain(your_map_c, opt='aspect')
hill_c <- hillShade(slope_c, aspect_c, 40, 270)

slope_d <- terrain(your_map_d, opt='slope')
aspect_d <- terrain(your_map_d, opt='aspect')
hill_d <- hillShade(slope_d, aspect_d, 40, 270)

slope_e <- terrain(your_map_e, opt='slope')
aspect_e <- terrain(your_map_e, opt='aspect')
hill_e <- hillShade(slope_e, aspect_e, 40, 270)

slope_f <- terrain(your_map_f, opt='slope')
aspect_f <- terrain(your_map_f, opt='aspect')
hill_f <- hillShade(slope_f, aspect_f, 40, 270)

slope_g <- terrain(your_map_g, opt='slope')
aspect_g <- terrain(your_map_g, opt='aspect')
hill_g <- hillShade(slope_g, aspect_g, 40, 270)

# crop
extent_a <- extent(your_map_a)
extent_b <- extent(your_map_b)
extent_c <- extent(your_map_c)
extent_d <- extent(your_map_d)
extent_e <- extent(your_map_e)
extent_f <- extent(your_map_f)
extent_g <- extent(your_map_g)

your_map_3a <- crop(your_map3, extent_a)
your_map_4b <- crop(your_map4, extent_b)
your_map_3c <- crop(your_map3, extent_c)
your_map_3d <- crop(your_map3, extent_d)
your_map_3e <- crop(your_map3, extent_e)
your_map_3f <- crop(your_map3, extent_f)
your_map_3g <- crop(your_map3, extent_g)

# plot a
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_a.pdf")

plot(your_map_3a, col=rev(ocean.haline(100)), zlim = c(300,700))
#plot(your_map_a, col=rev(map_ramp(100)), zlim = c(2993,120000))
plot(hill_a, col=(hillshade_ramp(100)), alpha=0.1, legend=F, add=T)
plot(your_water_a_1, col="white", border="white", add=T)
plot(your_water_a_2, col="white", border="white", add=T)
plot(your_water_a_3, col="white", border="white", add=T)
#plot(st_geometry(your_polygon_a), add=T, lwd=1, border="grey")
plot(st_geometry(your_points_a), pch= 21, lwd = 2, cex = 1, bg= NA, col = (rev(ocean.dense (145))[your_points_a$moist_mean]), add=T)

dev.off()

# plot b
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_b.pdf")

plot(your_map_4b, col=rev(ocean.haline(100)), zlim = c(300,700))
#plot(your_map_b, col=rev(map_ramp(100)), zlim = c(2993,120000))
plot(hill_b, col=(hillshade_ramp(100)), alpha=0.1, legend=F, add=T)
plot(your_water_b, col="white", border="white", add=T)
#plot(st_geometry(your_polygon_b), add=T, lwd=1, border="grey")
plot(st_geometry(your_points_b), pch= 21, lwd = 2, cex = 1, bg= NA, col = (rev(ocean.dense (145))[your_points_b$moist_mean]), add=T)

dev.off()

# plot c
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_c.pdf")

plot(your_map_3c, col=rev(ocean.haline(100)), zlim = c(300,700))
#plot(your_map_c, col=rev(map_ramp(100)), zlim = c(2993,120000))
plot(hill_c, col=(hillshade_ramp(100)), alpha=0.1, legend=F, add=T)
plot(your_water_c, col="white", border="white", add=T)
#plot(st_geometry(your_polygon_c), add=T, lwd=1, border="grey")
plot(st_geometry(your_points_c), pch= 21, lwd = 2, cex = 1, bg= NA, col = (rev(ocean.dense (145))[your_points_c$moist_mean]), add=T)

dev.off()

# plot d
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_d.pdf")

plot(your_map_3d, col=rev(ocean.haline(100)), zlim = c(300,700))
#plot(your_map_d, col=rev(map_ramp(100)), zlim = c(2993,120000))
plot(hill_d, col=(hillshade_ramp(100)), alpha=0.1, legend=F, add=T)
plot(your_water_d, col="white", border="white", add=T)
#plot(st_geometry(your_polygon_d), add=T, lwd=1, border="grey")
plot(st_geometry(your_points_d), pch= 21, lwd = 2, cex = 1, bg= NA, col = (rev(ocean.dense (145))[your_points_d$moist_mean]), add=T)

dev.off()

# plot e
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_e.pdf")

plot(your_map_3e, col=rev(ocean.haline(100)), zlim = c(300,700))
#plot(your_map_e, col=rev(map_ramp(100)), zlim = c(2993,120000))
plot(hill_e, col=(hillshade_ramp(100)), alpha=0.1, legend=F, add=T)
plot(your_water_e, col="white", border="white", add=T)
#plot(st_geometry(your_polygon_e), add=T, lwd=1, border="grey")
plot(st_geometry(your_points_e), pch= 21, lwd = 2, cex = 1, bg= NA, col = (rev(ocean.dense (145))[your_points_e$moist_mean]), add=T)

dev.off()

# plot f
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_f.pdf")

plot(your_map_3f, col=rev(ocean.haline(100)), zlim = c(300,700))
#plot(your_map_f, col=rev(map_ramp(100)), zlim = c(2993,120000))
plot(hill_f, col=(hillshade_ramp(100)), alpha=0.1, legend=F, add=T)
plot(your_water_f, col="white", border="white", add=T)
#plot(st_geometry(your_polygon_f), add=T, lwd=1, border="grey")
plot(st_geometry(your_points_f), pch= 21, lwd = 2, cex = 1, bg= NA, col = (rev(ocean.dense (145))[your_points_f$moist_mean]), add=T)

dev.off()

# plot g
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig1/fig1_g.pdf")

plot(your_map_3g, col=rev(ocean.haline(100)), zlim = c(300,700))
#plot(your_map_g, col=rev(map_ramp(100)), zlim = c(2993,120000))
plot(hill_g, col=(hillshade_ramp(100)), alpha=0.1, legend=F, add=T)
plot(your_water_g, col="white", border="white", add=T)
#plot(st_geometry(your_polygon_g), add=T, lwd=1, border="grey")
plot(st_geometry(your_points_g), pch= 21, lwd = 2, cex = 1, bg= NA, col = (rev(ocean.dense (145))[your_points_g$moist_mean]), add=T)

dev.off()