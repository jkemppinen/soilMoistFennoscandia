library(raster)
library(sf)
library(pals)
library(lubridate)
library(tidyverse)
library(grid)
library(pals)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
library(moments)
library(patchwork)
library(scales)
library(sf)

# bring dem files
your_map_KIL  <- raster ("data/dem10_kilpis.tif")
your_map_RAS  <- raster ("data/dem2_rastigaisa.tif")
your_map_VAR  <- raster ("data/dem2_varrio.tif")
your_map_TII  <- raster ("data/dem2_tiilikka.tif")
your_map_PIS  <- raster ("data/dem2_pisa.tif")
your_map_HYY  <- raster ("data/dem2_hyytiala.tif")
your_map_KAR  <- raster ("data/dem2_karkali.tif")

# bring logger point files and logger soil moisture files
your_points <- st_read("data/points_all.gpkg")

your_points %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_points

your_points$area <- recode_factor(your_points$area,
                              RAS = "RAS",
                              KIL = "KIL",
                              VAR = "VÄR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR")


# bring logger soil moisture files
your_sm  <- read_csv ("data/all_data_daily_2021.csv") %>% 
  filter(!grepl("PIS1", site)) # Exclude PISA sites in active forestry areas

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
                              RAS = "RAS",
                              KIL = "KIL",
                              VAR = "VÄR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR")

# Select which year and months to include
your_sm %>% 
  filter(year(date) %in% c(2020)) %>%
  filter(month(date) %in% c(4:9)) -> your_sm 

# Keep data only after snow melt
your_sm %>% 
  filter(T1_mean > 1) %>% 
  group_by(site) %>% 
  summarise(first_date = min(date)) -> aggr1

full_join(your_sm, aggr1) %>% 
  filter(date >= first_date) %>% 
  select(-first_date) -> your_sm

# Based on the snow free season, keep only sites which have >= 90% of the period covered
your_sm %>% group_by(site) %>% 
  summarise(moist_prop2 = mean(moist_prop)) -> aggr2

full_join(your_sm, aggr2) %>% filter(moist_prop2 >= 90) %>% 
  filter(moist_prop >= 90) %>% 
  dplyr::select(-moist_prop2) -> your_sm

# How many sites in total?
length(unique(your_sm$site)) # 503 sites selected

# How many measurements in total?
your_sm %>% filter(!is.na(moist_mean)) %>% nrow()*24*4 # The total number of obsevations

# calculate mean soil moisture
your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T)) -> data_means

# join dataset
data_means %>% 
  mutate(id = parse_number(site)) -> data_means

your_points <- right_join(your_points %>% mutate(id = as.numeric(id)), data_means)

your_points %>% filter(is.na(sm_mean))

# colour code column for plotting
your_points$color = (rev(parula (max(as.numeric(round(your_points$sm_mean)))+1))[as.numeric(round(your_points$sm_mean))+1])

# make spatial data
your_points_KIL <- your_points %>% st_transform(as.character(crs(your_map_KIL)))
your_points_RAS <- your_points %>% st_transform(as.character(crs(your_map_RAS)))
your_points_VAR <- your_points %>% st_transform(as.character(crs(your_map_VAR)))
your_points_TII <- your_points %>% st_transform(as.character(crs(your_map_TII)))
your_points_PIS <- your_points %>% st_transform(as.character(crs(your_map_PIS)))
your_points_HYY <- your_points %>% st_transform(as.character(crs(your_map_HYY)))
your_points_KAR <- your_points %>% st_transform(as.character(crs(your_map_KAR)))

# bring study area polygon files
your_polygon <- st_read("data/area_polygons_all.gpkg")

your_polygon_KIL <- your_polygon %>% st_transform(as.character(crs(your_map_KIL)))
your_polygon_RAS <- your_polygon %>% st_transform(as.character(crs(your_map_RAS)))
your_polygon_VAR <- your_polygon %>% st_transform(as.character(crs(your_map_VAR)))
your_polygon_TII <- your_polygon %>% st_transform(as.character(crs(your_map_TII)))
your_polygon_PIS <- your_polygon %>% st_transform(as.character(crs(your_map_PIS)))
your_polygon_HYY <- your_polygon %>% st_transform(as.character(crs(your_map_HYY)))
your_polygon_KAR <- your_polygon %>% st_transform(as.character(crs(your_map_KAR)))

your_map_RAS <- crop(your_map_RAS, your_polygon_RAS %>% filter(area %in% c("RAS")))
your_map_KIL <- crop(your_map_KIL, your_polygon_KIL %>% filter(area %in% c("AIL","MAL","SAA")))
your_map_VAR <- crop(your_map_VAR, your_polygon_VAR %>% filter(area %in% c("VAR")))
your_map_TII <- crop(your_map_TII, your_polygon_TII %>% filter(area %in% c("TII")))
your_map_PIS <- crop(your_map_PIS, your_polygon_PIS %>% filter(area %in% c("PIS")))
your_map_HYY <- crop(your_map_HYY, your_polygon_HYY %>% filter(area %in% c("HYY")))
your_map_KAR <- crop(your_map_KAR, your_polygon_KAR %>% filter(area %in% c("KAR")))

# bring waterbodies polygon files
your_water_KIL1 <- st_read("data/waterbodies_saana.gpkg")
your_water_KIL2 <- st_read("data/waterbodies_malla.gpkg")
your_water_KIL3 <- st_read("data/waterbodies_ailakka.gpkg")
your_water_RAS <- st_read("data/waterbodies_rastigaisa.gpkg")
your_water_VAR <- st_read("data/waterbodies_varrio.gpkg")
your_water_TII <- st_read("data/waterbodies_tiilikka.gpkg")
your_water_PIS <- st_read("data/waterbodies_pisa.gpkg")
your_water_HYY <- st_read("data/waterbodies_hyytiala.gpkg")
your_water_KAR <- st_read("data/waterbodies_karkali.gpkg")

your_water_KIL1 <- your_water_KIL1 %>% st_transform(as.character(crs(your_map_KIL)))
your_water_KIL2 <- your_water_KIL2 %>% st_transform(as.character(crs(your_map_KIL)))
your_water_KIL3 <- your_water_KIL3 %>% st_transform(as.character(crs(your_map_KIL)))
your_water_RAS <- your_water_RAS %>% st_transform(as.character(crs(your_map_RAS)))
your_water_VAR <- your_water_VAR %>% st_transform(as.character(crs(your_map_VAR)))
your_water_TII <- your_water_TII %>% st_transform(as.character(crs(your_map_TII)))
your_water_PIS <- your_water_PIS %>% st_transform(as.character(crs(your_map_PIS)))
your_water_HYY <- your_water_HYY %>% st_transform(as.character(crs(your_map_HYY)))
your_water_KAR <- your_water_KAR %>% st_transform(as.character(crs(your_map_KAR)))

your_water_KIL1 <- st_crop (your_water_KIL1, your_map_KIL)
your_water_KIL2 <- st_crop (your_water_KIL2, your_map_KIL)
your_water_KIL3 <- st_crop (your_water_KIL3, your_map_KIL)
your_water_RAS <-  st_crop (your_water_RAS , your_map_RAS )
your_water_VAR <-  st_crop (your_water_VAR , your_map_VAR )
your_water_TII <-  st_crop (your_water_TII , your_map_TII )
your_water_PIS <-  st_crop (your_water_PIS , your_map_PIS )
your_water_HYY <-  st_crop (your_water_HYY , your_map_HYY )
your_water_KAR <-  st_crop (your_water_KAR , your_map_KAR )

# colors
#ocean.speed (10)
#map_ramp <- colorRampPalette(c( "#FFFDCD", "#EBDC90", "#D0C054", "#A8AC23", "#789A0E", "#468715", "#1D7126", "#0D582B"))
hillshade_ramp <- colorRampPalette(c("#999999", "#FFFFFF"))


# hillshade
slope_RAS <- terrain(your_map_RAS, opt='slope')
aspect_RAS <- terrain(your_map_RAS, opt='aspect')
hill_RAS <- hillShade(slope_RAS, aspect_RAS, 40, 270)

slope_KIL <- terrain(your_map_KIL, opt='slope')
aspect_KIL <- terrain(your_map_KIL, opt='aspect')
hill_KIL <- hillShade(slope_KIL, aspect_KIL, 40, 270)

slope_VAR <- terrain(your_map_VAR, opt='slope')
aspect_VAR <- terrain(your_map_VAR, opt='aspect')
hill_VAR <- hillShade(slope_VAR, aspect_VAR, 40, 270)

slope_TII <- terrain(your_map_TII, opt='slope')
aspect_TII <- terrain(your_map_TII, opt='aspect')
hill_TII <- hillShade(slope_TII, aspect_TII, 40, 270)

slope_PIS <- terrain(your_map_PIS, opt='slope')
aspect_PIS <- terrain(your_map_PIS, opt='aspect')
hill_PIS <- hillShade(slope_PIS, aspect_PIS, 40, 270)

slope_HYY <- terrain(your_map_HYY, opt='slope')
aspect_HYY <- terrain(your_map_HYY, opt='aspect')
hill_HYY <- hillShade(slope_HYY, aspect_HYY, 40, 270)

slope_KAR <- terrain(your_map_KAR, opt='slope')
aspect_KAR <- terrain(your_map_KAR, opt='aspect')
hill_KAR <- hillShade(slope_KAR, aspect_KAR, 40, 270)

# plot rastigaisa
extent(your_map_RAS)
your_points_RAS %>% filter(area == "RAS") %>% pull(sm_mean)

dev.off()
pdf(file="fig/fig_map_RAS.pdf", width = 6, height = 6)
plot(your_map_RAS, col=(hillshade_ramp(100)), zlim=c(2994,102817), axes = F, box=F, legend=F)
plot(hill_RAS, col=(hillshade_ramp(100)), alpha=3/10, legend=F, add=T)
plot(your_water_RAS, col="white", border="white", add=T)
plot(your_points_RAS["sm_mean"], lwd = 2, cex = 2, bg= NA, pch=19, 
     col = your_points_RAS$color, add=T)
scalebar(1000, xy=c(473530.5 , 7762539), type="line", label = c("1 km"))
#plot_RAS <- recordPlot()
dev.off()

# plot kilpisjärvi
extent(your_map_KIL)
your_points_KIL %>% filter(area == "KIL") %>% pull(sm_mean)

dev.off()
pdf(file="fig/fig_map_KIL.pdf", width = 6, height = 6)
plot(your_map_KIL, col=(hillshade_ramp(100)), zlim=c(2994,102817), axes = F, box=F, legend=F)
plot(hill_KIL, col=(hillshade_ramp(100)), alpha=3/10, legend=F, add=T)
plot(your_water_KIL1, col="white", border="white", add=T)
plot(your_water_KIL2, col="white", border="white", add=T)
plot(your_water_KIL3, col="white", border="white", add=T)
plot(your_points_KIL["sm_mean"], lwd = 2, cex = 1, bg= NA, pch=19, 
     col = your_points_KIL$color, add=T)
scalebar(1000, xy=c(484683.9 ,7650158 ), type="line", label = c("1 km"))
#plot_KIL <- recordPlot()
dev.off()

# plot värriö
extent(your_map_VAR)
your_points_VAR %>% filter(area == "VÄR") %>% pull(sm_mean)

dev.off()
pdf(file="fig/fig_map_VAR.pdf", width = 6, height = 6)
plot(your_map_VAR, col=(hillshade_ramp(100)), zlim=c(2994,102817), axes = F, box=F, legend=F)
plot(hill_VAR, col=(hillshade_ramp(100)), alpha=3/10, legend=F, add=T)
plot(your_water_VAR, col="white", border="white", add=T)
plot(your_points_VAR["sm_mean"], lwd = 2, cex = 2, bg= NA, pch=19, 
     col = your_points_VAR$color, add=T)
scalebar(1000, xy=c(606904,7512840 ), type="line", label = c("1 km"))
#plot_VAR <- recordPlot()
dev.off()

# plot tiilikka
extent(your_map_TII)
your_points_TII %>% filter(area == "TII") %>% pull(sm_mean)

dev.off()
pdf(file="fig/fig_map_TII.pdf", width = 6, height = 6)
plot(your_map_TII, col=(hillshade_ramp(100)), zlim=c(2994,102817), axes = F, box=F, legend=F)
plot(hill_TII, col=(hillshade_ramp(100)), alpha=3/10, legend=F, add=T)
plot(your_water_TII, col="white", border="white", add=T)
plot(your_points_TII["sm_mean"], lwd = 2, cex = 2, bg= NA, pch=19, 
     col = your_points_TII$color, add=T)
scalebar(1000, xy=c(562456   ,7055514   ), type="line", label = c("1 km"))
#plot_TII <- recordPlot()
dev.off()

# plot pisa
extent(your_map_PIS)
your_points_PIS %>% filter(area == "PIS") %>% pull(sm_mean)

dev.off()
pdf(file="fig/fig_map_PIS.pdf", width = 6, height = 6)
plot(your_map_PIS, col=(hillshade_ramp(100)), zlim=c(2994,102817), axes = F, box=F, legend=F)
plot(hill_PIS, col=(hillshade_ramp(100)), alpha=3/10, legend=F, add=T)
plot(your_water_PIS, col="white", border="white", add=T)
plot(your_points_PIS["sm_mean"], lwd = 2, cex = 2, bg= NA, pch=19, 
     col = your_points_PIS$color, add=T)
scalebar(1000, xy=c(564740,7007400), type="line", label = c("1 km"))
#plot_PIS <- recordPlot()
dev.off()

# plot hyytiälä
extent(your_map_HYY)
your_points_HYY %>% filter(area == "HYY") %>% pull(sm_mean)

dev.off()
pdf(file="fig/fig_map_HYY.pdf", width = 6, height = 6)
plot(your_map_HYY, col=(hillshade_ramp(100)), zlim=c(2994,102817), axes = F, box=F, legend=F)
plot(hill_HYY, col=(hillshade_ramp(100)), alpha=3/10, legend=F, add=T)
plot(your_water_HYY, col="white", border="white", add=T)
plot(your_points_HYY["sm_mean"], lwd = 2, cex = 2, bg= NA, pch=19, 
     col = your_points_HYY$color, add=T)
scalebar(1000, xy=c(348532 ,6854810 ), type="line", label = c("1 km"))
#plot_HYY <- recordPlot()
dev.off()

# plot karkali
extent(your_map_KAR)
your_points_KAR %>% filter(area == "KAR") %>% pull(sm_mean)

dev.off()
pdf(file="fig/fig_map_KAR.pdf", width = 6, height = 6)
plot(your_map_KAR, col=(hillshade_ramp(100)), zlim=c(2994,102817), axes = F, box=F, legend=F)
plot(hill_KAR, col=(hillshade_ramp(100)), alpha=3/10, legend=F, add=T)
plot(your_water_KAR, col="white", border="white", add=T)
plot(your_points_KAR["sm_mean"], lwd = 2, cex = 2, bg= NA, pch=19, 
     col = your_points_KAR$color, add=T)
scalebar(1000, xy=c(319518  ,6680106  ), type="line", label = c("1 km"))
#plot_KAR <- recordPlot()
dev.off()

