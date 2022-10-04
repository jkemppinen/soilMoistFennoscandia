library(terra)
library(tidyverse)
library(sf)

tifs <- list.files("/appl/data/geo/luke/vmi/2019", pattern = "img$", full.names = T)

# Prepocess study area polygons

rois <- bind_rows(st_read("data/area_polygons_all.gpkg") %>% 
                    filter(area %in% c("AIL","MAL","SAA","RAR")) %>% 
                    st_bbox() %>% 
                    st_as_sfc() %>% st_as_sf() %>% 
                    mutate(area = "KIL",
                           full_name = "kilpisjarvi") %>% 
                    rename(geom = x),
                  st_read("data/area_polygons_all.gpkg") %>% 
                    filter(!(area %in% c("AIL","MAL","SAA","RAR")))) %>% 
  st_transform(crs = 3067)

rois <- rois %>% filter(area != "RAS")

# LUKE TWI

for(area_name in rois$area){
  
  print(area_name)
  
  roi <- rois %>%
    filter(area == area_name)
  
  roi_t <- roi %>% 
    st_buffer(3000)
  
  r <- rast("/appl/data/geo/luke/twi/TWI_16m_Finland_NA_lakes_int.tif")
  
  r <- crop(r, roi_t)
  
  plot(r, main = area_name)
  
  writeRaster(r, paste0("data/luke_twi_", area_name,".tif"),
              datatype = "INT2U", filetype = "GTiff", overwrite = T)
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

#############################

for(area_name in rois$area){
  
  print(area_name)
  
  roi <- rois %>%
    filter(area == area_name)
  
  roi_t <- roi %>% 
    st_buffer(500)
  
  # 0.5 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_00_5ha/DTW_00_5ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_00_5ha/", pols$location)
  
  rast.list <- list()
  for(ii in 1:length(f)) { rast.list[ii] <- rast(f[ii]) }
  
  rast.list <- terra::src(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  plot(rast.mosaic, main = area_name)
  
  writeRaster(round(rast.mosaic*100), paste0("data/luke_dtw_0.5ha_",area_name,".tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  # 1 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_01_0ha/DTW_01_0ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_01_0ha/", pols$location)
  
  rast.list <- list()
  for(ii in 1:length(f)) { rast.list[ii] <- rast(f[ii]) }
  
  rast.list <- terra::src(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  plot(rast.mosaic, main = area_name)
  
  writeRaster(round(rast.mosaic*100), paste0("data/luke_dtw_1ha_",area_name,".tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  # 4 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_04_0ha/DTW_04_0ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_04_0ha/", pols$location)
  
  rast.list <- list()
  for(ii in 1:length(f)) { rast.list[ii] <- rast(f[ii]) }
  
  rast.list <- terra::src(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  plot(rast.mosaic, main = area_name)
  
  writeRaster(round(rast.mosaic*100), paste0("data/luke_dtw_4ha_",area_name,".tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  # 10 ha
  pols <- st_read("/appl/data/geo/luke/dtw/DTW_10_0ha/DTW_10_0ha.shp")
  pols <- pols[roi_t,]
  
  f <- paste0("/appl/data/geo/luke/dtw/DTW_10_0ha/", pols$location)
  
  rast.list <- list()
  for(ii in 1:length(f)) { rast.list[ii] <- rast(f[ii]) }
  
  rast.list <- terra::src(rast.list)
  rast.mosaic <- mosaic(rast.list)
  
  rast.mosaic <- terra::crop(rast.mosaic, roi_t)
  rast.mosaic[rast.mosaic < 0] <- 0
  
  plot(rast.mosaic, main = area_name)
  
  writeRaster(round(rast.mosaic*100), paste0("data/luke_dtw_10ha_",area_name,".tif"),
              filetype = "GTiff", datatype = "INT2U", overwrite = T)
  
  unlink(list.files(tempdir(), full.names = T, recursive = T))
  
}

###################################################
# EXTRACT TO POINTS

add_zeros <- function(x){
  if(nchar(x) == 1){
    return(as.character(paste0("00",x)))
  }
  if(nchar(x) == 2){
    return(as.character(paste0("0",x)))
  }
  if(nchar(x) > 2){
    return(as.character(x))
  }
}

p <- st_read("data/points_all.gpkg") %>% 
  mutate(site = paste0(area,unlist(lapply(id, add_zeros)))) %>% 
  mutate(area = ifelse(area %in% c("AIL","MAL","SAA","RAR"), "KIL", area)) %>% 
  filter(area != "RAS")

unique(p$area)

tifs <- list.files("data", pattern = "luke.*tif$", full.names = T)

alld <- tibble(p[0,])
for(area_name in unique(p$area)){
  ptemp <- p %>% filter(area == area_name)
  
  tifs2 <- tifs[grepl(area_name, tifs)]
  
  for(i in tifs2){
    r <- rast(i)
    
    ptemp$temp <- terra::extract(r, vect(ptemp %>% st_transform(crs = crs(r, proj = T))))[,2]
    
    names(ptemp)[match("temp", names(ptemp))] <- gsub("data/luke_","",gsub(paste0("_",area_name,".tif"),"",i))
  }
  
  alld <- bind_rows(alld, ptemp %>% tibble())
  
}

write_csv(alld %>% select(area, site, logger, dtw_0.5ha:twi),
          "data/luke_variables.csv")

unlink(list.files("data", pattern = "luke.*tif$", full.names = T))