library(tidyverse)
library(raster)

f <- c(list.files("E:/kriging_microclim/Data/Finland", pattern = "precipit", full.names = T),
       list.files("E:/kriging_microclim/Data/Finland2", pattern = "precipit", full.names = T))


f <- f[grepl("2020-06", f) | grepl("2020-07", f) | grepl("2020-08", f) | grepl("2020-09", f)]


s <- stack()
for(i in f){
  print(i)
  s <- stack(s, round(raster(i)*10000))
}

prec <- round(sum(s, na.rm = T)/10)

plot(prec)

writeRaster(prec, "data/ERA_precipitation_2020.tif",
            format = "GTiff", datatype = "INT2U")
