library(pals)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
library(moments)
library(patchwork)
library(scales)
library(sf)

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
  select(-moist_prop2) -> your_sm

# How many sites in total?
length(unique(your_sm$site)) # 503 sites selected

# How many measurements in total?
your_sm %>% filter(!is.na(moist_mean)) %>% nrow()*24*4 # The total number of obsevations

# Calculate moisture variables
your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ungroup() -> d

d %>% mutate(area2 = substr(site, 1, 3)) -> d

# How many sites per area included after filtering?
table(d$area)

# bring predictors
predictors <- read_csv("data/all_env_variables.csv") %>% select(-area) %>% filter(logger == "Tomst" | is.na(logger))
predictors_climate <- read_csv("data/ERA5_means.csv") %>% 
  rename(area2 = area)

# Rastigaisa is only Tundra, thus we impute missing canopy variables with zero
predictors <- predictors %>% mutate(across(chm:pgaps5m_metsakeskus_20, ~ifelse(grepl("RAS", id_code), 0, .x)))
predictors %>% filter(grepl("RAS", id_code))


#rename id_code
predictors <- rename(predictors, plot = site)
predictors <- rename(predictors, site = id_code)

# Get projected UTM coordinates
predictors %>% select(site, x, y) %>% 
  st_as_sf(coords = c("x","y"), crs = 4326) %>% 
  st_transform(crs = 32635) %>% 
  mutate(x_utm = st_coordinates(.)[,"X"],
         y_utm = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  full_join(., predictors) -> predictors
  

left_join(d, predictors) -> d
left_join(d, predictors_climate) %>% 
  select(-area2) -> d

summary(d)
d %>% filter(is.na(x_utm)) # None, so good!

# 2-D pairwise distances based on soil moisture mean & sd

d %>% column_to_rownames("site") %>% 
  select(sm_mean, sm_sd) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~scale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm1 <- data.frame(xy, moist=dm[xy])

d %>% column_to_rownames("site") %>% 
  select(sm_mean) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm1 <- full_join(dm1, data.frame(xy, moist_mean=dm[xy]))

d %>% column_to_rownames("site") %>% 
  select(sm_sd) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm1 <- full_join(dm1, data.frame(xy, moist_sd=dm[xy]))


# Geographic distances

d %>% column_to_rownames("site") %>% 
  select(y_utm, x_utm) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm_geo <- data.frame(xy, geo_dist=dm[xy])

d %>% column_to_rownames("site") %>% 
  select(y_utm) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm_geo <- full_join(dm_geo, data.frame(xy, lat_dist=dm[xy]))

d %>% column_to_rownames("site") %>% 
  select(x_utm) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm_geo <- full_join(dm_geo, data.frame(xy, lon_dist=dm[xy]))

# Climate distances

d %>% column_to_rownames("site") %>% 
  select(mean_2m_temperature, sum_total_precipitation) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~scale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm_clim <- data.frame(xy, clim_dist=dm[xy])

d %>% column_to_rownames("site") %>% 
  select(mean_2m_temperature) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm_clim <- full_join(dm_clim, data.frame(xy, temp_dist=dm[xy]))

d %>% column_to_rownames("site") %>% 
  select(sum_total_precipitation) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm_clim <- full_join(dm_clim, data.frame(xy, prec_dist=dm[xy]))


# swi
d %>% column_to_rownames("site") %>% 
  select(swi) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~scale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm2 <- data.frame(xy, swi=dm[xy])


# Vegetation distance
d %>% column_to_rownames("site") %>% 
  select(canopy_cover2m_5m, chm) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~scale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm3 <- data.frame(xy, canopy=dm[xy])

## allwet_prop_10m
#d %>% column_to_rownames("site") %>% 
#  select(allwet_prop_10m) %>% 
#  filter(complete.cases(.)) %>% 
#  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm
#
#xy <- t(combn(colnames(dm), 2))
#dm3 <- data.frame(xy, allwet_prop_10m=dm[xy])
#
## fluvial_effect
#d %>% column_to_rownames("site") %>% 
#  select(fluvial_effect) %>% 
#  filter(complete.cases(.)) %>% 
#  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm
#
#xy <- t(combn(colnames(dm), 2))
#dm4 <- data.frame(xy, fluvial_effect=dm[xy])


# Combine the pairwise distance dataframes

all <- full_join(dm1, dm2) %>% 
  full_join(., dm3) %>% 
  full_join(., dm_geo) %>% 
  full_join(., dm_clim) %>% 
  mutate(geo_dist_log = log(geo_dist+1),
         lat_dist_log = log(lat_dist+1),
         lot_dist_log = log(lon_dist+1))

names (all)

# Correlation between all variables
cor(all %>% select(moist:lot_dist_log), use = "pairwise.complete.obs", method = "spearman") %>% round(3)
# cor(all %>% select(moist:lot_dist_log), use = "pairwise.complete.obs", method = "pearson") %>% round(3)

# For focus variables only
cor(all %>% select(moist,swi,canopy,geo_dist,clim_dist), use = "pairwise.complete.obs", method = "spearman") %>% round(3)
# Latitude, Longitude, Temperature and Precipitation as separate variables
cor(all %>% select(moist,lat_dist,lon_dist,temp_dist,prec_dist), use = "pairwise.complete.obs", method = "spearman") %>% round(3)

# lm
# Scale variables to enable parameter comparison
all_scaled <- all %>% 
  as.data.frame() %>% mutate(across(moist:lot_dist_log, ~scale(.x)))


hist(sqrt(all$moist))

lm.moist_base <- lm(moist ~ swi + canopy + geo_dist + clim_dist, data =all_scaled)
summary(lm.moist_base)
# With log-transformed geographic distance
lm.moist_log <- lm(moist ~ swi + canopy + geo_dist_log + clim_dist, data =all_scaled)
summary(lm.moist_log)
# Nothing especially interesting here, maybe we use only the pairwise correlations?

# Scatter plots
plot_swi <- all %>%
  ggplot(aes(x=swi, y=moist)) +
  geom_point(size = 1, alpha=1/100) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method = lm, se = T, colour="red") +
  ylab ("Moisture space distance") +
  xlab ("Topograhic distance") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

plot_geo_dist <- all %>% 
  ggplot(aes(x=geo_dist, y=moist)) +
  geom_point(size = 1, alpha=1/100) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method = lm, se = T, colour="red") +
  ylab ("Moisture space distance") +
  xlab ("Geographic distance") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

plot_clim_dist <- all %>% 
  ggplot(aes(x=clim_dist, y=moist)) +
  geom_point(size = 1, alpha=1/100) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method = lm, se = T, colour="red") +
  ylab ("Moisture space distance") +
  xlab ("Climate distance") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

plot_veg_dist <- all %>% 
  ggplot(aes(x=canopy, y=moist)) +
  geom_point(size = 1, alpha=1/100) +
  scale_fill_continuous(type = "viridis") +
  geom_smooth(method = lm, se = T, colour="red") +
  ylab ("Moisture space distance") +
  xlab ("Vegetation distance") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())


(plot_swi | plot_geo_dist) / (plot_clim_dist | plot_veg_dist) + plot_layout(guides = "collect")
