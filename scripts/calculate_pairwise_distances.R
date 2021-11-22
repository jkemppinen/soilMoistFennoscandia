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
your_sm  <- read_csv ("data/all_data_daily_2021.csv")

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
                              RAS = "a",
                              KIL = "b",
                              VAR = "c",
                              TII = "d",
                              PIS = "e",
                              HYY = "f",
                              KAR = "g")

your_sm %>% 
  filter(year(date) %in% c(2020)) %>%
  filter(month(date) %in% c(7, 8)) -> your_sm

your_sm %>% group_by(site) %>% 
  summarise(moist_prop2 = mean(moist_prop)) -> aggr

aggr %>% filter(moist_prop2 != 100 & moist_prop2 > 90)

full_join(your_sm, aggr) %>% filter(moist_prop2 > 90) %>% 
  select(-moist_prop2) -> your_sm

your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = log(sd(moist_mean, na.rm = T)),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
    ungroup() -> d

# bring predictors
predictors <- read_csv("data/all_env_variables.csv") %>% select(-area) %>% filter(logger == "Tomst")

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
  

full_join(d, predictors) -> d

summary (d)

# 2-D pairwise distances based on soil moisture mean & sd

d %>% column_to_rownames("site") %>% 
  select(sm_mean, sm_sd) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm1 <- data.frame(xy, moist=dm[xy])

# Geographic distances
d %>% column_to_rownames("site") %>% 
  select(y_utm, x_utm) %>% 
  filter(complete.cases(.)) %>% 
  dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm_geo <- data.frame(xy, geo_dist=dm[xy])

# swi
d %>% column_to_rownames("site") %>% 
  select(swi) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm2 <- data.frame(xy, swi=dm[xy])

# allwet_prop_10m
d %>% column_to_rownames("site") %>% 
  select(allwet_prop_10m) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm3 <- data.frame(xy, allwet_prop_10m=dm[xy])

# fluvial_effect
d %>% column_to_rownames("site") %>% 
  select(fluvial_effect) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm4 <- data.frame(xy, fluvial_effect=dm[xy])

# canopy_portion_conif
d %>% column_to_rownames("site") %>% 
  select(canopy_portion_conif) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm5 <- data.frame(xy, canopy_portion_conif=dm[xy])

names(d)

# Combine the pairwise distance dataframes

all <- full_join(dm1, dm2) %>% 
  full_join(., dm3) %>% 
  full_join(., dm4) %>% 
  full_join(., dm5) %>% 
  full_join(., dm_geo)

names (all)

cor(all$moist, all$canopy_portion_conif, use = "pairwise.complete.obs")
cor(all[,3:7], use = "pairwise.complete.obs", method = "spearman")
cor(all[,3:8] %>% mutate(geo_dist = sqrt(geo_dist)), use = "pairwise.complete.obs", method = "spearman")
# lm
lm.moist <- lm(moist ~ swi + allwet_prop_10m + fluvial_effect + canopy_portion_conif, data =all)
lm.moist <- lm(moist ~ swi + allwet_prop_10m + fluvial_effect + canopy_portion_conif + sqrt(geo_dist), data =all)

anova(lm.moist)
summary(lm.moist)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.moist, las = 1)      # Residuals, Fitted, ...
par(opar)

plot_swi <- all %>% 
  ggplot(aes(x=moist, y=swi)) +
  geom_point(aes(size = 0.5), alpha=1/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="red") +
  
  ggtitle ("Distance in relation to swi") +
  ylab ("") +
  xlab ("Distance in relation to mean and SD") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

plot_allwet_prop_10m <- all %>% 
  ggplot(aes(x=moist, y=allwet_prop_10m)) +
  geom_point(aes(size = 0.5), alpha=1/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="red") +
  
  ggtitle ("Distance in relation to allwet_prop_10m") +
  ylab ("") +
  xlab ("Distance in relation to mean and SD") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

plot_fluvial_effect <- all %>% 
  ggplot(aes(x=moist, y=fluvial_effect)) +
  geom_point(aes(size = 0.5), alpha=1/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="red") +
  
  ggtitle ("Distance in relation to fluvial_effect") +
  ylab ("") +
  xlab ("Distance in relation to mean and SD") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

plot_canopy_portion_conif <- all %>% 
  ggplot(aes(x=moist, y=canopy_portion_conif)) +
  geom_point(aes(size = 0.5), alpha=1/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="red") +
  
  ggtitle ("Distance in relation to canopy_portion_conif") +
  ylab ("") +
  xlab ("Distance in relation to mean and SD") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

(plot_swi | plot_allwet_prop_10m) / (plot_fluvial_effect | plot_canopy_portion_conif) + plot_layout(guides = "collect")

gam_moist <- gam(moist~s(swi, k = 4)+s(allwet_prop_10m, k = 4)+s(plot_fluvial_effect, k = 4)+s(plot_canopy_portion_conif, k = 4),data=all)
summary(gam_moist)

