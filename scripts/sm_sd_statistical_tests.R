library(tidyverse)
library(lubridate)
library(scales)
library(broom)

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

# These are used to filter monthly datasets
your_sm %>% 
  mutate(month = month(date)) %>% 
  group_by(area, site, month) %>% count() %>% filter(n > 10) -> site_aggr

site_aggr %>% 
  group_by(area, month) %>% count() %>% filter(n > 20) -> area_aggr

#subset
your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(4)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_04

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(5)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_05

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(6)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_06

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(7)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_07

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(8)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_08

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(9)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_09

# agregate moisture mean and sd by site
your_sm_aggr <- your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T))


# LM models using all data
lml <- your_sm_aggr %>%  lm(sm_sd ~ poly(sm_mean, 1), data = .)
lmp <- your_sm_aggr %>%  lm(sm_sd ~ poly(sm_mean, 2), data = .)

aovsa <- anova(lml, lmp) %>% tidy() %>% slice(2) %>% 
  mutate(area = "ALL", month = "All") %>% 
  select(area, p.value, month)

glance(lml) %>% mutate(area = "ALL") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfitsa

glance(lmp) %>% mutate(area = "ALL") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfitsa

fitsa <- full_join(aovsa, lfitsa) %>% 
  full_join(., pfitsa)


# By area
your_sm_aggr %>% 
  nest(-area) %>% 
  mutate(fit_linear = map(data, ~ lm(sm_sd ~ poly(sm_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(sm_sd ~ poly(sm_mean, 2), data = .))) -> fits

names(fits$fit_linear) <- names(fits$fit_poly) <- fits$area

fits <- fits %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs <- map(fits$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "All") %>% 
  select(area, p.value, month)

map(fits$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfits

map(fits$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfits

fits0 <- full_join(aovs, lfits) %>% 
  full_join(., pfits)

# Monthly and by area

# April
your_sm_04 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  nest(-area) %>% 
  mutate(fit_linear = map(data, ~ lm(sm_sd ~ poly(sm_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(sm_sd ~ poly(sm_mean, 2), data = .))) -> fits4

names(fits4$fit_linear) <- names(fits4$fit_poly) <- fits4$area

fits4 <- fits4 %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs4 <- map(fits4$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "April") %>% 
  select(area, p.value, month)

map(fits4$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfits4

map(fits4$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfits4

fits4 <- full_join(aovs4, lfits4) %>% 
  full_join(., pfits4)

# May
your_sm_05 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  nest(-area) %>% 
  mutate(fit_linear = map(data, ~ lm(sm_sd ~ poly(sm_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(sm_sd ~ poly(sm_mean, 2), data = .))) -> fits5

names(fits5$fit_linear) <- names(fits5$fit_poly) <- fits5$area

fits5 <- fits5 %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs5 <- map(fits5$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "May") %>% 
  select(area, p.value, month)

map(fits5$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfits5

map(fits5$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfits5

fits5 <- full_join(aovs5, lfits5) %>% 
  full_join(., pfits5)


# June
your_sm_06 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  nest(-area) %>% 
  mutate(fit_linear = map(data, ~ lm(sm_sd ~ poly(sm_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(sm_sd ~ poly(sm_mean, 2), data = .))) -> fits6

names(fits6$fit_linear) <- names(fits6$fit_poly) <- fits6$area

fits6 <- fits6 %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs6 <- map(fits6$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "June") %>% 
  select(area, p.value, month)

map(fits6$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfits6

map(fits6$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfits6

fits6 <- full_join(aovs6, lfits6) %>% 
  full_join(., pfits6)


# July
your_sm_07 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  nest(-area) %>% 
  mutate(fit_linear = map(data, ~ lm(sm_sd ~ poly(sm_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(sm_sd ~ poly(sm_mean, 2), data = .))) -> fits7

names(fits7$fit_linear) <- names(fits7$fit_poly) <- fits7$area

fits7 <- fits7 %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs7 <- map(fits7$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "July") %>% 
  select(area, p.value, month)

map(fits7$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfits7

map(fits7$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfits7

fits7 <- full_join(aovs7, lfits7) %>% 
  full_join(., pfits7)


# August
your_sm_08 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  nest(-area) %>% 
  mutate(fit_linear = map(data, ~ lm(sm_sd ~ poly(sm_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(sm_sd ~ poly(sm_mean, 2), data = .))) -> fits8

names(fits8$fit_linear) <- names(fits8$fit_poly) <- fits8$area

fits8 <- fits8 %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs8 <- map(fits8$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "July") %>% 
  select(area, p.value, month)

map(fits8$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfits8

map(fits8$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfits8

fits8 <- full_join(aovs8, lfits8) %>% 
  full_join(., pfits8)


# September
your_sm_09 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  nest(-area) %>% 
  mutate(fit_linear = map(data, ~ lm(sm_sd ~ poly(sm_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(sm_sd ~ poly(sm_mean, 2), data = .))) -> fits9

names(fits9$fit_linear) <- names(fits9$fit_poly) <- fits9$area

fits9 <- fits9 %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs9 <- map(fits9$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "Semptember") %>% 
  select(area, p.value, month)

map(fits9$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> lfits9

map(fits9$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> pfits9

fits9 <- full_join(aovs9, lfits9) %>% 
  full_join(., pfits9)

# Combine results

ress <- bind_rows(fitsa,
                  fits0,
                  fits4,
                  fits5,
                  fits6,
                  fits7,
                  fits8,
                  fits9) %>% 
  relocate(R2_poly, .after = R2_linear) %>% 
  relocate(p.value, .after = AIC_poly) %>% 
  rename(anova_p_value = p.value) %>% 
  mutate(signf = ifelse(anova_p_value < 0.1, ".", ""),
         signf = ifelse(anova_p_value < 0.05, "*", signf),
         signf = ifelse(anova_p_value < 0.01, "**", signf),
         signf = ifelse(anova_p_value < 0.001, "***", signf))
  

write_csv(ress, "output/sd_sm_model_results.csv")

