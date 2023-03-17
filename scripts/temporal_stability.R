library(tidyverse)
library(lubridate)
library(scales)
library(broom)

# bring logger soil moisture files

areas_to_model <- c("KIL","VAR","TII","PIS","HYY","KAR","RAS")

d <- read_csv("data/all_data_daily_2021.csv") %>% 
  filter(area %in% areas_to_model) %>% 
  filter(moist_prop > 95) %>% 
  mutate(week = week(date))

d %>% filter(!is.na(moist2_median)) %>% 
  group_by(area, date) %>% 
  count() -> count_sites 

left_join(count_sites,
          count_sites %>% group_by(area) %>% summarise(maxn = max(n))) %>% 
  mutate(n_prop = n/maxn) %>% 
  filter(n_prop >= 0.666) -> count_sites

left_join(d, count_sites) %>% 
  filter(!is.na(moist2_median)) %>% 
  filter(!is.na(n_prop)) %>% 
  select(area, site, date, moist2_median) %>% 
  ungroup() -> d

d <- d %>% 
  mutate(area = recode_factor(area,
                              RAS = "RAS",
                              KIL = "KIL",
                              VAR = "VAR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR"))

for(areaid in unique(d$area)){
  
}

cors <- lapply(unique(d$area), function(areaid){
  cmat <- d %>% 
    filter(area == areaid) %>% 
    pivot_wider(id_cols = c(area, site), names_from = date, values_from = moist2_median) %>% 
    select(-area, -site) %>% 
    cor(., use = "pairwise.complete.obs")
  vals <- cmat[upper.tri(cmat, diag = FALSE)]
  return(tibble(area = areaid,
                mean_cor = mean(vals),
                min_cor = min(vals),
                max_cor = max(vals)))
})

cors <- bind_rows(cors) %>% 
  mutate(area = recode_factor(area,
                              RAS = "RAS",
                              KIL = "KIL",
                              VAR = "VAR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR")) %>% 
  arrange(area)

cors %>% 
  mutate(across(mean_cor:max_cor, ~round(.x, 2)))

cors %>% write_csv("output/temporal_stability.csv")
