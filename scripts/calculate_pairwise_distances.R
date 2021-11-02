
library(tidyverse)
library(scales)

your_sm  <- read_csv ("data/all_data_daily.csv")

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
                              RAS = "a",
                              KIL = "b",
                              VAR = "c",
                              TII = "d",
                              PIS = "e",
                              HYY = "f",
                              KAR = "g")

your_sm %>% group_by(id_code, area) %>% 
  filter(moist_prop == 100 & error_tomst < 2) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            T1_mean = mean(T1_mean, na.rm = T)) %>% 
  ungroup() -> d

# 2-D pairwise distances based on soil moisture mean & sd

d %>% column_to_rownames("id_code") %>% 
  select(sm_mean, sm_sd) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm1 <- data.frame(xy, moist=dm[xy])

# 1-D pairwise distances based on T1 mean temperature

d %>% column_to_rownames("id_code") %>% 
  select(T1_mean) %>% 
  filter(complete.cases(.)) %>% 
  mutate(across(everything(), ~rescale(.x))) %>% dist() %>% as.matrix() -> dm

xy <- t(combn(colnames(dm), 2))
dm2 <- data.frame(xy, T1=dm[xy])

# Combine the pairwise distance dataframes

all <- full_join(dm1, dm2)

cor(all$moist, all$T1)
