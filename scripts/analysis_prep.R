library(pals)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
library(moments)
library(patchwork)
library(scales)

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

write_csv(your_sm, "data/analysis_data.csv")

# bring predictors
predictors <- read_csv("data/all_env_variables.csv") %>% select(-area) %>% filter(logger == "Tomst")

#rename id_code
predictors <- rename(predictors, plot = site)
predictors <- rename(predictors, site = id_code)

write_csv(predictors, "data/analysis_data_env.csv")
