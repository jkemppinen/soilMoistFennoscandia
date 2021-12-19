library(tidyverse)
library(lubridate)


d <- bind_rows(read_csv("data/ERA_raw_values.csv"),
               read_csv("data/ERA_raw_values2.csv")) %>% 
  arrange(area, var, date)# %>% 
  # mutate(area = ifelse(area %in% c("SAA","AIL","MAL"), "KIL", area)) %>% 
  # group_by(area, date, var) %>% 
  # summarise_all(mean)

unique(d$var)

d %>% filter(date >= "2020-04-01") %>% 
  filter(date < "2020-10-01") %>% 
  pivot_longer(cols = starts_with("X"), names_to = "hour") %>% 
  mutate(hour = parse_number(hour)) %>% 
  filter(!(var == "total_precipitation" & hour > 1)) %>% 
  group_by(area, var) %>% 
  summarise(mean = mean(value),
            sum = sum(value)) %>% 
  filter(var %in% c("2m_temperature","total_precipitation")) %>% 
  pivot_wider(id_cols = area, names_from = var, values_from = mean:sum) %>% 
  select(-mean_total_precipitation, -sum_2m_temperature) %>% 
  mutate(mean_2m_temperature = mean_2m_temperature-272.15, 
         sum_total_precipitation = sum_total_precipitation*1000) -> area_means

write_csv(area_means, "data/ERA5_means.csv")

# Downscaled

d <- read_csv("data/all_era.csv") %>% 
  filter(date >= "2020-06-01") %>% 
  filter(date < "2020-10-01")

d %>% filter(area == "MAL", hour == 11, id %in% c(1, 87))

d %>% group_by(area, id) %>% 
  summarise(t = mean(t),
            precip = sum(precip, na.rm = T)) -> dam
dam %>% mutate(precip = ifelse(precip > 1000, 416, precip)) -> dam

dam %>% filter(area == "SAA") %>% summary()
dam %>% filter(area == "MAL", id %in% c(1, 87))
dam %>% filter(area == "AIL", id %in% c(105, 156, 197))


dam %>% filter(precip > 420)
