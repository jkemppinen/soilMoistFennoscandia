###############################################################################
# Gather and combine moisture data from study area specific Github repositories


library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)

# Download datasets
# Rastigaisa
d1 <- read_csv("https://raw.githubusercontent.com/poniitty/Rastigaisa_microclimate/main/output/tomst_data_daily.csv")

# Kilpisjärvi
d2 <- read_csv("https://raw.githubusercontent.com/poniitty/kilpisjarvi_microclimate/main/output/tomst_data_daily.csv") %>% 
  filter(!grepl("RA", site))

# Värriö
d3 <- read_csv("https://raw.githubusercontent.com/poniitty/varrio_microclimate/main/output/tomst_data_daily.csv")

# Tiilikka
d4 <- read_csv("https://raw.githubusercontent.com/poniitty/Tiilikka_microclimates/main/output/tomst_data_daily.csv")

# Pisa
d5 <- read_csv("https://raw.githubusercontent.com/poniitty/Pisa_microclimates/main/output/tomst_data_daily.csv")

# Hyytiälä
d6 <- read_csv("https://raw.githubusercontent.com/poniitty/Hyytiala_microclimates/main/output/tomst_data_daily.csv")

# Karkali
d7 <- read_csv("https://raw.githubusercontent.com/poniitty/Karkali_microclimate/main/output/tomst_data_daily.csv")

# Function for site identifiers
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

# Combine all data and edit the sites ID's
bind_rows(d1 %>% mutate(site = paste0("RAS",unlist(lapply(site, add_zeros)))),
          d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> daily

# Check
unique(daily$area)

# Daily data ready!
write_csv(daily, "data/all_data_daily_2021.csv")

############################################################################
# Aggregate to monthly values

# Create a matrix of number of days in each calender month
daycount <- data.frame(date = c(as_date(floor_date(min(daily$date), unit = "month"):
                                          (ceiling_date(max(daily$date), unit = "month")-days(1))))) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(year, month) %>% 
  summarise(ndaysmax = n())

# Aggregation to monthly values is done separately for each sensors 
# due to different filtering based on error codes

# First T1
daily %>% 
  filter(T1_prop == 100,
         probl %in% c(0,3),
         is.finite(T1_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T1_mean = round(mean(T1_mean, na.rm = T),2),
            T1_absmax = round(max(T1_max, na.rm = T),2),
            T1_absmin = round(min(T1_min, na.rm = T),2),
            T1_meanmax = round(mean(T1_max, na.rm = T),2),
            T1_meanmin = round(mean(T1_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T1 = ndays/ndaysmax) %>% 
  relocate(day_frac_T1, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T1

# T2
daily %>% 
  filter(T2_prop == 100,
         probl %in% c(0,3,4),
         is.finite(T2_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T2_mean = round(mean(T2_mean, na.rm = T),2),
            T2_absmax = round(max(T2_max, na.rm = T),2),
            T2_absmin = round(min(T2_min, na.rm = T),2),
            T2_meanmax = round(mean(T2_max, na.rm = T),2),
            T2_meanmin = round(mean(T2_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T2 = ndays/ndaysmax) %>% 
  relocate(day_frac_T2, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T2

# T3
daily %>% 
  filter(T3_prop == 100,
         probl %in% c(0),
         is.finite(T3_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            T3_mean = round(mean(T3_mean, na.rm = T),2),
            T3_absmax = round(max(T3_max, na.rm = T),2),
            T3_absmin = round(min(T3_min, na.rm = T),2),
            T3_meanmax = round(mean(T3_max, na.rm = T),2),
            T3_meanmin = round(mean(T3_min, na.rm = T),2)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_T3 = ndays/ndaysmax) %>% 
  relocate(day_frac_T3, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_T3

# moist
daily %>% 
  filter(moist_prop == 100,
         probl %in% c(0,3,4),
         is.finite(moist_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            moist_sd = round(sd(moist_mean, na.rm = T),2),
            moist_cv = round(sd(moist_mean, na.rm = T)/mean(moist_mean, na.rm = T),2),
            moist_med = round(median(moist_mean, na.rm = T),1),
            moist_mean = round(mean(moist_mean, na.rm = T),1),
            moist_absmax = round(max(moist_max, na.rm = T),1),
            moist_absmin = round(min(moist_min, na.rm = T),1)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_moist = ndays/ndaysmax) %>% 
  relocate(day_frac_moist, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_moist

full_join(dm_T1, dm_T2) %>% 
  full_join(., dm_T3) %>% 
  full_join(., dm_moist) %>% 
  mutate(across(starts_with("day_f"), ~ifelse(!is.finite(.x), 0, .x))) %>% 
  relocate(starts_with("day_f"), .after = month) %>% 
  relocate(starts_with("moist"), .after = day_frac_moist)-> dm

rm(dm_T1, dm_T2, dm_T3, dm_moist)

write_csv(dm, "data/tomst_data_monthly_2021.csv")

###################################################################################
# SNOW COVER DURATION

# Here I calculate stuff to determine which dates have been under snow
daily %>% group_by(site) %>% 
  select(site:date, T2_min, T2_max) %>% 
  mutate(center_t = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         center_tmin = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "center"),
         lead_t = rollapply(T2_max, width=10, FUN=max, fill = NA, partial = T, align = "right"),
         lead_tmin = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "right"),
         lag_t = rollapply(T2_max, width=10, FUN=max, fill = NA, partial = T, align = "left"),
         lag_tmin = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "left")) %>% 
  mutate(center_range = center_t-center_tmin,
         lead_range = lead_t-lead_tmin,
         lag_range = lag_t-lag_tmin,
         center_t = ifelse(center_t < 1 & center_range < 10, 1, 0),
         lead_t = ifelse(lead_t < 1 & lead_range < 10, 1, 0),
         lag_t = ifelse(lag_t < 1 & lag_range < 10, 1, 0)) %>% 
  mutate(lead_c = rollapply(center_t, width=5, FUN=max, fill = NA, partial = T, align = "right"),
         lag_c = rollapply(center_t, width=5, FUN=max, fill = NA, partial = T, align = "left")) %>% 
  rowwise() %>% mutate(center_c = max(c(lead_c, lag_c))) -> dd1

# Plot an example

id_to_plot <- "AIL101"

dd1 %>% filter(site == id_to_plot) %>%
  ggplot(aes_string(x="date")) +
  geom_hline(yintercept = 0)+
  geom_ribbon(aes(ymin=T2_min, ymax=T2_max), fill = "gray10")+
  geom_line(aes(y = lead_t), col = "white", size = 0.8) +
  geom_line(aes(y = lag_t), col = "blue", size = 0.8) +
  geom_line(aes(y = center_c), col = "red", size = 0.8) +
  theme_dark() +
  ylab("Temperature") + xlab("Date")+
  ggtitle(id_to_plot)

###############################################################################################
# SIMILAR CALCULATIONS BUT EXCUDING VERY SHALLOW SNOW WITH POOR INSULATING CAPACITY

# Here I calculate stuff to determine which dates have been under snow
daily %>% group_by(site) %>% 
  dplyr::select(site:date, T2_min, T2_max, T3_min, T3_max) %>% 
  mutate(T2_range = T2_max-T2_min,
         T3_range = T3_max-T3_min) %>% 
  mutate(center_t2 = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         center_t2min = rollapply(T2_min, width=10, FUN=min, fill = NA, partial = T, align = "center")) %>% 
  mutate(center_range2 = center_t2-center_t2min) %>% 
  mutate(center_t3 = rollapply(T3_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         center_t3min = rollapply(T3_min, width=10, FUN=min, fill = NA, partial = T, align = "center")) %>% 
  mutate(center_range3 = center_t3-center_t3min) %>% 
  mutate(T2_range = rollapply(T2_range, width=9, FUN=mean, fill = NA, partial = T, align = "center"),
         T3_range = rollapply(T3_range, width=9, FUN=mean, fill = NA, partial = T, align = "center"),
         T2_max = rollapply(T2_max, width=9, FUN=max, fill = NA, partial = T, align = "center"),
         T3_max = rollapply(T3_max, width=9, FUN=max, fill = NA, partial = T, align = "center")) %>% 
  mutate(T2_snow = ifelse(T2_max < 1 & T2_range < 1 & center_range2 < 2, 1, 0),
         T3_snow = ifelse(T3_max < 1 & T3_range < 1 & center_range3 < 2, 1, 0)) %>% 
  mutate(lead_T2 = rollapply(T2_snow, width=5, FUN=max, fill = NA, partial = T, align = "right"),
         lag_T2 = rollapply(T2_snow, width=5, FUN=max, fill = NA, partial = T, align = "left"),
         lead_T3 = rollapply(T3_snow, width=5, FUN=max, fill = NA, partial = T, align = "right"),
         lag_T3 = rollapply(T3_snow, width=5, FUN=max, fill = NA, partial = T, align = "left")) %>% 
  rowwise() %>% mutate(T2_snow = max(c(lead_T2, lag_T2)),
                       T3_snow = max(c(lead_T3, lag_T3))) -> dd2


# Plot an example site
dd2 %>% filter(site == id_to_plot) %>%
  ggplot(aes_string(x="date")) +
  geom_hline(yintercept = 0)+
  geom_ribbon(aes(ymin=T2_min, ymax=T2_max), fill = "gray10")+
  geom_line(aes(y = T2_snow), col = "white", size = 0.8) +
  geom_line(aes(y = T3_snow), col = "blue", size = 0.8) +
  theme_dark() +
  ylab("Temperature") + xlab("Date")+
  ggtitle(id_to_plot)


# Combine the snow variables

dd <- full_join(dd1 %>% select(site, area, date, center_c) %>% 
                  rename(snowy = center_c),
                dd2 %>% select(site, area, date, T2_snow, T3_snow) %>% 
                  rename(snowy_deep2 = T2_snow,
                         snowy_deep3 = T3_snow))


dd %>% as.data.table() %>% sample_n(10)

# Write results to csv
write_csv(dd, "data/snowy_days.csv")
