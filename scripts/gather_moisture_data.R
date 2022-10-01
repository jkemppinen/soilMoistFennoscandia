###############################################################################
# Gather and combine moisture data from study area specific Github repositories

library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)

d <- fread("C:/datacloud/biogeoclimate/microclimate/data/logger/all_data.csv") %>% 
  select(-moist_count) %>% 
  relocate(arh, .after = T4) %>% 
  mutate(datetime = with_tz(datetime, tzone = "Etc/GMT-2")) %>% 
  mutate(date = as_date(datetime)) %>% 
  relocate(date, .after = datetime)

########################################################################
# AGGREGATE TO DAILY VALUES

notNA_prop <- function(x){ round(sum(is.finite(x)/length(x))*100,1) }

d %>% 
  group_by(id_code, date) %>%
  summarise(across(T1:moist1, ~notNA_prop(.x), 
                   na.rm = F, .names = "{.col}_prop"),
            across(T1:arh, list(mean = mean, min = min, max = max), 
                   na.rm = T, .names = "{.col}_{.fn}"),
            across(moist1:moist3, list(mean = mean, median = median, min = min, max = max), 
                   na.rm = T, .names = "{.col}_{.fn}"),
            error_tomst = max(error_tomst, na.rm = T),
            error_T4 = max(error_T4, na.rm = T),
            cal_class = max(cal_class, na.rm = T)) %>% 
  rename(site = id_code) %>% 
  ungroup() -> daily

# Change Inf and -Inf to NA
infmutate <- function(x) ifelse(is.infinite(x),NA,x)
daily %>% mutate(across(c(T1_prop:arh_prop, error_tomst, error_T4, cal_class), infmutate)) -> daily


# Combine all data and edit the sites ID's
daily %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> daily

# Check
unique(daily$area)

daily <- daily %>% rename(moist_prop = moist1_prop)

daily2 <- daily %>% 
  select(site:date, T1_mean, T2_mean, T3_mean, T4_mean, 
         starts_with("moist"), error_tomst, error_T4, cal_class) %>% 
  # filter(date >= "2020-01-01", date <= "2020-12-31") %>% 
  filter(date >= "2020-03-01", date <= "2020-10-31") %>% 
  mutate(area = ifelse(area %in% c("AIL","MAL","SAA"), "KIL",area)) %>% 
  filter(!grepl("PIS1", site)) %>%  # Exclude PISA sites in active forestry areas
  filter(!area %in% c("RAR"))

# Daily data ready!
write_csv(daily2, "data/all_data_daily_2021.csv")

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
         error_tomst %in% c(0,3),
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
         error_tomst %in% c(0,3,4),
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
         error_tomst %in% c(0),
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

# moist1
daily %>% 
  filter(moist_prop == 100,
         error_tomst %in% c(0,3,4),
         is.finite(moist1_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            moist1_sd = round(sd(moist1_mean, na.rm = T),2),
            moist1_cv = round(sd(moist1_mean, na.rm = T)/mean(moist1_mean, na.rm = T),2),
            moist1_med = round(median(moist1_median, na.rm = T),1),
            moist1_mean = round(mean(moist1_mean, na.rm = T),1),
            moist1_max = round(max(moist1_mean, na.rm = T),1),
            moist1_min = round(min(moist1_mean, na.rm = T),1),
            moist1_absmax = round(max(moist1_max, na.rm = T),1),
            moist1_absmin = round(min(moist1_min, na.rm = T),1)) %>% 
  left_join(., daycount) %>% 
  mutate(day_frac_moist = ndays/ndaysmax) %>% 
  relocate(day_frac_moist, .after = ndays) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_moist1

# moist2
daily %>% 
  filter(moist_prop == 100,
         error_tomst %in% c(0,3,4),
         is.finite(moist2_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            moist2_sd = round(sd(moist2_mean, na.rm = T),2),
            moist2_cv = round(sd(moist2_mean, na.rm = T)/mean(moist2_mean, na.rm = T),2),
            moist2_med = round(median(moist2_median, na.rm = T),1),
            moist2_mean = round(mean(moist2_mean, na.rm = T),1),
            moist2_max = round(max(moist2_mean, na.rm = T),1),
            moist2_min = round(min(moist2_mean, na.rm = T),1),
            moist2_absmax = round(max(moist2_max, na.rm = T),1),
            moist2_absmin = round(min(moist2_min, na.rm = T),1)) %>% 
  left_join(., daycount) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_moist2

# moist3
daily %>% 
  filter(moist_prop == 100,
         error_tomst %in% c(0,3,4),
         is.finite(moist3_mean)) %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(site, year, month) %>% 
  summarise(ndays = n(),
            moist3_sd = round(sd(moist3_mean, na.rm = T),2),
            moist3_cv = round(sd(moist3_mean, na.rm = T)/mean(moist3_mean, na.rm = T),2),
            moist3_med = round(median(moist3_median, na.rm = T),1),
            moist3_mean = round(mean(moist3_mean, na.rm = T),1),
            moist3_max = round(max(moist3_mean, na.rm = T),1),
            moist3_min = round(min(moist3_mean, na.rm = T),1),
            moist3_absmax = round(max(moist3_max, na.rm = T),1),
            moist3_absmin = round(min(moist3_min, na.rm = T),1)) %>% 
  left_join(., daycount) %>% 
  ungroup() %>% select(-ndaysmax,-ndays) -> dm_moist3


full_join(dm_T1, dm_T2) %>% 
  full_join(., dm_T3) %>% 
  full_join(., dm_moist1) %>%  
  full_join(., dm_moist2) %>%  
  full_join(., dm_moist3) %>% 
  mutate(across(starts_with("day_f"), ~ifelse(!is.finite(.x), 0, .x))) %>% 
  relocate(starts_with("day_f"), .after = month) %>% 
  relocate(starts_with("moist"), .after = day_frac_moist)-> dm

rm(dm_T1, dm_T2, dm_T3, dm_moist1, dm_moist2, dm_moist3)

dm <- dm %>%
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site)

write_csv(dm %>% filter(!area %in% c("RAR")), "data/tomst_data_monthly_2021.csv")

dm %>% 
  filter(!area %in% c("RAR")) %>% 
  filter(year == 2020, month == 7) %>% 
  select(moist1_mean, moist2_mean, moist3_mean) %>% cor(., method = "spearman", use = "pairwise.complete.obs")

dm %>% filter(year == 2020, month == 7) %>% 
  select(area, moist1_mean, moist2_mean, moist3_mean) %>% 
  group_by(area) %>% summary

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
