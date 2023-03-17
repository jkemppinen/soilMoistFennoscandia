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

d <- d %>% 
  group_by(area, date) %>% 
  summarise(moist_mean = mean(moist2_median),
            moist_sd = sd(moist2_median),
            moist_cv = moist_sd/moist_mean)

left_join(count_sites,
          count_sites %>% group_by(area) %>% summarise(maxn = max(n))) %>% 
  mutate(n_prop = n/maxn) %>% 
  filter(n_prop >= 0.666) -> count_sites

left_join(d, count_sites) %>% 
  filter(!is.na(moist_mean)) %>% 
  filter(!is.na(n_prop)) %>% 
  select(area:moist_cv) %>% 
  ungroup() -> d

d <- bind_rows(d,
               d %>% mutate(area = "ALL")) %>% 
  mutate(area = recode_factor(area,
                              ALL = "ALL",
                              RAS = "RAS",
                              KIL = "KIL",
                              VAR = "VAR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR"))

# LM models using all data and by area
d %>% 
  nest(data = -area) %>% 
  mutate(fit_linear = map(data, ~ lm(moist_sd ~ poly(moist_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(moist_sd ~ poly(moist_mean, 2), data = .))) -> fits

names(fits$fit_linear) <- names(fits$fit_poly) <- fits$area

fits <- fits %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

aovs <- map(fits$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "All") %>% 
  select(area, statistic, p.value, month)

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

ress <- fits0 %>% 
  relocate(R2_poly, .after = R2_linear) %>% 
  relocate(statistic:p.value, .after = AIC_poly) %>% 
  rename(anova_p_value = p.value,
         F_statistic = statistic) %>% 
  mutate(signf = ifelse(anova_p_value < 0.1, ".", ""),
         signf = ifelse(anova_p_value < 0.05, "*", signf),
         signf = ifelse(anova_p_value < 0.01, "**", signf),
         signf = ifelse(anova_p_value < 0.001, "***", signf)) %>% 
  select(-starts_with("AIC")) %>% 
  # mutate(area = gsub("Ä","A",area)) %>% 
  mutate(area = factor(area, levels = c("ALL", "RAS", "KIL", "VAR", "TII", "PIS", "HYY", "KAR"))) %>% 
  arrange(area)

write.csv(ress, "output/sd_sm_model_results.csv",fileEncoding = "latin1")

ress %>% filter(area != "ALL") %>% 
  group_by(month) %>% 
  summarise(R2_linear = mean(R2_linear),
            R2_poly = mean(R2_poly))



# LM models using all data and by area COEFFICIENT OF VARIATION
d %>% 
  nest(data = -area) %>% 
  mutate(fit_linear = map(data, ~ lm(moist_cv ~ poly(moist_mean, 1), data = .)),
         fit_poly = map(data, ~ lm(moist_cv ~ poly(moist_mean, 2), data = .))) -> cvfits

names(cvfits$fit_linear) <- names(cvfits$fit_poly) <- cvfits$area

cvfits <- cvfits %>% 
  mutate(aov = map2(fit_linear, fit_poly, stats::anova))

cvaovs <- map(cvfits$aov, tidy) %>% map(function(x){ x %>% slice(2)}) %>% 
  bind_rows(.id = "area") %>% 
  mutate(month = "All") %>% 
  select(area, statistic, p.value, month)

map(cvfits$fit_linear, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_linear = r.squared,
         AIC_linear = AIC) -> cvlfits

map(cvfits$fit_poly, glance) %>% bind_rows(.id = "area") %>% 
  select(area, nobs, r.squared, AIC) %>% 
  rename(R2_poly = r.squared,
         AIC_poly = AIC) -> cvpfits

cvfits0 <- full_join(cvaovs, cvlfits) %>% 
  full_join(., cvpfits)

cvress <- cvfits0 %>% 
  relocate(R2_poly, .after = R2_linear) %>% 
  relocate(statistic:p.value, .after = AIC_poly) %>% 
  rename(anova_p_value = p.value,
         F_statistic = statistic) %>% 
  mutate(signf = ifelse(anova_p_value < 0.1, ".", ""),
         signf = ifelse(anova_p_value < 0.05, "*", signf),
         signf = ifelse(anova_p_value < 0.01, "**", signf),
         signf = ifelse(anova_p_value < 0.001, "***", signf)) %>% 
  select(-starts_with("AIC")) %>% 
  # mutate(area = gsub("Ä","A",area)) %>% 
  mutate(area = factor(area, levels = c("ALL", "RAS", "KIL", "VAR", "TII", "PIS", "HYY", "KAR"))) %>% 
  arrange(area)

write.csv(cvress, "output/cv_sm_model_results.csv",fileEncoding = "latin1")

cvress %>% filter(area != "ALL") %>% 
  group_by(month) %>% 
  summarise(R2_linear = mean(R2_linear),
            R2_poly = mean(R2_poly))

# PLOTS
d %>% 
  ggplot(aes(x = moist_mean, y = moist_sd)) +
  geom_point(size = 0.5, color = "gray90") +
  geom_smooth(method = "lm", formula = 'y ~ 0 + x + I(x^2)') +
  geom_smooth(method = "lm", formula = 'y ~ x + I(x^2)', color = "black") +
  facet_wrap(vars(area)) +
  theme_bw()

d %>% 
  ggplot(aes(x = moist_mean, y = moist_cv)) +
  geom_point(size = 0.5, color = "gray90") +
  geom_smooth(method = "lm", formula = 'y ~ x + I(x^2)', color = "black") +
  facet_wrap(vars(area)) +
  theme_bw()
