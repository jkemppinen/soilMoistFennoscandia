library(pals)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
library(moments)
library(patchwork)

#library(scales)

# bring logger soil moisture files
your_sm  <- read.csv ("data/all_data_daily.csv")

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
                              RAS = "a",
                              KIL = "b",
                              VAR = "c",
                              TII = "d",
                              PIS = "e",
                              HYY = "f",
                              KAR = "g")

your_sm %>% mutate (date = as_date(date))-> your_sm

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot
fig_sd = your_sm %>% group_by(id_code, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Standard deviation") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(strip.text = element_blank())

fig_cv = your_sm %>% group_by(id_code, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_cv)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Coefficient of variation") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(strip.text = element_blank())

fig_skew = your_sm %>% group_by(id_code, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_skew)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Skewness") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(strip.text = element_blank())

fig_sd + fig_cv + fig_skew + plot_layout(guides = "collect")

fig_sd_all = your_sm %>% group_by(id_code, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Standard deviation") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

fig_cv_all = your_sm %>% group_by(id_code, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_cv)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Coefficient of variation") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

fig_skew_all = your_sm %>% group_by(id_code, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_skew)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Skewness") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank())

fig_sd_all + fig_cv_all + fig_skew_all + plot_layout(guides = "collect")

