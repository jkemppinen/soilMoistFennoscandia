library(pals)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
library(moments)
library(patchwork)
library(scales)

# bring logger soil moisture files
your_sm  <- read.csv ("data/analysis_data.csv")

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot
fig_min = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_min = min(moist_mean, na.rm = T),
            sm_max = max(moist_mean, na.rm = T),
            sm_range = sm_max-sm_min) %>% 
  ggplot(aes(x=sm_mean, y=sm_min)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Minimum") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(strip.text = element_blank())

fig_max = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_min = min(moist_mean, na.rm = T),
            sm_max = max(moist_mean, na.rm = T),
            sm_range = sm_max-sm_min) %>% 
  ggplot(aes(x=sm_mean, y=sm_max)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Maximum") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(strip.text = element_blank())

fig_range = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_min = min(moist_mean, na.rm = T),
            sm_max = max(moist_mean, na.rm = T),
            sm_range = sm_max-sm_min) %>% 
  ggplot(aes(x=sm_mean, y=sm_range)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Range") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(strip.text = element_blank())

fig_min + fig_max + fig_range + plot_layout(guides = "collect")

fig_min_all = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_min = min(moist_mean, na.rm = T),
            sm_max = max(moist_mean, na.rm = T),
            sm_range = sm_max-sm_min) %>% 
  ggplot(aes(x=sm_mean, y=sm_min)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Minimum") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  theme(strip.text = element_blank())

fig_max_all = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_min = min(moist_mean, na.rm = T),
            sm_max = max(moist_mean, na.rm = T),
            sm_range = sm_max-sm_min) %>% 
  ggplot(aes(x=sm_mean, y=sm_max)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Maximum") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  theme(strip.text = element_blank())

fig_range_all = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_min = min(moist_mean, na.rm = T),
            sm_max = max(moist_mean, na.rm = T),
            sm_range = sm_max-sm_min) %>% 
  ggplot(aes(x=sm_mean, y=sm_range)) +
  geom_point(aes(colour=area, size = 3), alpha=8/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=4), se = T, colour="black") +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle ("Range") +
  ylab ("") +
  xlab ("Mean soil moisture") +
  theme_cowplot(12) +
  theme (aspect.ratio = 1, legend.title = element_blank()) +
  theme(strip.text = element_blank())

fig_min_all + fig_max_all + fig_range_all + plot_layout(guides = "collect")

your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_min = min(moist_mean, na.rm = T),
            sm_max = max(moist_mean, na.rm = T),
            sm_range = sm_max-sm_min) %>% 
  ungroup() %>% 
  select(-site, -area) %>% cor(., use = "pairwise.complete.obs")
