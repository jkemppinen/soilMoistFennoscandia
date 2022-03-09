library(pals)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
library(moments)
library(patchwork)
library(scales)

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

# How many sites in total?
length(unique(your_sm$site)) # 503 sites selected

# How many measurements in total?
your_sm %>% filter(!is.na(moist_mean)) %>% nrow()*24*4 # The total number of observations

# How many sites per area included after filtering?
length(unique(your_sm$site))

# Calculate mean for each area
your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  group_by(area) %>% 
  summarise(sm_mean = mean(sm_mean, na.rm = T),
            sm_sd = mean(sm_sd, na.rm = T)) -> your_sm_means

#subset
your_sm %>% 
  filter(month(date) %in% c(4)) -> your_sm_04

your_sm %>% 
  filter(month(date) %in% c(5)) -> your_sm_05

your_sm %>% 
  filter(month(date) %in% c(6)) -> your_sm_06

your_sm %>% 
  filter(month(date) %in% c(7)) -> your_sm_07

your_sm %>% 
  filter(month(date) %in% c(8)) -> your_sm_08

your_sm %>% 
  filter(month(date) %in% c(9)) -> your_sm_09

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot
fig_sd = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  geom_point(size=0.5, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("Standard deviation of VWC%") +
  xlab ("Mean VWC%") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")

fig_sd2 = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  geom_point(size=0.5, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, colour="black", fill="black", size=0.5, linetype = "dashed") +
  geom_point(data = your_sm_means, size = 4, pch=18) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("Standard deviation of VWC%") +
  xlab ("Mean VWC%") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")

layout <- '
AAAAA#
AAAAAB
AAAAA#
'

dev.off()
pdf(file="fig/fig_variation.pdf", width = 7.48, height = 5)

wrap_plots(A = fig_sd2,
           B = p_names,
           design = layout) & 
  theme(plot.tag = element_text(size = 8))

dev.off()

# APPENDIX

fig_sd_04 = your_sm %>% filter(month(date) %in% c(4)) %>%
  group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("April") +
  ylab ("Standard deviation of VWC%") +
  xlab ("") +
  # ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

fig_sd_05 = your_sm %>% filter(month(date) %in% c(5)) %>%
  group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("May") +
  ylab ("") +
  xlab ("") +
  # ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

fig_sd_06 = your_sm %>% filter(month(date) %in% c(6)) %>%
  group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("June") +
  ylab ("") +
  xlab ("") +
  # ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

fig_sd_07 = your_sm %>% filter(month(date) %in% c(7)) %>%
  group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("July") + 
  ylab ("") +
  xlab ("Mean VWC%") +
  # ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

fig_sd_08 = your_sm %>% filter(month(date) %in% c(8)) %>%
  group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("August") +
  ylab ("") +
  xlab ("") +
  # ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

fig_sd_09 = your_sm %>% filter(month(date) %in% c(9)) %>%
  group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("September") + 
  ylab ("") +
  xlab ("") +
  # ylim (0,8) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

fig_sd = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = F, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("All months") +
  ylab ("") +
  xlab ("") +
  # ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

# plot area names only
your_palette(7)

p_names = ggplot() +
  annotate("text", x = 1, y =1.7, size = 3.5, fontface =2,
           label = "Rásttigáisá",
           colour="#FFDB24") +
  annotate("text", x = 1, y =1.6, size = 3.5, fontface =2,
           label = "Kilpisjärvi",
           colour="#FF9F5F") +
  annotate("text", x = 1, y =1.5, size = 3.5, fontface =2,
           label = "Värriö",
           colour="#F9649B") +
  annotate("text", x = 1, y =1.4, size = 3.5, fontface =2,
           label = "Tiilikka",
           colour="#C728D6") +
  annotate("text", x = 1, y =1.3, size = 3.5, fontface =2,
           label = "Pisa",
           colour="#6A05FA") +
  annotate("text", x = 1, y =1.2, size = 3.5, fontface =2,
           label = "Hyytiälä",
           colour="#0D00FF") +
  annotate("text", x = 1, y =1.1, size = 3.5, fontface =2,
           label = "Karkali",
           colour="#000099") +
  theme_void()

layout <- '
ABCDEGH
ABCDEGH
ABCDEGH
ABCDEGH
##III##
'

dev.off()
pdf(file="fig/fig_variation2.pdf", width = 7.48, height = 7.48)

wrap_plots(A = fig_sd_04,
           B = fig_sd_05,
           C = fig_sd_06,
           D = fig_sd_07,
           E = fig_sd_08,
           G = fig_sd_09,
           H = fig_sd,
           I = p_names, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()
