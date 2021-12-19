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
your_sm %>% filter(!is.na(moist_mean)) %>% nrow()*24*4 # The total number of obsevations

# How many sites per area included after filtering?
table(d$area)

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

# plot areas individually
fig_sd_04 = your_sm %>% filter(month(date) %in% c(4)) %>%
  group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("April") +
  ylab ("Standard deviation of VWC%") +
  xlab ("") +
  ylim (0,8) + 
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
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("May") +
  ylab ("") +
  xlab ("") +
  ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.y = element_blank()) +
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
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("June") +
  ylab ("") +
  xlab ("") +
  ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.y = element_blank()) +
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
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("July") + 
  ylab ("") +
  xlab ("Mean VWC%") +
  ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.y = element_blank()) +
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
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("August") +
  ylab ("") +
  xlab ("") +
  ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.y = element_blank()) +
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
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("September") + 
  ylab ("") +
  xlab ("") +
  ylim (0,8) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.y = element_blank()) +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

fig_sd = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  #  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("All months") +
  ylab ("") +
  xlab ("") +
  ylim (0,8) + 
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.y = element_blank()) +
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

# plot areas together
fig_sd_all_06 = your_sm_06 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = (sd(moist_mean, na.rm = T))) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, fill=area), size = 1, alpha=3/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
  geom_point(data = your_sm_means_06, aes(colour=area, fill=area), size = 4) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("June") +
  ylab ("Standard deviation of VWC%") +
  xlab ("") +
  ylim (0,20) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.x = element_blank())

fig_sd_all_07 = your_sm_07 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = (sd(moist_mean, na.rm = T))) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, fill=area), size = 1, alpha=3/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
  geom_point(data = your_sm_means_07, aes(colour=area, fill=area), size = 4) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("July") +
  ylab ("") +
  xlab ("") +
  ylim (0,20) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.x = element_blank(),
    axis.text.y = element_blank())

fig_sd_all_08 = your_sm_08 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = (sd(moist_mean, na.rm = T))) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, fill=area), size = 1, alpha=3/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
  geom_point(data = your_sm_means_08, aes(colour=area, fill=area), size = 4) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("August") + 
  ylab ("Standard deviation of VWC%") +
  xlab ("") +
  ylim (0,20) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.x = element_blank())

fig_sd_all_09 = your_sm_09 %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = (sd(moist_mean, na.rm = T))) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, fill=area), size = 1, alpha=3/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
  geom_point(data = your_sm_means_09, aes(colour=area, fill=area), size = 4) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("September") +
  ylab ("") +
  xlab ("Mean VWC%") +
  ylim (0,20) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.y = element_blank())

fig_sd_all = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = (sd(moist_mean, na.rm = T))) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, fill=area), size = 1, alpha=3/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
  geom_point(data = your_sm_means, aes(colour=area, fill=area), size = 4) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("All months") +
  ylab ("Standard deviation of VWC%") +
  xlab ("Mean VWC%") +
  ylim (0,20) +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")

layout <- '
AB
AB
CD
CD
EG
E#
'

dev.off()
pdf(file="fig/fig_variation.pdf", width = 7.48, height = 8.66)

wrap_plots(A = fig_sd_all_06,
           B = fig_sd_all_07,
           C = fig_sd_all_08,
           D = fig_sd_all_09,
           E = fig_sd_all,
           G = p_names, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()

# correlation
your_sm_all %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ungroup() %>% 
  select(-site, -area) %>% cor(., use = "pairwise.complete.obs")

#p_names = ggplot() +
#  annotate("text", x = 0.2, y =1, size = 3.5, fontface =2,
#           label = "Rásttigáisá",
#           colour="#FFDB24") +
#  annotate("text", x = 0.3, y =1, size = 3.5, fontface =2,
#           label = "Kilpisjärvi",
#           colour="#FF9F5F") +
#  annotate("text", x = 0.4, y =1, size = 3.5, fontface =2,
#           label = "Värriö",
#           colour="#F9649B") +
#  annotate("text", x = 0.5, y =1, size = 3.5, fontface =2,
#           label = "Tiilikka",
#           colour="#C728D6") +
#  annotate("text", x = 0.6, y =1, size = 3.5, fontface =2,
#           label = "Pisa",
#           colour="#6A05FA") +
#  annotate("text", x = 0.7, y =1, size = 3.5, fontface =2,
#           label = "Hyytiälä",
#           colour="#0D00FF") +
#  annotate("text", x =0.8, y =1, size = 3.5, fontface =2,
#           label = "Karkali",
#           colour="#000099") +
#  theme_void() +
#  theme(
#    aspect.ratio = 0.1
#  )