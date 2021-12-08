library(pals)
library(tidyverse)
library(lubridate)
library(cowplot)
library(mgcv)
library(moments)
library(patchwork)
library(scales)

your_sm  <- read_csv ("data/all_data_daily_2021.csv")

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
                              RAS = "RAS",
                              KIL = "KIL",
                              VAR = "VÄR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR")

your_sm %>% 
  filter(year(date) %in% c(2020)) %>%
  filter(month(date) %in% c(6, 7, 8, 9)) -> your_sm

your_sm %>% filter(moist_prop > 90) -> your_sm

your_sm %>%
  group_by(area, site) %>%
  count() %>%
  group_by(area) %>%
  count()

# calculate values by area
your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  group_by(area) %>% 
  summarise(sm_mean = mean(sm_mean, na.rm = T),
            sm_sd = mean(sm_sd, na.rm = T)) -> your_sm_means

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot areas individually
fig_sd = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  geom_point(size=1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("Standard deviation of VWC%") +
  xlab ("Mean VWC%") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

#fig_cv = your_sm %>% group_by(site, area) %>% 
#  summarise(sm_mean = mean(moist_mean, na.rm = T),
#            sm_sd = sd(moist_mean, na.rm = T),
#            sm_cv = sm_sd/sm_mean,
#            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
#  ggplot(aes(x=sm_mean, y=sm_cv, colour=area, fill=area)) +
#  geom_point(size=1, alpha=3/10) +
#  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
#  scale_fill_manual(values = rev(your_palette(7))) +
#  scale_color_manual(values = rev(your_palette(7))) +
#  ylab ("Coefficient of variation") +
#  xlab ("") +
#  theme_classic() +
#  theme(
#    aspect.ratio = 1,
#    legend.position = "None") +
#  facet_wrap(vars(area), ncol= 1) +
#  theme(
#    strip.background = element_blank(),
#    strip.text.x = element_blank())
#
#fig_skew = your_sm %>% group_by(site, area) %>% 
#  summarise(sm_mean = mean(moist_mean, na.rm = T),
#            sm_sd = sd(moist_mean, na.rm = T),
#            sm_cv = sm_sd/sm_mean,
#            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
#  ggplot(aes(x=sm_mean, y=sm_skew, colour=area, fill=area)) +
#  geom_point(size=1, alpha=3/10) +
#  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
#    scale_fill_manual(values = rev(your_palette(7))) +
#  scale_color_manual(values = rev(your_palette(7))) +
#  ylab ("Skewness") +
#  xlab ("") +
#  theme_classic() +
#  theme(
#    aspect.ratio = 1,
#    legend.position = "None") +
#  facet_wrap(vars(area), ncol= 1) +
#  theme(
#    strip.background = element_blank(),
#    strip.text.x = element_blank())
#
#fig_sd + fig_cv + fig_skew

# plot areas together
fig_sd_all = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = (sd(moist_mean, na.rm = T)),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd)) +
  geom_point(aes(colour=area, fill=area), size = 1, alpha=5/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
  geom_point(data = your_sm_means, aes(colour=area, fill=area), size = 4) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("") +
  xlab ("") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None",
    axis.text.x = element_blank())

#fig_cv_all = your_sm %>% group_by(site, area) %>% 
#  summarise(sm_mean = mean(moist_mean, na.rm = T),
#            sm_sd = sd(moist_mean, na.rm = T),
#            sm_cv = sm_sd/sm_mean,
#            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
#  ggplot(aes(x=sm_mean, y=sm_cv)) +
#  geom_point(aes(colour=area, fill=area), size = 1, alpha=3/10) +
#  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
#  scale_fill_manual(values = rev(your_palette(7))) +
#  scale_color_manual(values = rev(your_palette(7))) +
#  ylab ("") +
#  xlab ("Mean soil moisture") +
#  theme_classic() +
#  theme(
#    aspect.ratio = 1,
#    legend.position = "None")
#
#fig_skew_all = your_sm %>% group_by(site, area) %>% 
#  summarise(sm_mean = mean(moist_mean, na.rm = T),
#            sm_sd = sd(moist_mean, na.rm = T),
#            sm_cv = sm_sd/sm_mean,
#            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
#  ggplot(aes(x=sm_mean, y=sm_skew)) +
#  geom_point(aes(colour=area, fill=area), size = 1, alpha=3/10) +
#  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, colour="black", fill="black", size=0.5) +
#  scale_fill_manual(values = rev(your_palette(7))) +
#  scale_color_manual(values = rev(your_palette(7))) +
#  ylab ("") +
#  xlab ("Mean soil moisture") +
#  theme_classic() +
#  theme(
#    aspect.ratio = 1,
#    legend.position = "None")
#
#fig_sd_all / fig_cv_all / fig_skew_all

# plot areas models together
fig_sd_models = your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ggplot(aes(x=sm_mean, y=sm_sd, colour=area, fill=area)) +
  geom_point(size=1, alpha=0/10) +
  geom_smooth(method = gam, formula = y ~ s(x, k=3), se = T, size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("") +
  xlab ("Mean VWC%") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")

# correlation
your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T),
            sm_cv = sm_sd/sm_mean,
            sm_skew = skewness(moist_mean, na.rm = T)) %>% 
  ungroup() %>% 
  select(-site, -area) %>% cor(., use = "pairwise.complete.obs")

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

layout <- '
ABBB
ABBB
ABBB
ACCC
ACCC
ACCC
A#D#
'

dev.off()
pdf(file="fig/fig_variation.pdf", width = 6.30, height = 8.66)

wrap_plots(A = fig_sd,
           B = fig_sd_all,
           C = fig_sd_models,
           D = p_names, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()
