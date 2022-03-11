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

# Calculate moisture variables
your_sm %>% group_by(site, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            sm_sd = sd(moist_mean, na.rm = T)) %>% 
  ungroup() -> d

d %>% mutate(area2 = substr(site, 1, 3)) -> d

# How many sites per area included after filtering?
table(d$area)

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# create spatial sd data
your_sm %>% group_by(date, area) %>% 
  summarise(sm_sd = sd(moist_mean, na.rm = T),
            n = n()) %>% 
  filter(n > 20) -> spatial_data

your_sm %>% group_by(date, area) %>% 
  summarise(sm_mean = mean(moist_mean, na.rm = T),
            n = n()) %>% 
  filter(n > 20) -> mean_data

# create temporal sd data
your_sm %>% group_by(site, area) %>% 
  summarise(sm_sd = sd(moist_mean, na.rm = T),
            n = n()) %>% 
  filter(n > 20) %>% 
  group_by(area) %>% 
  summarise(sm_median = median(sm_sd, na.rm = T),
            sm_quantile25 = quantile(sm_sd, 0.25, na.rm = T),
            sm_quantile75 = quantile(sm_sd, 0.75, na.rm = T)) -> temporal_data

# plot spatial vs temporal sd in one plot - line
p_sd_all_line = full_join (spatial_data, temporal_data)  %>% 
  ggplot(aes(x=date, group = area)) +
  geom_line(aes(y=sm_median, colour=area), size=0.5, linetype=2) +
  geom_line(aes(y=sm_sd, colour=area), size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("Standard deviation") +
  xlab ("Time") +
  ylim (0, 21) +
  scale_x_date(date_breaks = "2 month", date_labels =  "%d.%m.") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")

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

# print pdf
layout <- '
AA
AA
B#

'

dev.off()
pdf(file="fig/fig_sd.pdf", width = 3.54, height = 5)

wrap_plots(A = p_sd_all_line,
           B = p_names, design = layout)

dev.off()


# plot spatial vs temporal sd in one plot - smooth

p_mean_all_smooth = mean_data  %>% 
  ggplot(aes(x=date, group = area)) +
  geom_smooth(aes (y=sm_mean, colour=area, fill=area), size=0.5, method = "lm", se = T, alpha=3/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("") +
  xlab ("Time") +
  ylim (0, 45) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m.") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")

p_sd_all_smooth = spatial_data  %>% 
  ggplot(aes(x=date, group = area)) +
  geom_smooth(aes (y=sm_sd, colour=area, fill=area), size=0.5, method = "lm", se = T, alpha=3/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("") +
  xlab ("Time") +
  ylim (0, 21) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m.") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")


# EXTRA FOR APPENDIX?

# plot spatial vs temporal sd
p_sd = full_join (spatial_data, temporal_data)  %>% 
  ggplot(aes(x=date, group = area)) +
  geom_ribbon(aes(ymin = sm_quantile25, ymax = sm_quantile75, fill = area), alpha=3/10)+
  geom_line(aes(y=sm_median, colour=area), size=0.5, linetype=2) +
  geom_line(aes(y=sm_sd, colour=area), size=0.5) +
  geom_smooth(aes (y=sm_sd, colour=area, fill=area), size=0.5, method = "lm", se = T, alpha=3/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("Standard deviation") +
  xlab ("Time") +
  scale_x_date(date_breaks = "2 month", date_labels =  "%d.%m.") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None") +
  #facet_grid(facets = area ~ .) +
  facet_wrap(vars(area), ncol= 1) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank())

# plot spatial vs temporal sd in one plot - line
p_sd_all_line = full_join (spatial_data, temporal_data)  %>% 
  ggplot(aes(x=date, group = area)) +
  geom_line(aes(y=sm_median, colour=area), size=0.5, linetype=2) +
  geom_line(aes(y=sm_sd, colour=area), size=0.5) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("") +
  xlab ("") +
  ylim (0, 21) +
  theme_classic() +
  theme(
    axis.text.x = element_blank(),
    aspect.ratio = 1,
    legend.position = "None")

# plot spatial vs temporal sd in one plot - smooth
p_sd_all_smooth = full_join (spatial_data, temporal_data)  %>% 
  ggplot(aes(x=date, group = area)) +
  geom_line(aes(y=sm_median, colour=area), size=0.5, linetype=2) +
  geom_smooth(aes (y=sm_sd, colour=area, fill=area), size=0.5, method = "lm", se = T, alpha=3/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab ("") +
  xlab ("Time") +
  ylim (0, 21) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m.") +
  theme_classic() +
  theme(
    aspect.ratio = 1,
    legend.position = "None")

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
           label = "Pisa",
           colour="#C728D6") +
  annotate("text", x = 1, y =1.3, size = 3.5, fontface =2,
           label = "Tiilikka",
           colour="#6A05FA") +
  annotate("text", x = 1, y =1.2, size = 3.5, fontface =2,
           label = "Hyytiälä",
           colour="#0D00FF") +
  annotate("text", x = 1, y =1.1, size = 3.5, fontface =2,
           label = "Karkali",
           colour="#000099") +
  theme_void()

# print pdf
layout <- '
ABBB
ABBB
ABBB
ACCC
ACCC
ACCC
ADDD
'

dev.off()
pdf(file="fig/fig_sd.pdf", width = 7.48, height = 8.66)

wrap_plots(A = p_sd,
           B = p_sd_all_line,
           C = p_sd_all_smooth,
           D = p_names, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()