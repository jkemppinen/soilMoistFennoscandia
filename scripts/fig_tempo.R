library(pals)
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

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

# These are used to filter monthly datasets
your_sm %>% 
  mutate(month = month(date)) %>% 
  group_by(area, site, month) %>% count() %>% filter(n > 10) -> site_aggr

site_aggr %>% 
  group_by(area, month) %>% count() %>% filter(n > 20) -> area_aggr

#subset
your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(4)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_04

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(5)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_05

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(6)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_06

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(7)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_07

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(8)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_08

your_sm %>% 
  mutate(month = month(date)) %>% 
  filter(month %in% c(9)) %>% 
  left_join(., area_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) %>% 
  left_join(., site_aggr) %>% 
  filter(!is.na(n)) %>% select(-n) -> your_sm_09

your_sm = bind_rows(your_sm_04,
          your_sm_05,
          your_sm_06,
          your_sm_07,
          your_sm_08,
          your_sm_09)

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot timeseries as lines
p_line = your_sm %>% group_by(area, date) %>% 
  mutate(moist_mean_all = mean(moist_mean, na.rm = T)) %>% 
  ungroup(area, date) %>% 
  ggplot(aes(x=date, colour = area)) +
  geom_line(aes(y=moist_mean, group = site), alpha=2/10, size=0.25) +
  geom_line(aes(y=moist_mean_all), size =0.75) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab("VWC%") +
  xlab("") +
  ylim(0,60) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m.") +
  theme_classic() +
  theme(
    legend.position = "None",
    axis.text.x = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank())

# plot area timeseries as lines

p_line_all = your_sm %>% group_by(area, date) %>% 
  mutate(moist_mean_all = mean(moist_mean, na.rm = T)) %>% 
  ungroup(area, date) %>% 
  ggplot(aes(x=date, colour = area)) +
  geom_line(aes(y=moist_mean_all), size =0.50) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab("VWC%") +
  xlab("") +
  ylim(0,60) +
  scale_x_date(date_breaks = "1 month", date_labels =  "%d.%m.") +
  theme_classic() +
  theme(
    legend.position = "None")
  
# plot area summaries together as boxplots
p_boxplot = your_sm %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(x=factor(month), y=moist_mean, fill = area, color = area)) +
  geom_boxplot (notch=TRUE, notchwidth = 0.05, width=0.5, position=position_dodge(0.8), outlier.size = 0.5, lwd=0.5, alpha=7/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab("VWC%") +
  xlab("Time") +
  ylim(0,60) +
  scale_x_discrete(labels=c("4" = "April",
                            "5" = "May",
                            "6" = "June",
                            "7" = "July",
                            "8" = "August",
                            "9" = "September")) +
  theme_classic() +
  theme(
    legend.position = "None")

# plot area summaries separate as density plots
p_density = your_sm %>%
  ggplot(aes(y=moist_mean, colour = area, fill=area)) +
  geom_histogram(aes(x=..density..), colour="white", bins=20, alpha=3/10, size=0.1) +
  geom_density(size=0.5, alpha=0/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab("") +
  xlab("") +
  ylim(0,60) +
  xlim(0.00,0.06) +
  theme_classic() +
  theme(
    legend.position = "None",
    axis.text.y = element_blank(),
    axis.text.x = element_blank()) +
  facet_grid(facets = area ~ .) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank())

# plot area summaries together as a density plot
p_density_all = your_sm %>%
  ggplot(aes(y=moist_mean, colour = area)) +
  geom_density(size=0.50) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ylab("") +
  xlab("Density") +
  ylim(0,60) +
  xlim(0.00,0.06) +
  theme_classic() +
  theme(
    axis.title.y=element_blank(),
    legend.position = "None",
    axis.text.y = element_blank())

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

#(p_line + p_density) / (p_line_all + plot_spacer()) / (p_boxplot + plot_spacer()) +
#  plot_layout(heights = c(2,0.5,0.5), widths = c(5, 0.5), guides = "collect") +
#  plot_annotation(tag_levels = 'A') & 
#  theme(plot.tag = element_text(size = 8))

# print pdf
layout <- '
AAAB
AAAB
AAAB
AAAB
CCCD
EEEG
'

dev.off()
pdf(file="fig/fig_tempo.pdf", width = 7.48, height = 9.45)

wrap_plots(A = p_line,
           B = p_density,
           C = p_line_all,
           D = p_density_all,
           E = p_boxplot,
           G = p_names, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()
