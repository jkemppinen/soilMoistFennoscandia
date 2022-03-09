library(pals)
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

your_sm  <- read_csv ("data/all_data_daily_2021.csv")

your_sm %>% filter (area %in% c("AIL","MAL","VAR","TII","PIS","HYY","KAR")) -> your_sm


your_sm$area <- recode_factor(your_sm$area,
                              AIL = "AIL",
                              MAL = "MAL",
                              VAR = "VAR",
                              TII = "TII",
                              PIS = "PIS",
                              HYY = "HYY",
                              KAR = "KAR")

your_sm %>% 
  filter(year(date) %in% c(2020)) -> your_sm

your_sm %>% filter(T1_prop  > 90) %>% 
  filter(T2_prop  > 90) %>% 
  filter(T3_prop  > 90) -> your_sm

your_sm %>%
  group_by(area, site) %>%
  count() %>%
  group_by(area) %>%
  count()

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot area summaries together as boxplots
p_T1 = your_sm %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(x=factor(month), y=T1_mean, fill = area, color = area)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_boxplot (notch=TRUE, notchwidth = 0.05, width=0.5, position=position_dodge(0.8), outlier.size = 0.5, lwd=0.25, alpha=2/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("T1") +
  ylab("Temperature (°C)") +
  xlab("Time") +
  ylim(-20,30) +
  scale_x_discrete(labels=c("1" = "JAN",
                            "2" = "FEB",
                            "3" = "MAR",
                            "4" = "APR",
                            "5" = "MAY",
                            "6" = "JUN",
                            "7" = "JUL",
                            "8" = "AUG",
                            "9" = "SEP",
                            "10" = "OCT",
                            "11" = "NOV",
                            "12" = "DEC")) +
  theme_classic() +
  theme(
    legend.position = "None")

p_T2 = your_sm %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(x=factor(month), y=T2_mean, fill = area, color = area)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_boxplot (notch=TRUE, notchwidth = 0.05, width=0.5, position=position_dodge(0.8), outlier.size = 0.5, lwd=0.25, alpha=2/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("T2") +
  ylab("Temperature (°C)") +
  xlab("") +
  ylim(-20,30) +
  scale_x_discrete(labels=c("1" = "JAN",
                            "2" = "FEB",
                            "3" = "MAR",
                            "4" = "APR",
                            "5" = "MAY",
                            "6" = "JUN",
                            "7" = "JUL",
                            "8" = "AUG",
                            "9" = "SEP",
                            "10" = "OCT",
                            "11" = "NOV",
                            "12" = "DEC")) +
  theme_classic() +
  theme(
    legend.position = "None",
    axis.text.x = element_blank())

p_T3 = your_sm %>% 
  mutate(month = month(date)) %>% 
  ggplot(aes(x=factor(month), y=T3_mean, fill = area, color = area)) +
  geom_hline(yintercept=0, linetype="dashed")+
  geom_boxplot (notch=TRUE, notchwidth = 0.05, width=0.5, position=position_dodge(0.8), outlier.size = 0.5, lwd=0.25, alpha=2/10) +
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  ggtitle("T3") +
  ylab("Temperature (°C)") +
  xlab("") +
  ylim(-20,30) +
  scale_x_discrete(labels=c("1" = "JAN",
                            "2" = "FEB",
                            "3" = "MAR",
                            "4" = "APR",
                            "5" = "MAY",
                            "6" = "JUN",
                            "7" = "JUL",
                            "8" = "AUG",
                            "9" = "SEP",
                            "10" = "OCT",
                            "11" = "NOV",
                            "12" = "DEC")) +
  theme_classic() +
  theme(
    legend.position = "None",
    axis.text.x = element_blank())

# plot area names only
your_palette(7)

p_names = ggplot() +
  annotate("text", x = 1, y =1.7, size = 3.5, fontface =2,
           label = "Ailakkavaara",
           colour="#FFDB24") +
  annotate("text", x = 1, y =1.6, size = 3.5, fontface =2,
           label = "Malla",
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
AAAAA#
BBBBB#
CCCCCD
'

layout <- '
A
B
C
D
'

dev.off()
pdf(file="fig/fig_temperature_tempo.pdf", width = 6.30, height = 8.66)

wrap_plots(A = p_T3,
           B = p_T2,
           C = p_T1,
           D = p_names,design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))
dev.off()

