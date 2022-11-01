library(tidyverse)
library(lubridate)
library(viridis)
library(ggthemes)
library(cowplot)
library(pals)
library(patchwork)

# Set plotting order for focus area and months
plotting_order <- c("KIL","VAR","TII","PIS","HYY","KAR")

# colors
your_palette <- (c("#FF9F5F", "#F9649B", "#C728D6", "#6A05FA", "#0D00FF", "#000099"))

#################################################################
# Univariate

coef_df_uni <- read_csv("output/uni_coef_df2.csv")
summ_df_uni <- read_csv("output/uni_summ_df2.csv")

coef_df_uni %>% 
  filter(term != "(Intercept)") %>% 
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(pred = factor(pred, levels = c("swi", "twi_luke", "dtw_0_5ha"), labels = c("SWI", "TWI", "DTW"))) %>%
  ggplot(aes(y = estimate, x = week, color = area)) +
  geom_line(size = 2, alpha = 0.5) +
  geom_smooth(span = 0.5, size = 1, se = F) +
  facet_grid(cols = vars(area), rows = vars(pred), scales = "free_y") +
  scale_color_manual(values = (your_palette)) +
  ylab("Slope") +
  xlab("Week") +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_blank()) -> g1

summ_df_uni %>% 
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(pred = factor(pred, levels = c("swi", "twi_luke", "dtw_0_5ha"), labels = c("SWI", "TWI", "DTW"))) %>%
  ggplot(aes(x = week, y = loocv_R2, color = pred, group = pred))+
  geom_line(size = 2, alpha = 0.5) +
  geom_smooth(span = 0.5, size = 1, se = F) +
  facet_grid(rows = vars(area), scales = "free_y") +
  scale_color_manual(values=as.vector(parula(7))) +
  ylab(bquote("LOOCV R"^2)) +
  xlab("Week") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL)) +
  theme_classic() +
  theme(legend.position="bottom",
        strip.background = element_blank()) -> g2

# print pdf
dev.off()
pdf(file="fig/fig_univariate.pdf", width = 7.48, height = 4)
g1
dev.off()

# print pdf
dev.off()
pdf(file="fig/fig_univariate_sum.pdf", width = 3.74, height = 6)
g2
dev.off()

##############################################################################
# Multiple regressions

coef_df <- read_csv("output/coef_df2.csv")
summ_df <- read_csv("output/summ_df2.csv")
vi_df   <- read_csv("output/vi_df.csv")

# GAM
summ_df %>% 
  filter(model == "gam") %>% 
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  ggplot(aes(x = week, y = loocv_R2, color = area))+
  geom_line(size = 2, alpha = 0.5) +
  geom_smooth(span = 0.5, size = 1, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_manual(values = (your_palette)) +
  ylab(bquote("LOOCV R"^2)) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  theme(strip.text.y = element_blank()) +
  theme(legend.position = "none") -> gg1

summ_df %>% 
  filter(model == "gam") %>% 
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  ggplot(aes(x = week, y = loocv_rmse, color = area))+
  geom_line(size = 2, alpha = 0.5) +
  geom_smooth(span = 0.5, size = 1, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_manual(values = (your_palette)) +
  ylab(bquote("RMSE")) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  theme(strip.text.y = element_blank()) +
  theme(legend.position = "none") -> gg2

vi_df %>% 
  filter(model == "gam") %>%
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(Variable = recode_factor(Variable,
                                  pisr_summer_10m = "Solar radiation",
                                  wet_effect = "Distance to water",
                                  tpi20 = "TPI (20 m radius)",
                                  tpi500 = "TPI (500 m radius)",
                                  dtw_0_5ha = "DTW",
                                  twi_luke = "TWI")) %>%
  filter(Variable != "cal_class") %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  ggplot(aes(y = Importance, x = week, fill = Variable)) +
  geom_bar(position="fill", stat="identity", width=1) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_fill_manual(values=as.vector(rev(parula(7)))) +
  ylim(0,1)+
  ylab(paste0("Stacked variable importance")) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none",
        strip.background = element_blank()) -> gg3

vi_df %>% 
  filter(model == "gam") %>%
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(Variable = recode_factor(Variable,
                                  pisr_summer_10m = "Solar radiation",
                                  wet_effect = "Distance to water",
                                  tpi20 = "TPI (20 m radius)",
                                  tpi500 = "TPI (500 m radius)",
                                  dtw_0_5ha = "DTW",
                                  twi_luke = "TWI")) %>%
  filter(Variable != "cal_class") %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  ggplot(aes(y = Importance, x = week, fill = Variable)) +
  geom_bar(position="fill", stat="identity", width=1) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_fill_manual(values=as.vector(rev(parula(7)))) +
  ylim(0,1)+
  ylab(paste0("Stacked variable importance")) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="bottom",
        strip.background = element_blank()) -> gg3_version_with_legend

legend <- cowplot::get_legend(gg3_version_with_legend)
plot (legend)

# print pdf
layout <- '
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
##DDD
'

dev.off()
pdf(file="fig/fig_variable_importance_gam.pdf", width = 7.48, height = 7.48)

wrap_plots(A = gg1,
           B = gg2,
           C = gg3,
           D = legend, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()

# LM
summ_df %>% 
  filter(model == "lm") %>% 
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  ggplot(aes(x = week, y = loocv_R2, color = area))+
  geom_line(size = 2, alpha = 0.5) +
  geom_smooth(span = 0.5, size = 1, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_manual(values = (your_palette)) +
  ylab(bquote("LOOCV R"^2)) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  theme(strip.text.y = element_blank()) +
  theme(legend.position = "none") -> gg4

summ_df %>% 
  filter(model == "lm") %>% 
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  ggplot(aes(x = week, y = loocv_rmse, color = area))+
  geom_line(size = 2, alpha = 0.5) +
  geom_smooth(span = 0.5, size = 1, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_manual(values = (your_palette)) +
  ylab(bquote("RMSE")) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  theme(strip.text.y = element_blank()) +
  theme(legend.position = "none") -> gg5

vi_df %>% 
  filter(model == "lm") %>%
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(Variable = recode_factor(Variable,
                                  pisr_summer_10m = "Solar radiation",
                                  wet_effect = "Distance to water",
                                  tpi20 = "TPI (20 m radius)",
                                  tpi500 = "TPI (500 m radius)",
                                  dtw_0_5ha = "DTW",
                                  twi_luke = "TWI")) %>%
  filter(Variable != "cal_class") %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  ggplot(aes(y = Importance, x = week, fill = Variable)) +
  geom_bar(position="fill", stat="identity", width=1) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_fill_manual(values=as.vector(rev(parula(7)))) +
  ylim(0,1)+
  ylab(paste0("Stacked variable importance")) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="none",
        strip.background = element_blank()) -> gg6

vi_df %>% 
  filter(model == "lm") %>%
  mutate(area = factor(area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(Variable = recode_factor(Variable,
                                  pisr_summer_10m = "Solar radiation",
                                  wet_effect = "Distance to water",
                                  tpi20 = "TPI (20 m radius)",
                                  tpi500 = "TPI (500 m radius)",
                                  dtw_0_5ha = "DTW",
                                  twi_luke = "TWI")) %>%
  filter(Variable != "cal_class") %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  ggplot(aes(y = Importance, x = week, fill = Variable)) +
  geom_bar(position="fill", stat="identity", width=1) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_fill_manual(values=as.vector(rev(parula(7)))) +
  ylim(0,1)+
  ylab(paste0("Stacked variable importance")) +
  xlab("Week") +
  theme_classic() +
  theme(strip.text.x = element_blank()) +
  guides(fill=guide_legend(title=NULL)) +
  theme(legend.position="bottom",
        strip.background = element_blank()) -> gg6_version_with_legend

legend <- cowplot::get_legend(gg3_version_with_legend)
plot (legend)

# print pdf
layout <- '
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
ABCCC
##DDD
'

dev.off()
pdf(file="fig/fig_variable_importance_lm.pdf", width = 7.48, height = 7.48)

wrap_plots(A = gg4,
           B = gg5,
           C = gg6,
           D = legend, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()

#################################################################
# Transferability

cv_df %>% write_csv("output/cv_df_weekly.csv")
cv_df <- read_csv("output/cv_df_weekly.csv")

cv_df %>% 
  filter(model == "lm") %>% 
  group_by(fit_area, pred_area) %>% 
  summarise(loocv_R2 = mean(loocv_R2, na.rm = T),
            loocv_rmse = mean(loocv_rmse, na.rm = T)) %>% 
  mutate(x = 1, y = 1) %>% 
  mutate(loocv_R2 = round(loocv_R2,2)) %>% 
  ungroup() %>% 
  mutate(fit_area = factor(fit_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(pred_area = factor(pred_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  ggplot(aes(x = x, y = y, color = loocv_R2))+
  geom_point(aes(size = loocv_R2)) +
  geom_text(aes(label = loocv_R2), color = "white", size=3) +
  facet_grid(rows = vars(pred_area), cols = vars(fit_area)) +
  scale_size_continuous(range = c(7.5, 14)) +
  #scale_color_viridis(option="mako", direction = 1, begin = 0, end = 0.8) +
  scale_colour_gradient2 (low = "black", mid = "grey", high = "#F9649B", midpoint = 0.25) +
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        strip.text.x = element_text(size = 7),
        strip.text.y = element_text(size = 7),
        strip.background = element_blank()) +
  guides(color = FALSE, size = FALSE) +
  ylab("Study area for predictions based on LM") +
  xlab("Study area for model fitting based on LM") +
  scale_y_continuous(position= "right") + 
  scale_x_continuous(position= "top") -> gg7

cv_df %>% 
  filter(model == "gam") %>% 
  group_by(fit_area, pred_area) %>% 
  summarise(loocv_R2 = mean(loocv_R2, na.rm = T),
            loocv_rmse = mean(loocv_rmse, na.rm = T)) %>% 
  mutate(x = 1, y = 1) %>% 
  mutate(loocv_R2 = round(loocv_R2,2)) %>% 
  ungroup() %>% 
  mutate(fit_area = factor(fit_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(pred_area = factor(pred_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  ggplot(aes(x = x, y = y, color = loocv_R2))+
  geom_point(aes(size = loocv_R2)) +
  geom_text(aes(label = loocv_R2), color = "white", size=3) +
  facet_grid(rows = vars(pred_area), cols = vars(fit_area)) +
  scale_size_continuous(range = c(7.5, 14)) +
  #scale_color_viridis(option="mako", direction = 1, begin = 0, end = 0.8) +
  scale_colour_gradient2 (low = "black", mid = "grey", high = "#F9649B", midpoint = 0.25) +
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        strip.text.x = element_text(size = 7),
        strip.text.y = element_text(size = 7),
        strip.background = element_blank()) +
  guides(color = FALSE, size = FALSE) +
  ylab("Study area for predictions based on GAM") +
  xlab("Study area for model fitting based on GAM") +
  scale_y_continuous(position= "right") + 
  scale_x_continuous(position= "top") -> gg8

# print pdf
layout <- '
AB
'

dev.off()
pdf(file="fig/fig_model_transferability.pdf", width = 7.48, height = 4)

wrap_plots(A = gg7,
           B = gg8, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()

# Transferability RMSE

cv_df %>% 
  filter(model == "lm") %>% 
  group_by(fit_area, pred_area) %>% 
  summarise(loocv_R2 = mean(loocv_R2, na.rm = T),
            loocv_rmse = mean(loocv_rmse, na.rm = T)) %>% 
  mutate(x = 1, y = 1) %>% 
  mutate(loocv_R2 = round(loocv_R2,2),
         loocv_rmse = round(loocv_rmse,1)) %>% 
  ungroup() %>% 
  mutate(fit_area = factor(fit_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(pred_area = factor(pred_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>%
  mutate(rmse_size = loocv_rmse*(-1)) %>% 
  ggplot(aes(x = x, y = y, color = loocv_rmse))+
  geom_point(aes(size = rmse_size)) + 
  geom_text(aes(label = loocv_rmse), color = "white", size=3) +
  facet_grid(rows = vars(pred_area), cols = vars(fit_area)) +
  scale_size_continuous(range = c(7.5, 14)) +
  #scale_color_viridis(option="mako", direction = 1, begin = 0, end = 0.8) +
  scale_colour_gradient2 (high = "black", mid = "grey", low = "#F9649B", midpoint = 17.1) +
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        strip.text.x = element_text(size = 7),
        strip.text.y = element_text(size = 7),
        strip.background = element_blank()) +
  guides(color = FALSE, size = FALSE) +
  ylab("Study area for predictions based on LM") +
  xlab("Study area for model fitting based on LM") +
  scale_y_continuous(position= "right") + 
  scale_x_continuous(position= "top") -> gg9

cv_df %>% 
  filter(model == "gam") %>% 
  group_by(fit_area, pred_area) %>% 
  summarise(loocv_R2 = mean(loocv_R2, na.rm = T),
            loocv_rmse = mean(loocv_rmse, na.rm = T)) %>% 
  mutate(x = 1, y = 1) %>% 
  mutate(loocv_R2 = round(loocv_R2,2),
         loocv_rmse = round(loocv_rmse,1)) %>% 
  ungroup() %>% 
  mutate(fit_area = factor(fit_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  mutate(pred_area = factor(pred_area, levels = plotting_order, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>%
  mutate(rmse_size = loocv_rmse*(-1)) %>% 
  ggplot(aes(x = x, y = y, color = loocv_rmse))+
  geom_point(aes(size = rmse_size)) + 
  geom_text(aes(label = loocv_rmse), color = "white", size=3) +
  facet_grid(rows = vars(pred_area), cols = vars(fit_area)) +
  scale_size_continuous(range = c(7.5, 14)) +
  #scale_color_viridis(option="mako", direction = 1, begin = 0, end = 0.8) +
  scale_colour_gradient2 (high = "black", mid = "grey", low = "#F9649B", midpoint = 23.45) +
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        strip.text.x = element_text(size = 7),
        strip.text.y = element_text(size = 7),
        strip.background = element_blank()) +
  guides(color = FALSE, size = FALSE) +
  ylab("Study area for predictions based on GAM") +
  xlab("Study area for model fitting based on GAM") +
  scale_y_continuous(position= "right") + 
  scale_x_continuous(position= "top") -> gg10

# print pdf
layout <- '
AB
'

dev.off()
pdf(file="fig/fig_model_transferability_RMSE.pdf", width = 7.48, height = 4)

wrap_plots(A = gg9,
           B = gg10, design = layout) +
  plot_annotation(tag_levels = 'A') & 
  theme(plot.tag = element_text(size = 8))

dev.off()



