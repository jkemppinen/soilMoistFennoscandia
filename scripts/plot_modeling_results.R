library(tidyverse)
library(lubridate)
library(viridis)
library(ggthemes, lib.loc = "/projappl/project_2003061/Rpackages/")
library(cowplot)

# Set plotting order for focus area and months
plotting_order <- c("KIL","VAR","TII","PIS","HYY","KAR")


#################################################################
# Univariate

coef_df_uni <- read_csv("output/uni_coef_df2.csv")
summ_df_uni <- read_csv("output/uni_summ_df2.csv")

coef_df_uni %>% 
  filter(term != "(Intercept)") %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(y = estimate, x = week, color = area)) +
  geom_line(linetype = 2) +
  geom_smooth(span = 0.5, size = 1.5, se = F) +
  facet_grid(cols = vars(area), rows = vars(pred), scales = "free_y") +
  scale_color_economist()+
  theme_clean() + theme(legend.position = "bottom") +
  ylab("Slope") + xlab("Week number") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL))

summ_df_uni %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(x = week, y = loocv_R2, color = pred, group = pred))+
  geom_line(linetype = 2) +
  geom_smooth(span = 0.5, size = 1.5, se = F) +
  facet_grid(rows = vars(area), scales = "free_y") +
  scale_color_economist()+
  theme_clean() + theme(legend.position = "bottom") +
  ylab(bquote(LOOCV_R^2)) + xlab("Week number") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL))


##############################################################################
# Multiple regressions

coef_df <- read_csv("output/coef_df2.csv")
summ_df <- read_csv("output/summ_df2.csv")
vi_df   <- read_csv("output/vi_df.csv")

# GAM
summ_df %>% 
  filter(model == "gam") %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(x = week, y = loocv_R2, color = area))+
  geom_line(linetype = 2) +
  geom_smooth(span = 0.5, size = 1.5, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_economist()+
  theme_clean() + theme(legend.position = "bottom") +
  ylab(bquote(LOOCV_R^2)) + xlab("Week number") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL)) -> gg1

summ_df %>% 
  filter(model == "gam") %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(x = week, y = loocv_rmse, color = area))+
  geom_line(linetype = 2) +
  geom_smooth(span = 0.5, size = 1.5, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_economist()+
  theme_clean() + theme(legend.position = "bottom") +
  ylab(bquote(LOOCV_RMSE)) + xlab("Week number") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL)) -> gg2

vi_df %>% 
  filter(model == "gam") %>% 
  # mutate(Variable = recode_factor(Variable, altitude = "Elevation",
  #                                 pisr = "Solar radiation",
  #                                 snow_days = "Snow cover",
  #                                 canopy_cover2m_10m = "Canopy cover",
  #                                 tpi100 = "TPI")) %>% 
  filter(Variable != "cal_class") %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  ggplot(aes(y = Importance, x = week, fill = Variable)) +
  geom_bar(position="fill", stat="identity", width=1) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_fill_colorblind()+
  # scale_fill_manual(values = var_colors)+
  theme_clean() + theme(legend.position = "bottom") +
  ylim(0,1.1)+
  ylab(paste0("Stacked variable importance")) + xlab("Week number") +
  theme(legend.text=element_text(size=7.5))+
  guides(fill=guide_legend(title=NULL)) -> gg3

plot_grid(gg1, gg2, gg3, nrow = 1, labels = "auto")

# LM
summ_df %>% 
  filter(model == "lm") %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(x = week, y = loocv_R2, color = area))+
  geom_line(linetype = 2) +
  geom_smooth(span = 0.5, size = 1.5, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_economist()+
  theme_clean() + theme(legend.position = "bottom") +
  ylab(bquote(LOOCV_R^2)) + xlab("Week number") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL)) -> gg4

summ_df %>% 
  filter(model == "lm") %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(x = week, y = loocv_rmse, color = area))+
  geom_line(linetype = 2) +
  geom_smooth(span = 0.5, size = 1.5, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_economist()+
  theme_clean() + theme(legend.position = "bottom") +
  ylab(bquote(LOOCV_RMSE)) + xlab("Week number") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL)) -> gg5

vi_df %>% 
  filter(model == "lm") %>% 
  # mutate(Variable = recode_factor(Variable, altitude = "Elevation",
  #                                 pisr = "Solar radiation",
  #                                 snow_days = "Snow cover",
  #                                 canopy_cover2m_10m = "Canopy cover",
  #                                 tpi100 = "TPI")) %>% 
  filter(Variable != "cal_class") %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  ggplot(aes(y = Importance, x = week, fill = Variable)) +
  geom_bar(position="fill", stat="identity", width=1) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_fill_colorblind()+
  # scale_fill_manual(values = var_colors)+
  theme_clean() + theme(legend.position = "bottom") +
  ylim(0,1.1)+
  ylab(paste0("Stacked variable importance")) + xlab("Week number") +
  theme(legend.text=element_text(size=7.5))+
  guides(fill=guide_legend(title=NULL)) -> gg6

plot_grid(gg4, gg5, gg6, nrow = 1, labels = "auto")

#################################################################
# Transferability

cv_df %>% write_csv("output/cv_df_weekly.csv")
cv_df <- read_csv("output/cv_df_weekly.csv")

cv_df %>% 
  filter(model == "gam") %>% 
  group_by(fit_area, pred_area) %>% 
  summarise(loocv_R2 = mean(loocv_R2, na.rm = T),
            loocv_rmse = mean(loocv_rmse, na.rm = T)) %>% 
  mutate(x = 1, y = 1) %>% 
  mutate(loocv_R2 = round(loocv_R2,2)) %>% 
  ungroup() %>% 
  mutate(fit_area = factor(fit_area, levels = plotting_order),
         pred_area = factor(pred_area, levels = plotting_order)) %>% 
  ggplot(aes(x = x, y = y, color = loocv_R2))+
  geom_point(aes(size = loocv_R2)) +
  geom_text(aes(label = loocv_R2), color = "black") +
  # geom_point(aes(color = loocv_R2)) +
  facet_grid(rows = vars(pred_area), cols = vars(fit_area)) +
  scale_size_continuous(range = c(10, 30)) +
  scale_color_viridis(option = "D")+
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  ylab(NULL) + xlab(NULL) +
  guides(color=guide_legend(title=NULL), size=guide_legend(title=NULL))+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Study area for predictions", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Study area for model fitting", breaks = NULL, labels = NULL))

cv_df %>% 
  filter(model == "lm") %>% 
  group_by(fit_area, pred_area) %>% 
  summarise(loocv_R2 = mean(loocv_R2, na.rm = T),
            loocv_rmse = mean(loocv_rmse, na.rm = T)) %>% 
  mutate(x = 1, y = 1) %>% 
  mutate(loocv_R2 = round(loocv_R2,2)) %>% 
  ungroup() %>% 
  mutate(fit_area = factor(fit_area, levels = plotting_order),
         pred_area = factor(pred_area, levels = plotting_order)) %>% 
  ggplot(aes(x = x, y = y, color = loocv_R2))+
  geom_point(aes(size = loocv_R2)) +
  geom_text(aes(label = loocv_R2), color = "black") +
  # geom_point(aes(color = loocv_R2)) +
  facet_grid(rows = vars(pred_area), cols = vars(fit_area)) +
  scale_size_continuous(range = c(10, 30)) +
  scale_color_viridis(option = "D")+
  theme_classic() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank()) +
  ylab(NULL) + xlab(NULL) +
  guides(color=guide_legend(title=NULL), size=guide_legend(title=NULL))+ 
  scale_y_continuous(sec.axis = sec_axis(~ . , name = "Study area for predictions", breaks = NULL, labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ . , name = "Study area for model fitting", breaks = NULL, labels = NULL))

