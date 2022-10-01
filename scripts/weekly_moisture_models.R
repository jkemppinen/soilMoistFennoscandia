
library(tidyverse)
library(lubridate)
library(broom)
library(vip)
library(pdp)

# The study areas modelled
areas_to_model <- c("KIL","VAR","TII","PIS","HYY","KAR")

d <- read_csv("data/all_data_daily_2021.csv") %>% 
  select(site:date, T1_mean, starts_with("moist"), error_tomst) %>% 
  # filter(date >= "2020-01-01", date <= "2020-12-31") %>% 
  filter(date >= "2020-03-01", date <= "2020-10-31") %>% 
  mutate(area = ifelse(area %in% c("AIL","MAL","SAA"), "KIL",area)) %>% 
  filter(!grepl("PIS1", site)) %>%  # Exclude PISA sites in active forestry areas
  filter(area %in% areas_to_model) %>% 
  filter(moist_prop > 95) %>% 
  mutate(week = week(date))

d2 <- d %>% 
  group_by(area, site) %>% 
  summarise(moist_mean = mean(moist2_median),
            moist_sd = sd(moist2_median),
            moist_cv = sd(moist2_median)/mean(moist2_median)*100)

d <- d %>% 
  group_by(area, site, week) %>% 
  summarise(moist_mean = mean(moist2_median))


d %>% filter(!is.na(moist_mean)) %>% 
  group_by(area, week) %>% 
  count() -> count_sites 

left_join(count_sites,
          count_sites %>% group_by(area) %>% summarise(maxn = max(n))) %>% 
  mutate(n_prop = n/maxn) %>% 
  filter(n_prop >= 0.666) -> count_sites

left_join(d, count_sites) %>% 
  filter(!is.na(moist_mean)) %>% 
  filter(!is.na(n_prop)) %>% 
  select(area:moist_mean) %>% 
  ungroup() -> d

#########################################################################
# Env variables

e <- read_csv("data/all_env_variables.csv") %>% 
  filter(logger == "Tomst" | is.na(logger)) %>% 
  mutate(area = ifelse(area %in% c("AIL","MAL","SAA"), "KIL",area)) %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100)) %>% 
  select(-site) %>% 
  rename(site = id_code)
names(e)
unique(e$area)
# Select and preprocess other env data

# Predictors in the models
predictors <- c("pisr_summer_10m", "swi", "costdist2water", "tpi20", "tpi500")


e %>% 
  mutate(altitude = altitude/100) %>% 
  mutate(across(starts_with("pgaps"), ~abs(.x-100))) %>% 
  select(site, all_of(predictors)) %>% 
  mutate(across(all_of(predictors), ~as.numeric(scale(.x)))) -> e

#####################################################################################
# Modelling

coef_df <- tibble()
summ_df <- tibble()
vi_df <- tibble()
for(i in areas_to_model){
  print(i)
  
  for(ii in d %>% filter(area == i) %>% pull(week) %>% unique){
    print(ii)
    
    d %>% filter(area == i,
                 week == ii) -> temp
    
    responses <- names(temp)[grepl("^moist", names(temp))]
    
    for(iii in responses){
      
      # LM
      env_vars <- predictors
      
      model_formula_lm <- formula(paste0(iii, " ~ ", paste(c(env_vars), collapse = " + ")))
      
      temp %>% select(area, site, week, all_of(iii)) %>%
        left_join(e) %>% 
        drop_na() -> mod_data
      
      mod <- lm(model_formula_lm, data = mod_data)
      
      coef_df <- bind_rows(coef_df,
                           tidy(mod) %>% 
                             mutate(area = i,
                                    week = ii,
                                    resp = iii,
                                    model = "lm"))
      
      preds <- c()
      for(ir in seq_len(nrow(mod_data))){
        
        mod <- lm(model_formula_lm, data = mod_data[-ir,])
        preds <- c(preds, predict(mod, mod_data[ir,]))
        
      }
      
      summ_df <- bind_rows(summ_df,
                           glance(mod) %>% 
                             mutate(loocv_R2 = cor(preds, mod_data[,iii])^2,
                                    area = i,
                                    week = ii,
                                    resp = iii,
                                    model = "lm"))
      
      vi_df <- bind_rows(vi_df,
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars),
                                    metric = "rsquared", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_r2",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "lm"),
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars),
                                    metric = "rmse", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_rmse",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "lm"),
                         vi_firm(mod, feature_names = env_vars) %>% 
                           mutate(method = "pdp",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "lm"))
      
      
      # GAM
      
      model_formula_gam <- formula(paste0(iii, " ~ ", paste("s(", c(env_vars), ", k = 3)", collapse = " + ")))
      
      mod <- mgcv::gam(model_formula_gam, method = "REML", data = mod_data)
      
      coef_df <- bind_rows(coef_df,
                           tidy(mod) %>% 
                             mutate(area = i,
                                    week = ii,
                                    resp = iii,
                                    model = "gam"))
      
      preds <- c()
      for(ir in seq_len(nrow(mod_data))){
        
        mod <- mgcv::gam(model_formula_gam, method = "REML", data = mod_data[-ir,])
        preds <- c(preds, predict(mod, mod_data[ir,]))
        
      }
      
      summ_df <- bind_rows(summ_df,
                           glance(mod) %>% 
                             mutate(r.squared = summary(mod)$dev.expl,
                                    adj.r.squared = summary(mod)$r.sq,
                                    loocv_R2 = cor(preds, mod_data[,iii])^2,
                                    area = i,
                                    week = ii,
                                    resp = iii,
                                    model = "gam"))
      
      vi_df <- bind_rows(vi_df,
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars),
                                    metric = "rsquared", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_r2",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "gam"),
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars),
                                    metric = "rmse", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_rmse",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "gam"),
                         vi_firm(mod, feature_names = env_vars) %>% 
                           mutate(method = "pdp",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "gam"))
      
    }
    
  }
  
}

coef_df %>% write_csv("output/model_outputs/coef_df2.csv")
summ_df %>% write_csv("output/model_outputs/summ_df2.csv")
vi_df %>% write_csv("output/model_outputs/vi_df.csv")

# Write out modelling data for later use in plottings

d %>% left_join(e) -> mod_data

mod_data %>% write_csv("output/model_outputs/model_input_data.csv")

# correlations by area and month

mod_data %>%
  select(area, predictors) %>% 
  distinct() -> df

by(df, INDICES = df$area, FUN = function(x) cor(x[, -1], method = "spearman"))

library(viridis)
library(ggthemes)
library(cowplot)

# Set plotting order for focus area and months
plotting_order <- c("KIL","VAR","TII","PIS","HYY","KAR")

# Color palette
var_colors <- c("darkorange","gold","gray90","olivedrab3","lightgoldenrod3","#0080FF","#0019FF","darkorchid4")

summ_df %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  ggplot(aes(x = week, y = loocv_R2, color = area))+
  geom_line(linetype = 2) +
  geom_smooth(span = 0.5, size = 2, se = F) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_color_economist()+
  theme_clean() + theme(legend.position = "bottom") +
  ylab(bquote(LOOCV_R^2)) + xlab("Week number") +
  guides(color=guide_legend(title=NULL), linetype=guide_legend(title=NULL)) -> gg1

vi_df %>% 
  # mutate(Variable = recode_factor(Variable, altitude = "Elevation",
  #                                 pisr = "Solar radiation",
  #                                 snow_days = "Snow cover",
  #                                 canopy_cover2m_10m = "Canopy cover",
  #                                 tpi100 = "TPI")) %>% 
  mutate(area = factor(area, levels = plotting_order)) %>% 
  mutate(Importance = ifelse(Importance < 0, 0, Importance)) %>% 
  filter(method == "perm_r2") %>% 
  ggplot(aes(y = Importance, x = week, fill = Variable)) +
  geom_bar(position="stack", stat="identity", width=1) +
  facet_grid(rows = vars(area), cols = vars(model)) +
  scale_fill_colorblind()+
  # scale_fill_manual(values = var_colors)+
  theme_clean() + theme(legend.position = "bottom") +
  ylim(0,1.1)+
  ylab(paste0("Stacked variable importance")) + xlab("Week number") +
  theme(legend.text=element_text(size=7.5))+
  guides(fill=guide_legend(title=NULL)) -> gg2

plot_grid(gg1, gg2, nrow = 1, labels = "auto")
