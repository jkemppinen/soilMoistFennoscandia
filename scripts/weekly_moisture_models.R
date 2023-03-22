
library(tidyverse)
library(lubridate)
library(broom)
# library(vip)
library(vip, lib.loc = "/projappl/project_2003061/Rpackages/")
library(mgcv)
library(lme4)
# library(pdp)

add_zeros <- function(x){
  if(nchar(x) == 1){
    return(as.character(paste0("00",x)))
  }
  if(nchar(x) == 2){
    return(as.character(paste0("0",x)))
  }
  if(nchar(x) > 2){
    return(as.character(x))
  }
}

# The study areas modelled
areas_to_model <- c("KIL","VAR","TII","PIS","HYY","KAR")

d <- read_csv("data/all_data_daily_2021.csv") %>% 
  filter(area %in% areas_to_model) %>% 
  filter(moist_prop > 95) %>% 
  mutate(week = week(date))

d2 <- d %>% 
  group_by(area, site) %>% 
  summarise(moist_mean = mean(moist2_median),
            moist_sd = sd(moist2_median),
            moist_cv = sd(moist2_median)/mean(moist2_median)*100,
            cal_class = median(cal_class))

d <- d %>% 
  group_by(area, site, week) %>% 
  summarise(moist_mean = mean(moist2_median),
            cal_class = median(cal_class))

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
  select(area:cal_class) %>% 
  ungroup() -> d

#########################################################################
# Env variables

e <- read_csv("data/all_env_variables.csv") %>% 
  filter(logger == "Tomst" | is.na(logger)) %>% 
  mutate(area = ifelse(area %in% c("AIL","MAL","SAA"), "KIL",area)) %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100)) %>% 
  select(-site) %>% rename(site = id_code)

e <- left_join(e, 
               read_csv("data/luke_variables.csv") %>% 
                 filter(logger == "Tomst" | is.na(logger)) %>% 
                 select(area, site, logger, dtw_0.5ha:twi) %>% 
                 rename(twi_luke = twi)) %>% 
  rename_with(., ~ gsub(".", "_", .x, fixed = TRUE))

names(e)
unique(e$area)

################################################3

d %>% filter(week == 26) %>% 
  left_join(., e) %>% 
  # filter(area == "TII") %>% 
  select(-starts_with("pisr_m")) %>% 
  mutate(across(starts_with("dtw_"), ~log(.x+1))) %>% 
  select_if(is_numeric) %>% cor(., method = "pearson", use = "pairwise.complete.obs") -> cors

cors[,"moist_mean"] %>% abs %>% sort()
cors[,"twi_luke"] %>% abs %>% sort()
cors[,"swi"] %>% abs %>% sort()


d %>% 
  filter(week == 26) %>% 
  left_join(., e) %>% 
  select(-starts_with("pisr_m")) %>% 
  mutate(across(starts_with("dtw_"), ~log(.x+1))) %>% 
  group_by(area) %>% 
  summarise(r = cor(twi_luke, dtw_0_5ha, use = "pairwise.complete.obs"))

# Select and preprocess other env data
# preprocess soil class
st <- d %>% 
  group_by(site) %>% 
  summarise(soil_type = max(cal_class)) %>% 
  mutate(soil_type = ifelse(soil_type == 100, 8, soil_type)) %>% 
  mutate(soil_type = ifelse(soil_type == 4, 5, soil_type)) %>% 
  mutate(soil_type = ifelse(soil_type == 6, 5, soil_type)) %>% 
  mutate(soil_type = ifelse(soil_type == 7, 5, soil_type)) %>% 
  mutate(soil_type = ifelse(soil_type == 1, 2, soil_type)) %>% 
  mutate(soil_type = factor(soil_type,
                            levels = c(2,5,8),
                            labels = c("sand","silt","peat")))

# Predictors in the models
predictors <- c("pisr_summer_10m","twi_luke","dtw_0_5ha","tpi20","tpi500","wet_effect")
class_pred <- c("soil_type")

d %>% left_join(e %>% select(site, all_of(predictors))) -> mod_data_all


e %>% 
  left_join(., st) %>%
  mutate(altitude = altitude/100) %>% 
  mutate(across(starts_with("pgaps"), ~abs(.x-100))) %>% 
  select(site, all_of(predictors), class_pred) %>% 
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
      
      model_formula_lm <- formula(paste0(iii, " ~ ", paste(c(env_vars), collapse = " + "), " + ", class_pred))
      
      temp %>% select(area, site, week, all_of(iii), cal_class) %>%
        left_join(e) %>%
        mutate(cal_class = factor(cal_class)) %>% 
        drop_na() -> mod_data
      
      ct <- table(mod_data[,class_pred])
      
      if(min(ct) == 1 & names(which.min(ct)) == "peat"){
        mod_data <- mod_data %>% 
          mutate(soil_type = recode(soil_type, peat = "silt"))
      }
      
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
                             mutate(loocv_R2 = cor(as.numeric(preds), mod_data %>% pull(iii))^2,
                                    loocv_rmse = sqrt(mean((as.numeric(preds) - mod_data %>% pull(iii))^2)),
                                    area = i,
                                    week = ii,
                                    resp = iii,
                                    model = "lm"))
      
      vi_df <- bind_rows(vi_df,
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars, class_pred),
                                    metric = "rsquared", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_r2",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "lm"),
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, env_vars, class_pred),
                                    metric = "rmse", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_rmse",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "lm"))
      
      
      # GAM
      
      model_formula_gam <- formula(paste0(iii, " ~ ", paste("s(", c(env_vars), ", k = 3)", collapse = " + "), " + ", class_pred))
      
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
                                    loocv_R2 = cor(as.numeric(preds), mod_data %>% pull(iii))^2,
                                    loocv_rmse = sqrt(mean((as.numeric(preds) - mod_data %>% pull(iii))^2)),
                                    area = i,
                                    week = ii,
                                    resp = iii,
                                    model = "gam"))
      
      vi_df <- bind_rows(vi_df,
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, class_pred, env_vars),
                                    metric = "rsquared", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_r2",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "gam"),
                         vi_permute(mod, target = iii, train = mod_data %>% select(iii, class_pred, env_vars),
                                    metric = "rmse", pred_wrapper = predict, nsim = 10) %>% 
                           mutate(method = "perm_rmse",
                                  area = i,
                                  week = ii,
                                  resp = iii,
                                  model = "gam"))
      
    }
  }
}

coef_df %>% write_csv("output/coef_df2.csv")
summ_df %>% write_csv("output/summ_df2.csv")
vi_df   %>% write_csv("output/vi_df.csv")

# Write out modelling data for later use in plottings

d %>% left_join(e) -> mod_data

mod_data_all %>% 
  filter(week == 30) %>% 
  mutate(area = factor(area, levels = areas_to_model, labels = c("Kilpisjärvi", "Värriö", "Tiilikka", "Pisa", "Hyytiälä", "Karkali"))) %>% 
  select(area, all_of(predictors)) %>% 
  mutate(dtw_0_5ha = dtw_0_5ha/100,
         twi_luke = twi_luke/1000) %>% 
  pivot_longer(cols = pisr_summer_10m:wet_effect) %>% 
  mutate(name = recode_factor(name,
                                  pisr_summer_10m = "Solar radiation",
                                  wet_effect = "Distance to water",
                                  soil_type = "Soil type",
                                  tpi20 = "TPI (20 m radius)",
                                  tpi500 = "TPI (500 m radius)",
                                  dtw_0_5ha = "DTW",
                                  twi_luke = "TWI")) %>% 
  group_by(area, name) %>% 
  summarise(mean = round(mean(value, na.rm = T),2),
            min = min(value, na.rm = T),
            max = max(value, na.rm = T)) %>% 
  mutate(value = paste0(mean, " [", min, ", ", max, "]")) %>% 
  pivot_wider(id_cols = area, names_from = name, values_from = value) -> sdf

write_csv2(sdf, "output/predictors_summary_table.csv")

# correlations by area and month

mod_data %>%
  select(area, all_of(predictors)) %>% 
  distinct() -> df

by(df, INDICES = df$area, FUN = function(x) cor(x[, -1], method = "spearman", use = "pairwise.complete.obs"))

#################################################################
# Transferability

cv_df <- tibble()
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
      
      model_formula_lm <- formula(paste0(iii, " ~ ", paste(c(env_vars), collapse = " + "), " + ", class_pred))
      
      temp %>% select(area, site, week, all_of(iii), cal_class) %>%
        left_join(e, by = "site") %>%
        mutate(cal_class = factor(cal_class)) %>% 
        drop_na() -> mod_data
      
      mod <- lm(model_formula_lm, data = mod_data)
      
      ct <- table(mod_data[,class_pred])
      
      for(iiii in areas_to_model[-which(areas_to_model == i)]){
        
        temp2 <- d %>% 
          filter(area == iiii, week == ii) %>% 
          select(area, site, week, all_of(iii), cal_class) %>%
          left_join(e, by = "site") %>%
          mutate(cal_class = factor(cal_class))
        
        if(min(ct) == 0 & names(which.min(ct)) == "sand"){
          temp2 <- temp2 %>% 
            mutate(soil_type = recode(soil_type, sand = "silt"))
        }
        
        temp2 <- temp2 %>%
          drop_na() %>% 
          mutate(preds = predict(mod, .))
        
        cv_df <- bind_rows(cv_df,
                           tibble(fit_area = i,
                                  week = ii,
                                  resp = iii,
                                  pred_area = iiii,
                                  model = "lm",
                                  loocv_R2 = cor(temp2 %>% pull(preds), temp2 %>% pull(iii))^2,
                                  loocv_rmse = sqrt(mean((temp2 %>% pull(preds) - temp2 %>% pull(iii))^2))))
        
      }
      
      if(min(ct) == 1 & names(which.min(ct)) == "peat"){
        mod_data2 <- mod_data %>% 
          mutate(soil_type = recode(soil_type, peat = "silt"))
      } else {
        mod_data2 <- mod_data
      }
      
      preds <- c()
      for(ir in seq_len(nrow(mod_data2))){
        
        mod <- lm(model_formula_lm, data = mod_data2[-ir,])
        preds <- c(preds, predict(mod, mod_data2[ir,]))
        
      }
      
      cv_df <- bind_rows(cv_df,
                           tibble(fit_area = i,
                                  week = ii,
                                  resp = iii,
                                  pred_area = i,
                                  model = "lm",
                                  loocv_R2 = cor(as.numeric(preds), mod_data %>% pull(iii))^2,
                                  loocv_rmse = sqrt(mean((as.numeric(preds) - mod_data %>% pull(iii))^2))))
      
      # GAM
      
      model_formula_gam <- formula(paste0(iii, " ~ ", paste("s(", c(env_vars), ", k = 3)", collapse = " + "), " + ", class_pred))
      
      mod <- mgcv::gam(model_formula_gam, method = "REML", data = mod_data)
      
      for(iiii in areas_to_model[-which(areas_to_model == i)]){
        
        temp2 <- d %>% 
          filter(area == iiii, week == ii) %>% 
          select(area, site, week, all_of(iii), cal_class) %>%
          left_join(e, by = "site") %>%
          mutate(cal_class = factor(cal_class))
        
        if(min(ct) == 0 & names(which.min(ct)) == "sand"){
          temp2 <- temp2 %>% 
            mutate(soil_type = recode(soil_type, sand = "silt"))
        }
        
        temp2 <- temp2 %>%
          drop_na() %>% 
          mutate(preds = predict(mod, .))
        
        cv_df <- bind_rows(cv_df,
                           tibble(fit_area = i,
                                  week = ii,
                                  resp = iii,
                                  pred_area = iiii,
                                  model = "gam",
                                  loocv_R2 = cor(temp2 %>% pull(preds), temp2 %>% pull(iii))^2,
                                  loocv_rmse = sqrt(mean((temp2 %>% pull(preds) - temp2 %>% pull(iii))^2))))
        
      }
      
      preds <- c()
      for(ir in seq_len(nrow(mod_data2))){
        
        mod <- mgcv::gam(model_formula_gam, method = "REML", data = mod_data2[-ir,])
        preds <- c(preds, predict(mod, mod_data2[ir,]))
        
      }
      cv_df <- bind_rows(cv_df,
                         tibble(fit_area = i,
                                week = ii,
                                resp = iii,
                                pred_area = i,
                                model = "gam",
                                loocv_R2 = cor(as.numeric(preds), mod_data %>% pull(iii))^2,
                                loocv_rmse = sqrt(mean((as.numeric(preds) - mod_data %>% pull(iii))^2))))
    }
  }
}

cv_df %>% write_csv("output/cv_df_weekly.csv")

#################################################################
# Univariate

uni_predictors <- c("twi_luke","dtw_0_5ha","swi")

e <- read_csv("data/all_env_variables.csv") %>% 
  filter(logger == "Tomst" | is.na(logger)) %>% 
  mutate(area = ifelse(area %in% c("AIL","MAL","SAA"), "KIL",area)) %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100)) %>% 
  select(-site) %>% rename(site = id_code)

e <- left_join(e, 
               read_csv("data/luke_variables.csv") %>% 
                 filter(logger == "Tomst" | is.na(logger)) %>% 
                 select(area, site, logger, dtw_0.5ha:twi) %>% 
                 mutate(across(dtw_0.5ha:dtw_4ha, ~.x/100),
                        across(twi, ~.x/1000)) %>% 
                 rename(twi_luke = twi)) %>% 
  rename_with(., ~ gsub(".", "_", .x, fixed = TRUE))

names(e)
unique(e$area)

e <- e %>% 
  select(site, all_of(uni_predictors))

coef_df <- tibble()
summ_df <- tibble()
for(i in areas_to_model){
  print(i)
  
  for(ii in d %>% filter(area == i) %>% pull(week) %>% unique){
    print(ii)
    
    d %>% filter(area == i,
                 week == ii) -> temp
    
    responses <- names(temp)[grepl("^moist", names(temp))]
    
    for(iii in responses){
      
      temp %>% select(area, site, week, all_of(iii)) %>%
        left_join(e) %>%
        drop_na() -> mod_data
      
      for(iiii in uni_predictors){
        # LM
        
        model_formula_lm <- formula(paste0(iii, " ~ ", iiii))
        
        mod <- lm(model_formula_lm, data = mod_data)
        
        coef_df <- bind_rows(coef_df,
                             tidy(mod) %>% 
                               mutate(area = i,
                                      week = ii,
                                      resp = iii,
                                      pred = iiii,
                                      model = "lm"))
        
        preds <- c()
        for(ir in seq_len(nrow(mod_data))){
          
          mod <- lm(model_formula_lm, data = mod_data[-ir,])
          preds <- c(preds, predict(mod, mod_data[ir,]))
          
        }
        
        summ_df <- bind_rows(summ_df,
                             glance(mod) %>% 
                               mutate(loocv_R2 = cor(as.numeric(preds), mod_data %>% pull(iii))^2,
                                      loocv_rmse = sqrt(mean((as.numeric(preds) - mod_data %>% pull(iii))^2)),
                                      area = i,
                                      week = ii,
                                      resp = iii,
                                      pred = iiii,
                                      model = "lm"))
        
      }
    }
  }
}

coef_df %>% write_csv("output/uni_coef_df2.csv")
summ_df %>% write_csv("output/uni_summ_df2.csv")

