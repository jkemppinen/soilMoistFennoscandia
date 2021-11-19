
library(tidyverse)

# The study areas modelled
areas_to_model <- c("RAS","KIL","VAR","TII","PIS","HYY","KAR")

d <- read_csv("data/all_data_daily_2021.csv") %>% 
  select(site:date, T1_mean, starts_with("moist"), probl) %>% 
  # filter(date >= "2020-01-01", date <= "2020-12-31") %>% 
  filter(date >= "2020-03-01", date <= "2020-10-31") %>% 
  mutate(area = ifelse(area %in% c("AIL","MAL","SAA"), "KIL",area))

d %>% filter(!is.na(moist_mean)) %>% 
  group_by(area, date) %>% 
  count() -> count_days 

left_join(count_days,
          count_days %>% group_by(area) %>% summarise(maxn = max(n))) %>% 
  mutate(n_prop = n/maxn) %>% 
  filter(n_prop >= 0.5) -> count_days

count_days %>% group_by(area) %>% 
  count()

left_join(d, count_days) %>% 
  filter(!is.na(moist_mean)) %>% 
  filter(!is.na(n_prop)) %>% 
  select(site:probl) -> d

#########################################################################
# Modelling

# Predictors in the models
predictors <- c("altitude", "pisr", "moist_med", "snow_days", "tpi100",
                "water_prop_500m", "pgaps2m_metsakeskus")

e <- read_csv("data/all_env_variables.csv") %>% 
  filter(logger == "Tomst") %>% 
  filter(area %in% areas_to_model) %>% 
  filter(!(area == "PIS" & site >= 100))

# Preprocess snow variables

snow_var <- data.frame()
for(i in areas_to_model){
  # i <- "VAR"
  snow %>% filter(area == i) -> temp
  
  mod <- glmer(snow_days ~ factor(hydro_year) + (1 | id_code), data = temp, family = ifelse(i == "VAR", "gaussian","poisson"))
  
  df <- data.frame(hydro_year = "2020",
                   id_code = unique(e %>% filter(area == i) %>% pull(id_code)),
                   area = i)
  
  df$snow_days <- round(predict(mod, df, type = "response", allow.new.levels = T))
  
  snow_var <- bind_rows(snow_var, df)
}

# Preprocess soil moisture

sm <- read_csv("https://raw.githubusercontent.com/poniitty/Fennoscandia_microclimates/main/data/all_data_monthly.csv") %>% 
  select(id_code:month, starts_with("moist"))

sm %>% select(id_code:month, moist_prop, moist_med) %>% 
  mutate(moist_med = ifelse(moist_prop < 0.1, NA, moist_med)) %>% 
  arrange(id_code, year, month) %>% 
  group_by(id_code) %>% 
  mutate(moist_med = na.locf(moist_med, na.rm = F)) -> sm

moist_var <- data.frame()
for(i in areas_to_model){
  
  sm %>% filter(area == i) -> temp
  
  mod <- lmer(moist_med ~ factor(year) + factor(month) + (1 | id_code), data = temp)
  
  expand_grid(ym = paste0(c(rep(2019, times = 2),rep(2020, times = 10)),
                          "_",
                          c(11:12,1:10)),
              id_code = unique(e %>% filter(area == i) %>% pull(id_code)),
              area = i) %>% 
    mutate(year = as.numeric(unlist(lapply(ym, function(x) { str_split(x, "_")[[1]][1]}))),
           month = as.numeric(unlist(lapply(ym, function(x) { str_split(x, "_")[[1]][2]})))) %>% 
    arrange(id_code, year, month) %>% 
    select(-ym) -> df
  
  df$moist_med <- round(predict(mod, df, type = "response", allow.new.levels = T),2)
  
  moist_var <- bind_rows(moist_var, df)
}


# Monthly radiation values to long format
extr_month <- function(x){ as.numeric(gsub("m","",unlist(lapply(x, function(xx) strsplit(xx,"_")[[1]][2]))))}

e %>% dplyr::select(id_code, starts_with("pisr_m")) %>% 
  pivot_longer(cols = starts_with("pisr_m"), names_to = "month", values_to = "pisr") %>% 
  mutate(month = extr_month(month)) -> pisr_var

# Select and preprocess other env data

e %>% 
  mutate(altitude = altitude/100) %>% 
  mutate(across(starts_with("pgaps"), ~abs(.x-100))) %>% 
  select(id_code, names(.)[names(.) %in% predictors]) -> e

#####################################################################################
# Modelling

coef_df <- tibble()
summ_df <- tibble()
coef_df_step <- tibble()
summ_df_step <- tibble()
for(i in areas_to_model){
  print(i)
  
  for(ii in 1:12){
    print(ii)
    
    d %>% filter(area == i,
                 month == ii) -> temp
    
    responses <- names(temp)[grepl("^T[0-9]", names(temp))]
    
    for(iii in responses){
      
      env_vars <- predictors
      if(ii %in% c(1:3,10:12)){
        env_vars <- env_vars[-which(env_vars == "pisr")]
      }
      if(i == "KAR"){
        env_vars <- env_vars[-which(grepl("snow", env_vars))]
      }
      if(i %in% c("KAR","HYY","TII")){
        env_vars <- env_vars[-which(grepl("altitude", env_vars))]
      }
      if(i %in% c("VAR")){
        env_vars <- env_vars[-which(grepl("water", env_vars))]
      }
      
      model_formula_lm <- formula(paste0(iii, " ~ ", paste(c(env_vars), collapse = " + ")))
      
      temp %>% select(id_code, year, month, iii) %>% 
        left_join(pisr_var) %>% 
        left_join(moist_var) %>% 
        left_join(snow_var) %>% 
        left_join(e) %>% 
        filter(complete.cases(.)) -> mod_data
      
      mod <- lm(model_formula_lm, data = mod_data)
      
      coef_df <- bind_rows(coef_df,
                           tidy(mod) %>% 
                             mutate(area = i,
                                    month = ii,
                                    resp = iii))
      summ_df <- bind_rows(summ_df,
                           glance(mod) %>% 
                             mutate(area = i,
                                    month = ii,
                                    resp = iii))
      
      mod <- step(mod, trace = -1)
      
      coef_df_step <- bind_rows(coef_df_step,
                                tidy(mod) %>% 
                                  mutate(area = i,
                                         month = ii,
                                         resp = iii))
      summ_df_step <- bind_rows(summ_df_step,
                                glance(mod) %>% 
                                  mutate(area = i,
                                         month = ii,
                                         resp = iii))
      
      
    }
    
  }
  
}

coef_df %>% write_csv("output/model_outputs/coef_df2.csv")
summ_df %>% write_csv("output/model_outputs/summ_df2.csv")
coef_df_step %>% write_csv("output/model_outputs/coef_df_step2.csv")
summ_df_step %>% write_csv("output/model_outputs/summ_df_step2.csv")

# Write out modelling data for later use in plottings

d %>% select(id_code, area, year, month, starts_with("T")) %>% 
  left_join(pisr_var) %>% 
  left_join(moist_var) %>% 
  left_join(snow_var) %>% 
  left_join(e) -> mod_data

mod_data %>% write_csv("output/model_outputs/model_input_data.csv")

# correlations by area and month

mod_data %>%
  filter(month %in% c(1,7)) %>% 
  mutate(group = paste0(area, "_", month)) %>% 
  select(group, predictors) -> df

by(df, INDICES = df$group, FUN = function(x) cor(x[, -1], method = "spearman"))



