###############################################################################
# Gather and combine moisture data from study area specific Github repositories


library(tidyverse)

# Download datasets
# Rastigaisa
d1 <- read_csv("https://raw.githubusercontent.com/poniitty/Rastigaisa_microclimate/main/output/tomst_data_daily.csv")

# Kilpisjärvi
d2 <- read_csv("https://raw.githubusercontent.com/poniitty/kilpisjarvi_microclimate/main/output/tomst_data_daily.csv") %>% 
  filter(!grepl("RA", site))

# Värriö
d3 <- read_csv("https://raw.githubusercontent.com/poniitty/varrio_microclimate/main/output/tomst_data_daily.csv")

# Tiilikka
d4 <- read_csv("https://raw.githubusercontent.com/poniitty/Tiilikka_microclimates/main/output/tomst_data_daily.csv")

# Pisa
d5 <- read_csv("https://raw.githubusercontent.com/poniitty/Pisa_microclimates/main/output/tomst_data_daily.csv")

# Hyytiälä
d6 <- read_csv("https://raw.githubusercontent.com/poniitty/Hyytiala_microclimates/main/output/tomst_data_daily.csv")

# Karkali
d7 <- read_csv("https://raw.githubusercontent.com/poniitty/Karkali_microclimate/main/output/tomst_data_daily.csv")

# Function for site identifiers
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

# Combine all data and edit the sites ID's
bind_rows(d1 %>% mutate(site = paste0("RAS",unlist(lapply(site, add_zeros)))),
          d2,
          d3 %>% mutate(site = paste0("VAR",unlist(lapply(site, add_zeros)))),
          d4 %>% mutate(site = paste0("TII",unlist(lapply(site, add_zeros)))),
          d5 %>% mutate(site = paste0("PIS",unlist(lapply(site, add_zeros)))),
          d6 %>% mutate(site = paste0("HYY",unlist(lapply(site, add_zeros)))),
          d7 %>% mutate(site = paste0("KAR",unlist(lapply(site, add_zeros))))) %>% 
  mutate(area = substr(site, 1, 3)) %>% 
  relocate(area, .after = site) -> d

# Check
unique(d$area)

# Daily data ready!
write_csv(d, "data/all_data_daily_2021.csv")
