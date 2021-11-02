library(pals)
library(tidyverse)
library(lubridate)
library(scales)

# bring logger soil moisture files
your_sm  <- read.csv ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/all_data_daily.csv")

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
         RAS = "a",
         KIL = "b",
         VAR = "c",
         TII = "d",
         PIS = "e",
         HYY = "f",
         KAR = "g")

your_sm %>% mutate (date = as_date(date))-> your_sm

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig2/fig2_path_per_area.pdf")

your_sm %>% 
  filter(year(date) %in% c(2020)) %>%
  filter(month(date) %in% c(5, 6, 7, 8, 9)) %>%
  ggplot(aes(x=(date), y= (moist_mean), colour = area)) +
  geom_path(aes(group=area), alpha = 0.3) +

  scale_color_manual(values = rev(your_palette(7))) +
  
  ylab("VWC%") +
  xlab("") +

  #set theme
  theme(
        
        axis.line.y=element_line("black", size= 0.2),
        axis.line.x=element_line("black", size= 0.2),

        aspect.ratio = 0.1,
        panel.background = element_blank()) +
    
  facet_grid(facets = area ~ .)
  
dev.off()
