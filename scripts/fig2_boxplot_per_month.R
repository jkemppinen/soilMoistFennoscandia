library(pals)
library(tidyverse)

# bring logger soil moisture files
your_sm  <- read.csv ("D:/ARTIKKELIT/ARTIKKELI8/analysis/data/biogeoclimate/all_data_monthly.csv")

your_sm %>% mutate(area = ifelse(area %in% c("AIL", "MAL", "SAA"), "KIL", area)) -> your_sm

your_sm$area <- recode_factor(your_sm$area,
         KIL = "a",
         RAS = "b",
         VAR = "c",
         TII = "d",
         PIS = "e",
         HYY = "f",
         KAR = "g")

# colors
gnuplot (10)
your_palette <- colorRampPalette(c("#000099", "#0000FF", "#5000FF", "#9F0FF0", "#EF42BD", "#FF758A", "#FFA857", "#FFDB24"))

# plot
dev.off()
pdf(file="D:/ARTIKKELIT/ARTIKKELI8/analysis/results/fig2/fig2_boxplot_per_month.pdf")

your_sm %>% 
  filter(month %in% c(5, 6, 7, 8, 9)) %>%
  ggplot(aes(x=factor(month), y=moist_mean, fill = area, color = area)) +

  geom_boxplot (notch=TRUE, notchwidth = 0.1,
                width=0.5, position=position_dodge(0.7),
                outlier.size = 0.5)  +
  
  scale_fill_manual(values = rev(your_palette(7))) +
  scale_color_manual(values = rev(your_palette(7))) +
  
  ylab("VWC%") +
  xlab("") +
  
  #set theme
  theme(axis.text.y=element_text(colour="black", size= 10),
        axis.text.x=element_text(colour="black", size= 10),
        
        axis.title.y=element_text("black", size= 10),
        axis.title.x=element_text("black", size= 10),
        
        axis.line.y=element_line("black", size= 0.2),
        axis.line.x=element_line("black", size= 0.2),

        aspect.ratio = 0.3,
        panel.background = element_blank())

dev.off()
