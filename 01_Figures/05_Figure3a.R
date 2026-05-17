# Figure 3a - Captive wolf boxplot 

library (dplyr)
library(ggplot2)

dat <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)

# Final - Figure 3c captive vs wild wolf box plot 
Captive <-  subset(dat, Group=="captive")

p1=Captive %>% ggplot(aes(x=LatGrouping_by10, y=DOY))+  
  geom_boxplot()+
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0), 
             pch=21, size=2.5, aes(fill=factor(Region2)))
p2 <- p1+scale_fill_manual(values=c("darkgoldenrod1", "gray", "olivedrab3", "orangered", "#56B4E9" )) + theme_classic()


p2 + scale_y_continuous(breaks = round(seq(min(25), max(250), by = 25),0)) 
p2

ggsave("May15_Figure1c.pdf", width=10,height=4)

