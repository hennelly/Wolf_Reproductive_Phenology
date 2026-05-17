## Figure 1C ###

library(dbplyr)

dat <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)

Wild <- subset(dat, Group=="wild ")

p1=Wild %>% ggplot(aes(x=LatGrouping_by10, y=DOY))+  
  geom_boxplot()+
  geom_point(position=position_jitterdodge(jitter.width=1, dodge.width = 0), 
             pch=21, size=2.5, aes(fill=factor(Region2)))
p2 <- p1+scale_fill_manual(values=c("olivedrab3", "forestgreen", "orangered", "darkgoldenrod1", "#56B4E9", "gray", "purple", "gray", "white", "black", "brown")) + theme_classic()
p2 + scale_y_continuous(breaks = round(seq(min(25), max(250), by = 25),0)) 

ggsave("2026_May15_Figure1c_May5_2026.pdf", width=6,height=4)
