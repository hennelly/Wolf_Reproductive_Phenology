dat <- read.csv ("2026_May15_Published_unpublished_DOY_vs_latitude_FINALIZED.csv", header=TRUE)

plot <- ggplot(dat, aes(x=DOY, y=Lat,
                                fill = Region))+ 
          geom_point(size=3, shape=dat$unpublished.vs.published2) + 
  geom_errorbar(aes(ymin = Lat_lowerrange, ymax = Lat_upperrange, color=Region)) +
  geom_errorbarh(aes(xmin = DOY_lowerrange, xmax = DOY_upperrange, color=Region)) +
        theme_classic() +scale_fill_manual(values=c("purple", "olivedrab3", "orangered", "darkgoldenrod1", "#56B4E9", "gray", "gray")) +scale_color_manual(values=c( "purple", "olivedrab3" , "orangered", "darkgoldenrod1", "#56B4E9", "gray", "gray"))  

plot3 <- plot + geom_smooth( method="lm", se=FALSE, aes(color=Region)) 
plot3 + scale_y_continuous(breaks = round(seq(min(10), max(90), by = 10),0)) + scale_x_continuous(breaks = round(seq(min(0), max(250), by = 25),0)) 

ggsave("2026_May15_FigureS2.pdf", width=7,height=4)
