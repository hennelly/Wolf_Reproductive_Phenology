#Figure 3c captive vs wild wolf box plot 

dat <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)

plot <- ggplot(dat, aes(x=LatGrouping_by10, y=DOY, colour=Group, fill=Group)) +
  geom_boxplot(position = position_dodge(width = 1), fill = NA, lwd=1)

p2 <- plot+scale_color_manual(values=c("plum3", "lightsalmon3")) + scale_fill_manual(values=c("black", "black")) + theme_classic()

p2 + scale_y_continuous(breaks = round(seq(min(25), max(365), by = 25),0)) 

ggsave("May15_2026_Figure3b.pdf", width=10,height=3)
