# Daylength change graph with wolf birthing times 

library(ggplot2)
library(scales)

daylen_df <- read.csv("daylength_df.csv")
daychange <- read.csv("Figure2_May15.csv")

str(daylen_df)

daychange$Date <- as.Date(daychange$Date, format = "%m/%d/%y")
daychange$Lat <- factor(daychange$Lat)

p <- ggplot(daychange) +
  geom_line(aes(x = Date, y = daylength, color = Lat), alpha=0.8, size=1)+
  scale_colour_discrete()+
  geom_point(aes(x = Date, y = mday_daylength, fill = Lat), pch=21, size=3)  + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")  + theme(axis.text.x=element_text(angle=60, hjust=1))
p + theme_classic() + scale_color_manual(values=c("orangered4", "orangered3", "orangered2", "orangered1", "orangered", "palevioletred2", "palevioletred1", "lightcoral", "pink2", "chocolate1", "chocolate2", "sienna2", "sienna1", "tan2", "tan1", "sandybrown", "peachpuff1", "yellow", "lightgoldenrod2", "lightgoldenrod1", "wheat2", "wheat1", "slategray1", "skyblue", "skyblue1", "deepskyblue", "deepskyblue1", "deepskyblue2", "dodgerblue", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +  scale_fill_manual(values=c("orangered4", "orangered3", "orangered2", "orangered1", "orangered", "palevioletred2", "palevioletred1", "lightcoral", "pink2", "chocolate1", "chocolate2", "sienna2", "sienna1", "tan2", "tan1", "sandybrown", "peachpuff1", "yellow", "lightgoldenrod2", "lightgoldenrod1", "wheat2", "wheat1", "slategray1", "skyblue", "skyblue1", "deepskyblue", "deepskyblue1", "deepskyblue2", "dodgerblue", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4"))

# Just population 
p <- ggplot(daychange) +
  geom_line(aes(x = Date, y = daylength, color = Lat), size=0.5)+
  scale_colour_discrete()+
  geom_point(aes(x = Date, y = mday_daylength, fill = Population), pch=21, size=3)  + scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")  + theme(axis.text.x=element_text(angle=60, hjust=1))
p + theme_classic() + scale_color_manual(values=c("gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray")) +  scale_fill_manual(values=c("olivedrab3", "olivedrab3", "forestgreen", "orangered", "#56B4E9", "gray", "darkgoldenrod1", "gray", "gray", "chocolate1", "chocolate2", "sienna2", "sienna1", "tan2", "tan1", "sandybrown", "peachpuff1", "yellow", "lightgoldenrod2", "lightgoldenrod1", "wheat2", "wheat1", "slategray1", "skyblue", "skyblue1", "deepskyblue", "deepskyblue1", "deepskyblue2", "dodgerblue", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4"))

ggsave("2026_May17_Figure2.pdf", width=12,height=7)
