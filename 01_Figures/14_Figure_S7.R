#install.packages("ggpmisc")
library(ggpmisc)

dat <- read.csv ("Wild_wolf_parturition_May15_final_2026.csv", header=TRUE)

## Wolves except Indian wolves 
not_Indian <- subset(dat, Region != "Indian")
#PC1
plot <- ggplot(not_Indian, aes(x=DOY, y=PC1)) + 
          geom_point(size=3, fill="darkgoldenrod1", colour="black", pch=21) + 
          geom_smooth(method=lm) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
          )
plot2 <- plot + stat_poly_line() + stat_poly_eq()
plot2 + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25), 0))
ggsave("2026_May15_noIndian_FigureSx_PC1_March9_2026.pdf", width=5,height=4)

#Latitude
plot <- ggplot(not_Indian, aes(x=DOY, y=Lat)) + 
          geom_point(size=3, fill="darkgoldenrod1", colour="black", pch=21) + 
          geom_smooth(method=lm) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
          )
plot2 <- plot + stat_poly_line() + stat_poly_eq()
plot2 + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25), 0))
ggsave("2026_May15_not_Indian_Lat.pdf", width=5,height=4)


#Mating date
plot <- ggplot(not_Indian, aes(x=DOY, y=daylength_original_matingdate)) + 
          geom_point(size=3, fill="darkgoldenrod1", colour="black", pch=21) + 
          geom_smooth(method=lm) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
          )
plot2 <- plot + stat_poly_line() + stat_poly_eq()
plot2 + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25), 0))
ggsave("2026_May15_notIndian_daylength_original_matingdate.pdf", width=5,height=4)


#Precipitation seasonality 
plot <- ggplot(not_Indian, aes(x=DOY, y=Precipitation_Seasonality)) + 
          geom_point(size=3, fill="darkgoldenrod1", colour="black", pch=21) + 
          geom_smooth(method=lm) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
          )
plot2 <- plot + stat_poly_line() + stat_poly_eq()
plot2 + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25), 0))
ggsave("2026_May15_notIndian_Precipitation_Seasonality.pdf", width=5,height=4)


#Annual_Precipitation 
plot <- ggplot(not_Indian, aes(x=DOY, y=Annual_Precipitation)) + 
          geom_point(size=3, fill="darkgoldenrod1", colour="black", pch=21) + 
          geom_smooth(method=lm) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
          )
plot2 <- plot + stat_poly_line() + stat_poly_eq()
plot2 + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25), 0))

ggsave("2026_May15_notIndian_annual_precipitation.pdf", width=5,height=4)

#mean_ndvi 
plot <- ggplot(not_Indian, aes(x=DOY, y=mean_ndvi)) + 
          geom_point(size=3, fill="darkgoldenrod1", colour="black", pch=21) + 
          geom_smooth(method=lm) + 
          theme_classic() +
          theme(
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=16),
            axis.title.y = element_text(size=16)
          )
plot2 <- plot + stat_poly_line() + stat_poly_eq()
plot2 + scale_x_continuous(breaks = round(seq(min(0), max(365), by = 25), 0))
ggsave("2026_May15_notIndian_mean_ndvi.pdf", width=5,height=4)

