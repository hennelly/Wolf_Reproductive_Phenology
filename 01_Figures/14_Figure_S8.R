#install.packages("ggpmisc")
library(ggpmisc)

dat <- read.csv ("Wild_wolf_parturition_May15_final_2026.csv", header=TRUE)

## Europe
Europe <- subset(dat, Region=="Europe")
Europe$Region2 <- factor(Europe$Region2)

my_colors <- c("Iberian" = "purple", 
               "Europe" = "orange")


#PC1

ggplot(Europe, aes(x = DOY, y = PC1)) +
  geom_point(
    aes(fill = Region2),
    size = 3,
    pch = 21
  ) +
  stat_poly_line(
    formula = y ~ x,
    aes(group = 1),
    color = "black"
  ) +      stat_poly_eq() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors)

ggsave("2026_March10_Europe_FigureSx_PC1.pdf", width=5,height=4)

#Latitude
ggplot(Europe, aes(x = DOY, y = Lat)) +
  geom_point(
    aes(fill = Region2),
    size = 3,
    pch = 21
  ) +
  stat_poly_line(
    formula = y ~ x,
    aes(group = 1),
    color = "black"
  ) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors)
ggsave("2026_Feb10_Europe_FigureSx_Latitude_.pdf", width=5,height=4)

#Mating date

ggplot(Europe, aes(x = DOY, y = daylength_original_matingdate)) +
  geom_point(
    aes(fill = Region2),
    size = 3,
    pch = 21
  ) +
  stat_poly_line(
    formula = y ~ x,
    aes(group = 1),
    color = "black"
  ) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors)

ggsave("2026_Feb10_FigureSx_Europe_daylength_original_matingdate.pdf", width=5,height=4)

#Precipitation seasonality 
ggplot(Europe, aes(x = DOY, y = Precipitation_Seasonality)) +
  geom_point(
    aes(fill = Region2),
    size = 3,
    pch = 21
  ) +
  stat_poly_line(
    formula = y ~ x,
    aes(group = 1),
    color = "black"
  ) +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors)

#Mean NDVI 
ggplot(Europe, aes(x = DOY, y = mean_ndvi)) +
  geom_point(
    aes(fill = Region2),
    size = 3,
    pch = 21
  ) +
  stat_poly_line(
    formula = y ~ x,
    aes(group = 1),
    color = "black"
  ) + stat_poly_eq() +
  theme_classic() +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors)


ggsave("2026_May15_Europe_FigureS.pdf", width=5,height=4)
