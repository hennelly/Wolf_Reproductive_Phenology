
library(ggplot2)
library(dplyr)

dat <- read.csv("2026_May15_Published_unpublished_DOY_vs_latitude_Dec42025_FINAL2.csv", header = TRUE)

dat$Region <- recode(dat$Region,
                     "Middle East"  = "Southwest Asia",
                     "NorthAmerica" = "North America")

plot_base <- ggplot(dat, aes(x = DOY, y = Lat, fill = Region)) +
  geom_point(size = 3, shape = dat$unpublished.vs.published2) +
  geom_errorbar(aes(ymin = Lat_lowerrange, ymax = Lat_upperrange, color = Region)) +
  geom_errorbarh(aes(xmin = DOY_lowerrange, xmax = DOY_upperrange, color = Region)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = Region)) +
  scale_y_continuous(breaks = seq(10, 90, by = 10)) +
  scale_x_continuous(breaks = seq(0, 250, by = 25)) +
  scale_fill_manual(values = c("Asia"          = "purple",
                               "Europe"        = "olivedrab3",
                               "Indian"        = "orangered",
                               "Southwest Asia"= "darkgoldenrod1",
                               "North America" = "#56B4E9",
                               "Tibetan"       = "gray")) +
  scale_color_manual(values = c("Asia"          = "purple",
                                "Europe"        = "olivedrab3",
                                "Indian"        = "orangered",
                                "Southwest Asia"= "darkgoldenrod1",
                                "North America" = "#56B4E9",
                                "Tibetan"       = "gray")) +
  guides(
    fill  = guide_legend(override.aes = list(shape    = 21,
                                             size     = 4,
                                             color    = "black",
                                             linetype = 0)),
    color = "none"
  ) +
  labs(x = "Estimated timing of birth (DOY)", y = "Latitude", fill = "Region") +
  theme_classic()

print(plot_base)



ggsave("2026_May17_FigureS2.pdf", width=7,height=4)
