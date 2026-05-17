#Figure 3b captive wolves vs wild wolf

library(nls2)
library(ggplot2)
library(ggpmisc)

dat <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)

lat_seq_wild    <- seq(min(dat_wild$Lat),    max(dat_wild$Lat),    length.out = 300)
lat_seq_captive <- seq(min(dat_captive$Lat), max(dat_captive$Lat), length.out = 300)

curve_wild <- data.frame(
  DOY = coef(fit_asy)["a"] - coef(fit_asy)["b"] * exp(-coef(fit_asy)["c"] * lat_seq_wild),
  Lat = lat_seq_wild
)

curve_captive <- data.frame(
  DOY = coef(fit_log1)["a"] / (1 + exp(-coef(fit_log1)["k"] * (lat_seq_captive - coef(fit_log1)["x0"]))),
  Lat = lat_seq_captive
)

curve_df2 <- rbind(
  data.frame(DOY = curve_wild$DOY,    Lat = curve_wild$Lat,    Group = "wild"),
  data.frame(DOY = curve_captive$DOY, Lat = curve_captive$Lat, Group = "captive")
)

plot2 <- ggplot(captive_clean, aes(x = DOY, y = Lat, fill = Group)) +
  geom_point(size = 3, colour = "black", pch = 21) +
  geom_line(data = curve_df2, aes(x = DOY, y = Lat, color = Group),
            linewidth = 1.2, inherit.aes = FALSE) +
  scale_y_continuous(breaks = seq(10, 80, by = 10)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values  = c("captive" = "lavender", "wild" = "peachpuff")) +
  scale_color_manual(values = c("captive" = "slateblue3", "wild" = "sienna3")) +
  coord_cartesian(xlim = c(0, 365), ylim = c(0, 85)) +
  annotate("text", x = 5, y = 85,
           label = paste0("R² = ", round(r2_captive, 2), " (captive, logistic)"),
           color = "slateblue3", hjust = 0, size = 4) +
  annotate("text", x = 5, y = 79,
           label = paste0("R² = ", round(r2_wild, 2), " (wild, asymptotic)"),
           color = "sienna3", hjust = 0, size = 4) +
  labs(x = "Estimated timing of birth (DOY)", y = "Latitude") +
  theme_classic()

print(plot2)


ggsave("May15_2026_Figure_wildasy_captivelog.pdf", plot = plot2, width = 10, height = 4)
