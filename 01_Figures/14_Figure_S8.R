library(ggplot2)
library(ggpmisc)
library(patchwork)

# ── Data ───────────────────────────────────────────────────────
Europe <- subset(dat, Region == "Europe")
Europe$Region2 <- factor(Europe$Region2)

my_colors <- c("Iberian" = "purple", "Europe" = "orange")

# ── Individual plots ───────────────────────────────────────────
p1 <- ggplot(Europe, aes(x = DOY, y = Lat)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors) +
  labs(x = "DOY", y = "Lat") +
  theme_classic()

p2 <- ggplot(Europe, aes(x = DOY, y = PC1)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors) +
  labs(x = "DOY", y = "PC1") +
  theme_classic()

p3 <- ggplot(Europe, aes(x = DOY, y = daylength_original_matingdate)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors) +
  labs(x = "DOY", y = "Daylength (mating date)") +
  theme_classic()

p4 <- ggplot(Europe, aes(x = DOY, y = Precipitation_Seasonality)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors) +
  labs(x = "DOY", y = "Precipitation Seasonality") +
  theme_classic()

p5 <- ggplot(Europe, aes(x = DOY, y = mean_ndvi)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors) +
  labs(x = "DOY", y = "Mean NDVI") +
  theme_classic()

p6 <- ggplot(Europe, aes(x = DOY, y = Annual_Precipitation)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors) +
  labs(x = "DOY", y = "Annual Precipitation") +
  theme_classic()
