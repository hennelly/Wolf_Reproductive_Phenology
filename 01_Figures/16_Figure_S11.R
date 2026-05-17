library(ggplot2)
library(ggpmisc)
library(patchwork)

# ── Load and subset ────────────────────────────────────────────
dat <- read.csv("Wild_wolf_parturition_May15_final_2026.csv", header = TRUE)

NorthAmerica <- subset(dat, Region == "NorthAmerica")
NorthAmerica$Region2 <- factor(NorthAmerica$Region2)

my_colors_na <- setNames(rep("darkgoldenrod1", length(levels(NorthAmerica$Region2))),
                          levels(NorthAmerica$Region2))

# ── Individual plots ───────────────────────────────────────────
p1_na <- ggplot(NorthAmerica, aes(x = DOY, y = Lat)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors_na) +
  labs(x = "DOY", y = "Lat") +
  theme_classic()

p2_na <- ggplot(NorthAmerica, aes(x = DOY, y = PC1)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors_na) +
  labs(x = "DOY", y = "PC1") +
  theme_classic()

p3_na <- ggplot(NorthAmerica, aes(x = DOY, y = daylength_original_matingdate)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors_na) +
  labs(x = "DOY", y = "Daylength (mating date)") +
  theme_classic()

p4_na <- ggplot(NorthAmerica, aes(x = DOY, y = Precipitation_Seasonality)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors_na) +
  labs(x = "DOY", y = "Precipitation Seasonality") +
  theme_classic()

p5_na <- ggplot(NorthAmerica, aes(x = DOY, y = mean_ndvi)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors_na) +
  labs(x = "DOY", y = "Mean NDVI") +
  theme_classic()

p6_na <- ggplot(NorthAmerica, aes(x = DOY, y = Annual_Precipitation)) +
  geom_point(aes(fill = Region2), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = my_colors_na) +
  labs(x = "DOY", y = "Annual Precipitation") +
  theme_classic()

# ── Combine ────────────────────────────────────────────────────
combined_na <- (p1_na | p2_na) / (p3_na | p4_na) / (p5_na | p6_na) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

print(combined_na)

ggsave("May17_2026_NorthAmerica_panel.pdf",
       plot = combined_na, width = 10, height = 12, dpi = 300)
