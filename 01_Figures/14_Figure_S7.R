library(ggplot2)
library(ggpmisc)
library(patchwork)
library(dplyr)

# ── Load and recode ────────────────────────────────────────────
dat <- read.csv("Wild_wolf_parturition_May15_final_2026.csv", header = TRUE)

dat$Region <- recode(dat$Region,
                     "Middle East"  = "Southwest Asia",
                     "NorthAmerica" = "North America")

dat <- subset(dat, Region != "Indian")

# ── Color scale ────────────────────────────────────────────────
region_colors <- c("Asia"           = "purple",
                   "Europe"         = "olivedrab3",
                   "Southwest Asia" = "darkgoldenrod1",
                   "North America"  = "#56B4E9",
                   "Tibetan"        = "gray")

# ── Individual plots ───────────────────────────────────────────
p1_all <- ggplot(dat, aes(x = DOY, y = Lat)) +
  geom_point(aes(fill = Region), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = region_colors) +
  labs(x = "DOY", y = "Lat") +
  theme_classic()

p2_all <- ggplot(dat, aes(x = DOY, y = PC1)) +
  geom_point(aes(fill = Region), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = region_colors) +
  labs(x = "DOY", y = "PC1") +
  theme_classic()

p3_all <- ggplot(dat, aes(x = DOY, y = daylength_original_matingdate)) +
  geom_point(aes(fill = Region), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = region_colors) +
  labs(x = "DOY", y = "Daylength (mating date)") +
  theme_classic()

p4_all <- ggplot(dat, aes(x = DOY, y = Precipitation_Seasonality)) +
  geom_point(aes(fill = Region), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = region_colors) +
  labs(x = "DOY", y = "Precipitation Seasonality") +
  theme_classic()

p5_all <- ggplot(dat, aes(x = DOY, y = mean_ndvi)) +
  geom_point(aes(fill = Region), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = region_colors) +
  labs(x = "DOY", y = "Mean NDVI") +
  theme_classic()

p6_all <- ggplot(dat, aes(x = DOY, y = Annual_Precipitation)) +
  geom_point(aes(fill = Region), size = 3, pch = 21) +
  stat_poly_line(formula = y ~ x, aes(group = 1), color = "black") +
  stat_poly_eq(formula = y ~ x, aes(group = 1)) +
  scale_x_continuous(breaks = seq(0, 365, by = 25)) +
  scale_fill_manual(values = region_colors) +
  labs(x = "DOY", y = "Annual Precipitation") +
  theme_classic()

# ── Combine ────────────────────────────────────────────────────
combined_all <- (p1_all | p2_all) / (p3_all | p4_all) / (p5_all | p6_all) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14))

print(combined_all)

ggsave("May17_2026_AllWolves_noIndian_panel.pdf",
       plot = combined_all, width = 10, height = 12, dpi = 300)
