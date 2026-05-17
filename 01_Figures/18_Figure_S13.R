library(ggplot2)

# ── Load and subset ────────────────────────────────────────────
dat <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header = TRUE)

dat <- subset(dat, Region2 %in% c("Canis lupus lupus", "Canis lupus signatus"))

# ── Plot ───────────────────────────────────────────────────────
plot <- ggplot(dat, aes(x = Lat, y = DOY, color = Region2)) +
  geom_point(size = 3, aes(fill = Region2), colour = "black", pch = 21) +
  geom_smooth(method = lm) +
  theme_classic() +
  scale_color_manual(values = c("Canis lupus lupus"    = "olivedrab3",
                                "Canis lupus signatus" = "purple")) +
  scale_fill_manual(values  = c("Canis lupus lupus"    = "olivedrab3",
                                "Canis lupus signatus" = "purple")) +
  scale_y_continuous(breaks = seq(0, 365, by = 25)) +
  scale_x_continuous(breaks = seq(0, 55, by = 5))

print(plot)

ggsave("May17_2026_Captive_lupus_vs_signatus.pdf", width = 7, height = 5)
