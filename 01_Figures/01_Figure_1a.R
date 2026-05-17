library(ggplot2)
library(maps)
library(mapdata)
library(rworldmap)
library(dplyr)
library(patchwork)

# ── Shared color scale ─────────────────────────────────────────
region_colors <- c("Europe"         = "olivedrab3",
                   "Iberian"        = "forestgreen",
                   "Indian"         = "orangered",
                   "Southwest Asia" = "darkgoldenrod1",
                   "North America"  = "#56B4E9",
                   "Tibetan"        = "gray",
                   "Asia"           = "purple")

# ── Shared DOY breaks ──────────────────────────────────────────
doy_breaks <- c(25, 50, 75, 100, 125, 150, 175, 200, 225)
doy_labels <- c("11/25", "12/20", "1/14", "2/8", "3/5", "3/30", "4/24", "5/19", "6/13")

# ── Shared axis theme ──────────────────────────────────────────
axis_theme <- theme(axis.text  = element_text(size = 13),
                    axis.title = element_text(size = 15))

# ============================================================
# PANEL A — World map
# ============================================================
dat_map <- read.csv("cleaned_wolf_parturition_May15_final_2026.csv", header = TRUE)

world <- map_data('world')

map_plot <- ggplot(world, aes(lat, long)) +
  geom_map(map = world, aes(map_id = region), fill = "gray95", color = "darkgray") +
  coord_quickmap(xlim = c(-171, 100), ylim = c(10, 85)) +
  geom_point(data = dat_map,
             aes(x = Long, y = Lat, fill = DOY),
             alpha = 1, size = 2.5, shape = 21) +
  scale_fill_gradient2(low = "white", mid = "yellow", high = "blue",
                       midpoint = 125, na.value = NA) +
  scale_y_continuous(breaks = seq(10, 90, by = 10)) +
  labs(x = NULL, y = "Latitude") +
  theme_classic() +
  axis_theme +
  theme(axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())

# ============================================================
# PANEL B — DOY vs Latitude with asymptotic curve
# ============================================================
dat_scatter <- read.csv("2026_May15_Published_unpublished_DOY_vs_latitude_Dec42025_FINAL2.csv",
                         header = TRUE)

dat_scatter$Region2 <- recode(dat_scatter$Region2,
                               "Middle East"  = "Southwest Asia",
                               "NorthAmerica" = "North America")

lat_seq  <- seq(min(dat_scatter$Lat), max(dat_scatter$Lat), length.out = 300)
doy_pred <- coef(fit_asy)["a"] - coef(fit_asy)["b"] * exp(-coef(fit_asy)["c"] * lat_seq)
curve_df <- data.frame(DOY = doy_pred, Lat = lat_seq)

scatter_plot <- ggplot(dat_scatter, aes(x = DOY, y = Lat, fill = Region2)) +
  geom_point(size = 3, shape = dat_scatter$unpublished.vs.published2) +
  geom_errorbar(aes(ymin = Lat_lowerrange, ymax = Lat_upperrange, color = Region2)) +
  geom_errorbarh(aes(xmin = DOY_lowerrange, xmax = DOY_upperrange, color = Region2)) +
  geom_line(data = curve_df, aes(x = DOY, y = Lat),
            inherit.aes = FALSE, color = "black", linewidth = 1) +
  scale_x_continuous(breaks = doy_breaks, labels = doy_labels) +
  scale_y_continuous(breaks = seq(10, 80, by = 10), limits = c(10, 85)) +
  scale_fill_manual(values  = region_colors) +
  scale_color_manual(values = region_colors) +
  labs(x = "Estimated timing of birth", y = "Latitude") +
  theme_classic() +
  axis_theme +
  theme(axis.text.x     = element_text(angle = 45, hjust = 1, size = 13),
        legend.position = "none")

# ============================================================
# PANEL C — Boxplot by latitudinal range
# ============================================================
dat_box <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header = TRUE)

Wild <- subset(dat_box, Group == "wild ")

Wild$Region2 <- recode(Wild$Region2,
                        "Middle East"  = "Southwest Asia",
                        "NorthAmerica" = "North America")

box_plot <- Wild %>%
  ggplot(aes(x = LatGrouping_by10, y = DOY)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 1, dodge.width = 0),
             pch = 21, size = 2.5,
             aes(fill = factor(Region2))) +
  scale_fill_manual(values = region_colors) +
  scale_y_continuous(breaks = doy_breaks, labels = doy_labels) +
  scale_x_discrete(labels = c("10-20", "20-30", "30-40",
                               "40-50", "50-60", "60-70",
                               "70-80", "80-90")) +
  labs(x = "Latitudinal range", y = "Estimated timing of birth",
       fill = NULL) +
  theme_classic() +
  axis_theme +
  theme(legend.text  = element_text(size = 11),
        legend.title = element_blank())

# ============================================================
# COMBINE
# ============================================================
combined <- map_plot / (scatter_plot | box_plot) +
  plot_layout(heights = c(1.2, 1)) &
  theme(legend.text  = element_text(size = 11),
        legend.title = element_text(size = 12))

print(combined)

ggsave("May17_2026_Figure1_panel.pdf",
       plot = combined, width = 14, height = 10, dpi = 300)



