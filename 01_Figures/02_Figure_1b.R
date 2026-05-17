## Code for Figure 2

dat <- read.csv ("2026_May15_Published_unpublished_DOY_vs_latitude_FINALIZED.csv", header=TRUE)

# Full Lat sequence across your data range
lat_seq  <- seq(min(dat$Lat), max(dat$Lat), length.out = 300)

# Predict DOY from Lat using asymptotic model (natural direction)
doy_pred <- coef(fit_asy)["a"] - coef(fit_asy)["b"] * exp(-coef(fit_asy)["c"] * lat_seq)

# Predict DOY from Lat using asymptotic model (natural direction)
doy_pred <- coef(fit_asy)["a"] - coef(fit_asy)["b"] * exp(-coef(fit_asy)["c"] * lat_seq)

curve_df <- data.frame(
  DOY = doy_pred,
  Lat = lat_seq
)

#Based on your figure, here are the axis adjustments needed — x-axis as dates and y-axis from 10 to 80:
#r# Date labels matching your figure
doy_breaks <- c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250)

## Plot Figure 
plot <- ggplot(dat, aes(x = DOY, y = Lat, fill = Region2)) + 
  geom_point(size = 3, shape = dat$unpublished.vs.published2) + 
  geom_errorbar(aes(ymin = Lat_lowerrange, ymax = Lat_upperrange, color = Region2)) +
  geom_errorbarh(aes(xmin = DOY_lowerrange, xmax = DOY_upperrange, color = Region2)) +
  geom_line(data = curve_df, aes(x = DOY, y = Lat),
            inherit.aes = FALSE,
            color = "black", linewidth = 1) +
  scale_x_continuous(
    breaks = seq(25, 250, by = 25)
  ) +
  scale_y_continuous(
    breaks = seq(10, 80, by = 10),
    limits = c(10, 85)
  ) +
  labs(x = "estimated timing of birth", y = "Latitude") +
  theme_classic() +
  scale_fill_manual(values = c("purple", "olivedrab3", "forestgreen", 
                                "orangered", "darkgoldenrod1", "#56B4E9", "gray", "gray")) +
  scale_color_manual(values = c("purple", "olivedrab3", "forestgreen", 
                                 "orangered", "darkgoldenrod1", "#56B4E9", "gray", "gray"))
plot

 ggsave("birth_timing_plot_Figure1b_May15_2026.pdf", plot = plot, width = 7, height = 4, dpi = 300)

