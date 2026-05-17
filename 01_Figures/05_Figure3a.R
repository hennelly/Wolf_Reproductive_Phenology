library(ggplot2)
library(ggpmisc)
library(nls2)
library(minpack.lm)
library(dplyr)
library(patchwork)

# ============================================================
# DATA
# ============================================================
dat <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header = TRUE)
dat$Group <- trimws(dat$Group)

captive_raw   <- dat
captive_clean <- subset(dat, Lat > 0)
dat_wild      <- subset(dat, Group == "wild")
dat_captive   <- subset(dat, Group == "captive")

# ── DOY breaks and labels ─────────────────────────────────────
doy_breaks <- c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 325, 350)
doy_labels <- c("11/25", "12/20", "1/14", "2/8", "3/5", "3/30", "4/24",
                "5/19", "6/13", "7/8", "8/2", "8/27", "9/21", "10/16")

# ── Axis theme ─────────────────────────────────────────────────
axis_theme <- theme(axis.text  = element_text(size = 13),
                    axis.title = element_text(size = 15))

# ============================================================
# MODELS
# ============================================================
fit_init_asy <- nls2(
  DOY ~ a - b * exp(-c * Lat),
  data      = dat_wild,
  start     = expand.grid(
    a = seq(180, 280, by = 10),
    b = seq(100, 250, by = 10),
    c = seq(0.001, 0.1, by = 0.005)
  ),
  algorithm = "brute-force"
)

fit_asy <- nlsLM(
  formula = as.formula("DOY ~ a - b * exp(-c * Lat)"),
  data    = dat_wild,
  start   = as.list(coef(fit_init_asy)),
  control = nls.lm.control(maxiter = 200)
)

fit_init_log1 <- nls2(
  DOY ~ a / (1 + exp(-k * (Lat - x0))),
  data      = dat_captive,
  start     = expand.grid(
    a  = seq(150, 300, by = 10),
    k  = seq(0.05, 1.5, by = 0.05),
    x0 = seq(40, 70, by = 2)
  ),
  algorithm = "brute-force"
)

fit_log1 <- nlsLM(
  formula = as.formula("DOY ~ a / (1 + exp(-k * (Lat - x0)))"),
  data    = dat_captive,
  start   = as.list(coef(fit_init_log1)),
  control = nls.lm.control(maxiter = 200)
)

r2_nls <- function(fit, dat) {
  ss_res <- sum(residuals(fit)^2)
  ss_tot <- sum((dat$DOY - mean(dat$DOY))^2)
  round(1 - ss_res / ss_tot, 2)
}

r2_wild    <- r2_nls(fit_asy,  dat_wild)
r2_captive <- r2_nls(fit_log1, dat_captive)

# ── Prediction curves ─────────────────────────────────────────
lat_seq_wild    <- seq(min(dat_wild$Lat),    max(dat_wild$Lat),    length.out = 300)
lat_seq_captive <- seq(min(dat_captive$Lat), max(dat_captive$Lat), length.out = 300)

curve_wild <- data.frame(
  DOY   = coef(fit_asy)["a"] - coef(fit_asy)["b"] * exp(-coef(fit_asy)["c"] * lat_seq_wild),
  Lat   = lat_seq_wild,
  Group = "wild"
)

curve_captive <- data.frame(
  DOY   = coef(fit_log1)["a"] / (1 + exp(-coef(fit_log1)["k"] * (lat_seq_captive - coef(fit_log1)["x0"]))),
  Lat   = lat_seq_captive,
  Group = "captive"
)

curve_df2 <- rbind(curve_wild, curve_captive)

# ============================================================
# PANEL A — Captive boxplot by latitudinal range
# ============================================================
Captive <- subset(dat, Group == "captive")

panel_A <- Captive %>%
  ggplot(aes(x = LatGrouping_by10, y = DOY)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 2, dodge.width = 0),
             pch = 21, size = 2.5, aes(fill = factor(Region2))) +
  scale_fill_manual(values = c("darkgoldenrod1", "gray", "olivedrab3",
                               "orangered", "#56B4E9")) +
  scale_y_continuous(breaks = doy_breaks, labels = doy_labels, limits = c(0, 350)) +
  scale_x_discrete(labels = c("1-10", "10-20", "20-30", "30-40",
                               "40-50", "50-60", "60-70")) +
  labs(x = "Latitudinal range", y = "Estimated timing of birth",
       fill = NULL) +
  theme_classic() +
  axis_theme +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 11),
        legend.text  = element_text(size = 11),
        legend.title = element_blank())

# ============================================================
# PANEL B — Scatter plot captive vs wild with curves
# ============================================================
panel_B <- ggplot(captive_clean, aes(x = DOY, y = Lat, fill = Group)) +
  geom_point(size = 3, colour = "black", pch = 21) +
  geom_line(data = curve_df2, aes(x = DOY, y = Lat, color = Group),
            linewidth = 1.2, inherit.aes = FALSE) +
  scale_y_continuous(breaks = seq(10, 80, by = 10)) +
  scale_x_continuous(breaks = doy_breaks, labels = doy_labels) +
  scale_fill_manual(values  = c("captive" = "lavender", "wild" = "peachpuff")) +
  scale_color_manual(values = c("captive" = "slateblue3", "wild" = "sienna3")) +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 85)) +
  annotate("text", x = 5, y = 85,
           label = paste0("R² = ", r2_captive, " (captive, logistic)"),
           color = "slateblue3", hjust = 0, size = 4) +
  annotate("text", x = 5, y = 79,
           label = paste0("R² = ", r2_wild, " (wild, asymptotic)"),
           color = "sienna3", hjust = 0, size = 4) +
  labs(x = "Estimated timing of birth", y = "Latitude") +
  theme_classic() +
  axis_theme +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 11),
        legend.text  = element_text(size = 11),
        legend.title = element_blank())

# ============================================================
# PANEL C — Captive vs wild boxplot by latitudinal range
# ============================================================
panel_C <- ggplot(dat, aes(x = LatGrouping_by10, y = DOY,
                            colour = Group, fill = Group)) +
  geom_boxplot(position = position_dodge(width = 1), fill = NA, lwd = 1) +
  scale_color_manual(values = c("captive" = "plum3", "wild" = "lightsalmon3")) +
  scale_y_continuous(breaks = doy_breaks, labels = doy_labels, limits = c(0, 350)) +
  scale_x_discrete(labels = c("1-10", "10-20", "20-30", "30-40",
                               "40-50", "50-60", "60-70", "70-80", "80-90")) +
  labs(x = "Latitudinal range", y = "Estimated timing of birth") +
  theme_classic() +
  axis_theme +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1),
        axis.text.y  = element_text(size = 11),
        legend.text  = element_text(size = 11),
        legend.title = element_blank())

# ============================================================
# COMBINE
# ============================================================
combined <- panel_A / panel_B / panel_C +
  plot_layout(heights = c(1, 1, 1))

print(combined)

ggsave("May17_2026_Figure3_panel.pdf",
       plot = combined, width = 10, height = 16, dpi = 300)




# Figure 3a - Captive wolf boxplot 

library (dplyr)
library(ggplot2)

dat <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)

# Final - Figure 3c captive vs wild wolf box plot 
Captive <-  subset(dat, Group=="captive")

p1=Captive %>% ggplot(aes(x=LatGrouping_by10, y=DOY))+  
  geom_boxplot()+
  geom_point(position=position_jitterdodge(jitter.width=2, dodge.width = 0), 
             pch=21, size=2.5, aes(fill=factor(Region2)))
p2 <- p1+scale_fill_manual(values=c("darkgoldenrod1", "gray", "olivedrab3", "orangered", "#56B4E9" )) + theme_classic()


p2 + scale_y_continuous(breaks = round(seq(min(25), max(250), by = 25),0)) 
p2

ggsave("May15_Figure1c.pdf", width=10,height=4)

