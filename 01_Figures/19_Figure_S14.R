library(nls2)

Captive <- read.csv("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)

# Final - Figure 3c captive vs wild wolf box plot 
dat <- subset(Captive, Group=="captive")

lat_seq <- seq(min(dat$Lat), max(dat$Lat), length.out = 300)

#linear model 
fit_lm <- lm(DOY ~ Lat, data = dat)

#ASYMPTOTIC model 

start_grid_asy <- expand.grid(
  a = c(220, 230, 240, 250),
  b = c(150, 180, 200, 220),
  c = c(0.005, 0.01, 0.03, 0.05)
)
 
fit_init_asy <- nls2(
  DOY ~ a - b * exp(-c * Lat),
  data      = dat,
  start     = start_grid_asy,
  algorithm = "brute-force"
)
 
fit_asy <- nls2(
  DOY ~ a - b * exp(-c * Lat),
  data      = dat,
  start     = coef(fit_init_asy),
  algorithm = "default"
)
 
summary(fit_asy)

# logistic model
start_grid_log1 <- expand.grid(
    a  = seq(150, 300, by = 10),
    k  = seq(0.05, 1.5, by = 0.05),
    x0 = seq(40, 70, by = 2)
)

fit_init_log1 <- nls2(
    DOY ~ a / (1 + exp(-k * (Lat - x0))),
    data      = dat,
    start     = start_grid_log1,
    algorithm = "brute-force"
)

fit_log1 <- nls2(
    DOY ~ a / (1 + exp(-k * (Lat - x0))),
    data      = dat,
    start     = coef(fit_init_log1),
    algorithm = "default"
)

# model summary
print(summary(fit_lm))
print(summary(fit_asy))
print(summary(fit_log1))

# Pseudo-R²
r2_nls <- function(fit, data) {
  ss_res <- sum(residuals(fit)^2)
  ss_tot <- sum((data$DOY - mean(data$DOY))^2)
  round(1 - ss_res / ss_tot, 4)
}

# AIC BIC compare
print(AIC(fit_lm, fit_asy, fit_log1))
print(BIC(fit_lm, fit_asy, fit_log1))

# Psuedo R 2
round(summary(fit_lm)$r.squared, 4)
r2_nls(fit_asy,  dat)
r2_nls(fit_log1, dat)

#STD error 
round(sigma(fit_lm),   2)
round(sigma(fit_asy),  2)
round(sigma(fit_log1), 2)

#LInear summary
cat("  RSE:  ", round(summary(fit_lm)$sigma, 3), "\n")
cat("  df:   ", summary(fit_lm)$df[2], "\n")
cat("  p:    ", pf(summary(fit_lm)$fstatistic[1],
                   summary(fit_lm)$fstatistic[2],
                   summary(fit_lm)$fstatistic[3],
                   lower.tail = FALSE), "\n")

#Asympoptic summary
cat("  RSE:  ", round(summary(fit_asy)$sigma, 3), "\n")
cat("  df:   ", summary(fit_asy)$df[2], "\n")
cat("  p:    ", summary(fit_asy)$parameters[, "Pr(>|t|)"], "\n")

# Logistic summary
cat("  RSE:  ", round(summary(fit_log1)$sigma, 3), "\n")
cat("  df:   ", summary(fit_log1)$df[2], "\n")
cat("  p:    ", summary(fit_log1)$parameters[, "Pr(>|t|)"], "\n")


#Figures
png("birth_timing_model_comparison_captive.png", width = 1400, height = 1000, res = 140)

cols <- c(linear    = "darkgreen",
          asymptote = "steelblue",
          log1      = "firebrick")

# ── Panel 1: All fits overlaid ───────────────────────────────
plot(dat$Lat, dat$DOY, pch = 16, col = "grey60", cex = 0.7,
     xlab = "Latitude (°N)", ylab = "DOY of birth",
     main = "All models")

lines(lat_seq, predict(fit_lm,   newdata = data.frame(Lat = lat_seq)),
      col = cols["linear"],    lwd = 2, lty = 3)
lines(lat_seq, predict(fit_asy,  newdata = data.frame(Lat = lat_seq)),
      col = cols["asymptote"], lwd = 2, lty = 2)
lines(lat_seq, predict(fit_log1, newdata = data.frame(Lat = lat_seq)),
      col = cols["log1"],      lwd = 2)

legend("bottomright",
       legend = c("Linear", "Asymptotic", "Logistic"),
       col    = unname(cols), lwd = 2, lty = c(3, 2, 1),
       cex    = 0.8, bty = "n")





