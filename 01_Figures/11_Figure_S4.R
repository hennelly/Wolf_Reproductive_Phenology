library(nls2)

wild <- read.csv ("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)
dat <- subset(wild, Group=="wild ")

## Comparing models: linear, asymptotic, logistic
lat_seq <- seq(min(dat$Lat), max(dat$Lat), length.out = 300)

#linear model
fit_lm <- lm(DOY ~ Lat, data = dat)

#asymptotic model
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

#Logistic
start_grid_log1 <- expand.grid(
  a  = c(200, 220, 240),
  k  = c(0.1, 0.2, 0.5, 1.0),
  x0 = c(50, 55, 60, 65)
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

#Model summary
print(summary(fit_lm))
print(summary(fit_asy))
print(summary(fit_log1))


# Pseudo-R²
r2_nls <- function(fit, data) {
  ss_res <- sum(residuals(fit)^2)
  ss_tot <- sum((data$DOY - mean(data$DOY))^2)
  round(1 - ss_res / ss_tot, 4)
}

# AIC/BIC Comparison
print(AIC(fit_lm, fit_asy, fit_log1))
print(BIC(fit_lm, fit_asy, fit_log1))

# Pseudo-R²
round(summary(fit_lm)$r.squared, 4)
r2_nls(fit_asy,  dat)
r2_nls(fit_log1, dat)

#STD error
round(sigma(fit_lm),   2)
round(sigma(fit_asy),  2)
round(sigma(fit_log1), 2)

#Linear RSE, df, p
cat("  RSE:  ", round(summary(fit_lm)$sigma, 3), "\n")
cat("  df:   ", summary(fit_lm)$df[2], "\n")
cat("  p:    ", pf(summary(fit_lm)$fstatistic[1],
                   summary(fit_lm)$fstatistic[2],
                   summary(fit_lm)$fstatistic[3],
                   lower.tail = FALSE), "\n")

#Asymptotic RSE, df, p
cat("  RSE:  ", round(summary(fit_asy)$sigma, 3), "\n")
cat("  df:   ", summary(fit_asy)$df[2], "\n")
cat("  p:    ", summary(fit_asy)$parameters[, "Pr(>|t|)"], "\n")

#Logistic RSE, df, p
cat("  RSE:  ", round(summary(fit_log1)$sigma, 3), "\n")
cat("  df:   ", summary(fit_log1)$df[2], "\n")
cat("  p:    ", summary(fit_log1)$parameters[, "Pr(>|t|)"], "\n")


pred_lm   <- data.frame(Lat = lat_seq, DOY = predict(fit_lm,   newdata = data.frame(Lat = lat_seq)))
pred_asy  <- data.frame(Lat = lat_seq, DOY = predict(fit_asy,  newdata = data.frame(Lat = lat_seq)))
pred_log1 <- data.frame(Lat = lat_seq, DOY = predict(fit_log1, newdata = data.frame(Lat = lat_seq)))

#Plot 
plot(dat$Lat, dat$DOY,
     pch  = 16,
     col  = "gray60",
     cex  = 0.8,
     xlab = "Latitude (°N)",
     ylab = "DOY of birth",
     xlim = c(15, 82),
     ylim = c(0, 230),
     las  = 1)

# Linear — green dashed
lines(pred_lm$Lat,   pred_lm$DOY,   col = "darkgreen", lty = 2, lwd = 2)

# Asymptotic — blue dashed
lines(pred_asy$Lat,  pred_asy$DOY,  col = "steelblue",  lty = 2, lwd = 2)

# Logistic — red solid
lines(pred_log1$Lat, pred_log1$DOY, col = "red",        lty = 1, lwd = 2)

# ── Legend ────────────────────────────────────────────────────
legend("bottomright",
       legend = c("Linear", "Asymptotic", "Logistic"),
       col    = c("darkgreen", "steelblue", "red"),
       lty    = c(2, 2, 1),
       lwd    = 2,
       bty    = "n")


