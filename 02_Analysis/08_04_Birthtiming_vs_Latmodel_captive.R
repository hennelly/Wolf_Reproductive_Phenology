Captive <- read.csv("2026_May15_plotting_captivevswild_lat.csv", header=TRUE)

# Final - Figure 3c captive vs wild wolf box plot 
dat <- subset(Captive, Group=="captive")
 
library(nls2)
 
lat_seq <- seq(min(dat$Lat), max(dat$Lat), length.out = 300)
 
# 1. LINEAR MODEL
fit_lm <- lm(DOY ~ Lat, data = dat)
 
# 2. ASYMPTOTIC MODEL:  DOY = a - b * exp(-c * Lat)
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

#Formula: DOY ~ a - b * exp(-c * Lat)

#Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#a 2.280e+02  1.272e+01  17.926  < 2e-16 ***
#b 1.706e+02  1.102e+01  15.490  < 2e-16 ***
#c 2.892e-02  5.836e-03   4.956 9.15e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 33.36 on 665 degrees of freedom

#Number of iterations to convergence: 16 
#Achieved convergence tolerance: 7.886e-06

# 3. LOGISTIC MODEL 
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
 
# 5. MODEL SUMMARIES
cat("\n===== LINEAR =====\n");         print(summary(fit_lm))
#===== LINEAR =====

#Call:
#lm(formula = DOY ~ Lat, data = dat)#

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-184.18  -10.85    0.12   12.78  193.90 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 100.6979     5.5317   18.20   <2e-16 ***
#Lat           1.6796     0.1079   15.57   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 34.3 on 666 degrees of freedom
#Multiple R-squared:  0.2669,	Adjusted R-squared:  0.2658 
#F-statistic: 242.5 on 1 and 666 DF,  p-value: < 2.2e-16
cat("\n===== ASYMPTOTIC =====\n");     print(summary(fit_asy))
#Formula: DOY ~ a - b * exp(-c * Lat)

#Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#a 2.280e+02  1.272e+01  17.926  < 2e-16 ***
#b 1.706e+02  1.102e+01  15.490  < 2e-16 ***
#c 2.892e-02  5.836e-03   4.956 9.15e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 33.36 on 665 degrees of freedom

#Number of iterations to convergence: 16 
#Achieved convergence tolerance: 7.886e-06

cat("\n===== LOGISTIC (original) =====\n"); print(summary(fit_log1))
#Formula: DOY ~ a/(1 + exp(-k * (Lat - x0)))

#Parameters:
#    Estimate Std. Error t value Pr(>|t|)    
#a  2.017e+02  3.468e+00  58.167   <2e-16 ***
#k  8.083e-02  8.513e-03   9.495   <2e-16 ***
#x0 1.537e+01  1.295e+00  11.868   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#

#Residual standard error: 32.62 on 665 degrees of freedom

#Number of iterations to convergence: 14 
#Achieved convergence tolerance: 4.467e-06

# 6. MODEL COMPARISON TABLE
 
# Pseudo-R² 
r2_nls <- function(fit, data) {
  ss_res <- sum(residuals(fit)^2)
  ss_tot <- sum((data$DOY - mean(data$DOY))^2)
  round(1 - ss_res / ss_tot, 4)
}
 
cat("\n===== AIC / BIC COMPARISON =====\n")
print(AIC(fit_lm, fit_asy, fit_log1))
#         df      AIC
#fit_lm    3 6622.500
#fit_asy   4 6586.568
#fit_log1  4 6556.624

print(BIC(fit_lm, fit_asy, fit_log1))
#          df      BIC
#fit_lm    3 6636.013
#fit_asy   4 6604.585
#fit_log1  4 6574.641

cat("\n===== PSEUDO-R² =====\n")
cat("Linear:             ", round(summary(fit_lm)$r.squared, 4), "\n")
#Linear:              0.2669 

cat("Asymptotic:         ", r2_nls(fit_asy,  dat), "\n")
#Asymptotic:          0.3074 

cat("Logistic (original):", r2_nls(fit_log1, dat), "\n")
#Logistic (original): 0.3378 

cat("\n===== RESIDUAL STD ERROR =====\n")
cat("Linear:             ", round(sigma(fit_lm),   2), "\n")
#Linear:              34.3 
cat("Asymptotic:         ", round(sigma(fit_asy),  2), "\n")
#Asymptotic:          33.36 
cat("Logistic (original):", round(sigma(fit_log1), 2), "\n")
#Logistic (original): 32.62 


