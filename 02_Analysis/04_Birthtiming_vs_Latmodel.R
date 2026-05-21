library(nls2)

wild <- read.csv ("2026_May15_final_plotting_captivevswild_lat.csv", header=TRUE)
dat <- subset(wild, Group=="wild ")



lat_seq <- seq(min(dat$Lat), max(dat$Lat), length.out = 300)
 
# ============================================================
# 1. LINEAR MODEL
# ============================================================
fit_lm <- lm(DOY ~ Lat, data = dat)
 
# ============================================================
# 2. ASYMPTOTIC MODEL:  DOY = a - b * exp(-c * Lat)
# ============================================================
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
#a 1.863e+02  1.428e+00 130.400   <2e-16 ***
#b 6.227e+02  6.330e+01   9.837   <2e-16 ***
#c 9.080e-02  5.859e-03  15.499   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 16.76 on 569 degrees of freedom

#Number of iterations to convergence: 10 
#Achieved convergence tolerance: 6.054e-06

# ============================================================
# 3. LOGISTIC MODEL
# ============================================================
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
 
print(summary(fit_lm))

#Call:
#lm(formula = DOY ~ Lat, data = dat)

#Residuals:
#    Min      1Q  Median      3Q     Max 
#-86.991  -9.247   0.389   9.938  77.839 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 73.98254    3.80631   19.44   <2e-16 ***
#Lat          1.90689    0.07282   26.18   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 23.65 on 570 degrees of freedom
#Multiple R-squared:  0.5461,	Adjusted R-squared:  0.5453 
#F-statistic: 685.7 on 1 and 570 DF,  p-value: < 2.2e-16

 print(summary(fit_asy))
#Formula: DOY ~ a - b * exp(-c * Lat)
#
#Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#a 1.863e+02  1.428e+00 130.400   <2e-16 ***
#b 6.227e+02  6.330e+01   9.837   <2e-16 ***
# 9.080e-02  5.859e-03  15.499   <2e-16 ***
#--
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 16.76 on 569 degrees of freedom

##Number of iterations to convergence: 10 
#Achieved convergence tolerance: 6.054e-06

print(summary(fit_log1))


#Formula: DOY ~ a/(1 + exp(-k * (Lat - x0)))

#Parameters:
#    Estimate Std. Error t value Pr(>|t|)    
#a  181.57049    0.99969  181.63   <2e-16 ***
#k    0.16911    0.01175   14.40   <2e-16 ***
#0  21.43195    0.37974   56.44   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 17.18 on 569 degrees of freedom

#Number of iterations to convergence: 29 
#Achieved convergence tolerance: 7.144e-06

# ============================================================
# 6. MODEL COMPARISON TABLE
# ============================================================
 
# Pseudo-R² 
r2_nls <- function(fit, data) {
  ss_res <- sum(residuals(fit)^2)
  ss_tot <- sum((data$DOY - mean(data$DOY))^2)
  round(1 - ss_res / ss_tot, 4)
}
 
cat("\n===== AIC / BIC COMPARISON =====\n")
print(AIC(fit_lm, fit_asy, fit_log1))
#         df      AIC
#fit_lm    3 5246.299
#fit_asy   4 4852.848
#fit_log1  4 4881.458

print(BIC(fit_lm, fit_asy, fit_log1))
#         df      BIC
#         df      BIC
#fit_lm    3 5259.346
#fit_asy   4 4870.245
#fit_log1  4 4898.855

 
PSEUDO-R²
cat("Linear:             ", round(summary(fit_lm)$r.squared, 4), "\n")
cat("Asymptotic:         ", r2_nls(fit_asy,  dat), "\n")
cat("Logistic (original):", r2_nls(fit_log1, dat), "\n")

cat("\n===== RESIDUAL STD ERROR =====\n")
cat("Linear:             ", round(sigma(fit_lm),   2), "\n")
#Linear:              0.5461 
cat("Asymptotic:         ", round(sigma(fit_asy),  2), "\n")
#Asymptotic:          0.7726 
cat("Logistic (original):", round(sigma(fit_log1), 2), "\n")
#Logistic (original): 0.761 
#

cat("===== RSE / DF / P-VALUES =====\n")

cat("\nLinear:\n")
cat("  RSE:  ", round(summary(fit_lm)$sigma, 3), "\n")
 # RSE:   23.947 

cat("  df:   ", summary(fit_lm)$df[2], "\n")
#  df:    571 
cat("  p:    ", pf(summary(fit_lm)$fstatistic[1],
                   summary(fit_lm)$fstatistic[2],
                   summary(fit_lm)$fstatistic[3],
                   lower.tail = FALSE), "\n")
 # p:     1.097637e-98 

cat("\nAsymptotic:\n")
cat("  RSE:  ", round(summary(fit_asy)$sigma, 3), "\n")
#RSE:   17.361
cat("  df:   ", summary(fit_asy)$df[2], "\n")
#  df:    570 
cat("  p:    ", summary(fit_asy)$parameters[, "Pr(>|t|)"], "\n")
#  p:     0 3.055317e-21 1.131208e-43 

cat("\nLogistic:\n")
cat("  RSE:  ", round(summary(fit_log1)$sigma, 3), "\n")
#  RSE:   17.803 

cat("  df:   ", summary(fit_log1)$df[2], "\n")
#  df:    570 

cat("  p:    ", summary(fit_log1)$parameters[, "Pr(>|t|)"], "\n")
#  p:     0 2.631531e-42 8.972063e-225 


