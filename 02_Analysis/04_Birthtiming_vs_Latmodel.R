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

#Formula: DOY ~ a - b * exp(-c * Lat)#

#Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#a 186.51764    1.51809 122.864   <2e-16 ***
#b 599.56811   60.87432   9.849   <2e-16 ***
#c   0.08861    0.00586  15.120   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1#
#
##Residual standard error: 17.36 on 570 degrees of freedom
#
#Number of iterations to convergence: 10 
#Achieved convergence tolerance: 5.797e-06


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
##    Min      1Q  Median      3Q     Max 
#-92.408  -9.114   0.527  10.054  78.089 #

##Coefficients:
  #          Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 73.50533    3.85169   19.08   <2e-16 ***
#Lat          1.91314    0.07371   25.95   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 23.95 on 571 degrees of freedom
##Multiple R-squared:  0.5412,	Adjusted R-squared:  0.5404 
#F-statistic: 673.6 on 1 and 571 DF,  p-value: < 2.2e-16

 print(summary(fit_asy))
#Formula: DOY ~ a - b * exp(-c * Lat)#

#Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#a 186.51764    1.51809 122.864   <2e-16 ***
#b 599.56811   60.87432   9.849   <2e-16 ***
#c   0.08861    0.00586  15.120   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 17.36 on 570 degrees of freedom

#Number of iterations to convergence: 10 
#Achieved convergence tolerance: 5.797e-06

print(summary(fit_log1))


#Formula: DOY ~ a/(1 + exp(-k * (Lat - x0)))##

#Parameters:
#    Estimate Std. Error t value Pr(>|t|)    
#a  181.85312    1.07282  169.51   <2e-16 ***
#k    0.16086    0.01085   14.83   <2e-16 ***
#x0  21.55495    0.40213   53.60   <2e-16 ***
---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 17.8 on 570 degrees of freedom

#Number of iterations to convergence: 27 
#Achieved convergence tolerance: 8.626e-06

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
#fit_lm    3 5269.634
#fit_asy   4 4902.052
#fit_log1  4 4930.882

print(BIC(fit_lm, fit_asy, fit_log1))
#         df      BIC
#fit_lm    3 5282.687
#fit_asy   4 4919.455
#fit_log1  4 4948.285

 
PSEUDO-R²
cat("Linear:             ", round(summary(fit_lm)$r.squared, 4), "\n")
cat("Asymptotic:         ", r2_nls(fit_asy,  dat), "\n")
cat("Logistic (original):", r2_nls(fit_log1, dat), "\n")

cat("\n===== RESIDUAL STD ERROR =====\n")
cat("Linear:             ", round(sigma(fit_lm),   2), "\n")
#Linear:              0.5412 
cat("Asymptotic:         ", round(sigma(fit_asy),  2), "\n")
#Asymptotic:          0.7593 
cat("Logistic (original):", round(sigma(fit_log1), 2), "\n")
#Logistic (original): 0.7469 


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


