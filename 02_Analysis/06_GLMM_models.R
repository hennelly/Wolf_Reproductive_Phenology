library(lme4)      
library(MuMIn)     

dat <- read.csv ("cleaned_wolf_parturition_May15_final_2026.csv", header=TRUE)

#scale
dat$PC1_z <- scale(dat$PC1)
dat$daylength_z <- scale(dat$daylength_original_matingdate)
dat$precip_z <- scale(dat$Annual_Precipitation)
dat$seasonality_z <- scale(dat$Precipitation_Seasonality)
dat$ndvi_z <- scale(dat$mean_ndvi)

model_z <- lmer(DOY ~ PC1_z + daylength_z + precip_z + seasonality_z + ndvi_z +  (1|Region), data = dat, na.action = na.fail)
summary(model_z)
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: DOY ~ PC1_z + daylength_z + precip_z + seasonality_z + ndvi_z +      (1 | Region)
#   Data: dat

#REML criterion at convergence: 4369.8

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-4.4011 -0.3251  0.0107  0.4460  8.3853 

#Random effects:
# Groups   Name        Variance Std.Dev.
# Region   (Intercept) 2524.7   50.25   
# Residual              118.8   10.90   
#Number of obs: 572, groups:  Region, 5

#Fixed effects:
#              Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)   159.6100    22.5905   4.0263   7.065  0.00207 ** 
#PC1_z          10.9944     1.7222 565.0532   6.384 3.60e-10 ***
#daylength_z     9.8180     0.5083 562.1349  19.314  < 2e-16 ***
#precip_z        3.5701     0.6814 563.6173   5.239 2.28e-07 ***
#seasonality_z   6.0244     0.8643 565.9930   6.970 8.85e-12 ***
#ndvi_z         -1.0721     0.8334 564.2501  -1.286  0.19885    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) PC1_z  dylng_ prcp_z ssnlt_
#PC1_z        0.080                            
#daylength_z -0.010  0.003                     
#precip_z     0.035  0.438 -0.108              
#seasonlty_z -0.065 -0.697 -0.064 -0.034       
#ndvi_z       0.025  0.296  0.264 -0.306 -0.424

AIC(model_z)
# [1] 4385.763


model_set <- dredge(model_z)
head(model_set)
best_model <- get.models(model_set, 1)[[1]]
summary(best_model)
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: DOY ~ daylength_z + ndvi_z + PC1_z + precip_z + seasonality_z +      (1 | Region)
#   Data: dat#

#REML criterion at convergence: 4369.8

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-4.4011 -0.3251  0.0107  0.4460  8.3853 

#Random effects:
# Groups   Name        Variance Std.Dev.
# Region   (Intercept) 2524.7   50.25   
# Residual              118.8   10.90   
#Number of obs: 572, groups:  Region, 5

#Fixed effects:
#              Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)   159.6100    22.5905   4.0262   7.065  0.00207 ** 
#daylength_z     9.8180     0.5083 562.1349  19.314  < 2e-16 ***
#ndvi_z         -1.0721     0.8334 564.2501  -1.286  0.19885    
#PC1_z          10.9944     1.7222 565.0532   6.384 3.60e-10 ***
#precip_z        3.5701     0.6814 563.6173   5.239 2.28e-07 ***
#seasonality_z   6.0244     0.8643 565.9930   6.970 8.85e-12 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) dylng_ ndvi_z PC1_z  prcp_z
#daylength_z -0.010                            
#ndvi_z       0.025  0.264                     
#PC1_z        0.080  0.003  0.296              
#precip_z     0.035 -0.108 -0.306  0.438       
#seasonlty_z -0.065 -0.064 -0.424 -0.697 -0.034

print(model_set)          # all models with AIC, delta, weight, etc.
as.data.frame(model_set)  # same, as a plain data frame (easier to work with)

> nrow(model_set)
#[1] 32

model_df <- as.data.frame(model_set)

# AIC column is called "AICc" by default in dredge (corrected AIC)
model_df[, c("df", "AICc", "delta", "weight")]   # key columns


library(dplyr)

model_table <- model_df %>%
  mutate(Model_Rank = row_number()) %>%
  select(Model_Rank, 
         PC1_z, daylength_z, precip_z, seasonality_z, ndvi_z,  # your predictors
         df, AICc, delta, weight) %>%
  arrange(delta)

print(model_table)
#   Model_Rank     PC1_z daylength_z   precip_z seasonality_z      ndvi_z df     AICc      delta        weight
#1           1 10.994417    9.817954  3.5701195      6.024369 -1.07205609  8 4386.019   0.000000  6.303823e-01
#2           2 11.658128    9.990116  3.3036561      5.549391          NA  7 4387.087   1.067733  3.696142e-01
#3           3  6.761822   10.066892         NA      6.319940          NA  6 4411.334  25.315419  2.006460e-06
#4           4  7.016898   10.106801         NA      6.190518  0.25793405  7 4411.864  25.845076  1.539632e-06
#           5        NA    9.809600  1.6670788      9.878796 -2.64960527  7 4425.955  39.935512  1.341893e-09
#6           6 19.402641   10.042355  3.7457199            NA  1.39125837  7 4431.725  45.705937  7.493519e-11
#7           7        NA    9.978495         NA      9.168320 -1.54999731  6 4431.740  45.721372  7.435911e-11
#8           8 19.402335    9.797454  4.1906849            NA          NA  6 4434.157  48.137877  2.221247e-11
#9           9        NA   10.262723         NA      8.997622          NA  5 4435.579  49.559511  1.091171e-11
#10         10        NA   10.275151  0.6330262      9.221401          NA  6 4435.654  49.634985  1.050761e-11
#11         11 15.474356   10.351931         NA            NA  2.86142266  6 4458.053  72.033983  1.437551e-16
#12         12 14.376011    9.862469         NA            NA          NA  5 4471.578  85.558899  1.662360e-19
#13         13        NA   10.507626 -2.4397586            NA  1.38902291  6 4623.432 237.412464  1.762554e-52
#14         14        NA   10.263103 -1.9956436            NA          NA  5 4625.313 239.293762  6.880561e-53
#15         15        NA   10.305752         NA            NA          NA  4 4634.533 248.513560  6.848100e-55
#16         16        NA   10.296583         NA            NA -0.05008604  5 4635.063 249.043623  5.253737e-55
#17         17 11.072541          NA  5.0225525      7.017751 -5.28358934  7 4670.997 284.978375  8.266774e-63
#18         18 14.590951          NA  3.7447565      4.596510          NA  6 4696.262 310.242486  2.699629e-68
#19         19        NA          NA  3.1058075     10.900725 -6.87170329  6 4696.546 310.526639  2.342076e-68
#20         20  5.413107          NA         NA      7.296179 -3.56729089  6 4702.730 316.710684  1.063537e-69
#21         21 20.880092          NA  5.2573403            NA -2.51397989  6 4709.221 323.202102  4.141516e-71
#22         22        NA          NA         NA      9.590215 -4.92961978  5 4710.843 324.823869  1.840760e-71
#23         23  9.069752          NA         NA      5.458199          NA  5 4714.652 328.632952  2.740723e-72
#24         24 20.951982          NA  4.4649177            NA          NA  5 4715.630 329.611231  1.680485e-72
#25         25        NA          NA         NA      9.052565          NA  4 4740.751 354.731838  5.896089e-78
#26         26        NA          NA  0.3948046      9.192199          NA  5 4741.304 355.284825  4.471822e-78
#27         27 15.375009          NA         NA            NA -0.59403522  5 4741.846 355.826486  3.410863e-78
#28         28 15.615986          NA         NA            NA          NA  4 4741.922 355.903007  3.282827e-78
#29         29        NA          NA -1.3459269            NA -2.71673123  5 4852.518 466.498890 3.167067e-102
#30         30        NA          NA         NA            NA -3.47201087  4 4854.557 468.538310 1.142359e-102
#31         31        NA          NA -2.2260127            NA          NA  4 4858.603 472.583539 1.511445e-103
#32         32        NA          NA         NA            NA          NA  3 4866.426 480.406552 3.024451e-105

#############################
## Only GPS collared wolves ##
#############################
dat_GPS <- subset(dat, Estimation_Method=="GPS")
#GLMM models and obtain AIC 
dat_GPS$PC1_z <- scale(dat_GPS$PC1)
dat_GPS$daylength_z <- scale(dat_GPS$daylength_original_matingdate)
dat_GPS$precip_z <- scale(dat_GPS$Annual_Precipitation)
dat_GPS$seasonality_z <- scale(dat_GPS$Precipitation_Seasonality)
dat_GPS$ndvi_z <- scale(dat_GPS$mean_ndvi)

model_z <- lmer(DOY ~ PC1_z + daylength_z + precip_z + seasonality_z + ndvi_z + (1|Region), data = dat_GPS,  na.action = na.fail)
summary(model_z)
AIC(model_z)
model_set <- dredge(model_z)
head(model_set)
best_model <- get.models(model_set, 1)[[1]]
summary(best_model)
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: DOY ~ daylength_z + ndvi_z + PC1_z + precip_z + seasonality_z +      (1 | Region)
#   Data: dat_GPS#

#REML criterion at convergence: 1612.2

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-3.3353 -0.5019  0.1235  0.5696  4.4106 ##

#Random effects:
# Groups   Name        Variance Std.Dev.
# Region   (Intercept) 156.86   12.525  
# Residual              13.11    3.621  
#Number of obs: 297, groups:  Region, 2

#Fixed effects:
#              Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)   189.4508     8.8653   0.9982  21.370 0.029926 *  
#daylength_z    11.9038     0.2203 290.0414  54.032  < 2e-16 ***
#ndvi_z         -4.6118     0.3541 290.1173 -13.025  < 2e-16 ***
#PC1_z           5.0961     0.5219 290.2320   9.764  < 2e-16 ***
#precip_z        3.4392     0.3163 290.2899  10.874  < 2e-16 ***
#seasonality_z   1.1835     0.3337 290.0722   3.546 0.000455 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) dylng_ ndvi_z PC1_z  prcp_z
#daylength_z -0.006                            
#ndvi_z      -0.010  0.145                     
#PC1_z        0.015  0.162  0.446              
#precip_z     0.016 -0.009 -0.087  0.515       
#seasonlty_z  0.008 -0.212 -0.127 -0.434  0.084#

AIC(model_z)
model_set <- dredge(model_z)
head(model_set)
best_model <- get.models(model_set, 1)[[1]]
summary(best_model)

print(model_set)          # all models with AIC, delta, weight, etc.
as.data.frame(model_set)  # same, 

#   (Intercept) daylength_z    ndvi_z      PC1_z   precip_z seasonality_z df     logLik     AICc      delta        weight
#32    189.4508    11.90378 -4.611830  5.0961493  3.4392015     1.1835044  8  -806.0908 1628.682   0.000000  9.928816e-01
#16    189.1978    12.06951 -4.451932  5.8977206  3.3441790            NA  7  -812.0850 1638.558   9.875867  7.118363e-03
#28    188.1708    11.55726 -6.145936         NA  1.8450012     2.5917101  7  -847.6245 1709.637  80.954846  2.616833e-18
#4    187.8429    11.92822 -4.272826  2.1671774         NA     0.8753322  7  -855.4840 1725.356  96.673882  1.010254e-21
#     187.6868    12.05125 -4.160496  2.8242886         NA            NA  6  -857.9239 1728.137  99.455796  2.513887e-22
#20    187.5315    11.72099 -5.252624         NA         NA     1.7740007  6  -863.9127 1740.115 111.433360  6.301599e-25
#12    186.9410    11.85773 -6.361242         NA  0.9127622            NA  6  -873.1157 1758.521 129.839486  6.347625e-29
#30    188.2494    12.32032        NA  8.1111461  3.0739258     0.6283374  7  -872.8862 1760.160 131.478294  2.797362e-29
#14    188.1349    12.40200        NA  8.4869071  3.0293000            NA  6  -874.0662 1760.422 131.740441  2.453716e-29
#4     186.7921    11.89844 -5.794866         NA         NA            NA  5  -877.0924 1764.391 135.709230  3.372971e-30
#6     186.8143    12.36605        NA  5.5264680         NA            NA  5  -900.7271 1811.660 182.978757  1.834701e-40
#22    186.8731    12.31550        NA  5.2690035         NA     0.3861362  6  -900.2355 1812.761 184.079070  1.058365e-40
#18    184.8976    11.89476        NA         NA         NA     3.2066429  5  -946.4883 1903.183 274.501047  2.453310e-60
#26    184.8596    11.91951        NA         NA -0.3314488     3.0138625  6  -946.1435 1904.577 275.894960  1.221991e-60
##10    183.1684    12.30442        NA         NA -1.4831000            NA  5  -963.4271 1937.060 308.378732  1.079709e-67
#2     183.1684    12.23500        NA         NA         NA            NA  4  -971.5034 1951.144 322.462073  9.443827e-71
#31    192.2229          NA -7.349725  0.4534378  3.5540477     4.9856614  7 -1155.6013 2325.590 696.908398 4.625531e-152
#27    192.0977          NA -7.482203         NA  3.4078255     5.1039070  6 -1157.0900 2326.470 697.788084 2.979479e-152
#23    190.5509          NA -7.000849 -2.5891126         NA     4.6724938  6 -1162.1759 2336.641 707.959693 1.842483e-154
#19    191.0006          NA -5.844835         NA         NA     3.6397560  5 -1165.0064 2340.219 711.537389 3.079758e-155
#15    191.2654          NA -6.809766  3.6940744  3.1372871            NA  6 -1166.8498 2345.989 717.307493 1.720103e-156
#11    189.8040          NA -7.976474         NA  1.6047321            NA  5 -1170.8366 2351.879 723.197632 9.047465e-158
#7     189.8251          NA -6.527917  0.8040131         NA            NA  5 -1171.9778 2354.162 725.480195 2.889847e-158
#3     189.5570          NA -6.986701         NA         NA            NA  4 -1173.3404 2354.818 726.136217 2.081715e-158
#29    190.4059          NA        NA  5.0792303  2.9580877     4.2955405  6 -1175.3901 2363.070 734.388231 3.361393e-160
#21    189.0605          NA        NA  2.3338231         NA     4.0579786  5 -1179.8839 2369.974 741.292225 1.064967e-161
#25    188.2061          NA        NA         NA  0.8012745     5.7280926  5 -1181.4536 2373.113 744.431637 2.216264e-162
#17    188.1336          NA        NA         NA         NA     5.2741938  4 -1182.6597 2373.456 744.774785 1.866843e-162
#13    189.6789          NA        NA  7.5948327  2.6295033            NA  5 -1183.2998 2376.806 748.124146 3.497867e-163
#5     188.5038          NA        NA  5.0095327         NA            NA  4 -1186.9861 2382.109 753.427569 2.467060e-164
#9     185.1784          NA        NA         NA -1.3989783            NA  4 -1196.0508 2400.239 771.556969 2.853845e-168
#1     184.7761          NA        NA         NA         NA            NA  3 -1198.2096 2402.501 773.819416 9.207595e-169


## GLMM Remove wolves we have multiple dates for, choose one date 
library(lme4)     
library(MuMIn)    
dat <- read.csv ("cleaned_wolf_parturition_May15_final_2026.csv", header=TRUE)

dat <- dat[!duplicated(dat$WolfID), ]
nrow(dat) #330
#GLMM models and obtain AIC 
dat$PC1_z <- scale(dat$PC1)
dat$daylength_z <- scale(dat$daylength_original_matingdate)
dat$precip_z <- scale(dat$Annual_Precipitation)
dat$seasonality_z <- scale(dat$Precipitation_Seasonality)
dat$ndvi_z <- scale(dat$mean_ndvi)

model_z <- lmer(DOY ~ PC1_z + daylength_z + precip_z + seasonality_z + ndvi_z +  (1|Region), data = dat, na.action = na.fail)
summary(model_z)
AIC(model_z)
#4547.9
model_set <- dredge(model_z)
head(model_set)
best_model <- get.models(model_set, 1)[[1]]
summary(best_model)
#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: DOY ~ daylength_z + ndvi_z + PC1_z + precip_z + seasonality_z +      (1 | Region)
#   Data: dat##

#REML criterion at convergence: 2602.2

#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-3.4279 -0.4204  0.0332  0.5256  7.1661 #

#Random effects:
# Groups   Name        Variance Std.Dev.
# Region   (Intercept) 2368.0   48.66   
# Residual              152.4   12.34   
#Number of obs: 330, groups:  Region, 5#

#Fixed effects:
##              Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)   160.3409    21.9127   4.0119   7.317 0.001835 ** 
#daylength_z    10.3114     0.7743 320.1874  13.317  < 2e-16 ***
#ndvi_z         -1.3751     1.2757 322.2124  -1.078 0.281899    
#PC1_z          10.7187     2.2140 323.6150   4.841    2e-06 ***
#recip_z        3.8790     1.0204 321.5550   3.801 0.000172 ***
#seasonality_z   4.1780     1.1386 323.8059   3.670 0.000284 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#            (Intr) dylng_ ndvi_z PC1_z  prcp_z
#daylength_z -0.010                            
#ndvi_z       0.034  0.320                     
#PC1_z        0.087 -0.019  0.325              
#precip_z     0.039 -0.157 -0.297  0.450       
#seasonlty_z -0.066 -0.129 -0.373 -0.559 -0.009
# ── 1. Full model selection table ─────────────────────────────────────────────
print(model_set)          # all models with AIC, delta, weight, etc.
as.data.frame(model_set)  # same, as a plain data frame (easier to work with)

# ── 2. Number of models tested ────────────────────────────────────────────────
nrow(model_set)

# ── 3. Extract AIC values specifically ────────────────────────────────────────
model_df <- as.data.frame(model_set)

# AIC column is called "AICc" by default in dredge (corrected AIC)
# If you used AIC, the column is "AIC"
model_df[, c("df", "AICc", "delta", "weight")]   # key columns
# or just AIC:
model_df$AICc

# ── 4. Clean summary table with model terms + AIC ─────────────────────────────
library(dplyr)

model_table <- model_df %>%
  mutate(Model_Rank = row_number()) %>%
  select(Model_Rank, 
         PC1_z, daylength_z, precip_z, seasonality_z, ndvi_z,  # your predictors
         df, AICc, delta, weight) %>%
  arrange(delta)










