install.packages("corrr")
library('corrr')
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("FactoMineR")
library("FactoMineR")
install.packages("factoextra")
library("factoextra")
install.packages("mixOmics")
install.packages("ade4")
install.packages("ExPosition")

dat <- read.csv ("PCA_May15_2026.csv", header=TRUE)


## Check for pairwise correlation 

cor(dat[, c("Annual_Mean_Temperature_bio1", "Mean_Diurnal_Range_BIO2", "Temperature_Annual_Range_BIO7", "Mean_Temperature_of_Wettest_Quarter_BIO8", "Mean_Temperature_of_Driest_Quarter_bio9", "Mean_Temperature_of_Warmest_Quarter_BIO10", "Mean_Temperature_of_Coldest_Quarter_bio11", "Annual_Precipitation", "Precipitation_Seasonality", "daylength_original_matingdate", "mean_ndvi", "sd_ndvi" )], use = "complete.obs", method = "pearson")

library(corrplot)

cor_matrix <- cor(dat[, c("Annual_Mean_Temperature_bio1", "Mean_Diurnal_Range_BIO2", "Temperature_Annual_Range_BIO7", "Mean_Temperature_of_Wettest_Quarter_BIO8", "Mean_Temperature_of_Driest_Quarter_bio9", "Mean_Temperature_of_Warmest_Quarter_BIO10", "Mean_Temperature_of_Coldest_Quarter_bio11", "Annual_Precipitation", "Precipitation_Seasonality", "daylength_original_matingdate", "mean_ndvi", "sd_ndvi")], use = "complete.obs")
corrplot(cor_matrix, method = "circle", tl.cex = 0.7)

