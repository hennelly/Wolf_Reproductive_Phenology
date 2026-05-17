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
## Plot correlation matrix 

high_cor <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)

data.frame(
  var1 = rownames(cor_matrix)[high_cor[,1]],
  var2 = colnames(cor_matrix)[high_cor[,2]],
  correlation = cor_matrix[high_cor]
)

                                        var1                                      var2 correlation
#1              Temperature_Annual_Range_BIO7              Annual_Mean_Temperature_bio1  -0.7118102
#2    Mean_Temperature_of_Driest_Quarter_bio9              Annual_Mean_Temperature_bio1   0.7949428
#3  Mean_Temperature_of_Warmest_Quarter_BIO10              Annual_Mean_Temperature_bio1   0.9018337
#4  Mean_Temperature_of_Coldest_Quarter_bio11              Annual_Mean_Temperature_bio1   0.9726023
#5               Annual_Mean_Temperature_bio1             Temperature_Annual_Range_BIO7  -0.7118102
#6    Mean_Temperature_of_Driest_Quarter_bio9             Temperature_Annual_Range_BIO7  -0.7374951
#7  Mean_Temperature_of_Coldest_Quarter_bio11             Temperature_Annual_Range_BIO7  -0.8423213
#8                  Precipitation_Seasonality  Mean_Temperature_of_Wettest_Quarter_BIO8   0.8191499
#9               Annual_Mean_Temperature_bio1   Mean_Temperature_of_Driest_Quarter_bio9   0.7949428
#10             Temperature_Annual_Range_BIO7   Mean_Temperature_of_Driest_Quarter_bio9  -0.7374951
#11 Mean_Temperature_of_Coldest_Quarter_bio11   Mean_Temperature_of_Driest_Quarter_bio9   0.8520798
#12                                   sd_ndvi   Mean_Temperature_of_Driest_Quarter_bio9  -0.7946475
#13              Annual_Mean_Temperature_bio1 Mean_Temperature_of_Warmest_Quarter_BIO10   0.9018337
#14 Mean_Temperature_of_Coldest_Quarter_bio11 Mean_Temperature_of_Warmest_Quarter_BIO10   0.7840319
#15              Annual_Mean_Temperature_bio1 Mean_Temperature_of_Coldest_Quarter_bio11   0.9726023
#16             Temperature_Annual_Range_BIO7 Mean_Temperature_of_Coldest_Quarter_bio11  -0.8423213
#17   Mean_Temperature_of_Driest_Quarter_bio9 Mean_Temperature_of_Coldest_Quarter_bio11   0.8520798
#18 Mean_Temperature_of_Warmest_Quarter_BIO10 Mean_Temperature_of_Coldest_Quarter_bio11   0.7840319
#19  Mean_Temperature_of_Wettest_Quarter_BIO8                 Precipitation_Seasonality   0.8191499
#20   Mean_Temperature_of_Driest_Quarter_bio9                                   sd_ndvi  -0.7946475


## removed collinear variables according to methods. The next file includes only variables that are uncorrelated 
dat <- read.csv ("PCA_May15_2026_correlated.csv", header=TRUE)

pca_result <- dudi.pca(dat, scannf = FALSE, nf = 2) # Compute PCA with 2 components
M <- cor(dat)
corrplot(M, method = "circle", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

pairs(dat) #data correlated
pca <- prcomp(dat, scale. = TRUE) #fit PCA and scale that data. 

pca$rotation #rotation cofficients or eigenvectors. The coefficents describe how each PC is a linear combination of the input variables. 

                                                  PC1          PC2         PC3         PC4          PC5          PC6         PC7
#Annual_Mean_Temperature_bio1              -0.42646017 -0.113479133  0.10138591  0.13494088 -0.001874645 -0.041332815  0.08418021
#Mean_Diurnal_Range_BIO2                   -0.06310527  0.331044668  0.46695390  0.33673878  0.377360273 -0.438888335 -0.03879052
#Temperature_Annual_Range_BIO7              0.37413175 -0.164066488  0.22884080  0.03961078  0.212646368 -0.157595243 -0.54042222
#Mean_Temperature_of_Wettest_Quarter_BIO8  -0.10466125 -0.564580605 -0.02748785  0.09444703  0.073421088  0.039139986  0.12825414
#Mean_Temperature_of_Driest_Quarter_bio9   -0.38390912  0.229865424  0.16895654  0.07424664 -0.089166166 -0.092089152 -0.08572741
#Mean_Temperature_of_Warmest_Quarter_BIO10 -0.34669304 -0.318482615  0.18514183  0.10963486  0.006164989 -0.043330714 -0.19087850
#Mean_Temperature_of_Coldest_Quarter_bio11 -0.43528991 -0.008831692  0.04769376  0.10159717 -0.077332261 -0.027376145  0.27755301
#Annual_Precipitation                      -0.18657460  0.152184327 -0.54501611 -0.02387114 -0.350054343 -0.548263318 -0.38923175
#Precipitation_Seasonality                 -0.03185093 -0.534590892  0.22653049 -0.07625265 -0.254150365 -0.194489782 -0.24163348
#daylength_original_matingdate             -0.13897296 -0.017368527  0.13454689 -0.86646604  0.275850946 -0.319580855  0.15758026
#mean_ndvi                                 -0.18432606 -0.157545859 -0.52387915  0.12919504  0.726404252  0.008958487 -0.11002414
#sd_ndvi                                    0.34212159 -0.208436320 -0.11199067  0.23709764 -0.046155737 -0.572330233  0.56147955

#obtain eigenvectors
pca_scores <- data.frame(pca$x) #
head(pca_scores)
#        PC1       PC2       PC3        PC4        PC5         PC6       PC7          PC8         PC9         PC10        PC11
#1 -5.719475 -2.320905 0.8146412 -0.5009154 -0.5029196  0.00283928 0.8096226 -0.045956045  0.10907998 -0.004039878 -0.03248503
#2 -5.593790 -2.126931 0.3629907 -0.4593144 -0.3743301 -0.23423934 0.6919097 -0.008894245 -0.02986989 -0.007935475 -0.01478226
#3 -5.585019 -2.125835 0.3544990 -0.4046288 -0.3917400 -0.21406950 0.6819643 -0.008109043 -0.03560179 -0.007636679 -0.01456286
# -5.599074 -2.127591 0.3681063 -0.4922582 -0.3638420 -0.24639007 0.6979010 -0.009367267 -0.02641687 -0.008115477 -0.01491442
#5 -5.585019 -2.125835 0.3544990 -0.4046288 -0.3917400 -0.21406950 0.6819643 -0.008109043 -0.03560179 -0.007636679 -0.01456286
#6 -5.426322 -2.694860 1.2523898  0.3974736 -0.2006798 -1.17219602 0.5764289 -0.139921069 -0.03834049  0.133525669 -0.01824260

pairs(pca_scores) #data uncorrelated

write.csv (pca_scores, "PCA_scores_temperature_May15_2026.csv")

pca$sdev #how total variance is re-partitioned in derived principal components. 
var_exp <- data.frame(pc = 1:8,
                      var_exp = pca$sdev^2 / sum(pca$sdev^2))

# add the variances cumulatively
var_exp$var_exp_cumsum <- cumsum(var_exp$var_exp)
var_exp
#  pc      var_exp var_exp_cumsum
#1  1 0.5843152897      0.5843153#
#2  2 0.3090701869      0.8933855
#3  3 0.0576541462      0.9510396
#4  4 0.0303550087      0.9813946
#5  5 0.0148137229      0.9962084
#6  6 0.0022561302      0.9984645
#7  7 0.0014287719      0.9998933
#8  8 0.0001067435      1.0000000








