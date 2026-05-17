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
library(ExPosition)

dat <- read.csv ("PCA_May15_2026_correlated.csv", header=TRUE)


res.pca <- PCA(dat, graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 80))
var <- get_pca_var(res.pca)
var
head(var$coord, 7)
fviz_pca_var(res.pca, col.var = "black")

