library(ggplot2)

dat <- read.csv("Iberian.csv", header = TRUE)

dat$Estimation_Method <- as.factor(dat$Estimation_Method)

ggplot(dat, aes(x = Estimation_Method, y = DOY)) +
  geom_boxplot(aes(group = Estimation_Method), 
               fill = NA, color = "black", 
               outlier.shape = NA, lwd = 0.8) +
  geom_point(aes(color = Study), 
             size = 3, 
             position = position_jitter(width = 0.1, height = 0)) +
  geom_errorbar(aes(ymin = DOY_lowerrange, ymax = DOY_upperrange, 
                    color = Study),
                width = 0.1, alpha = 0.5) +
  scale_x_discrete(labels = c("GPS" = "GPS", 
                               "Litter removed" = "Litter Removal")) +
  labs(x = "Estimation Method", 
       y = "Day of Year (DOY) of Birth",
       color = "Study",
       title = "Birth Timing by Estimation Method — Iberian Wolves") +
  theme_classic() +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 11))
