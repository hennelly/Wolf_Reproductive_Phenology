dat <- read.csv("May15_2026_comparison_wildvscaptive_final.csv", header=TRUE)

Captive <- subset(dat, Captive_vs_Wild=="Captive")
mean(Captive$adjusted_DOBirthYOD)
#183.4383 May 2rd
sd(Captive$adjusted_DOBirthYOD)
#40.52895
Wild <- subset(dat, Captive_vs_Wild=="Wild")
mean(Wild$adjusted_DOBirthYOD)
#170.229
sd(Wild$adjusted_DOBirthYOD)
#35.07582 with YELLOWSTONE
## Welch's t-test
dat <- read.csv("May15_2026_WildvsCaptive_Ttest_final.csv", header=TRUE)
t.test(dat$Captive_adjusted_DOBirthYOD, dat$Wild_adjustedDOY, var.equal = FALSE) 

#	Welch Two Sample t-test

#data:  dat$Captive_adjusted_DOBirthYOD and dat$Wild_adjustedDOY
#t = 6.3931, df = 1278.7, p-value = 2.273e-10
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  9.397723 17.718775
#sample estimates:
#mean of x mean of y 
 #183.4383  169.8801 


dat <- read.csv("2026_May15_plotting_captivevswild_lat.csv", header=TRUE)

# Split by latitude band and test each one
library(dplyr)

results <- dat %>%
  group_by(LatGrouping_by10) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(DOY ~ Group)$p.value,  # use wilcox if non-normal
      error = function(e) NA             # returns NA if too few obs
    ),
    n_captive = sum(Group == "captive"),
    n_wild = sum(Group == "wild "),
    mean_DOY_captive = mean(DOY[Group == "captive"], na.rm=TRUE),
    mean_DOY_wild = mean(DOY[Group == "wild "], na.rm=TRUE)
  )

print(results)

#  LatGrouping_by10   p_value n_captive n_wild mean_DOY_captive mean_DOY_wild
#  <chr>                <dbl>     <int>  <int>            <dbl>         <dbl>
#1 "1-10. "         NA                8      0            129.          NaN  
#2 "10-20. "         3.33e- 1        21     41             68.0          63.3
#3 "20-30. "         8.44e- 1        13      5            107.          109  
#4 "30-40. "         9.95e- 2        44     26            164.          180. 
#5 "40-50."          6.56e-31       208    221            193.          171. 
#6 "50-60. "         1.52e-15       256     73            191.          177. 
#7 "60-70."          4.27e-12       118    197            195.          187. 
#8 "80-90. "        NA                0     10            NaN           212  



