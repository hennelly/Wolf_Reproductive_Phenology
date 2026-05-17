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
