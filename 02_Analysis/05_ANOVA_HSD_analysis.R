dat <- read.csv("cleaned_wolf_parturition_May15_final_2026.csv", header = TRUE)

# Fit ANOVA
model <- aov(DOY ~ Region2, data = dat)

# ANOVA table
summary(model)
 #            Df Sum Sq Mean Sq F value Pr(>F)    
#Region2       5 550245  110049     409 <2e-16 ***
#Residuals   566 152307     269                   
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# Tukey HSD
TukeyHSD(model)
#  Tukey multiple comparisons of means
#    95% family-wise confidence level

#Fit: aov(formula = DOY ~ Region2, data = dat)

#$Region2
#                                diff         lwr         upr     p adj
#Iberian-Europe             17.107143    5.213910   29.000376 0.0006365
#Indian-Europe            -120.930233 -131.609015 -110.251450 0.0000000
#Middle East-Europe         -8.653846  -20.798549    3.490857 0.3223269
#NorthAmerica-Europe        -6.727689  -14.967911    1.512534 0.1819492
#Tibetan-Europe              2.333333  -25.885550   30.552217 0.9998990
#Indian-Iberian           -138.037375 -149.428290 -126.646460 0.0000000
#Middle East-Iberian       -25.760989  -38.536370  -12.985608 0.0000002
#NorthAmerica-Iberian      -23.834832  -32.979110  -14.690554 0.0000000
#Tibetan-Iberian           -14.773810  -43.269808   13.722189 0.6756463
#Middle East-Indian        112.276386  100.623156  123.929617 0.0000000
#NorthAmerica-Indian       114.202544  106.705529  121.699558 0.0000000
#Tibetan-Indian            123.263566   95.252687  151.274445 0.0000000
#NorthAmerica-Middle East    1.926157   -7.542879   11.395194 0.9922177
#Tibetan-Middle East        10.987179  -17.614687   39.589046 0.8819646
#Tibetan-NorthAmerica        9.061022  -18.113860   36.235905 0.9321524


