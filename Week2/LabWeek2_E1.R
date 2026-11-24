#Exercise 1
#Q1
#When examining two-dimensional tables, we look for patterns across the columns and down the
#rows. Here we see that across the table (as temperature increases) the lifetime decreases, and down
#the table (as material type changes) the lifetime increases. The one exception is that lifetimes for
#Material 2 at Temperature −10 is higher than that for Material 3. Overall, Material 3 seems to
#be the best performing at all temperatures.

#Q2
Material <- factor(rep(c(1,2,3), each=12))
Temperature <- factor(rep(c(-10,20,55), each=4))
Life <- c(130, 155, 74, 180, 34, 40, 80, 75, 20, 70, 82, 58, 
          150, 188, 159, 126, 136, 122, 106, 115, 25, 70, 58, 45,
          138, 110, 168, 160, 174, 120, 150, 139, 96, 104, 82, 60)
Battery <- data.frame(Material, Temperature, Life)

#Q3
#We want the mean lifetime at each temperature for each material type.
with(Battery, tapply(Life, list(Material, Temperature), mean))

#     -10     20   55
#1 134.75  57.25 57.5
#2 155.75 119.75 49.5
#3 144.00 145.75 85.5

#The table of means confirms our earlier observation. Material 3 is the best performing
#overall at every temperature, except of the temperature −10

#Q4
bat.lm <- lm(Life ~ Material + Temperature, data = Battery)
summary(bat.lm)

#Call:
#lm(formula = Life ~ Material + Temperature, data = Battery)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-54.389 -21.681   2.694  17.215  57.528 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     122.47      11.17  10.965 3.39e-12 ***
#Material2        25.17      12.24   2.057  0.04819 *  
#Material3        41.92      12.24   3.426  0.00175 ** 
#Temperature20   -37.25      12.24  -3.044  0.00472 ** 
#Temperature55   -80.67      12.24  -6.593 2.30e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 29.97 on 31 degrees of freedom
#Multiple R-squared:  0.6414,	Adjusted R-squared:  0.5951 
#F-statistic: 13.86 on 4 and 31 DF,  p-value: 1.367e-06

bat.lm2 <- lm(Life ~ Material*Temperature, data = Battery)
summary(bat.lm2)

#Call:
#lm(formula = Life ~ Material * Temperature, data = Battery)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-60.750 -14.625   1.375  17.937  45.250 
#
#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)               134.75      12.99  10.371 6.46e-11 ***
#Material2                  21.00      18.37   1.143 0.263107    
#Material3                   9.25      18.37   0.503 0.618747    
#Temperature20             -77.50      18.37  -4.218 0.000248 ***
#Temperature55             -77.25      18.37  -4.204 0.000257 ***
#Material2:Temperature20    41.50      25.98   1.597 0.121886    
#Material3:Temperature20    79.25      25.98   3.050 0.005083 ** 
#Material2:Temperature55   -29.00      25.98  -1.116 0.274242    
#Material3:Temperature55    18.75      25.98   0.722 0.476759    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 25.98 on 27 degrees of freedom
#Multiple R-squared:  0.7652,	Adjusted R-squared:  0.6956 
#F-statistic:    11 on 8 and 27 DF,  p-value: 9.426e-07

#Q5:
#Choosing the model with the interaction term because it has the lower residual standard error
#and higher Adjusted R-squared

#Q6:
hist(bat.lm2$residuals, xlab = "Residuals")
box()

sres <- rstandard(bat.lm2)
hist(sres, xlab = "Standardised residuals")
box()

plot(sres ~ bat.lm2$fitted.values, xlab = "Fitted Values", ylab = "Standardised Residuals")

qqnorm(sres)
qqline(sres)

#Based on the diagnostics, there is not evidence against the model assumptions (normality, 
#homogeneous variance and linear model).

#Q7
with(Battery, interaction.plot(Material, Temperature, Life))
with(Battery, interaction.plot(Temperature, Material, Life))

#Q8 Interpret your model.
#Based on the linear model and interaction plot, the mean lifetime is higher for material 3 
#at a temperature of 20 degrees. At the other temperatures there is no significant 
#difference, although the mean is again higher for material 3 at 55 degrees.

#Q9 Which material would you recommend for the batteries? Justify your selection.
#Based on the analysis we recommend Material 3 for the batteries.
