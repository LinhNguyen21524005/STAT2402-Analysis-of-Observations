###Part 1: General case of multiple variables
beef <- read.table("C:/Users/Linh Nguyen/OneDrive/GFE2024/Tutoring/2026/TuanAnh-Analysis of Observations/Code/dataset/Beef.txt", header = T, sep = "\t", stringsAsFactors = T)

#The Initial Model (baseline)
beef.lm <- lm(PBE ~ CBE + PFO + DINC + CFO + RDINC + RFP, data = beef)
summary(beef.lm)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.2913 -0.7621  0.1914  1.1036  2.0770 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 213.36345   57.33459   3.721  0.00397 ** 
#CBE          -1.09641    0.14798  -7.409 2.29e-05 ***
#PFO           2.36201    1.58705   1.488  0.16751    
#DINC         -3.97466    2.00628  -1.981  0.07573 .  
#CFO          -1.59962    0.60274  -2.654  0.02415 *  
#RDINC         2.62025    1.27235   2.059  0.06646 .  
#RFP          -0.08535    0.10355  -0.824  0.42903    
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.672 on 10 degrees of freedom
#Multiple R-squared:  0.9635,	Adjusted R-squared:  0.9416 
#F-statistic: 43.99 on 6 and 10 DF,  p-value: 1.28e-06

#The model should include only significant variables. Thus to find the best model 
#we remove non-significant variables from the model one at a time, re-fitting the model
#at each stage, until the variables that remain are all significant.

#Interpretation: P-values indicate which variables in the model are significant.
#Thus PFO, DINC, RDINC and RFP are not significant. 
#We refit model by starting drop the non-significant variable with the largest p-value
#above 0.05 (RFP in this case)

#Model Re-fitted without RFP
beef.lm1 <- update(beef.lm, . ~ . - RFP)
summary(beef.lm1)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-2.2961 -0.9603 -0.0510  1.0334  2.1738 

#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 194.1883    51.6330   3.761 0.003150 ** 
#CBE          -1.0995     0.1458  -7.543 1.14e-05 ***
#PFO           1.0998     0.4105   2.679 0.021431 *  
#DINC         -2.4255     0.6916  -3.507 0.004908 ** 
#CFO          -1.4120     0.5499  -2.568 0.026157 *  
#RDINC         1.6073     0.3245   4.953 0.000434 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.647 on 11 degrees of freedom
#Multiple R-squared:  0.961,	Adjusted R-squared:  0.9433 
#F-statistic: 54.23 on 5 and 11 DF,  p-value: 2.228e-07

#R Square does not adjust for the number of variables in the model. However, if 
#another variable is added to the model it will (almost) always increase the R Square.
#A simpler model, that is, one with fewer variables, is to be preferred over one 
#with more variables.
#To select better model, we choose larger value of adjust R-squared and smaller standard error


#Interpretation: Now all the remaining variables are significant!
#The multiple R is almost unchanged from the full model (with all the variables in it).
#However, the Adjusted R Square has increased!

#Model Diagnostics
#Normal distribution assumption
qqnorm(beef.lm1$residuals)
qqline(beef.lm1$residuals)

hist(beef.lm1$residuals, xlab = "Residuals")
box()

sres <- rstandard(beef.lm1)
hist(sres, xlab = "Residuals")
box()

sres <- rstandard(beef.lm1)
hist(sres, xlab = "Residuals", nclass = 4)
box()

#Interpretation: Histogram is not too different from that expected for a normal distribution.
#On the basis of the normal probability plot and the histogram of residuals, we conclude 
#that the normality assumption is not violated.

#Scatter plot for fitted values and (standardised residuals)
plot(sres ~ beef.lm$fitted.values, xlab = "Fitted values", ylab = "Standardised residuals")

#No clear pattern in the plot (so linear model is appropriate). 
#No outliers (none of the points are beyond 2 or −2).
#No change in spread with predicted values --> so no evidence against homogeneous variance 
#or homoscedastic assumption

###Part 2: Including categorical variables

#Note: R can it regression models with categorical variables without requiring dummy variables
#to be defined. For a categorical variable with two or more levels, the first level in 
#alphabetical order is taken as the base level, and all the other levels are compared with 
#this. Any variable that contains characters is taken to be categorical. Numerical variables 
#that are categorical need to be coerced to be categorical.

Income <- read.table("C:/Users/Linh Nguyen/OneDrive/GFE2024/Tutoring/2026/TuanAnh-Analysis of Observations/Code/dataset/Income.txt", header = T, sep = "\t", stringsAsFactors = T)
head(Income)

Income.lm <- lm(MonthlyIncome ~ Age.10years + Male, data = Income)
summary(Income.lm)


#Residuals:
#Min        1Q    Median        3Q       Max 
#-0.136697 -0.067380  0.001351  0.054888  0.154863 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  0.73206    0.23558   3.107  0.00906 ** 
#Age.10years  0.11122    0.07208   1.543  0.14880    
#Male         0.45868    0.05346   8.580 1.82e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.09679 on 12 degrees of freedom
#Multiple R-squared:   0.89,	Adjusted R-squared:  0.8717 
#F-statistic: 48.54 on 2 and 12 DF,  p-value: 1.773e-06

#Interpretation: Male is significant in the model.Since the coefficient of Male > 0 
#this indicates that Males have a higher income. The average difference in the incomes
#for Males and Females is 0.4587 in the appropriate units.

plot(Income$MonthlyIncome ~ Income$Male)

x <- rep(c(3:6), length = 30)
y <- rnorm(n = length(x), mean = log(x + 1), sd = 1)
eg.lm <- lm(y ~ x)
summary(eg.lm)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-2.2646 -0.5102  0.2769  0.6709  1.2866 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)
#(Intercept)   0.6362     0.7426   0.857    0.399
#x             0.1731     0.1624   1.065    0.296
#
#Residual standard error: 0.993 on 28 degrees of freedom
#Multiple R-squared:  0.03896,	Adjusted R-squared:  0.004632 
#F-statistic: 1.135 on 1 and 28 DF,  p-value: 0.2958

#Suppose x is the number of children and y is the expense on toys. The model indicates 
#that, on average, the cost of toys for a family of two children is more 17,31% than that for a 
#family with one child. Not reasonable.

plot(y ~ x)

#The plot shows that the cost is not linear with respect to the number of children.


#Better model: dummy variables
x <- factor(x)
eg.lm1 <- lm(y ~ x)
summary(eg.lm1)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-2.5423 -0.2336  0.2100  0.6150  1.1332 
#
#Coefficients:
#            Estimate Std. Error t value Pr(>|t|)  
#(Intercept)   0.8734     0.3456   2.528   0.0179 *
#x4            0.7326     0.4887   1.499   0.1459  
#x5            0.9597     0.5058   1.897   0.0690 .
#x6            0.4741     0.5058   0.937   0.3572  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.9774 on 26 degrees of freedom
#Multiple R-squared:  0.1354,	Adjusted R-squared:  0.03565 
#F-statistic: 1.357 on 3 and 26 DF,  p-value: 0.2777

#The coefficients are the mean difference in y at each level of x compared with the 
#reference level.

###Part 3: Transforming variables (refering lecture slides)

#Note: Linear regression requires the residuals to be normal. Examination of residuals 
#will indicate if this assumption holds.
#If the residuals are skewed then this can often be corrected by transforming the 
#response variable:

#For Right Skewed Data: square root < cube root < ln/log

#For Left Skewed Data: square < cube < exp

###Part 4: Interactions

bank <- read.table("C:/Users/Linh Nguyen/OneDrive/GFE2024/Tutoring/2026/TuanAnh-Analysis of Observations/Code/dataset/Bank.txt", sep = "\t", header = T, stringsAsFactors = T)
bank$EducLev <- factor(bank$EducLev)
table(bank$EducLev)
levels(bank$EducLev) <- c("HS", "Ter", "Bach", "PartPG", "PG")
table(bank$EducLev)

#Baseline model
bank.lm <- lm(Salary ~ Gender + EducLev, data = bank)
summary(bank.lm)

#Residuals:
#Min      1Q  Median      3Q     Max 
#-15.585  -5.585  -1.631   3.515  53.947 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    35.1411     1.6461  21.348  < 2e-16 ***
#GenderMale      5.1772     1.5643   3.310  0.00111 ** 
#EducLevTer     -1.4394     2.3412  -0.615  0.53936    
#EducLevBach     0.7352     2.0894   0.352  0.72529    
#EducLevPartPG   4.3550     3.8753   1.124  0.26244    
#EducLevPG       9.2665     2.1642   4.282 2.87e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 9.846 on 202 degrees of freedom
#Multiple R-squared:  0.2534,	Adjusted R-squared:  0.2349 
#F-statistic: 13.71 on 5 and 202 DF,  p-value: 1.571e-11

#Interpretation: Males are paid $5,177.20 more than females on average.

interaction.plot(x.factor = bank$EducLev, trace.factor = bank$Gender, response = bank$Salary,
                 fun = mean, type = "l", ylab = "Salary (,000)", xlab = "Education", col = c("blue4",
                "red4"), lty = 1, lwd = 2, trace.label = "Gender", xpd = FALSE)

#Interpretation: The mean salary for females is lower at all education levels except
#employees with some tertiary education.

#Model 2 with interaction term:
bank.lm2 <- lm(Salary ~ Gender + EducLev + Gender*EducLev, data = bank)
summary(bank.lm2)

#If the interaction term between two variables is
significant, then the main effects need to be included in
the model even if they are not significant.


#
install.packages("faraway")
library(faraway)
data(pvc)
summary(pvc)

#Ploting
library(car)
with(pvc, boxplot(psize ~ resin * operator))

oldpar <- par(mfrow = c(1, 2))
with(pvc, interaction.plot(operator, resin, psize))
with(pvc, interaction.plot(resin, operator, psize))
par(oldpar)

#Further data exploration
with(pvc, tapply(psize, operator, mean))
with(pvc, tapply(psize, resin, mean))
with(pvc, tapply(psize, list(operator, resin), mean))

#Interpretation: Mean particle size depends on both the operator and resin railcar.
#The mean particle size for a given resin railcar is not the same for each operator.
#The mean particle size for a given operator is not the same for each resin railcar.

#Main Effect Model:
psize.lm <- lm(psize ~ operator + resin, data = pvc)
sumary(psize.lm)

#            Estimate Std. Error t value  Pr(>|t|)
#(Intercept) 36.23958    0.52260 69.3448 < 2.2e-16
#operator2   -0.26250    0.40480 -0.6485 0.5205861
#operator3   -1.50625    0.40480 -3.7209 0.0006401
#resin2      -1.03333    0.66104 -1.5632 0.1262993
#resin3      -5.80000    0.66104 -8.7740 1.135e-10
#resin4      -6.18333    0.66104 -9.3539 2.113e-11
#resin5      -4.80000    0.66104 -7.2613 1.093e-08
#resin6      -5.45000    0.66104 -8.2446 5.457e-10
#resin7      -2.91667    0.66104 -4.4122 8.164e-05
#resin8      -0.18333    0.66104 -0.2773 0.7830225
#
#n = 48, p = 10, Residual SE = 1.14496, R-Squared = 0.86


#Interpretation: Operator 3 has a lower mean psize compared with the other two operators.
#Resins 1, 7 and 8 have a higher psize than the others.


plot(psize.lm$fitted.values ~ pvc$psize, xlab = "psize", ylab = "Fitted value")

install.packages("emmeans")
library(emmeans)
emmeans(psize.lm, pairwise ~ operator)

# operator emmean    SE df lower.CL upper.CL
#1          32.9 0.286 38     32.4     33.5
#2          32.7 0.286 38     32.1     33.3
#3          31.4 0.286 38     30.9     32.0

#Results are averaged over the levels of: resin 
#Confidence level used: 0.95 

#$contrasts
#contrast              estimate    SE df t.ratio p.value
#operator1 - operator2    0.263 0.405 38   0.648  0.7944
#operator1 - operator3    1.506 0.405 38   3.721  0.0018
#operator2 - operator3    1.244 0.405 38   3.072  0.0107

#Results are averaged over the levels of: resin 
#P value adjustment: tukey method for comparing a family of 3 estimates 

#Conclusion as before: Operator 3 has a lower mean psize compared with the other two.

install.packages("multcomp")
library(multcomp)
post.hoc <- glht(psize.lm, linfct = mcp(operator = "Tukey"))
summary(post.hoc)

#Same as for emmeans. Other options also exist.

#Note: The p-value of 0.05 indicates that on average 5% of the time an incorrect conclusion
#will be made. Type I Error Rejecting a correct null hypothesis. We want to minimise this. 
#The maximum probability of a Type I error is the significance level. If several comparisons
#are conducted the chances of Type I Error increases. See later when we cover binomial 
#distribution. To compensate for this increase in chances of a Type I Error, some adjustment
#or correction needs to be made. Two common ones are Bonferroni, and Tukeys Honest 
#Significance Difference (HSD),

#Model with interactions 1
psize.lm1 <- lm(psize ~ operator * resin, data = pvc)
sumary(psize.lm1)

#Model with interactions 2
psize.lm2 <- lm(psize ~ operator + resin + operator:resin, data = pvc)
sumary(psize.lm2)

#Interpretation: In this case no significant interactions exist between the variables.
#The model should therefore be refitted without the interaction terms. Usually no more
#than two-way interactions are considered, as higher order interactions are difficult to 
#interpret. Note that interest is usually in the main effects, but interactions need to 
#considered. The inclusion of interactions can change the significance of the main effects.

#Example: weekly gas consumption and average external temperature
library(MASS)
data("whiteside")
summary(whiteside)

#Model 1: No interactions (Main effect)
gas.lm1 <- lm(Gas ~ Temp + Insul, data = whiteside)
summary(gas.lm1)

#Model 2: Interaction
gas.lm2 <- lm(Gas ~ Temp * Insul, data = whiteside)
summary(gas.lm2)

AIC(gas.lm1, gas.lm2)

#Interpretation: Lower intercept and lower slope for model with interaction. Also lower
#residual SE and higher adjusted r 2. Finally, lower AIC.