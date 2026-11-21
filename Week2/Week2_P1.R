pharmex <- read.table("C:/Users/Linh Nguyen/OneDrive/GFE2024/Tutoring/2026/TuanAnh-Analysis of Observations/Code/dataset/Pharmex.txt", header = T, stringsAsFactors = T)
phmx.lm <- lm(Sales ~ Promote, data = pharmex)
summary(phmx.lm)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  25.1264    11.8826   2.115   0.0397 *  
#  Promote       0.7623     0.1209   6.304  8.6e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 7.395 on 48 degrees of freedom
#Multiple R-squared:  0.4529,	Adjusted R-squared:  0.4415 
#F-statistic: 39.74 on 1 and 48 DF,  p-value: 8.601e-08

#Interpretation the output:
#1.If Pharmex spends nothing on promotion then its sales are 25% of that of its competitor.
#2.For every percent increase is promotion, sales increase by 0.76 percent of that of its competitor.
#3.If Pharmex spends the same as its competitor on promotion (that is, 100%) then its sales are 
#25.1264% + 0.7623 x 100% = 101.3564% of that of its competitor.

#Before Model diagnostics:
#1.The model looks quite good.The correlation coefficient is sqrt(0.4415) = 0.6645, which is reasonable.
#2.Adjusted R-squared = 0.4415, so about 44% of the variation is sales is explained by expenditure on promotions.
#3.BUT, we first need to verify model assumptions, that is, we need to perform model diagnostics.

#Scatter plot for x and y

#Method 1: primitive commands
plot(pharmex$Sales ~ pharmex$Promote, main = "Scatter plot of Sales against Promote",
     xlab = "Promotion expense", ylab = "Sales")

#Method 2: using ggplot2
library(ggplot2)
phmx <- ggplot(pharmex, aes(x = Promote, y = Sales)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
phmx


#Data summary:
summary(pharmex)

#     Region         Promote           Sales       
#Min.   : 1.00   Min.   : 77.00   Min.   : 81.00  
#1st Qu.:13.25   1st Qu.: 93.00   1st Qu.: 92.25  
#Median :25.50   Median : 96.50   Median :101.00  
#Mean   :25.50   Mean   : 97.88   Mean   : 99.74  
#3rd Qu.:37.75   3rd Qu.:102.75   3rd Qu.:107.75  
#Max.   :50.00   Max.   :117.00   Max.   :119.00  

#The data range for Promotions is 77-117 with mean 97.88 and
#median 96.5, so promotional expenditure exceeds that of the
#competitor for less half the stores. Sales have a range of 81-119,
#with mean 99.74 and median 101, so sales exceed that of the
#competitor in more than half the stores.


#Scatter plot of fitted values and residuals
#Method 1:
resplot <- ggplot(phmx.lm, aes(x = phmx.lm$fitted.values, y = phmx.lm$residuals)) + geom_point()
resplot <- resplot + xlab("Fitted Values") + ylab("Residuals")
resplot

#Method 2:
plot(phmx.lm$residuals ~ phmx.lm$fitted.values)

#The plot of residuals against fitted values shows no clear pattern. This indicates that 
#a linear model is appropriate.
#However, there seems some evidence of a change in spread with fitted values—the lower 
#values have a higher spread. That is, there the plot show “fanning in”. This indicates
#that the homogeneous variance assumptions may not be satisfied.

#Histogram of residuals
par(mfrow = c(1, 2))
hist(phmx.lm$residuals, xlab = "Residuals")
box()

#Normal Q-Q plot:
qqnorm(phmx.lm$residuals, ylab = "Residuals", xlab = "Normal Scores", main = "Normal probability plot")
qqline(phmx.lm$residuals)

#Method 1: Using ggplot
ggplot(data.frame(residuals = residuals(phmx.lm)), 
       aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Q-Q Plot")

#Method 2: Using basic R
qqnorm(residuals(phmx.lm),
       main = "Normal Q-Q Plot")

qqline(residuals(phmx.lm),
       col = "red")


#The histogram is fairly symmetric and not unlike that expected for a normal distribution. 
#The normal probability plot is fairly linear. Based on these we conclude there is no 
#evidence against the normality assumption.

#So overall it appears that the homogenenous variance/homoscedacity assumptions does 
#not hold.

