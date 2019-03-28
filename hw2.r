# Marianna Ohanyan 22.02.19
install.packages("ggplot2")
library(ggplot2)
# Problem 1. Use set.seed(1) in random functions:
# a) Create a random vector X containing 100 observations drawn from a N(0,1) distribution
# (score=3).
set.seed(1)
X=rnorm(100)

# b) Create a vector e containing 100 observations drawn from a N(0, 0.25) distribution (score=3).

e=rnorm(100, mean=0, sd=.25)

### Instructor's comment: Standard deviation=0.5 not 0.25, variance=0.25

# c) Generate a vector Y according to the model (score=3)
# Y = −1 + 0.5X + e 
Y = -1 + 0.5*X+e

# d) Create a scatterplot displaying the relationship between X and Y. Comment on what you
# observe (score=3).
df <- data.frame(X = X, Y=Y)
ggplot(df, aes(X,Y))+geom_point()

# we can see that X and Y are linearly correlated

# e) Fit a least squares linear model to predict Y using X. Use summary(x)
#and comment on the model obtained. How do B0^ and B1^ compare to B0 and B1 (score=3)?

ggplot(df, aes(X,Y))+geom_point()+geom_smooth(method=lm)
model = lm(Y~X, data=df)
summary(model)

#we got good model as B0^=-1.00942 it is close to BO=1 and B1^= 0,49973 it is close to B1=0,5

### Instructor's comment: In real-life problems we don't have \beta_0 and \beta_1 to compare with obtained coef-s.

# f) Display the least squares line on the scatterplot obtained in d).
# Draw the population regression line on the plot, in a different color.
# Use the legend() command to create an appropriate legend (score=4).
ggplot(df, aes(X,Y))+geom_point()+geom_smooth(method=lm, aes(color = "Regression line"))+
  scale_color_manual( values = c("Regression line" = "pink"))+labs(color="legend")

### Instructor's comment: Draw the population regression line abline(-1, 0.5)
  

# g) Fit a polynomial regression model that predicts Y using X and X^2. 
#Is there evidence that the quadratic term improves the model fit?
#Explain your answer (score=6).

model2 = lm(Y~poly(X,2),data=df)
ggplot(df, aes(X,Y))+geom_point()+geom_smooth(method='lm',formula = y~poly(x,2, raw=TRUE))
 
summary(model2)


#however R^2 increases a little bit, but quadratic form does not improve our model,
#as p-value is not significant 


# h) What are the confidence intervals for B0 and B1 based on the original 
#data set, the noisier data set, and the less noisy data set? 
#Comment on your results (score=5).

#to compare original.noisy/less noisy data's confidence intervals we need to generate 
#the data for that lets change error term 

eeror_noisy2 = rnorm(100, mean=0, sd=.10)
error_less_noisy3 =rnorm(100, mean=0, sd=20)

Y2 = -1 + 0.5*X+eeror_noisy2
Y3 = -1 + 0.5*X+error_less_noisy3
df2 <- data.frame(X = X, Y2=Y2)
model2 = lm(Y2~X, data=df2)
model3 = lm(Y3~X, data=df2)
confint(model)
confint(model2)
confint(model3)

# we are getting different confident intervals which is natural as 
# when we have a lot of noise we are predicting the coefficients hardly and
#we need bigger interval for that





### Instructor's comment: 24/30.


# Problem 2. Use “Auto” data set (“ISLR” package).
install.packages("ISLR")
install.packages("ggplot2")
install.packages("corrplot")
library(ISLR)
library(ggplot2)
library(corrplot)

# a) Perform a simple linear regression with “mpg” as
# the response and “horsepower” as the predictor (score=2).
# Use the summary() function to print the results. 
# Explain the output: 
df <- na.omit(Auto)
model = lm(mpg ~ horsepower,data = df)
summary(model)
ggplot(df, aes(horsepower,mpg))+geom_point()+geom_smooth(method=lm)

df2 <- data.frame(mpg = df$mpg, horsepower = df$horsepower)

# I. Is there a relationship between the predictor and the response (score=2)?

# from the model we can see that there is a relationship between mpg and horsepower,
# as R^2 of our model is 0.6 which explains 60% ov variance in the model , and p value 
# is significant which is another factor

### Instructor's comment: p-value for horsepower <2*10^{-16}, that is why there is a relation.

# II. How strong is the relationship between the predictor and the response (score=2)?
corr <- round(cor(df2), 1)
corrplot(corr, method = "circle")
# we can see correlation = -0.8 which means the relationship is very strong , and
# more over their correlation is negative

### Instructor's comment: Use RSE or R^2 to measure how strong the relationship is.
### For simple linear regression (cor. coef)^2=R^2. 

# III.What is the predicted “mpg” associated with a “horsepower” of 98 (score=2)?

mpg98 <-predict(model, data.frame(horsepower = 98))
mpg98
# IV. What are the associated 95% confidence and prediction intervals (score=2)?

predict(model, data.frame("horsepower"=98), interval="confidence")
predict(model, data.frame("horsepower"=98), interval="prediction")

# b) Plot the response and the predictor. Use the abline() function to display
# the least squares regression line (score=2).

#I prefer drawing with ggplot , so I will do both versions not to lose points
#you can find ggplot plot with regression above
plot(df$horsepower, df$mpg)
abline(model, lwd=2,col="pink")

# c) Produce diagnostic plots of the least squares regression fit. 
# Comment on any problems you see with the fit (score=4).

plot(model, which=c(1)) # Residuals vs Fitted values.
# we can observe that our model is not very linear

### Instructor's comment: Not the model but relationship between mpg and horsepower

plot(model, which=c(2)) #checking normality
# we have normally distributed model as points are not far from the middle line

plot(model, which=c(3)) # sqrt(|Standardized Residuals|) vs Fitted values.
#error terms does not have equal varience

plot(model, which=c(4)) # Cook's Distance
# we can observe outliers in our model

plot(model, which=c(5)) # Residuals vs Leverage.

# d) Do the residual plots suggest any unusually large outliers? 
# Does the leverage plot identify any observations with unusually high leverage (score=4)?

# yes we have unusally large outliers, it would be better to delete outliers with IQR,
# and I think we need to use polinomial regression to fit our model better

### Instructor's comment: Use plot(predict(), rstudent()) or plot(fitted.values(), rstudent()) and
### plot(hatvalues()) for identifying outliers and high leverage points.





### Instructor's comment: 16/20.


# Problem 3. Use “Carseats” data set (“ISLR” package).
install.packages("ISLR")
df <- na.omit(Carseats)
# a) Fit a multiple regression model to predict “Sales” using “Price”, 
# “Urban”, and “US” (score=2).

model = lm(Sales~Price+Urban+US, data=df)
summary(model)

# b) For which of the predictors can you reject the null hypothesis
# H0: Bj = 0 (score=2)?

#when p value is small we can reject null hypotesis 
#in our case Price and USYes have small P values so we reject them

# c) On the basis of your response to the previous question, fit a
# smaller model that only uses the
# predictors for which there is evidence of association with the outcome (score=4).

model2 = lm(Sales~Price+US, data=df)
summary(model2)

# d) How well do the models in a) and c) fit the data (score=2)?

#both models are not good as we have only 23% explained variance,
#however the second model (c) is better because we dont have a gap 
# between R^2 and R^2 adjusted which means we dont have insignificant variables 
#in our model2

# e) Using the model from c), obtain 95% confidence intervals for the coefficient(s) (score=2).

confint(model2)

# f) Produce diagnostic plots of the linear regression fit.
# Comment on any problems you see with the fit (score=4).
# g) Do the residual plots suggest any unusually large outliers?
# Does the leverage plot identify any observations with unusually high leverage (score=4)?


par(mfrow = c(2, 2))
plot(model2)

plot(predict(model2), rstudent(model2))  # Studentized Residuals vs Fitted values
abline(h = -3, col = 'red')
abline(h = 3, col = 'red')               
which(abs(rstudent(model2))>3)           

#residual plot does not show us outliers

hatvalues(model2)                        # Leverage statistics. 
plot(hatvalues(model2))                   
abline(h=2*2/506, col = 'red')          
which(hatvalues(model)>2*2/506)

### Instructor's comment: p=2 is the numbers of the predictors of your model. So the threshold =2*3/400 or 3*3/400.

#the model is good normality, linearity satisfied,
# a little problem with constant variance of error terms , as the line in 
# Scale-Location is not streight and the only problem is outliers which we
#can observe in Residulas vc Leverage plot, if we will remove the outliears model 
#will be improved a little bit 





### Instructor's comment: 19/20.





install.packages("dplyr")
install.packages("ISLR")
install.packages("corrplot")
library(dplyr)
library(ISLR)
library(corrplot)
# Problem 4. Use “Auto” data set (“ISLR” package).

df <- na.omit(Auto)
# a) Produce a scatterplot matrix which includes all of the variables in
# the data set (score=2).

pairs(Auto)

# b) Compute the matrix of correlations between the variables.
# You will need to exclude the “name” variable, which is qualitative (score=2).

df_q  = select(df,-c("name"))
corr <- round(cor(df_q), 1)
corrplot(corr, method = "circle")

# c) Perform a multiple linear regression with “mpg” as the response and all 
# other variables except “name” as the predictors (score=2).

model = lm(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin, data = df)

# Use the summary() function to print the results. Comment on the output:

summary(model)

# I. Is there a relationship between the predictors and the response (score=2)?

# Yes there is a obvious relationship between predictors and response
# as our R squared can explain 82% of overall variance and R  squared adjusted is 
# very close to R

### Instructor's comment: F-stat.=252.4>>1 and corresponding p-value<2.2*10^{-16} implies that there is a relationship.
### R^2=0.8215 shows how good model fits data.


# II. Which predictors appear to have a statistically significant relationship 
# to the response(score=2)?

# displacement,weight,year,origin are statistically significant

# III.What does the coefficient for the “year” variable suggest (score=2)?

# it influence the mpg positively and it means that the average effect of an
# increase of 1 year is an increase of 0.7507727 in mpg(case when we consider only
# dependancy from mpg )

# d) Produce diagnostic plots of the linear regression fit. 
# Comment on any problems you see with the fit (score=4).
# e) Do the residual plots suggest any unusually large outliers? 
# Does the leverage plot identify any observations with unusually high leverage (score=4)?

par(mfrow = c(2, 2))
plot(model)

#1.linearity of the model is violated 
#2. qq plot is good in the middle however it violates normality in
#the end , we can consider it partually normal 
#3. errors variance is also violated, but it is close to 0 line so we can consider it ok 
#4. we can obseve several outliers in leverage plot (14, and between(-2,2)


plot(predict(model), rstudent(model))  # Studentized Residuals vs Fitted values
abline(h = -3, col = 'red')
abline(h = 3, col = 'red')               
which(abs(rstudent(model))>3)           

# we can clearly see outliers in both plots 

hatvalues(model)                        # Leverage statistics. 
plot(hatvalues(model))                   
abline(h=2*2/506, col = 'red')          
which(hatvalues(model)>2*2/506)

### Instructor's comment: p=7 is the numbers of the predictors of your model. So the threshold =2*8/392 or 3*8/392.

# f) Use the “*” and “:” symbols to fit linear regression models with
# interaction effects. Do any interactions appear to be statistically significant (score=5)?
model2 = lm(mpg~cylinders*displacement+weight*acceleration+year, data = df_q)
summary(model2)

#we can see that  that both interaction terms cylinders*displacement and 
#weight*acceleration are statistically significant

# g) Try a few different transformations of the variables, such as log(x), srtX,X^2. 
# Comment on your findings (score=5).

par(mfrow = c(2, 2))

plot(df$weight, df$mpg)
plot(log(df$weight), df$mpg)
plot(sqrt(df$weight), df$mpg)
plot((df$weight)^2, df$mpg)
# we can see that log and sqrt are increasing variance it can affect 
#model not positively,however it is closer to line 
#if we will lok at square transformation it is not a good one as it is more polynomialy shaped

par(mfrow = c(2, 2))

plot(df$displacement, df$mpg)
plot(log(df$displacement), df$mpg)
plot(sqrt(df$displacement), df$mpg)
plot((df$displacement)^2, df$mpg)

# in displacement case we have best transformation when using log , as it is 
# close to line, and it closes tha gaps bettwen data point , in sqrt and displacement
# we have lots of gaps 





### Instructor's comment: 27/30.

