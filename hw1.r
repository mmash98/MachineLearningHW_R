install.packages("ISLR")
install.packages("ggplot2")
install.packages("corrplot")
library(ISLR)
library(ggplot2)
library(corrplot)
# 1. Remove the missing values (score =3).
newdata <- na.omit(Auto)

# 2. Which of the predictors are quantitative, and which are qualitative? (score =3)
 str(newdata)
# we can notice that parametr "origin" has discrete values and we 
# can consider it as qualitative variable 
# quantitative: mpg, cylinders, displacement, horsepower, weight, acceleration, year 
# qualitative: name, origin
 
 
#3. What is the range of each quantitative predictor
qualit<- c(ncol(newdata),ncol(newdata)-1)
quantData<-newdata[,-qualit]
sapply(quantData, range)

#4. What is the mean and standard deviation of each quantitative predictor?

mean<-sapply(quantData, mean)
sd<-sapply(quantData, sd)

#5. (score =14) Investigate the predictors graphically, using scatterplots or
#other tools of your choice from ggplot library. Create some plots highlighting 
#the relationships among the predictors. Comment on your findings. 
#Plot the same graphs using the basic plot functionality, for comparison. 

# I will visualise only variables with hight correlation for example
#"yearn and mpg",
#"cylinders and mpg", 
#"horsepower and weight"
corr <- round(cor(quantData), 1)
corrplot(corr, method = "circle")

##ggplot
ggplot(quantData, aes(year,mpg))+geom_point()+ geom_smooth(method="lm", se=F)
ggplot(quantData, aes(cylinders,mpg))+geom_point()+ geom_smooth(method="lm", se=F)
ggplot(quantData, aes(horsepower,weight))+geom_point()+ geom_smooth(method="lm", se=F)

#there is a negative correlation betwean horsepower and mpg which is natural
# As cars with more horsepower consume more gas => cannot travel long distances 
# with few gallons of gas
ggplot(quantData, aes(horsepower,mpg))+geom_point()+ geom_smooth(method="lm", se=F)

#plot
pairs(data=newdata, main="Simple Scatterplot Matrix")
pairs(~mpg+cylinders+displacement+horsepower+weight ,data=newdata, 
      main="Some variables plot")

### Instructor's comment: pairs(newdata, ...) instead of pairs(data=newdata, ...)

# 6.Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables.
# Do your plots suggest that any of the other variables might be
# useful in predicting mpg? Justify your answer.


# While predicting mpg I will take variables which are correlated with mpg
#and independentt betwwen each other: more practically I will take 
#cylinders, displacement,horsepower, weight,year, origin 

linearMod <- lm(mpg ~ cylinders+displacement+horsepower+weight+year, data=quantData)
summary(linearMod)
# only year and weigths are significant so I m going to remove horsepower and see the change of R squared in model 

linearMod <- lm(mpg ~ displacement+weight+year, data=quantData)
summary(linearMod)

# I will predict mpg with displacement+weight+year or only with weight+year





### Instructor's comment: 30/30.


###########

#9Exercise involves the Boston housing data set (MASS library).
install.packages("MASS")
install.packages("corrplot")
library(MASS)
library(corrplot)


#1.How many rows are in this data set? How many columns? 
#What do the rows and columns represent?
  
dim(Boston) 
# we have 506 rows and 14 columns
# rows represent the suburbs of Boston, columns represent 
#some features of suburbs

#2.Make some pairwise scatterplots of the predictors (columns) in this data set.
#Describe your findings.
attach(Boston)
corr <- round(cor(Boston), 1)
corrplot(corr, method = "circle")
pairs(Boston)
# examples of variables which are higly correlated , you can see it in correlation plot also
# nox and indus
# dis and indus
# tax and indus
# nox and dis 

ggplot(Boston, aes(x=nox, y=indus)) + geom_point() + geom_smooth(method='lm')
ggplot(Boston, aes(x=dis, y=indus)) + geom_point() + geom_smooth(method='lm')
ggplot(Boston, aes(x=tax, y=indus)) + geom_point() + geom_smooth(method='lm')
ggplot(Boston, aes(x=nox, y=dis)) + geom_point() + geom_smooth(method='lm')

#3.Are any of the predictors associated with per capita crime rate? 
#If so, explain the relationship.
ggplot(Boston, aes(x=rad, y=crim)) + geom_point() + geom_smooth(method='lm')
ggplot(Boston, aes(x=tax, y=crim)) + geom_point() + geom_smooth(method='lm')

#rad and tax are highly correlated with crim 
#index of accessibility to radial highways and full-value property-tax rate per USD 10,000
# in creases and crim rate increases also 

#4.Do any of the suburbs of Boston appear to have particularly high crime rates? 
#Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.

# suburbs which have hight crime rates 
head(Boston[order(-crim),])

# suburbs which have hight Tax rates 
head(Boston[order(-tax),])

# suburbs which have hight Pupil-teacher ratio 
head(Boston[order(-ptratio),])


#5.How many of the suburbs in this data set bound the Charles river?
nrow(Boston[Boston$chas==1,])
# 35 suburbs are near Charles river





### 20/20.



