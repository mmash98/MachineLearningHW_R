#Marianna Ohanyan, ml hw3 13.03.2019

install.packages("glmnet")
library(glmnet)


set.seed(1)
#1 response generation
x = rnorm(100)
e=rnorm(100, mean=0, sd=.25)
b0 = 10; b1 = 2; b2 = 3; b3 = 5
y = b0 + b1*x + b2*x^2 + b3*x^3 + e
plot(x,y)

## creating matrix and deviding into training and testing sets 
x=model.matrix(x~poly(x, 10))[,-1]

### Instructor's comment: For using x, x^2, ... insert raw=TRUE in poly(x,10), otherwise you get the
### orthogonalized polynomials of degree 1 to 10 over the specified set of points x (see ?poly for details).

grid=10^seq(10,-2,length=100)   

train = sample(1:nrow(x), nrow(x)/2)
test=(-train)
y_test=y[test]


# #2  Ridge model
# fitting ridge model
RidgeModel = glmnet(x, y, alpha=0,lambda=grid, standardize=TRUE)

plot(RidgeModel)

#cross validation 
cv_out_ridge=cv.glmnet(x[train,],y[train],alpha=0, nfolds = 10)
plot(cv_out_ridge)
bestlam1=cv_out_ridge$lambda.min
#best lambda
bestlam1    # best lambda is 1.003086

#getting test MSE
ridge_pred=predict(RidgeModel,s=bestlam1,newx=x[test,])
mean((ridge_pred-y_test)^2)  #MSE is 1.576503 ridge

# coefficients 
coef(RidgeModel, s=bestlam1)[,1]

# As expected, none of the coefficients are zero - ridge regression
# does not perform variable selection, which issupposed to be not accurate however 
#sometimes it might help, as after X^3 all coefficients are close to 0
#Even though we got all 10 coefficients our MSE is small



##3 
#Fit the Lasso model 

lassoModel = glmnet(x, y, alpha=1,lambda=grid, standardize=TRUE)

plot(lassoModel)

#validation 
cv_out=cv.glmnet(x[train,],y[train],alpha=0, nfolds = 10)

### Instructor's comment: alpha should be 1 for the lasso.

plot(cv_out)
#best lambda 
bestlam=cv_out$lambda.min
bestlam    # labmda is 1.003086

lasso_pred=predict(lassoModel,s=bestlam,newx=x[test,])
mean((lasso_pred-y_test)^2) # mse is 5.762456 lasso

coef(lassoModel, s=bestlam)[,1]

#Lasso regression gave us the coefficients of X, X^2,X^3 and intercenpet and gave 
# 0 for the rest of the results and increased MSE 

# 4
# While comparing Lasso and Ridge regularization approaches, I would say in this case 
# even thoug ridge regression is less flexible however when we have hight variance 
# it will give better results the lasso regresion which is flexible, I got this result 
# by comparing mean squares errors for both models.
# However, the lasso has a substantial advantage over ridge regression in that the resulting coefficient
# estimates are sparse

### Instructor's comment: Check the outputs of your code after implementation of the previous recommendations.





### Instructor's comment: 90/100.

