# Marianna Ohanyan , ml h4 10.04.19

#install.packages("caret")
#install.packages("ggplot2")
#install.packages("e1071")
library(caret)
library(MASS)
library(ggplot2)
library(plotROC)
library(e1071)


german_credit = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "response")


# response 1 = negative 
# responce 2 = positive 
#lets supstruct 1 for 0 - negative , 1 - positive 


df = german_credit

df$response_new = df$response - 1

df = df[, -21]

# deviding data into training and testing sets 
set.seed(1)
inds <- createDataPartition(df$response_new, p = 0.8, list=FALSE)
df_train <- df[inds,]
df_test <- df[-inds,]


# 1. Apply LDA on the training set. Draw the ROC curve and calculate the AUC (score = 10).

lda_fit=lda(response_new~., data = df_train)
lda_fit

### 69.7% of the training observations correspond to days during which the response was 1(negative)
### 30.2% of the training observations correspond to days during which the response was 2(positive)

lda.predict = predict(lda_fit, df_test)


# creating roc_frame for plotting roc
probabilities = as.data.frame.matrix(lda.predict$posterior)
names(probabilities) = c("neg","pos")

roc_frame = data.frame( response_new = df_test$response_new, 
                       prob = probabilities$pos )






#plotting roc 
tt_smooth = ggplot(roc_frame, aes(m = prob, d = response_new )) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth

# auc = 0.7354298
calc_auc(tt_smooth)



# 2. Apply QDA on the training set. Draw the ROC curve and calculate the AUC (score = 10).

qda_fit=qda(response_new~., data = df_train)
qda_fit


qda.predict = predict(qda_fit, df_test)
probabilities = as.data.frame.matrix(qda.predict$posterior)




names(probabilities) = c("neg","pos")
roc_frame = data.frame( response_new = df_test$response_new, 
                       prob = probabilities$pos )



tt_smooth = ggplot(roc_frame, aes(m = prob, d = response_new )) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth

calc_auc(tt_smooth)
# auc = 0.6629432


# 3. Apply Na??ve Bayes on the training set. Draw the ROC curve and calculate the AUC (score = 10).


naiveBayes_fit <- naiveBayes(response_new~., data = df_train)

naive_predict = predict(naiveBayes_fit, df_test, type = "raw")
naive_predict = as.data.frame.matrix(naive_predict)
names(naive_predict) = c("neg","pos")

roc_frame = data.frame( response_new = df_test$response_new, 
                        prob = naive_predict$pos )



tt_smooth = ggplot(roc_frame, aes(m = prob, d = response_new )) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth

calc_auc(tt_smooth)

#auc = 0.7763477



# 4. Apply Logistic Regression on the training set. Draw the ROC curve and calculate the AUC (score = 10).

glm_fits=glm(response_new~., data = df_train, family=binomial)
summary(glm_fits)


glm_probs=predict(glm_fits, df_test,type="response")

# creating roc_frame for plotting roc
roc_frame = data.frame(response_new = df_test$response_new, 
                       prob = glm_probs  )


tt_smooth = ggplot(roc_frame, aes(m = prob, d = response_new )) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth

calc_auc(tt_smooth)
# auc = 0.7440505


#5. Apply k-NN on the training set and by 10-fold cross validation find the optimal 
#value of the parameter k. For the optimal model draw the ROC curve and calculate the 
#AUC (score = 15).



set.seed(1)
model_knn_class = train(response_new ~ ., 
                        data = df_train, 
                        method = "knn", 
                        trControl = trainControl( "repeatedcv", number = 10, repeats = 3 ),
                        tuneGrid = expand.grid( k = 3 : 100 ))

model_knn_class$bestTune



knn_predict_test = predict(model_knn_class, newdata = df_test)
knn_predict_test

roc_frame = data.frame(response_new = df_test$response_new, 
                       prob = knn_predict_test  )

tt_smooth = ggplot(roc_frame, aes(m = prob, d = response_new )) + 
  geom_roc() + 
  coord_equal()+
  xlab("FPR") + 
  ylab("TPR")+
  ggtitle("ROC")

tt_smooth

calc_auc(tt_smooth)
#auc = 0.5735794



#6. Compare AUC measures of different models. Find the best model (score = 10).
 #1. lda - # auc = 0.7354298
 #2. qda - # auc = 0.6629432
 #3. naive - #auc = 0.7763477 
 #4. logistic reg - # auc = 0.7440505
 #5. knn - #auc = 0.5735794

# the best model is Naive Bayes 



#7 For the best model calculate the 
###test Accuracy, 
###Balanced Accuracy,
###Sensitivity 
###Precision 
#of the positive class (score = 15).

#QDA

naive_predict <- predict(naiveBayes_fit, df_test, type = "raw")

naive_predict = as.data.frame.matrix(naive_predict)
names(naive_predict) = c("neg","pos")


confusionMatrix(data = as.factor(as.numeric(naive_predict$pos>0.5)), 
                reference =  as.factor(df_test$response_new), 
                positive = '1')

#Accuracy : 0.735
#Balanced Accuracy : 0.6400 
#Sensitivity : 0.4138
#Specificity : 0.8662

### Instructor's comment: Precision=Pos. Pred. Value (-4).





### Instructor's comment: 96/100.


