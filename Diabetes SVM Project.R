#Loading packages
library(tidyverse)
library(caret)
library(readr)

#Loading in PIMA Indians Diabetes dataset

diabetes <- read_csv("diabetes.csv")

#Viewing the dataset

summary(diabetes)

#Turning zero values of glucose, blood pressure,skin thickness, insulin and BMI to NA 
diabetes[, 2:6][diabetes[, 2:6] == 0] <- NA

#Turning outcome variable into a factor with positive for 1 and negative for 0
diabetes$Outcome <- ifelse(diabetes$Outcome == 0, "neg", "pos")
diabetes$Outcome <- as.factor(diabetes$Outcome)

#Omit NA data
diabetes.data <- na.omit(diabetes)

#View data
sample_n(diabetes.data, 5)

# Applying Repeated k-fold Cross Validation due to insufficient data size for splitting
set.seed(123)
train_cv <- trainControl(method="repeatedcv", number=10, repeats=3)

#Fitting Linear SVM model with default cost of 1
#variables are normalized to make their scale easy to compare
diabetes.svm <- train(Outcome ~., data = diabetes.data, method = "svmLinear", trControl = train_cv,  preProcess = c("center","scale"))
diabetes.svm

#Fitting linear SVM for values of C and choosing the best final model for predictions
diabetes.svm2 <- train(Outcome ~., data = diabetes.data, method = "svmLinear", trControl = train_cv,  preProcess = c("center","scale"), tuneGrid = expand.grid(C = seq(0, 10, length = 20)))
diabetes.svm2            

#Plotting Accuracy against cost
plot(diabetes.svm2)

#Displaying best tuning parameter for C that maximizes model accuracy
diabetes.svm2$bestTune
diabetes.svm2_Res<-as_tibble(diabetes.svm2$results[which.max(diabetes.svm2$results[,2]),])
diabetes.svm2_Res

#Computing SVM using radial basis kernel
#Fitting model
diabetes.svm3 <- train(Outcome ~., data = diabetes.data, method = "svmRadial", trControl = train_cv, preProcess = c("center","scale"), tuneLength = 10)
diabetes.svm3

#Plotting Accuracy against cost
plot(diabetes.svm3)

#Displaying best tuning parameter for C that maximizes model accuracy
diabetes.svm3$bestTune
diabetes.svm3_Res<-as_tibble(diabetes.svm3$results[which.max(diabetes.svm3$results[,3]),])
diabetes.svm3_Res

#Computing SVM using polynomial basis kernel
# Fit the model 
diabetes.svm4 <- train(Outcome ~., data = diabetes.data, method = "svmPoly", trControl = train_cv, preProcess = c("center","scale"), tuneLength = 4)
diabetes.svm4
#Plotting Accuracy against cost
plot(diabetes.svm4)

#Displaying best tuning parameter for C that maximizes model accuracy
diabetes.svm4$bestTune
diabetes.svm4_Res<-as_tibble(diabetes.svm4$results[which.max(diabetes.svm4$results[,4]),])
diabetes.svm4_Res

#Displaying full table of best results 
res<-tibble(Model=c('SVM Linear','SVM Linear Having choice of cost','SVM Radial','SVM Poly'),Accuracy=c(diabetes.svm$results[2][[1]],diabetes.svm2_Res$Accuracy,diabetes.svm3_Res$Accuracy,diabetes.svm4_Res$Accuracy))
res %>% arrange(Accuracy)



