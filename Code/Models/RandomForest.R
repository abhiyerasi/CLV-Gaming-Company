#a. You need to use the data set you prepared in the previous CUTe exam. Since there is a change in your team members, take the data set among your new team members that got the best MAPE.
hopmonkdata=read.csv("hopmonk.csv")
hopmonkdata=subset(hopmonkdata,select=c(Customer_value360,Frequency,Frequency360,
                                        Customer_value180,Frequency7,Number_Games_Played, 
                                        Frequency180,FreqGamePlay,Customer_value7,FreqGamePlay360 ,
                                        Frequency30, No_of_Childrens, TenureDays,
                                        MaxChildAge,pur_freq, avg_order_value, MinChildAge, 
                                        maxRecencyCum, minRecencyCum, Country, FavoriteSource , 
                                        FavoriteGame90 ,FavoriteChannel180,Customer_value))


hopmonkdata$Customer_value=sqrt(hopmonkdata$Customer_value)
#b. Understand the summary of the data set
summary(hopmonkdata)

#c. Split the data into Train and Test

library(caret)
set.seed(123)
inTrain <- createDataPartition(hopmonkdata$Customer_value, p = .7, list = FALSE)
Train <- hopmonkdata[ inTrain, ] # Training dataset for all model development
Test <- hopmonkdata[ -inTrain, ] # Final sample for model validation


#e. Build the following models. Ensure you do the necessary data pre-processing before build the models

##i. Random forest
# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables

std_method <- preProcess(subset(Train,select=-c(Customer_value)), method = c("center", "scale"))

train_data <- predict(std_method, Train)

test_data <- predict(std_method, Test)

train_target <- Train$Customer_value
train_data$Customer_value <- NULL
test_target <- Test$Customer_value
test_data$Customer_value <- NULL


# Building Multiple Models

## Random Forest

# We can build a random forest model using the randomForest() function from the randomForest() package

# Below, we use the default parameters to build the random forest model

library(randomForest)

#model_rf <- randomForest(train_target ~ . , train_data,ntree = 300,mtry = 5)
#model_rf2 <- randomForest(train_target ~ . , train_data,ntree = 100,mtry = 3)
#model_rfnew <- randomForest(train_target ~ . , train_data,ntree = 100,mtry = 8)
model_rfnew2 <- randomForest(train_target ~ . , train_data,ntree = 100,mtry = 10)
model_rfnew3 <- randomForest(train_target ~ . , train_data,ntree = 200,mtry = 10)
model_rfnew4 <- randomForest(train_target ~ . , train_data,ntree = 400,mtry = 10)
model_rfnew5 <- randomForest(train_target ~ . , train_data,ntree = 50,mtry = 10)
model_rfnew6 <- randomForest(train_target ~ . , train_data,ntree = 50,mtry = 6)
model_rfnew7 <- randomForest(train_target ~ . , train_data,ntree = 50,mtry = 5)
model_rfnew8 <- randomForest(train_target ~ . , train_data,ntree = 50,mtry = 3)
model_rfnew9 <- randomForest(train_target ~ . , train_data,ntree = 50,mtry = 8)
# We can also look at variable importance from the built model using the importance() function and visualise it using the varImpPlot() funcion
varimp=importance(model_rfnew)

varImpPlot(model_rfnew2)

########################  RF2 #####################################################
# Store predictions from the model
preds_rf2 <- predict(model_rfnew2, test_data)
head(preds_rf2)
# Predict on the train data
preds_train_rf2 <- predict(model_rfnew2,train_data)
head(preds_train_rf2)
# Error Mertics
library(DMwR)
test_error2=regr.eval(test_target,preds_rf2)
head(test_target)
train_error2=regr.eval(train_target,preds_train_rf2)

error=data.frame(train_error2,test_error2)
########################  RF 3 #####################################################
# Store predictions from the model
preds_rf3 <- predict(model_rfnew3, test_data)
# Predict on the train data
preds_train_rf3 <- predict(model_rfnew3,train_data)
# Error Mertics
library(DMwR)
test_error3=regr.eval(test_target,preds_rf3)
train_error3=regr.eval(train_target,preds_train_rf3)

error=data.frame(error,train_error3,test_error3)
########################  RF 4 #####################################################
# Store predictions from the model
preds_rf4 <- predict(model_rfnew4, test_data)
# Predict on the train data
preds_train_rf4 <- predict(model_rfnew4,train_data)
# Error Mertics
library(DMwR)
test_error4=regr.eval(test_target,preds_rf4)
train_error4=regr.eval(train_target,preds_train_rf4)

error=data.frame(error,train_error4,test_error4)

# ROOT OVER MEDIAN SQUARE ERRORS
train_median=(sqrt(median((train_target- preds_train_rf4)^2)))
test_median=(sqrt(median((test_target- preds_rf4)^2)))

error_median=data.frame(train_median,test_median)
########################  RF 5 #####################################################
# Store predictions from the model
preds_rf5 <- predict(model_rfnew5, test_data)
# Predict on the train data
preds_train_rf5 <- predict(model_rfnew5,train_data)
# Error Mertics
library(DMwR)
test_error5=regr.eval(test_target,preds_rf5)
train_error5=regr.eval(train_target,preds_train_rf5)

error=data.frame(error,train_error5,test_error5)

########################  RF  #####################################################
# Store predictions from the model
preds_rf6 <- predict(model_rfnew6, test_data)
# Predict on the train data
preds_train_rf6 <- predict(model_rfnew6,train_data)
# Error Mertics
library(DMwR)
test_error6=regr.eval(test_target,preds_rf6)
train_error6=regr.eval(train_target,preds_train_rf6)

error=data.frame(error,train_error6,test_error6)
########################  RF  #####################################################
# Store predictions from the model
preds_rf7 <- predict(model_rfnew7, test_data)
# Predict on the train data
preds_train_rf7 <- predict(model_rfnew7,train_data)
# Error Mertics
library(DMwR)
test_error7=regr.eval(test_target,preds_rf7)
train_error7=regr.eval(train_target,preds_train_rf7)

error=data.frame(error,train_error7,test_error7)
########################  RF  #####################################################
# Store predictions from the model
preds_rf8 <- predict(model_rfnew8, test_data)
# Predict on the train data
preds_train_rf8 <- predict(model_rfnew8,train_data)
# Error Mertics
library(DMwR)
test_error8=regr.eval(test_target,preds_rf8)
train_error8=regr.eval(train_target,preds_train_rf8)

error=data.frame(error,train_error8,test_error8)
########################  RF  #####################################################
# Store predictions from the model
preds_rf9 <- predict(model_rfnew9, test_data)
# Predict on the train data
preds_train_rf9 <- predict(model_rfnew9,train_data)
# Error Mertics
library(DMwR)
test_error9=regr.eval(test_target,preds_rf9)
train_error9=regr.eval(train_target,preds_train_rf9)

error=data.frame(error,train_error9,test_error9)

###############################################################################
## Medain RMSE
train_median=median(sqrt(median((train_target- preds_train_rf)^2)))
test_median=median(sqrt(median((test_target- preds_rf)^2)))

error_median=data.frame(train_median,test_median)