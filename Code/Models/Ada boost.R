rm(list=ls(all=TRUE))
# install.packages("dummies")

# Load required libraries
library(vegan)
library(dummies)
library(xgboost)

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


numeric=hopmonkdata[sapply(hopmonkdata,is.numeric)]
numeric=subset(numeric,select=-c(Customer_value))

target=subset(hopmonkdata,select=c(Customer_value))

cat=hopmonkdata[sapply(hopmonkdata,is.factor)]

# Convert all categorical attributes to numeric 
# 1. Using dummy function, convert education and family categorical attributes into numeric attributes 
library(dummies)
cat=dummy.data.frame(cat)
final_Data=data.frame(numeric,cat,target)
#############################################################
library(caret)
set.seed(1234)

index_train <- createDataPartition(final_Data$Customer_value, p = 0.7, list = F)

pre_train <- final_Data[index_train, ]
tar_cal=pre_train$Customer_value
pre_test <- final_Data[-index_train,]
tes_cal=pre_test$Customer_value
################################
pre_train$Customer_value=(pre_train$Customer_value-0.05844956)/(18.32587-0.05844956)

normalised_train=pre_train$Customer_value

pre_train$Customer_value=ifelse(pre_train$Customer_value>0.5,1,0)
##################################
pre_test$Customer_value=(pre_test$Customer_value-0.05844956)/(18.32587-0.05844956)

normalised_test=pre_test$Customer_value

pre_test$Customer_value=ifelse(pre_test$Customer_value>0.5,1,0)
########################

# Decoupling target column
train_target <- pre_train$Customer_value
test_target <- pre_test$Customer_value
pre_train$Customer_value <- NULL
pre_test$Customer_value <- NULL

# Standardize all the real valued variables in the dataset as some models we use might be impacted due to non standardized variables
std_method <- preProcess(pre_train, method = c("center", "scale"))
train_Data <- predict(std_method, pre_train)
test_Data <- predict(std_method, pre_test)
# Let's use the preProcess() function from the caret package to standardize the variables, using just the data points in the training data

#############################################################################

#------------------------------------------------------
# ADA BOOST

library(ada) 
model = ada(train_target ~ ., iter = 40,data = train_Data, loss="e") 
# iter = 20 Iterations 
model
#out of bag error: its the error value on the variables which were not picked on baggging. 
# Incase we have ran a model,error would have been out of bag error value.

# predict the values using model on test and train data sets. 
pred_train = predict(model,train_Data,type="prob")
pred_test = predict(model, test_Data,type="prob")

#### Train
denormalize_train=(normalised_train*(max(pred_train)-min(pred_train))+min(pred_train))
totaldenormalizzation_train=(denormalize_train*(18.32587-0.05844956))+0.05844956
#### Test
denormalize_test=(normalised_test*(max(pred_test)-min(pred_test))+min(pre_test))
totaldenormalizzation_test=(denormalize_test*(18.32587-0.05844956))+0.05844956

# Error Metrics calculation
library(DMwR)
regr.eval(tar_cal,totaldenormalizzation_train)
regr.eval(tes_cal,totaldenormalizzation_test)

# ROOT OVER MEDIAN SQUARE ERRORS
train_median = (sqrt(median((tar_cal - totaldenormalizzation_train) ^ 2)))
test_median = (sqrt(median((tes_cal - totaldenormalizzation_test) ^ 2)))

error_median = data.frame(train_median, test_median)