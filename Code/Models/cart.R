################# Model Building using Cart ####################################
##ii. CART

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

# Load all the Libraries Required
library(caret)
library(DMwR)

# Build a regression model using rpart 
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
DT_rpart_Reg <- rpart(Customer_value~., data=Train, method="anova",cp=0.0001) # Default cp = 0.01
printcp(DT_rpart_Reg)
plot(printcp(DT_rpart_Reg), type='b')

# Predict on Train and Test data
predCartTrain <- predict(DT_rpart_Reg, newdata=Train, type="vector")
predCartTest <- predict(DT_rpart_Reg, newdata=Train, type="vector")

# Error verification on Train and Test data
error_df <- data.frame(RPart_Reg1_Train = regr.eval(Train[,"Customer_value"], predCartTrain))
error_df <- data.frame(error_df, RPart_Reg1_Test = regr.eval(Test[,"Customer_value"], predCartTest))

# Model with cp. Giving value as 0.00184164 Function will stop at the best CP value
DT_rpart_Reg1 <- rpart(Customer_value~., data=Train, method="anova", 
                       control = rpart.control(cp =0.00184164 ))
printcp(DT_rpart_Reg1)
plot(printcp(DT_rpart_Reg1), type='b')
plotcp(DT_rpart_Reg1)

# Predict on Train and Test data
predCartTrain <- predict(DT_rpart_Reg1, newdata=Train, type="vector")
predCartTest <- predict(DT_rpart_Reg1, newdata=Test, type="vector")

# Error verification on Train and Test data
error_df <- data.frame(error_df, RPart_Reg2_Train = regr.eval(Train[,"Customer_value"], predCartTrain))
error_df <- data.frame(error_df, RPart_Reg2_Test = regr.eval(Test[,"Customer_value"], predCartTest))
#################################################################################################3

############### Decission Tree #########################################3
# Build Regression model using RPART
library(rpart)

#a. Build model
DT_rpart_class <- rpart(Train$Customer_value~., data=subset(Train,select=-c(Customer_value)), method="anova",control = rpart.control(minsplit=10, cp=0.001))
printcp(DT_rpart_class)
plotcp(DT_rpart_class)
DT_rpart_class
summary(DT_rpart_class)
plot(DT_rpart_class)
text(DT_rpart_class)
rpart.plot(DT_rpart_class)

#b. Predict "Customer_value" for train and test datasets
pred_Train = predict(DT_rpart_class, newdata=subset(Train,select=-c(Customer_value)), type="vector")
pred_Test = predict(DT_rpart_class, newdata=subset(Test,select=-c(Customer_value)), type="vector")


#C Error Metrics
library(DMwR)
train_error_minsplit=regr.eval(pred_Train,Train$Customer_value)
test_error_minsplit=regr.eval(pred_Test,Test$Customer_value)

# Model with cp
DT_rpart_class2 <- rpart(Train$Customer_value~., data=subset(Train,select=-c(Customer_value)), method="anova", 
                         control = rpart.control(cp = 0.0016176))
printcp(DT_rpart_class2)
plotcp(DT_rpart_class2)

# Predict "Customer_value" for train and test datasets
pred_Train1 = predict(DT_rpart_class2, newdata=subset(Train,select=-c(Customer_value)), type="vector")
pred_Test1=predict(DT_rpart_class2,newdata =subset(Test,select=-c(Customer_value)), type="vector" )

train_error_minsplit1=regr.eval(Train$Customer_value, pred_Train1)
test_error_minsplit1=regr.eval(Test$Customer_value,pred_Test1)
error=data.frame(error_df,train_error_minsplit,test_error_minsplit,train_error_minsplit1,test_error_minsplit1)


train_median=(sqrt(median((Train$Customer_value- pred_Train1)^2)))
test_median=(sqrt(median((Test$Customer_value- pred_Test1)^2)))

error_median=data.frame(train_median,test_median)

#####################################################################################
######################################################################################
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(Customer_value ~., data = Train, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
varImp(dtree_fit)
dtree_fit
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)


train_pred <- predict(dtree_fit, newdata = subset(Train,select=-c(Customer_value)),type = "raw")
regr.eval(Train$Customer_value ,train_pred) 
test_pred <- predict(dtree_fit, newdata = subset(Test,select=-c(Customer_value)),type = "raw")
regr.eval(Test$Customer_value,test_pred ) 

####################################################################################
set.seed(111)
dtree_fit_gini <- train(Customer_value ~., data = Train, method = "rpart",
                        parms = list(split = "gini"),
                        trControl=trctrl,
                        tuneLength = 10)
varImp(dtree_fit_gini)
dtree_fit_gini
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1.2)


train_pred_gini <- predict(dtree_fit_gini, newdata = subset(Train,select=-c(Customer_value)),"raw")
regr.eval(train_pred_gini, Train$Customer_value ) 
test_pred_gini <- predict(dtree_fit_gini, newdata = subset(Test,select=-c(Customer_value)),"raw")
regr.eval(test_pred_gini, Test$Customer_value ) 

#####################################################################