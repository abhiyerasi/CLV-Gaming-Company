########################################################################################
############################## CUTe02- 20170220- CSE7305c #######################################
################################## Machine Learning  ################
#########################################################################################
############################################### ########################################

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

#d. Build the regression model that gave you the Best MAPE and show the results on test data set
##########################################################################################
############################ Regression Simple###########################
##########################################################################################

# BUILD LINEAR REGRESSION MODEL 
LinReg1 =lm(formula =Customer_value~., data = Train)

# Build model with all attributes into model. 
# "Customer_value" is the target variable 
summary(LinReg1)

############################## Residuals plot for checking the assumptions ################

#Review the residual plots
par(mfrow=c(2,2))
plot(LinReg1)

resid=residuals(LinReg1)
table(resid)

lmtest::bptest(LinReg1,studentize = F)# Brusche Pangen Test for hetorscadacity

plot(LinReg1,which=4) # Cook distance Levrages
par(mfrow=c(1,1)) #reset default

##################### Checking the residuals values of mse rmse #################
# Error metrics evaluation on train data and test data
library(DMwR)

#Error verification on train data
train_error=regr.eval(Train$Customer_value, LinReg1$fitted.values) 
train_error_df=data.frame(train_error)

#Error verification on test data
test=subset(Test,select = -c(Customer_value))
target=Test$Customer_value
pred=predict(LinReg1,test)

Pred<- regr.eval(target, pred)

test_error_df=data.frame(Pred)
colnames(test_error_df)="test_error"

error_df=data.frame(train_error_df,test_error_df)

# ROOT OVER MEDIAN SQUARE ERRORS
train_median=median(sqrt(median((Train$Customer_value- LinReg1$fitted.values)^2)))
test_median=median(sqrt(median((Test$Customer_value- pred)^2)))

error_median=data.frame(train_median,test_median)

plot((Train$Customer_value- LinReg1$fitted.values)^2)
########################################################################################
################################################################################################

