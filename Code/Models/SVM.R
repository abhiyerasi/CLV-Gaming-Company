#a. You need to use the data set you prepared in the previous CUTe exam. Since there is a change in your team members, take the data set among your new team members that got the best MAPE.
setwd('E:\\Cutes\\CUTe03\\New folder')
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
target=subset(numeric,select=c(Customer_value))

cat=hopmonkdata[sapply(hopmonkdata,is.factor)]

#c. StandardizationofData
#library(vegan)
#numeric=decostand(numeric,"range")

#dummify the cat data
cat_edu=model.matrix(~Country+FavoriteSource+FavoriteGame90+FavoriteChannel180,data = cat)[,-1]
cat=subset(cat,select=-c(Country,FavoriteSource,FavoriteGame90,FavoriteChannel180))
datanew=data.frame(numeric,cat,cat_edu,target)

#d. Split the data into train and test datasets
set.seed(123) # for reproducing results
rowIndices <- 1 : nrow(banknew) # prepare row indices
sampleSize <- 0.8 * length(rowIndices) # training sample size
trainingRows <- sample (rowIndices, sampleSize) # random sampling
trainingData <- datanew[trainingRows, ] # training data
testData <- datanew[-trainingRows, ] # test data

#3. Model Building

library(e1071)
x =subset(trainingData,select =-Customer_value) #removeresponse variable 
y =(trainingData$Customer_value)

model = svm(x,y,kernel = "linear",method="nu-regression") 
summary(model) #Interpretation of summary
plot(model)
# Predict the values on the train and test dataset
pred_train=predict(object = model,newdata = x)
pred_test=predict(object = model,newdata = subset(testData,select = -c(Customer_value)))


# Error metrics building for error metrics.
library(DMwR)
train_error=regr.eval(y,pred_train)
test_error=regr.eval(testData$Customer_value, pred_test)

# ROOT OVER MEDIAN SQUARE ERRORS
train_median =sqrt(median((y - pred_train) ^ 2))
test_median = sqrt(median((testData$Customer_value - pred_test) ^ 2))

error_median = data.frame(train_median, test_median)
#####################################################################################
#3. Model Building

library(e1071)
x1 =subset(trainingData,select =-Customer_value) #removeresponse variable 
y1 =(trainingData$Customer_value)

model = svm(x1,y1,kernel = "radial",method="eps-regression") 
summary(model) #Interpretation of summary

# Predict the values on the train and test dataset
pred_train=predict(object = model,newdata = x1)
pred_test=predict(object = model,newdata = subset(testData,select = -c(Customer_value)))

# Error matrix building for error metrics.
library(DMwR)
regr.eval(y1,pred_train)
regr.eval(testData$Customer_value, pred_test)

# ROOT OVER MEDIAN SQUARE ERRORS
train_median = median(sqrt(median((y1 - pred_train) ^ 2)))
test_median = median(sqrt(median((testData$Customer_value - pred_test) ^ 2)))

error_median = data.frame(train_median, test_median)
#####################################################################################
library(e1071)
x1 =subset(trainingData,select =-Customer_value) #removeresponse variable 
y1 =(trainingData$Customer_value)

model = svm(x1,y1,kernel = "polynomial",method="eps-regression") 
summary(model) #Interpretation of summary

# Predict the values on the train and test dataset
pred_train=predict(object = model,newdata = x1)
pred_test=predict(object = model,newdata = subset(testData,select = -c(Customer_value)))

# Confusion matrix building for error metrics.
library(DMwR)
regr.eval(y1,pred_train)
regr.eval(testData$Customer_value,pred_test)
