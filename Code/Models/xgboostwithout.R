rm(list=ls(all=TRUE))
# install.packages("dummies")

# Load required libraries
library(vegan)
library(dummies)
library(xgboost)
# Read the Data Frame
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
set.seed(123)

index_train <- createDataPartition(final_Data$Customer_value, p = 0.7, list = F)

pre_train <- final_Data[index_train, ]
pre_test <- final_Data[-index_train,]
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



# Constructing the Dense matrix on the train and test data
dtrain = xgb.DMatrix(data = as.matrix(train_Data),
                     label = train_target)

dtest = xgb.DMatrix(data = as.matrix(test_Data),
                    label = test_target)

# fit the model
model = xgboost(data = dtrain, max.depth = 5, 
                eta = 0.4, nthread = 3, nround = 50, 
                objective = "reg:linear", verbose = 1)

# objective = "reg:linear": we will train a regression model ;
# max.deph = 5: the trees won't be deep, because our case is very simple ;
# nthread = 3: the number of cpu threads we are going to use;
# nround : max number of boosting iterations.
# eta : It controls the learning rate
# verbose = 1: print evaluation metric

# Both xgboost (simple) and xgb.train (advanced) functions train models.

# Because of the way boosting works, there is a time when having too many rounds lead to an overfitting. One way to measure progress in learning of a model is to provide to XGBoost a second dataset already classified. 
#Therefore it can learn on the first dataset and test its model on the second one.
#Some metrics are measured after each round during the learning.

#Use watchlist parameter. It is a list of xgb.DMatrix, 
#each of them tagged with a name.

watchlist = list(train=dtrain, test=dtest)

model = xgb.train(data=dtrain, max.depth=2,
                  eta=0.3, nthread = 2, nround=60, 
                  watchlist=watchlist,
                  eval.metric = "rmse", 
                  objective = "reg:linear", verbose = 1)
# eval.metric allows us to monitor two new metrics for each round, logloss and error.

importance <- xgb.importance(feature_names = names(train_Data), model = model)
print(importance)
xgb.plot.importance(importance_matrix = importance)

# Gain is the improvement in accuracy brought by a feature to the branches it is on. 
# Cover measures the relative quantity of observations concerned by a feature.
# Frequency is the number of times a feature is used in all generated trees. 

# save model to binary local file
xgb.save(model, "xgboost.model")
rm(model)

# load binary model to R
model <- xgb.load("xgboost.model")

# prediction on test data
pred <- predict(model, as.matrix(test_Data))
pred_train <- predict(model, as.matrix(train_Data))
# size of the prediction vector
print(length(pred))

# limit display of predictions to the first 10
print(head(pred))

# The numbers we get are probabilities that a datum will be classified as 1. 
# Therefore, we will set the rule that if this probability for a 
# specific datum is > 0.5 then the observation is classified as 1 (or 0 otherwise).
library(DMwR)
xgtest=regr.eval(test_target,pred)
xgtrain=regr.eval(train_target,pred_train)
errors=data.frame(xgtrain,xgtest)

# ROOT OVER MEDIAN SQUARE ERRORS
train_median = (sqrt(median((train_target -pred_train) ^ 2)))
test_median = (sqrt(median((test_target - pred) ^ 2)))

error_median = data.frame(train_median, test_median)