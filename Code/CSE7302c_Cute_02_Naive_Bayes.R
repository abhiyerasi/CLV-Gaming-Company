rm(list=ls(all=TRUE))

library(e1071)

library(caret) # for box-cox transformation

library(lmtest) # bptest for testing heteroscedasticity

library(gvlma) # global validation of linear model assumptions

######################### Reading and seeing the structure of the data ###############

#DIY Set directory and read the data 
setwd("E:/Cute2/CSE7302c_CUTe01_Exam-Files/data/Hopmonk Final data with 30 Variables")
hopmod=read.csv("hopmonkdatawitgoodvalue.csv",header=T)
hopmod=subset(hopmod,select=-c(CONTACT_WID))

hopmod$TotalRevenueGenerated=ifelse(hopmod$TotalRevenueGenerated<10,1,2)
hopmod$TotalRevenueGenerated=as.factor(hopmod$TotalRevenueGenerated)

# Spliting the data into train and test
library(caTools)
set.seed(123)
split = sample.split(hopmod$TotalRevenueGenerated, SplitRatio = 0.7)
train_bin = subset(hopmod, split == TRUE)
test_bin = subset(hopmod, split == FALSE)

# Build the model
library(e1071)
classifier = naiveBayes(TotalRevenueGenerated~.,data = train_bin)
classifier

# Predict results on train data
train_pred <- predict(classifier, train_bin[,-ncol(train_bin)])

# Predict results on test data
test_pred <- predict(classifier, test_bin[,-ncol(test_bin)])

# Making the Confusion Matrix
train_cm=table(train_bin[,ncol(train_bin)],train_pred)
test_cm = table(test_bin[, ncol(test_bin)], test_pred)

# Auc Curve

library(ROCR)

ClassifierPred <- predict(classifier, subset(train_bin,select = -c(TotalRevenueGenerated)), type = 'class')

pred <- prediction(as.numeric(ClassifierPred), as.numeric(train_bin$TotalRevenueGenerated))

perf <- performance (pred, measure = "tpr", x.measure = "fpr")

cutoffs <- data.frame(cut= perf@alpha.values[[1]], 
                      fpr= perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]

plot(perf, col=rainbow(10), colorize=T,main="ROC Curve" ,print.cutoffs.at=seq(0,1,0.05))


# And then a lift chart
perf2 <- performance(pred,"lift","rpp")
plot(perf2, main="lift curve", colorize=T)

perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]
print(auc)


library(caret) 
train_stats=confusionMatrix(train_pred, train_bin$TotalRevenueGenerated, positive = "1")
train_stats[["byClass"]][7]
train_stats[["byClass"]][6]
test_stats=confusionMatrix(test_pred,test_bin$TotalRevenueGenerated,positive = "1")
test_stats[["byClass"]][7]
test_stats[["byClass"]][6]
############################################################################################
#############################################################################################
################################################################################################

