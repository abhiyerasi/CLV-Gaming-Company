#DIY Set directory and read the data 
setwd("E:/Cute2/CSE7302c_CUTe01_Exam-Files/data/New folder")
moddata=read.csv("final.csv",header=T)

#moddata$RecencyAPP=ifelse(moddata$RecencyAPP<95,5,ifelse(moddata$RecencyAPP>=95&moddata$RecencyAPP<190,4,ifelse(moddata$RecencyAPP>=190&moddata$RecencyAPP<300,3,ifelse(moddata$RecencyAPP>=300&moddata$RecencyAPP<473,2,1))))
#moddata$Recencylf=ifelse(moddata$Recencylf<95,5,ifelse(moddata$Recencylf>=95&moddata$Recencylf<190,4,ifelse(moddata$Recencylf>=190&moddata$Recencylf<300,3,ifelse(moddata$Recencylf>=300&moddata$Recencylf<473,2,1))))

#moddata$FrequencyApp=ifelse(moddata$FrequencyApp<31,1,ifelse(moddata$FrequencyApp>=31&moddata$FrequencyApp<62,2,ifelse(moddata$FrequencyApp>=62&moddata$FrequencyApp<93,3,ifelse(moddata$FrequencyApp>=93&moddata$FrequencyApp<124,4,5))))
#moddata$FrequencyLF=ifelse(moddata$FrequencyLF<6,1,ifelse(moddata$FrequencyLF>=6&moddata$FrequencyLF<12,2,ifelse(moddata$FrequencyLF>=10&moddata$FrequencyLF<15,3,ifelse(moddata$FrequencyLF>=15&moddata$FrequencyLF<21,4,5))))

data=moddata
##################################################################################################
################ Data Preprocessing ######################################
# working with the missing values
# Replace the outlier with median valuas mean and median of the data are same.
data$MaxChildAge=ifelse(data$MaxChildAge>100,median(data$MaxChildAge),data$MaxChildAge)
data$MinChildAge=ifelse(data$MinChildAge>100,median(data$MaxChildAge),data$MinChildAge)

# Replace Na with the Median for thr age as there might be 1 children in the house
data$MaxChildAge[is.na(data$MaxChildAge)]=median(data$MaxChildAge,na.rm = T)
data$MinChildAge[is.na(data$MinChildAge)]=median(data$MaxChildAge,na.rm = T)
data$ChildAgeRange=data$MaxChildAge-data$MinChildAge
data$ChildAgeRange=ifelse(data$ChildAgeRange<0,0,data$ChildAgeRange)

# Fill all the NA with 0 in the units as they might have not bought in that particular time.
data[,7:12][is.na(data[,7:12])] <- 0

# Fill the Over all Transaction with 0000-00-00 as no transaction is been made
library(zoo)
data$OveralllastTransaction1=as.character(data$OveralllastTransaction)
data$OveralllastTransaction1[is.na(data$OveralllastTransaction)]="0000-00-00"

#Fill all the column of frequency with 0 where ever there is an NA.
data[,15:34][is.na(data[,15:34])] <- 0
data$Number_Games_Played[is.na(data$Number_Games_Played)]<-0
# Fill all the Column with 0 were ever there is an NA in Revenue,Recency,macrec columns.
data[,35:68][is.na(data[,35:68])] <- 0

# Fill Number of childrens females and males in household of NA value with median value
data$No_of_Childrens[is.na(data$No_of_Childrens)]=median(data$No_of_Childrens,na.rm=T)
data$NumFemaleChildrenHousehold[is.na(data$NumFemaleChildrenHousehold)]=median(data$NumFemaleChildrenHousehold,na.rm=T)
data$NumMaleChildrenHousehold[is.na(data$NumMaleChildrenHousehold)]=median(data$NumMaleChildrenHousehold,na.rm=T)

# Fill the NA in Time value with 0
data[,70:76][is.na(data[,70:76])] <- 0

# Fill the NA in Number of game played and bought value with 0
data[,78:84][is.na(data[,78:84])] <- 0

# Get levels and add "None"
# refactor FavoriteSource to include "None" as a factor level
# and replace NA with "None"
levels = levels(data$FavoriteSource)
levels[length(levels) + 1] = "None"
data$FavoriteSource <- factor(data$FavoriteSource, levels = levels)
data$FavoriteSource[is.na(data$FavoriteSource)] <- "None"


data$FavoriteSource7 <- factor(data$FavoriteSource7, levels = levels)
data$FavoriteSource7[is.na(data$FavoriteSource7)] <- "None"

data$FavoriteSource30 <- factor(data$FavoriteSource30, levels = levels)
data$FavoriteSource30[is.na(data$FavoriteSource30)] <- "None"


data$FavoriteSource90 <- factor(data$FavoriteSource90, levels = levels)
data$FavoriteSource90[is.na(data$FavoriteSource90)] <- "None"


data$FavoriteSource180 <- factor(data$FavoriteSource180, levels = levels)
data$FavoriteSource180[is.na(data$FavoriteSource180)] <- "None"


data$FavoriteSource360 <- factor(data$FavoriteSource360, levels = levels)
data$FavoriteSource360[is.na(data$FavoriteSource360)] <- "None"

# refactor FavoriteChannel to include "None" as a factor level
# and replace NA with "None"
levels2 = levels(data$FavoriteChannel)
levels2[length(levels2) + 1] = "None"
data$FavoriteChannel <- factor(data$FavoriteChannel, levels = levels2)
data$FavoriteChannel[is.na(data$FavoriteChannel)] <- "None"


data$FavoriteChannel7 <- factor(data$FavoriteChannel7, levels = levels2)
data$FavoriteChannel7[is.na(data$FavoriteChannel7)] <- "None"

data$FavoriteChannel30 <- factor(data$FavoriteChannel30, levels = levels2)
data$FavoriteChannel30[is.na(data$FavoriteChannel30)] <- "None"

data$FavoriteChannel90 <- factor(data$FavoriteChannel90, levels = levels2)
data$FavoriteChannel90[is.na(data$FavoriteChannel90)] <- "None"

data$FavoriteChannel180 <- factor(data$FavoriteChannel180, levels = levels2)
data$FavoriteChannel180[is.na(data$FavoriteChannel180)] <- "None"

data$FavoriteChannel360 <- factor(data$FavoriteChannel360, levels = levels2)
data$FavoriteChannel360[is.na(data$FavoriteChannel360)] <- "None"

# refactor FavoriteGame to include "None" as a factor level
# and replace NA with "None"
levels3 = levels(data$FavoriteGame)
levels3[length(levels3) + 1] = "None"
data$FavoriteGame <- factor(data$FavoriteGame, levels = levels3)
data$FavoriteGame[is.na(data$FavoriteGame)] <- "None"


data$FavoriteGame7 <- factor(data$FavoriteGame7, levels = levels3)
data$FavoriteGame7[is.na(data$FavoriteGame7)] <- "None"


data$FavoriteGame30 <- factor(data$FavoriteGame30, levels = levels3)
data$FavoriteGame30[is.na(data$FavoriteGame30)] <- "None"


data$FavoriteGame90 <- factor(data$FavoriteGame90, levels = levels3)
data$FavoriteGame90[is.na(data$FavoriteGame90)] <- "None"


data$FavoriteGame180 <- factor(data$FavoriteGame180, levels = levels3)
data$FavoriteGame180[is.na(data$FavoriteGame180)] <- "None"


data$FavoriteGame360 <- factor(data$FavoriteGame360, levels = levels3)
data$FavoriteGame360[is.na(data$FavoriteGame360)] <- "None"

levels4 = levels(data$Strength_of_FavoriteGame)
levels4[length(levels4) + 1] = "None"
data$Strength_of_FavoriteGame <- factor(data$Strength_of_FavoriteGame, levels = levels4)
data$Strength_of_FavoriteGame[is.na(data$Strength_of_FavoriteGame)] <- "None"

data=subset(data,select = -c(OveralllastTransaction))

data$TotalRevenueGenerated=ifelse(data$TotalRevenueGenerated<26,0,1)


###################################################################################

hopmod=subset(data,select=c(MaxChildAge,MinChildAge,UNITS,UNITS360, 
                            TenureDays,FreqGamePlay, 
                            Frequencylf30, FrequencyApp30, 
                            Recencylf30, Recencylf360,maxRecencyCum,minRecencyCum, 
                            No_of_Childrens,FrequencyApp360, FavoriteSource, FavoriteChannel, 
                            FavoriteGame, FavoriteSource30, FavoriteChannel30, FavoriteGame30, FreqGamePlay360,  
                            Revenue180, Revenue360,RecencyAPP,Recencylf,FrequencyApp,FrequencyLF,TotalRevenueGenerated))
#hopmod$RecencyAPP=as.factor(hopmod$RecencyAPP)
#hopmod$Recencylf=as.factor(hopmod$Recencylf)
#hopmod$FrequencyApp=as.factor(hopmod$FrequencyApp)
#hopmod$FrequencyLF=as.factor(hopmod$FrequencyLF)
hopmod$TotalRevenueGenerated=as.factor(hopmod$TotalRevenueGenerated)
data0=hopmod[hopmod$TotalRevenueGenerated=='0',]
data1=hopmod[hopmod$TotalRevenueGenerated=='1',]



# Spliting the data into train and test
library(caTools)
set.seed(123)
split = sample.split(data0$TotalRevenueGenerated, SplitRatio = 0.7)
data0_train=subset(data0, split == TRUE)
data0_test=subset(data0 , split == F)
split2 = sample.split(data1$TotalRevenueGenerated, SplitRatio = 0.7)
data1_train=subset(data1, split2 == TRUE)
data1_test=subset(data1 , split2 == F)


# Attach the above Data Frame
train_bin=rbind(data0_train,data1_train)
test_bin=rbind(data0_test,data1_test)
# Build the model
classifier2 <- glm(TotalRevenueGenerated ~ ., data = train_bin, family = "binomial")
summary(classifier2)


prob_train=predict(classifier2,type = "response")
head(prob_train)
prob_train
prob_test=predict(classifier2,test_bin[,-ncol(test_bin)],type = "response")
head(prob_train)
prob_train

par(mfrow=c(1,1))

library(ROCR)
pred=prediction(prob_train,train_bin$TotalRevenueGenerated)
pred

perf <- performance(pred, measure="tpr", x.measure="fpr")


plot(perf,main="ROC Curve" ,col=rainbow(10), colorize=T, 
     print.cutoffs.at=seq(0,1,0.1))

perf_auc <- performance(pred, measure="auc")
auc <- perf_auc@y.values[[1]]

print(auc)

cutoffs <- data.frame(cut= perf@alpha.values[[1]], 
                      fpr= perf@x.values[[1]], 
                      tpr=perf@y.values[[1]])
cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]

plot(perf, colorize = TRUE,main="ROC Curve", print.cutoffs.at=seq(0,1,by=0.1), 
     text.adj=c(-0.2,1.7))

perf_lift=performance(pred,measure="lift")
plot(perf_lift,main="Lift Curve")

pred_class=ifelse(prob_train>0.4,1,0)
trainconfuse=table(train_bin$TotalRevenueGenerated,pred_class)

prob_test=predict(classifier2,subset(test_bin,select = -c(TotalRevenueGenerated),type="response"))
preds_test=ifelse(prob_test>0.4,1,0)

confuse=table(test_bin$TotalRevenueGenerated,preds_test)

#Confusion matrix
library(caret) 
confusionMatrix(test_bin$TotalRevenueGenerated,preds_test,positive = )
confusionMatrix(trainconfuse)
