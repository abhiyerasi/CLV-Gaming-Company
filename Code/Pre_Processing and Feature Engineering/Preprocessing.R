############################ Data Preprocessing #####################################
# Read the main final data frame
data=read.csv("Final 108 Variables.csv",na.strings = c("",NA))
colnames(data)[25]="FrequencyApp7"

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
data[,7:12][is.na(data[,7:12])] = 0

# Fill the Over all Transaction with 0000-00-00 as no transaction is been made
library(zoo)
data$OveralllastTransaction1=as.character(data$OveralllastTransaction)
data$OveralllastTransaction1[is.na(data$OveralllastTransaction)]="0000-00-00"

#Fill all the column of frequency with 0 where ever there is an NA.
data[,15:33][is.na(data[,15:33])] = 0

data$Number_Games_Played[is.na(data$Number_Games_Played)]=0

# Fill all the Column with 0 were ever there is an NA in Revenue.
data[,34:38][is.na(data[,34:38])] = 0

# Fill all the Column with -1 were ever there is an NA in Recency max recencya and min recency as they had not opened or downloaded the app.
data[,39:68][is.na(data[,39:68])] = -1

# Fill Number of childrens females and males in household of NA value with median value
data$No_of_Childrens[is.na(data$No_of_Childrens)]=median(data$No_of_Childrens,na.rm=T)
data$NumFemaleChildrenHousehold[is.na(data$NumFemaleChildrenHousehold)]=median(data$NumFemaleChildrenHousehold,na.rm=T)
data$NumMaleChildrenHousehold[is.na(data$NumMaleChildrenHousehold)]=median(data$NumMaleChildrenHousehold,na.rm=T)

# Fill the NA in Time value with 0
data[,70:75][is.na(data[,70:75])] <- 0

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


write.csv(data,"preprocesseddata.csv")
##################################################################################################
####################### Univariate Analysis and Plots ####################################
library(ggplot2)
ggplot(data,aes(x=Country))+geom_bar()+ggtitle("Number of Unique Users in Different Countries")
ggplot(data,aes(x=Country,fill=FavoriteChannel))+geom_bar()+ggtitle("Favourite Channel Of Transaction")
ggplot(data,aes(x=Country,fill=FavoriteSource))+geom_bar()+ggtitle("Favourite Source")

ggplot(data,aes(x=NumMaleChildrenHousehold,y=NumFemaleChildrenHousehold))+geom_bin2d(col="red")+ggtitle("Number of Male childrens to Female childrens in the Household")+
  facet_wrap(~Country)

ggplot(data,aes(x=No_of_Childrens,fill=FavoriteSource))+geom_bar()+ggtitle("Number of Childrens in the House Hold in Different Country")+
  facet_wrap(~Country)

ggplot(data,aes(x=No_of_Childrens,fill=FavoriteSource))+geom_bar()+ggtitle("Number of Childrens in the House Hold in Different Country")+
  facet_wrap(~Country)

ggplot(data,aes(x=UNITS))+geom_bar()+ggtitle("Revenue Plot")+
  facet_wrap(~Country)


# Data by country and UNITS
data_country_units=subset(data,select = c(Country,UNITS))
data_country_units=aggregate(UNITS~Country,data_country_units,FUN=sum)
plot.table(data_country_units)
country_units.plot <- barplot(data_country_units$UNITS, names.arg = data_country_units$Country,
                              ylab="Units",col=rainbow(20), xlab = "Country",las=1,main ="No of Units Sold in each Country",ylim = c(0,100000))
# Most Frequently Played Games 
# calculate frequencies
tab <- table(data$FavoriteGame)
# sort
tab_s <- data.frame(sort(tab))
# extract 10 most frequent nationalities
top5 <- data.frame(tail(tab_s, 5))
top5=top5[1:4,]
# plot for the top played games ######################### 
ggplot(top5,aes(Var1,Freq))+geom_count()
barplot(top5$Freq,names.arg = top5$Var1,ylab = "No of Times the Game Played by Different Users",cex.names=0.8,xlab = "Name of the Game",main = "Top 5 Games",col = 'Red')
#################################################################################################################################


