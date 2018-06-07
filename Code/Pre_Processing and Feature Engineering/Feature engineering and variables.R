###################### Selecting the Top Variables from the data frame ################################################################################################################ 
##################### Final data variables ######################################3
data=read.csv("preprocesseddata.csv")
#### Extracted out were the total revenue generated was 0
newdata=data[!(data$TotalRevenueGenerated==0),]

############################# Feature Engineering #############################

### Converting the frequency app and lf to one feature
newdata$Frequency=newdata$FrequencyApp+newdata$FrequencyLF
newdata$Frequency7=newdata$FrequencyApp7+newdata$Frequencylf7
newdata$Frequency30=newdata$FrequencyApp30+newdata$Frequencylf30
newdata$Frequency90=newdata$FrequencyApp90+newdata$Frequencylf90
newdata$Frequency180=newdata$FrequencyApp180+newdata$Frequencylf180
newdata$Frequency360=newdata$FrequencyApp360+newdata$Frequencylf360

### Converting the Revenue and Units to one Varibles
newdata$avg_order_value=newdata$TotalRevenueGenerated/newdata$UNITS
newdata$avg_order_value7=newdata$Revenue7/newdata$UNITS7
newdata$avg_order_value30=newdata$Revenue30/newdata$UNITS30
newdata$avg_order_value90=newdata$Revenue90/newdata$UNITS90
newdata$avg_order_value180=newdata$Revenue180/newdata$UNITS180
newdata$avg_order_value360=newdata$Revenue360/newdata$UNITS360


##### Purchase Frequency 
newdata$pur_freq=newdata$UNITS/length(unique(newdata$CONTACT_WID))
newdata$pur_freq7=newdata$UNITS7/length(unique(newdata$CONTACT_WID))
newdata$pur_freq30=newdata$UNITS30/length(unique(newdata$CONTACT_WID))
newdata$pur_freq90=newdata$UNITS90/length(unique(newdata$CONTACT_WID))
newdata$pur_freq180=newdata$UNITS180/length(unique(newdata$CONTACT_WID))
newdata$pur_freq360=newdata$UNITS360/length(unique(newdata$CONTACT_WID))

### MAX and Min recency
#newdata$lifespan=newdata$maxRecencyCum-newdata$minRecencyCum

###Customer Value = Average Order Value x Purchase Frequency

newdata$Customer_value=newdata$avg_order_value*newdata$pur_freq*10000
newdata$Customer_value7=newdata$avg_order_value7*newdata$pur_freq7*10000
newdata$Customer_value30=newdata$avg_order_value30*newdata$pur_freq30*10000
newdata$Customer_value90=newdata$avg_order_value90*newdata$pur_freq90*10000
newdata$Customer_value180=newdata$avg_order_value180*newdata$pur_freq180*10000
newdata$Customer_value360=newdata$avg_order_value360*newdata$pur_freq360*10000

#### Recency
newdata$Recency=ifelse((newdata$RecencyAPP>=0&newdata$Recencylf>=0),apply(subset(newdata,select = c(RecencyAPP,Recencylf)), 1, FUN=min),apply(subset(newdata,select = c(RecencyAPP,Recencylf)), 1, FUN=max))
newdata$Recency7=ifelse((newdata$RecencyAPP7>=0&newdata$Recencylf7>=0),apply(subset(newdata,select = c(RecencyAPP7,Recencylf7)), 1, FUN=min),apply(subset(newdata,select = c(RecencyAPP7,Recencylf7)), 1, FUN=max))
newdata$Recency30=ifelse((newdata$RecencyAPP30>=0&newdata$Recencylf30>=0),apply(subset(newdata,select = c(RecencyAPP30,Recencylf30)), 1, FUN=min),apply(subset(newdata,select = c(RecencyAPP30,Recencylf30)), 1, FUN=max))
newdata$Recency90=ifelse((newdata$RecencyAPP90>=0&newdata$Recencylf90>=0),apply(subset(newdata,select = c(RecencyAPP90,Recencylf90)), 1, FUN=min),apply(subset(newdata,select = c(RecencyAPP90,Recencylf90)), 1, FUN=max))
newdata$Recency180=ifelse((newdata$RecencyAPP180>=0&newdata$Recencylf180>=0),apply(subset(newdata,select = c(RecencyAPP180,Recencylf180)), 1, FUN=min),apply(subset(newdata,select = c(RecencyAPP180,Recencylf180)), 1, FUN=max))
newdata$Recency360=ifelse((newdata$RecencyAPP360>=0&newdata$Recencylf360>=0),apply(subset(newdata,select = c(RecencyAPP360,Recencylf360)), 1, FUN=min),apply(subset(newdata,select = c(RecencyAPP360,Recencylf360)), 1, FUN=max))


featured_df=subset(newdata,select = c(Country, MaxChildAge, MinChildAge, 
                                      ChildAgeRange, TenureDays, FreqGamePlay,
                                      FreqGamePlay7, FreqGamePlay30, 
                                      FreqGamePlay90, FreqGamePlay180, 
                                      FreqGamePlay360, Recencydown, 
                                      Recencydown7, Recencydown30, 
                                      Recencydown90, Recencydown180, 
                                      Recencydown360, No_of_Childrens, 
                                      Time, Time7, Time30, Time90, Time180, 
                                      Time360, NumFemaleChildrenHousehold, 
                                      NumMaleChildrenHousehold, 
                                      Number_Games_Played, Number_Games_Bought, 
                                      No_of_games_played7, No_of_games_played30, 
                                      No_of_games_played90, No_of_games_played180, 
                                      No_of_games_played360, FavoriteSource, 
                                      FavoriteChannel, FavoriteGame, 
                                      FavoriteSource7, FavoriteChannel7, 
                                      FavoriteGame7, FavoriteSource30, 
                                      FavoriteChannel30, FavoriteGame30,
                                      FavoriteSource90, FavoriteChannel90, 
                                      FavoriteGame90, FavoriteSource180, 
                                      FavoriteChannel180, FavoriteGame180, 
                                      FavoriteSource360, FavoriteChannel360,
                                      FavoriteGame360, Strength_of_FavoriteGame,
                                      OveralllastTransaction1, Frequency,
                                      Frequency7, Frequency30, Frequency90, 
                                      Frequency180, Frequency360, 
                                      Customer_value7, Customer_value90, 
                                      Customer_value180, Customer_value360, 
                                      Customer_value30, 
                                      maxRecencyCum,             
                                      minRecencyCum ,             maxRecencyCum7,            
                                      minRecencyCum7 ,            maxRecencyCum30,           
                                      minRecencyCum30 ,           maxRecencyCum90 ,          
                                      minRecencyCum90  ,          maxRecencyCum180 ,         
                                      minRecencyCum180  ,         maxRecencyCum360  ,        
                                      minRecencyCum360,Recency, Recency7, 
                                      Recency30, Recency90, Recency180, Recency360,
                                      pur_freq,avg_order_value,Customer_value
                                      
))




## Funstion for replacing all the values in a dataframe with Nan with 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

featured_df[is.nan(featured_df)] <- 0

summary(featured_df)
#######################################################################################

###################### Variables Selection ###################################

num=featured_df[sapply(featured_df,is.numeric)]
cat=featured_df[sapply(featured_df,is.factor)]
cat_target=data.frame(cat,num$Customer_value)

library(polycor)
corelation_num=cor(num)
write.csv(as.data.frame(as.table(corelation_num)),'co_num.csv')

corelation_cat=hetcor(cat_target)
cor_cat=corelation_cat$correlations
write.csv(as.data.frame(as.table(cor_cat)),'co_cat.csv')

### Categorical variables after co relation
'''
Country,FavoriteSource,FavoriteGame90,FavoriteChannel180,
FavoriteChannel360,FavoriteGame360,Strength_of_FavoriteGame
'''

### Numeric Variables
'''
Customer_value,Customer_value360,Frequency,Frequency360,
Customer_value180,Frequency7,Number_Games_Played,Customer_value30,
Frequency180,FreqGamePlay,Customer_value7,FreqGamePlay360,Frequency30,
No_of_Childrens,TenureDays,Recency90,FreqGamePlay90,Recency7,
NumFemaleChildrenHousehold,MaxChildAgeNumMaleChildrenHousehold,
Recencydown180,No_of_games_played7,Time,Recency360,FreqGamePlay7,
Time180,MinChildAge,Recencydown,Recency,pur_freq,avg_order_value,TotalRevenueGenerated
'''

hopmonkdata=subset(featured_df,select=c(Country,FavoriteSource,FavoriteGame90,FavoriteChannel180,
                                        Customer_value,Customer_value360,Frequency,Frequency360,
                                        Customer_value180,Frequency7,Number_Games_Played,Customer_value30,
                                        Frequency180,FreqGamePlay,Customer_value7,FreqGamePlay360,Frequency30,
                                        No_of_Childrens,TenureDays,Recency90,FreqGamePlay90,Recency7,
                                        MaxChildAge,
                                        Recencydown180,No_of_games_played7,Time,Recency360,FreqGamePlay7,
                                        Time180,pur_freq,avg_order_value,MinChildAge,Recencydown,maxRecencyCum,minRecencyCum,Recency))


write.csv(hopmonkdata,"hopmonk.csv")
