setwd("D:/HIRAL_Backup/D_Backup/IVY-Big data/R/Linear Regression/")

train <- read.csv("input/train.csv", header = T) #Load train dataset in train dataframe

test <- read.csv("input/test.csv", header = T) #Load test dataset in test dataframe
store <- read.csv("input/store.csv") #Load store dataset in store dataframe

?merge
train <- merge(train,store)
test <- merge(test,store)



munge_data <- function(dt){
  
  #Delete row that have open=0 as sales will be zero
  with(dt,subset(dt,dt$Open==1))
  
  # replacing NA's by the mean value  
  dt$CompetitionDistance[is.na(dt$CompetitionDistance)] = round(mean(dt$CompetitionDistance, na.rm = T))
  dt$CompetitionOpenSinceMonth[is.na(dt$CompetitionOpenSinceMonth)] = round(mean(dt$CompetitionOpenSinceMonth, na.rm = T))
  dt$CompetitionOpenSinceYear[is.na(dt$CompetitionOpenSinceYear)] = round(mean(dt$CompetitionOpenSinceYear, na.rm = T))
  dt$Promo2SinceWeek[is.na(dt$Promo2SinceWeek)] = round(mean(dt$Promo2SinceWeek, na.rm = T))
  dt$Promo2SinceYear[is.na(dt$Promo2SinceYear)] = round(mean(dt$Promo2SinceYear, na.rm = T))
  dt$Open[is.na(dt$Open)] = round(mean(dt$Open, na.rm = T))
  
  # converting to numeric
  dt$StateHoliday = as.numeric(dt$StateHoliday)
  dt$StoreType = as.numeric(dt$StoreType)
  dt$Assortment = as.numeric(dt$Assortment)
  dt$PromoInterval = as.numeric(dt$PromoInterval)
  
  # seperating out the elements of the date column for the dt set
  dt$Date = as.Date(dt$Date, format = "%Y-%m-%d")
  dt$month <- as.integer(format(dt$Date, "%m"))
  dt$year <- as.integer(format(dt$Date, "%y"))
  dt$day <- as.integer(format(dt$Date, "%d"))
  
  # removing the date columns (Date not used) (Customers affect simetry) (CompetitionOpenSinceYear not relevant/correlated)
  dt$Date = NULL
  dt$Customers = NULL
  dt$CompetitionOpenSinceYear = NULL
  
  
  return(dt)
}


train = munge_data(train)
test = munge_data(test)

plot(train$Store,train$Sales)

cor(train$Store,train$Sales)#0.00512
cor(train$DayOfWeek,train$Sales)#-0.462
cor(train$StateHoliday,train$Sales)#-0.229
cor(train$SchoolHoliday,train$Sales)#-0.085124
cor(train$CompetitionDistance,train$Sales)#-0.0192
cor(train$Promo,train$Sales)#0.4592
cor(train$Promo2,train$Sales)#-0.09103
cor(train$Promo2SinceWeek,train$Sales)#-0.037148
cor(train$Promo2SinceYear,train$Sales)#-0.00402 
cor(train$Assortment,train$Sales)#-0.074

cor(train)

?lm()

# sales,dayofweek,open,promo,stateholiday,school holiday has high correlation value so thaen that for predicting
mod = lm(Sales ~ train$Sales+train$DayOfWeek+train$Open+train$Promo+train$StateHoliday+train$SchoolHoliday+train$Promo2+train$CompetitionDistance+train$Promo2SinceWeek,data = train) #. is used to use all the data

summary(mod)

#R2=0.5553 for original test data  

y = predict(mod, newdata = test)

submission <- data.frame(y)
#submission <- data.frame(Id=test$Id, Sales=y)

write.csv(submission, "predicted.csv")
