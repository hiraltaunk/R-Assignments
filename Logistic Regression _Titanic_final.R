#read the training data into a dataframe called train
train= read.table("F:/Ivy-BigData/BigData-IVY-R/Class/logistic regression/train.csv",
                    header = TRUE, sep = ",")

#read the test data into a dataframe named test
test= read.table("F:/Ivy-BigData/BigData-IVY-R/Class/logistic regression/train.csv",
                  header = TRUE, sep = ",")

head(train)


ls.str(train)##check the type of variable 

#?factor()
#set the pclass, passengers pseudoclass, to be ordered categorical
train$pclass =factor(train$Pclass,levels = c(3, 2, 1), ordered = TRUE)

#pclass is categorical for test data also

test$pclass =factor(test$Pclass,levels = c(3, 2, 1), ordered = TRUE)

head(train)



#Data Cleaning

#Count the number of NA values in each column
apply(test, 2, FUN = function(x) sum(is.na(x)))
print(train$Age) #Only age column has na values
test$Age[is.na(test$Age)]=mean(test$Age,na.rm=T)

model=glm(Survived ~.,family =binomial(link='logit'),data=train)
summary(model)

#prop.table(mytable) # cell percentages
#prop.table(mytable, 1) # row percentages 
#prop.table(mytable, 2) # column percentages 

###Check how catogotical variable effects the survival rate 

#how Class effects the survival
Pclass_survival <- prop.table(table(train$Survived,train$Pclass),2) #Calculate the column percentages
barplot(Pclass_survival, main = "Survival rate by Pclass", xlab = "Pclass", ylab = "Percent Survived or Perished", col = c("red", "green"))
#we can see approx 40% class1 ,50% class2 and 75% class3 did not survive


#how sex affects survival
sex_survival = prop.table(table(train$Survived,train$Sex),2)
barplot(sex_survival, main = "Survival rate by sex", xlab = "sex", ylab = "Percent Survived or Perished", col = c("red", "green"))
#We can see approx 78% male and 22% female did not survive

#How embarked affects survival

embarked_survival = prop.table(table(train$Survived,train$Embarked),2)
barplot(embarked_survival, main = "Survival rate by embarked", xlab = "sex", ylab = "Percent Survived or Perished", col = c("red", "green"))
# no conclusion can be made from this


#how siblings/spouses affects the survival
siblings_survival = prop.table(table(train$Survived,train$SibSp),2)
barplot(siblings_survival, main = "Survival rate by sibsp", xlab = "sex", ylab = "Percent Survived or Perished", col = c("red", "green"))
#more number of  siblings/spouses less likely to 

#how parents childres aboard affects the survival
parch_survival = prop.table(table(train$Survived,train$Parch),2)
barplot(parch_survival, main = "Survival rate by parch", xlab = "sex", ylab = "Percent Survived or Perished", col = c("red", "green"))
#more number of parents/children means less likely to survive

#how age effects 

age_new=cut(train$Age,breaks = seq(0,100,by=10))
age_survival = prop.table(table(train$Survived,age_new),2)
barplot(age_survival, main = "Survival rate by age", xlab = "sex", ylab = "Percent Survived or Perished", col = c("red", "green"))


#create a super simple logistic regression model with the training data
#predicting survival based on passenger class,sex,age,sibsp,parch,family

logistic.model = glm(Survived ~ Pclass + Sex + Age +SibSp + Parch, family = binomial(), data=train)

summary(logistic.model)#AIC=648.62




#predicting test values
test_predictions = predict(logistic.model,newdata = test, type = "response")
test_predictions=ifelse(test_predictions>0.5,1,0)

#training error using cutoff at 0.5
test_error=mean(test_predictions!=train$Survived)
Accuracy=1-test_error
print(Accuracy)#Getting accuracy of 0.7901 so good predictive ability



write.table(test_predictions, "F:/Ivy-BigData/BigData-IVY-R/Class/logistic regression/test.csv",col.names = F,row.names=F,quote=FALSE)
