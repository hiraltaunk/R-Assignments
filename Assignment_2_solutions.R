df1 <- read.csv("F:/Ivy-BigData/BigData-IVY-R/Packages/forestfires.csv")
summary(df1)
#Question1

transform(df1,X_square=X*X)

#Question2

mean_FMMC=mean(df1[,5])
print(mean_FMMC)

sum_FMMC=sum(df1[,5])
print(sum_FMMC)

median_FMMC=median(df1[,5])
print(median_FMMC)

sd_FMMC=sd(df1[,5])
print(mean_FMMC)

mean_DMC=mean(df1[,6])
print(mean_DMC)

sum_DMC=sum(df1[,6])
print(sum_DMC)

median_DMC=median(df1[,6])
print(median_DMC)

sd_DMC=sd(df1[,6])
print(mean_DMC)

mean_DC=mean(df1[,7])
print(mean_DC)

sum_DC=sum(df1[,7])
print(sum_DC)

median_DC=median(df1[,7])
print(median_DC)

sd_DC=sd(df1[,7])
print(mean_DC)

cbind(sd_DC,sd_DMC,sd_FMMC,mean_DC,mean_DMC,mean_FMMC,median_DC,median_DMC,median_FMMC,sum_DC,sum_DMC,sum_FMMC)

#Question 5

print(df1)
x=df1[,1]
y=df1[,2]
cor.test(x,y)


#################################################################
#Question4

?colnames()

x=df1[,3]
n=length(x)
print(n)
i=1
for(i in n)
{
  print("yes")
}

print(length(x))
i=1
f=function(x)
{
  for(i in length(x))
  {
    if(x[i]=="Jan")

    {
      month_new="january"
    }
    
    if(x[i]=="feb")
    {
      month_new="february"
    }
    
    if(x[i]=="mar")
    {
      month_new="march"
    }
    
    if(x[i]=="apr")
    {
      month_new="april"
    }
    
    if(x[i]=="may")
    {
      month_new="may"
    }
    
    if(x[i]=="jun")
    {
      month_new="june"
    }
    
    if(x[i]=="jul")
    {
      month_new="july"
    }
    
    if(x[i]=="aug")
    {
      month_new="august"
    }
    
    if(x[i]=="sep")
    {
      month_new="september"
    }
    
    if(x[i]=="oct")
    {
      month_new="october"
    }
    
    if(x[i]=="nov")
    {
      month_new="november"
    }
    
    if(x[i]=="dec")
    {
      month_new="december"
    }
    
  }
  
  print(month_new)
  return(month_new)
}


lapply(df1,month_new)

#worng

f=function(month_new)
{
  if(month=="aug")
  {
    month_new="august"
  }
  
else
  {
    month_new="unknown"
  }
  
}

print(month_new)

lapply(af1,f)
df1

#########################################################
#Question 6 

install.packages("dplyr")
library(dplyr)

summarise(group_by(df, month), sum_rain=sum(rain),sum_wind=sum(wind))

#Question 7 
summarise(group_by(df,month),mean_rain=mean(rain),mean_wind=mean(wind),count=n())

#Question 8 
summarise(group_by(df,month),count=n())

#Question 9 
summarise(group_by(df,month,day),count=n())

