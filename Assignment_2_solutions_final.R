df1 <- read.csv("F:/Ivy-BigData/BigData-IVY-R/Packages/forestfires.csv") 
summary(df1) 

#Question1

transform(df1,X_square=X*X) 


#Question2

mean_FMMC=mean(df1[,5]) 

sum_FMMC=sum(df1[,5]) 

median_FMMC=median(df1[,5]) 

sd_FMMC=sd(df1[,5]) 

mean_DMC=mean(df1[,6]) 

sum_DMC=sum(df1[,6]) 

median_DMC=median(df1[,6]) 

sd_DMC=sd(df1[,6]) 

mean_DC=mean(df1[,7]) 

sum_DC=sum(df1[,7]) 

median_DC=median(df1[,7]) 

sd_DC=sd(df1[,7]) 

cbind(sd_DC,sd_DMC,sd_FMMC,mean_DC,mean_DMC,mean_FMMC,median_DC,median_DMC,median_FMMC,sum_DC,sum_DMC,sum_FMMC) 


#Question 3 
df1$Month <-  
  sapply(df$month,function(x){  
    if(x=="jan"){  
      x <- as.factor("January")  
    }  
    if(x=="feb"){  
      x <- as.factor("February")  
    }  
    if(x=="mar"){  
      x <- as.factor("March")  
    }  
    if(x=="apr"){  
      x <- as.factor("April")  
    }  
    if(x=="may"){  
      x <- as.factor("May")  
    }  
    if(x=="jun"){  
      x <- as.factor("June")  
    }  
    if(x=="jul"){  
      x <- as.factor("July")  
    }  
    if(x=="aug"){  
      x <- as.factor("August")  
    }  
    if(x=="sep"){  
      x <- as.factor("September")  
    }  
    if(x=="oct"){  
      x <- as.factor("October")  
    }  
    if(x=="nov"){  
      x <- as.factor("November")  
    }  
    if(x=="dec"){  
      x <- as.factor("December")  
    }  
    return( x )  
  }  
  ) 

print(df1)


# Question 4

df1$Day_num <-  
  sapply(df$day,function(x){  
    if(x=="sun"){  
      x <- as.factor("1")  
    }  
    if(x=="mon"){  
      x <- as.factor("2")  
    }  
    if(x=="tue"){  
      x <- as.factor("3")  
    }  
    if(x=="wed"){  
      x <- as.factor("4")  
    }  
    if(x=="thu"){  
      x <- as.factor("5")  
    }  
    if(x=="fri"){  
      x <- as.factor("6")  
    }  
    if(x=="sat"){  
      x <- as.factor("7")  
    }  
    
    return( x )  
  }  
  ) 

print(df1)


#Question5 

print(df1) 
x=df1[,1] 
y=df1[,2] 
cor.test(x,y) 

# Question 6
install.packages("dplyr") 
library(dplyr) 
summarise(group_by(df, month), sum_rain=sum(rain),sum_wind=sum(wind)) 


#Question 7 

summarise(group_by(df,month),mean_rain=mean(rain),mean_wind=mean(wind),count=n()) 


#Question 8

summarise(group_by(df,month),count=n()) 


#Question 9

summarise(group_by(df,month,day),count=n()) 
