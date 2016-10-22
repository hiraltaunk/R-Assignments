?plot
install.packages("scatterplot3d")
library(scatterplot3d)
install.packages("rgl")
library(rgl)

 ?hist
install.packages("ggmap")
attach(df1)

summary(df1)
# Scatterplot matrix of DMC,DC,wind,rain,temp

pairs(~DMC+DC+wind+rain+temp,data=df1, 
      main="Scatterplot matrix of DMC,DC,wind,rain,temp")

# 3D Scatterplot of wind,rain,area

scatterplot3d(wind,rain,area,data="3D Scatterplot of wind,rain,area")

# Interactive 3D Scatterplot of wind,rain,area

library(rgl)
plot3d(wind,rain,area,data="Interactive 3D Scatterplot of wind,rain,area")


# Boxplot of X and Y

boxplot(X~Y,data=df, main="Boxplot", 
        xlab="X", ylab="Y")

# Simple bar plot of temp, wind, rain [horizontal and vertical]

count1 =table(temp)
barplot(count1, main="Temperature Distribution", xlab="Temperature")

barplot(count1, main="Temperature Distribution",horiz = TRUE, xlab="Temperature")

count2=table(wind)
barplot(count2,main="Wind Distribution",xlab="wind")

barplot(count2,main="Wind Distribution",horz=TRUE,xlab="wind")


count3=table(rain)
barplot(count3,main="rain Distribution",xalb="rain")

barplot(count3,main="rain Distribution",horz=TRUE,xalb="rain")

# Grouped bar plot of X and Y
attach(df1)
counts <- table(X,Y)
barplot(counts, main="Distribution by X and Y",
        xlab="X", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


# Histogram of probability distribution of X, Y, wind, temp, area along with line density
?hist()

hist(X, 
     main="Histogram of probability distribution of X", 
     xlab="X", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(X))

hist(Y, 
     main="Histogram of probability distribution of Y", 
     xlab="Y", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(Y))

hist(wind, 
     main="Histogram of probability distribution of wind", 
     xlab="wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(wind))

hist(temp, 
     main="Histogram of probability distribution of temp", 
     xlab="wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(temp))

hist(area, 
     main="Histogram of probability distribution of area", 
     xlab="area", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = TRUE)
lines(density(area))

# Histogram of frequency distribution of X, Y, wind, temp, area


hist(X, 
     main="Histogram of probability distribution of X", 
     xlab="X", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = FALSE)
lines(density(X))

hist(Y, 
     main="Histogram of probability distribution of Y", 
     xlab="Y", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = FALSE)
lines(density(Y))

hist(wind, 
     main="Histogram of probability distribution of wind", 
     xlab="wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = FALSE)
lines(density(wind))

hist(temp, 
     main="Histogram of probability distribution of temp", 
     xlab="wind", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = FALSE)
lines(density(temp))

hist(area, 
     main="Histogram of probability distribution of area", 
     xlab="area", 
     border="blue", 
     col="green", 
     las=1, 
     breaks=10, 
     prob = FALSE)
lines(density(area))

# Pie Chart of area, wind, rain, temp by month


library(dplyr)

df_pivot <- summarize(group_by(df1,month),area=sum(area))
slices <- df_pivot[["area"]] 
pie(slices, labels=df1[["month"]], main="Pie Chart of area")

df_pivot <- summarize(group_by(df,month),wind=sum(wind))
slices <- df_pivot[["wind"]] 
pie(slices, labels=df1[["month"]], main="Pie Chart of Wind")


df_pivot <- summarize(group_by(df,month),rain=sum(rain))
slices <- df_pivot[["rain"]] 
pie(slices, labels=df1[["month"]], main="Pie Chart of rain")

# Pie Chart of area, wind, rain, temp by day

df_pivot <- summarize(group_by(df1,day),area=sum(area))
slices <- df_pivot[["area"]] 
pie(slices, labels=df1[["day"]], main="Pie Chart of area")

df_pivot <- summarize(group_by(df1,day),wind=sum(wind))
slices <- df_pivot[["wind"]] 
pie(slices, labels=df1[["day"]], main="Pie Chart of wind ")

df_pivot <- summarize(group_by(df1,day),rain=sum(rain))
slices <- df_pivot[["rain"]] 
pie(slices, labels=df1[["day"]], main="Pie Chart of rain")

df_pivot <- summarize(group_by(df1,day),temp=sum(temp))
slices <- df_pivot[["temp"]] 
pie(slices, labels=df1[["day"]], main="Pie Chart of temp")

# Map Plot of sourceAirportID

airports <- read.csv("F:/Ivy-BigData/BigData-IVY-R/Packages/airports.dat") 
routes<-  read.csv("F:/Ivy-BigData/BigData-IVY-R/Packages/routes.dat")

colnames(airports) <- c("ID", "name", "city", "country", "IATA_FAA", "ICAO", "lat", "lon", "altitude", "timezone", "DST")
colnames(routes) <- c("airline", "airlineID", "sourceAirport", "sourceAirportID", "destinationAirport", "destinationAirportID", "codeshare", "stops", "equipment")

##attach(routes)
##attach(airports)
map <- get_map(location = 'World', zoom = 4)
airportA <- merge(airports, routes, by.x = "ID", by.y = "sourceAirportID")
mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(sourceAirportID)), data = airportA, alpha = .5)
mapPoints


# Map Plot of destinationAirportID


mapPoints <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = sqrt(destinationAirportID)), data = airportB, alpha = .5)
mapPoints
