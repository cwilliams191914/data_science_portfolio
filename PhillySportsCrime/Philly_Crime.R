
library(ggplot2)


#load data
getwd()
setwd("C://Users//Kirill//Desktop//CS002")
#MAC: setwd("/Users/Kirill/Desktop/CS002")
getwd()

CrimeData <- read.csv("CrimeData210K.csv")

str(CrimeData)

#1.  Overall trend in crimes for the whole period of time in the dataset. The 
#    granularity should be at the Day level. 
head(CrimeData$Dispatch_Date_Time)
CrimeData$DateTime <- as.POSIXct(CrimeData$Dispatch_Date_Time, format="%Y-%m-%d %H:%M:%S", tz="EST")
?POSIXct
head(CrimeData$Dispatch_Date_Time)
head(CrimeData$DateTime)

#Create Date Column
CrimeData$Date <- as.Date(CrimeData$DateTime, tz="EST")
str(CrimeData)

#Count number of crimes by day
by_date <- aggregate(CrimeData$Date, by = list(Date = CrimeData$Date), FUN = length)
?aggregate
str(by_date)

#Rename columns
colnames(by_date) <- c("Date", "Total")

#Plot the result
ggplot(by_date, aes(Date, Total, color=Total)) + geom_line()


#2.  Which are the most and the least dangerous hours in Philadelphia? 

#Get hours from Datetime column
CrimeData$Hour <- strftime(CrimeData$DateTime, format = '%H', tz='EST')
str(CrimeData)

#Aggregate by hour
by_hour <- aggregate(CrimeData$Hour, by = list(Hour = CrimeData$Hour), FUN=length)
by_hour

#rename columns
colnames(by_hour) <- c("Hour", "Total")
str(by_hour)

#convert categorical hours to integer
by_hour$Hour <- as.integer(by_hour$Hour)
str(by_hour)

#plot the result
ggplot(by_hour, aes(Hour, Total)) +
  geom_line(colour="Red") +
  ggtitle("Crimes By Hour") +
  xlab("Hour of the Day") +
  ylab("Total Crimes")
  

#3.  Is there any seasonality in the crime rate? 

#Get months from Datetime column
CrimeData$Month <- strftime(CrimeData$DateTime, format = '%m', tz='EST')
str(CrimeData)

#Aggregate by hour
by_month <- aggregate(CrimeData$Month, by = list(Month = CrimeData$Month), FUN=length)
by_month

#rename columns
colnames(by_month) <- c("Month", "Total")
str(by_month)

#convert categorical hours to integer
by_month$Month <- as.integer(by_month$Month)
str(by_month)

#plot the result
ggplot(by_month, aes(Month, Total)) +
  geom_bar(fill="Maroon", stat="identity")+
  ggtitle("Crimes By Month") +
  xlab("Month of the Day") +
  ylab("Total Crimes")


#4.  What are the top 10 crimes crime types? 

#count by type
by_category <- aggregate(CrimeData$Text_General_Code,
                         by = list(Typec = CrimeData$Text_General_Code),
                         FUN = length)
by_category

#rename columns
colnames(by_category) <- c("Type", "Total")
by_category

#sort (Powerful Technique)
by_category_sorted <- by_category[order(by_category$Total, decreasing=T),]
by_category_sorted
?order

#select top 10 crimes
top10crimes <- by_category_sorted[1:10,]
top10crimes

#plot the result
ggplot(top10crimes, aes(x=reorder(Type,Total), y=Total)) +
  geom_bar(aes(fill=Type), stat="identity") +
  coord_flip()


#5.  Which police HQ is in the most need of strengthening? 

#count crimes by HQ
by_hq <- aggregate(CrimeData$Dc_Dist, by = list(HQ = CrimeData$Dc_Dist), FUN=length)

#rename columns
colnames(by_hq) <- c("HQ", "Total")

#Plot the result
ggplot(by_hq, aes(reorder(HQ, -Total), Total)) +
  geom_bar(color = "blue", stat="identity")





