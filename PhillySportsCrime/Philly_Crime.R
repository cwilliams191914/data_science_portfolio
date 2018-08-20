
library(ggplot2)


#load data
getwd()
setwd("C:/Users/JW19274/Desktop/Data Science Projects/Data Visualization/Philly_Crime/Data")

getwd()

CrimeData <- read.csv("CrimeData2.csv")

str(CrimeData)

#1.  Overall trend in crimes for the whole period of time in the dataset. The 
#    granularity should be at the Day level. 
head(CrimeData$Dispatch_Date_Time)
CrimeData$DateTime <- as.POSIXct(CrimeData$Dispatch_Date_Time, format="%Y-%m-%d %H:%M:%S", tz="EST")
?POSIXct
head(CrimeData$Dispatch_Date_Time)
head(CrimeData$DateTime)

#Create Date Column
CrimeData$Date <- as.Date(CrimeData$DateTime)
str(CrimeData)

#Count number of crimes by day
by_date <- aggregate(CrimeData$Date, by = list(Date = CrimeData$Date), FUN = length)
?aggregate
str(by_date)

#Rename columns
colnames(by_date) <- c("Date", "Total")

#Plot the result
x <- ggplot(by_date, aes(Date, Total,color="Total")) + geom_line() 
x <- x +  scale_color_manual(values= "#4cbb17")
x <- x + ggtitle("Crimes By Date")
# Remove legend
x <- x + theme(legend.position="none")
x


#2.  Which are the most and the least dangerous hours in Philadelphia? 

#Get hours from Datetime column
CrimeData$Hour <- strftime(CrimeData$DateTime, format = '%H')
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

#plot the result in Sixers colors
a <-ggplot(by_hour, aes(Hour, Total)) +
  geom_line(colour="white") +
  ggtitle("Crimes By Hour") +
  xlab("Hour of the Day") +
  ylab("Total Crimes")

a <- a + theme(panel.grid.major = element_line(colour = "red"), panel.grid.minor = element_line(colour = "red"))
a <- a + theme(panel.background = element_rect(fill = 'blue', colour = 'red'))
a

#3.  Is there any seasonality in the crime rate? 

#Get months from Datetime column
CrimeData$Month <- strftime(CrimeData$DateTime, format = '%m')
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

#plot the result 1970 to 1991 Phillies colors
ggplot(by_month, aes(Month, Total)) +
  geom_bar(fill="#800020", stat="identity", colour= "#ADD8E6")+
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

#Plot the result in Philly Flyers colors
y <- ggplot(by_hq, aes(reorder(HQ, -Total), Total)) +
  geom_bar(fill = "#F74902", color = "black", stat="identity")

y <- y + theme_bw()
y <-  y + theme(panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
panel.border = element_blank(),
panel.background = element_blank())
y



