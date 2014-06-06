#Loading and preparing the data

#reading the table
raw.data<-read.csv("activity.csv")

head(raw.data)

#ignore the missing values
data.no.NA<-subset(raw.data,is.na(raw.data[,"steps"])==FALSE)
head(data.no.NA)
#I want use the ggplot2 plotting system
library(ggplot2)

#date is  factor
#is.factor(data$date)
date.names<-levels(factor(raw.data$date))
date.names<-paste(substring(date.names,6,7),"/",
                  substring(date.names,9,10),sep="")
#date.names<-strptime(date.names,format="%m%d",tz="")

#the total number of steps taken per day
#ignoring the missing values
total.step.day<-tapply(data.no.NA$steps,data.no.NA$date,sum)
tsd<-data.frame(date=date.names,tsd=total.step.day)

g<-ggplot(tsd,aes(x=tsd))
g<-g+geom_histogram(binwidth=1000)
g+labs(list(title="The total number of steps taken per day",
               x="Total number of steps per each day",y="Frequency"))

#caluculate the mean and median
summary(tsd)
#only mean
(meanA<-mean(tsd$tsd,na.rm=TRUE))
#only median
(medianA<-median(tsd$tsd,na.rm=TRUE))
#if you want to median line on the screen
g<-g+labs(list(title="The total number of steps taken per day",
               x="Total number of steps per each day",y="Frequency"))
g<-g+geom_vline(xintercept=medianA,colour="red")
g+annotate("text",label="median",x=medianA+1700,y=8.0,colour="red")


#The average daily activity pattern
#ignore the missing values

#the class of date
#interval factor
int.names<-levels(as.factor(raw.data$interval))


#Calculate the average number of steps taken for each intaval
mean.int<-data.frame(interval=as.numeric(int.names),
                     mean=as.vector(tapply(data.no.NA$steps,
                                            data.no.NA$interval,mean)))
#plotting
q<-ggplot(mean.int,aes(x=interval,y=mean))+geom_line()
q+labs(list(title="average daily activity pattern",x="interval",
            y="the average number of steps taken for each intaval"))

#which 5-minutes interval contains the maximum number of steps
sort.data<-mean.int[order(mean.int$mean,decreasing=TRUE),]
sort.data[1,"interval"]


#Imputing missing values

#The number of NAs
nrow(subset(raw.data,is.na(raw.data[,"steps"])==TRUE))

#flling in the NA columns

mean.day<-data.frame(date=date.names,
                       mean=as.vector(tapply(data.no.NA$steps,
                                             data.no.NA$date,mean)))

mean.day[is.na(mean.day)]<-0
mean.int[is.na(mean.int)]<-0

replaced<-raw.data

for (i in 1:nrow(replaced)){
  if(is.na(replaced[i,"steps"])==TRUE){
    mean.today<-subset(mean.day,
                       mean.day[,"date"]==
                         paste(substring(replaced[i,"date"],6,7),"/",
                               substring(replaced[i,"date"],9,10),sep=""))
    mean.the.int<-subset(mean.int,
                         mean.int[,"interval"]==replaced[i,"interval"])
    replaced[i,"steps"]<-(mean.today[1,2]+mean.the.int[1,2])/2
  }
}


#Histogram
#the total number of steps taken per day
tsd2<-tapply(replaced$steps,replaced$date,sum)
tsd2<-data.frame(date.names,tsd2)

p<-ggplot(tsd2,aes(x=tsd2))
p<-p+geom_histogram(binwidth=1000)
p+labs(list(title="The total number of steps taken per day 
            (with missing data filled in)",
            x="Total number of steps per each day",y="Frequency"))

#caluculate the mean and median
summary(tsd2)
#only mean
(meanB<-mean(tsd2$tsd2,na.rm=TRUE))
#only median
(medianB<-median(tsd2$tsd2,na.rm=TRUE))

#Comparison
meanA-meanB
medianA-medianB
#many histograms
library(gridExtra)
grid.arrange(p,g,nrow=2)
#overlay multiple graphs
pg<-ggplot(data=tsd,aes(x=tsd))
pg<-pg+geom_histogram(binwidth=1000)
pg<-pg+geom_histogram(data=tsd2,aes(x=tsd2),binwidth=1000,alpha=0.5)
pg

#Are there differences in activity patterns between weekdays and weekends
#using the datasets with the missing cells are ffiled in

#making a new factor
#print "Date" in English
Sys.setlocale("LC_TIME","English")
replaced$weekdays<-weekdays(as.Date(raw.data$date),abbreviate=TRUE)
#convert into weekday,weekend
replaced$weekdays[replaced$weekdays=="Sun"|
                    replaced$weekdays=="Sat"]<-"weekend"
replaced$weekdays[replaced$weekdays!="weekend"]<-"weekday"

#by(data=replaced$steps,list(weekdays=replaced$weekdays,
#                            interval=replaced$interval),mean)

#steps per interval
end.data<-subset(replaced,replaced$weekdays=="weekend")
day.data<-subset(replaced,replaced$weekdays=="weekday")

end.int<-as.vector(tapply(end.data$steps,factor(end.data$interval),mean))
day.int<-as.vector(tapply(day.data$steps,factor(day.data$interval),mean))
  
week.int<-data.frame(interval=as.numeric(int.names),
                     weekend=end.int,weekday=day.int)

#plotting
#make a panel plot conteining a time series plot
#Using gridExtra package
a<-ggplot(week.int,aes(x=interval,y=weekend))+geom_line(col="blue")
b<-ggplot(week.int,aes(x=interval,y=weekday))+geom_line(col="blue")
a<-a+labs(list(title="average daily activity pattern on weekends",
               x="interval",y="average number of steps"))
b<-b+labs(list(title="average daily activity pattern on weekdays",
               x="interval",y="average number of steps"))
grid.arrange(a,b,nrow=2)



