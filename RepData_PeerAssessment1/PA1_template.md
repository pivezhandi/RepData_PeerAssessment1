---
title: "Peer Assessment 1"
author: "mt"
date: "Saturday, May 16, 2015"
output: html_document
---

#Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
head(activity,4)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
```

#What is mean total number of steps taken per day?


###1- Calculate the total number of steps taken per day

```r
library(dplyr)
by_date<-group_by(activity[!is.na(activity$steps),],date)
total_steps<-summarize(by_date, TotalStepsPerDay=sum(steps))
head(total_steps)
```

```
## Source: local data frame [6 x 2]
## 
##         date TotalStepsPerDay
## 1 2012-10-02              126
## 2 2012-10-03            11352
## 3 2012-10-04            12116
## 4 2012-10-05            13294
## 5 2012-10-06            15420
## 6 2012-10-07            11015
```


###2- If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day


```r
hist(total_steps$TotalStepsPerDay,30,col = "blue"
     ,xlab="total step per one day",ylab="steps",main="step per day variation")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


###3- Calculate and report the mean and median of the total number of steps taken per day

```r
library(dplyr)
ByDate<-group_by(activity[!is.na(activity$steps),],date)
MeanMedianSteps<-summarize(ByDate,MeanStepsPerDay=mean(steps),
                           MedianStepsPerDay=median(steps))
head(MeanMedianSteps,10)
```

```
## Source: local data frame [10 x 3]
## 
##          date MeanStepsPerDay MedianStepsPerDay
## 1  2012-10-02         0.43750                 0
## 2  2012-10-03        39.41667                 0
## 3  2012-10-04        42.06944                 0
## 4  2012-10-05        46.15972                 0
## 5  2012-10-06        53.54167                 0
## 6  2012-10-07        38.24653                 0
## 7  2012-10-09        44.48264                 0
## 8  2012-10-10        34.37500                 0
## 9  2012-10-11        35.77778                 0
## 10 2012-10-12        60.35417                 0
```


#What is the average daily activity pattern?


###1- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
library(dplyr)
#groupping activity values by intervals
by_interval<-group_by(activity[!is.na(activity$steps),],interval)
intervalsteps<-summarize(by_interval,AverageSteps=mean(steps))
plot(intervalsteps$interval,intervalsteps$AverageSteps,type="l",
     xlab="intervals",ylab="steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 


###2- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# first calculation of maximum interval point and then interval comes by 
# calculation of two susequence periods
MaxStepIndex=which(intervalsteps$AverageSteps==max(intervalsteps$AverageSteps))
if (abs(intervalsteps$AverageSteps[MaxStepIndex]-intervalsteps$AverageSteps[MaxStepIndex-1])>
            abs(intervalsteps$AverageSteps[MaxStepIndex]-intervalsteps$AverageSteps[MaxStepIndex+1]))
        {
        indexes<-c((MaxStepIndex-1),MaxStepIndex)
        }else{
        indexes<-c(MaxStepIndex,(MaxStepIndex+1))        
}

intervalsteps$interval[indexes]
```

```
## [1] 830 835
```


#Imputing missing values
###1- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
# loading data base that should be repaired
Ractivity<-read.csv("activity.csv")
sum(is.na(Ractivity))
```

```
## [1] 2304
```



###2- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
library(dplyr)
# average by date
by_date<-group_by(Ractivity[!is.na(Ractivity$steps),],date)
DateSteps<-summarize(by_date,MeanStepsPerDay=mean(steps))
# average by interval
by_interval<-group_by(Ractivity[!is.na(Ractivity$steps),],interval)
IntervalSteps<-summarize(by_interval,MeanStepsPerInterval=mean(steps))

#first removing NAs by date based mean second with interval based mean
for (i in 1:nrow(Ractivity)){
        if (is.na(Ractivity$steps[i])){
                if (sum((Ractivity$date[i]==DateSteps$date))!=0){
        Ractivity$steps[i] <- 
                DateSteps$MeanStepsPerDay[(Ractivity$date[i]==DateSteps$date)]
        }else{
        Ractivity$steps[i] <- 
                IntervalSteps$MeanStepsPerInterval[(Ractivity$interval[i]==IntervalSteps$interval)]
        }
        }
}
head(Ractivity,10)
```

```
##        steps       date interval
## 1  1.7169811 2012-10-01        0
## 2  0.3396226 2012-10-01        5
## 3  0.1320755 2012-10-01       10
## 4  0.1509434 2012-10-01       15
## 5  0.0754717 2012-10-01       20
## 6  2.0943396 2012-10-01       25
## 7  0.5283019 2012-10-01       30
## 8  0.8679245 2012-10-01       35
## 9  0.0000000 2012-10-01       40
## 10 1.4716981 2012-10-01       45
```


###3- Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
write.table(Ractivity,file="Ractivity.csv", row.name=F,col.names=T)
```


###4- Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
library(dplyr)
RepairedByDate<-group_by(Ractivity,date)  ##Repaired activity grouped by date
MeanMedianSteps<-summarize(RepairedByDate,MeanStepsPerDay=mean(steps),
                           MedianStepsPerDay=median(steps))
head(MeanMedianSteps,10)
```

```
## Source: local data frame [10 x 3]
## 
##          date MeanStepsPerDay MedianStepsPerDay
## 1  2012-10-01        37.38260          34.11321
## 2  2012-10-02         0.43750           0.00000
## 3  2012-10-03        39.41667           0.00000
## 4  2012-10-04        42.06944           0.00000
## 5  2012-10-05        46.15972           0.00000
## 6  2012-10-06        53.54167           0.00000
## 7  2012-10-07        38.24653           0.00000
## 8  2012-10-08        37.38260          34.11321
## 9  2012-10-09        44.48264           0.00000
## 10 2012-10-10        34.37500           0.00000
```
* as have been shown in resaults means values changed and median of repaired result shifted so hisogram have smoother characteristics than before


```r
TotalSteps<-summarize(RepairedByDate,TotalStepsPerDay=sum(steps))
hist(TotalSteps$TotalStepsPerDay,col= "blue",30,xlab="total step per one day",ylab="steps",main="step per day variation in repaired data")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

#Are there differences in activity patterns between weekdays and weekends?


###1- Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Ractivity<-read.table("Ractivity.csv",header=T)

library(dplyr);library(lubridate)
weekend<-c("ÔäÈå","íßÔäÈå")
week<-rep(0,nrow(Ractivity))
for (i in 1:nrow(Ractivity)){
        if (sum(weekdays(ymd(Ractivity$date[i]))==weekend)!=0){
        week[i] <- "weekend"
        }else{
        week[i] <- "weekday"
        }
}                
newactivity<-mutate(Ractivity,weekdays=factor(week))
```


###2- Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(dplyr)
weekendactivity<-newactivity[newactivity$weekdays=="weekend",]
weekdayactivity<-newactivity[newactivity$weekdays=="weekday",]

weekendgroup<-group_by(weekendactivity,interval)
WeekendMean<-summarize(weekendgroup,MeanPerInterval=mean(steps))

weekdaygroup<-group_by(weekdayactivity,interval)
WeekdayMean<-summarize(weekdaygroup,MeanPerInterval=mean(steps))

par(mfrow=c(2,1),mar=c(4,4,2,1),oma=c(0,0,2,0))
with(newactivity,{
        plot(WeekendMean$interval,WeekendMean$MeanPerInterval,type="l",
             xlab="intervals",ylab="steps",main="weekend")
        plot(WeekdayMean$interval,WeekdayMean$MeanPerInterval,type="l",
             xlab="intervals",ylab="steps",main="weekday")
        
})
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
