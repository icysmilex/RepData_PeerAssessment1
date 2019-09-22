#  Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset

# Loading and preprocessing the data
``` r
library(dplyr)
library(ggplot2)
library(knitr)
activity <-read.csv('C:\\Users\\Sing_pei\\Documents\\R program\\activity.csv')
```
# What is mean total number of steps taken per day?
```r
total_steps <- activity %>%
  group_by(date)%>%
  summarise(steps= sum(steps, na.rm = TRUE))

hist(total_steps$steps,main = "Total number of steps taken per day", xlab = "Total steps taken per day" , ylim = c(0,20), col = "darkgreen", breaks = seq(0,25000, by=2500))

mean(total_steps$steps  , na.rm = T)
median(total_steps$steps, na.rm = T)
```
![](https://github.com/icysmilex/RepData_PeerAssessment1/blob/master/image.png)

```r
mean(total_steps$steps  , na.rm = T)
```


    ## [1] 9354.23

```r
median(total_steps$steps, na.rm = T)
```


    ## [1] 10395



# What is the average daily activity pattern?
```r
average<- activity %>%
  group_by(interval)%>%
  summarise(steps= mean(steps, na.rm =T))

plot(average$interval,average$steps, type="l",ylab="average steps per day", xlab="Interval")
```

![](https://github.com/icysmilex/RepData_PeerAssessment1/blob/master/image2.png)

```r
average[which.max(average$steps), ]$interval
```
    ## [1] 835
    
# Imputing missing values
```r
sum(is.na(activity$steps))
```
    ## [1] 2304
  
```r
  mean <- activity %>%
  group_by(date)%>%
  summarise(steps= mean(steps, na.rm = TRUE))

  
  
  
mean<-na.omit(mean)
imputed_mean<-mean(mean$steps)

activity2<-cbind(activity,activity$steps)
colnames(activity2)<- c("steps","date","interval","imputed_steps")
activity2$imputed_steps[is.na(activity2$imputed_steps)] <- imputed_mean

total_imputed_steps <- activity2 %>%
  group_by(date)%>%
  summarise(imputed_steps= sum(imputed_steps))

hist(total_imputed_steps$imputed_steps,main = "Total number of steps taken per day", xlab = "Total steps taken per day" , ylim = c(0,30), col = "darkgreen", breaks = seq(0,25000, by=2500))
```
![](https://github.com/icysmilex/RepData_PeerAssessment1/blob/master/image3.png)

```r
mean(total_imputed_steps$imputed_steps)
```

     ## [1] 10766.19

```r
median(total_imputed_steps$imputed_steps)
```
     ## [1] 10766.19

# Are there differences in activity patterns between weekdays and weekends?
```r
activity$date <-as.Date(activity$date, '%Y-%m-%d')
weekday<-weekdays(activity$date)
activity<-cbind(activity,weekday)
activity[activity$weekday != "Saturday"|activity$weekday != "Sunday", "day"] = "Weekday"
activity[activity$weekday == "Saturday"|activity$weekday == "Sunday", "day"] = "Weekend"

activitylast<- cbind(activity,activity2$imputed_steps)
names(activitylast)[names(activitylast)=="activity2$imputed_steps"] <- "imputed_steps" 

activitylast<- activitylast %>%
  group_by(interval,day)%>%
  summarise(steps= mean(imputed_steps, na.rm =T))

ggplot(activitylast, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + xlab("5-minute interval") + ylab("Number of steps")
```
![](https://github.com/icysmilex/RepData_PeerAssessment1/blob/master/image4.png)




