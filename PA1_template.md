---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data  

Load the data:


```r
unzip('activity.zip')
data <- read.csv('activity.csv')
```

Process/transform the data into a format suitable for your analysis, so we need to change de class of the date variable to a Date format:


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
data$date <- ymd(data$date)
```

## What is mean total number of steps taken per day?  

Calculate the total number of steps taken per day and make a histogram:


```r
steps_day <- tapply(data$steps, data$date, sum)
hist(steps_day, xlab= 'Total steps per day', main='Histogram of the total number of steps taken each day')
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Report the mean and median of the total number of steps taken per day


```r
mean(steps_day, na.rm = T)
```

```
## [1] 10766.19
```

```r
median(steps_day, na.rm=T) 
```

```
## [1] 10765
```

The mean is **10766** steps and median is **10765** of the total number of steps per day.  

## What is the average daily activity pattern?  

Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
steps_interval <- tapply(data$steps, data$interval, mean, na.rm=TRUE)
plot(names(steps_interval), steps_interval, type='l', ylab = 'Mean of Steps', xlab='Time-Step Interval', main='Mean of steps per 5-minute interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
max(steps_interval)
```

```
## [1] 206.1698
```

```r
steps_interval[steps_interval == max(steps_interval)]
```

```
##      835 
## 206.1698
```
The **835** 5-minute interval contains the maximum number of steps: **206** steps.

## Imputing missing values 

Calculate and report the total number of missing values in the dataset 


```r
navalues <- sum(is.na(data$steps))
```

There is a **2304** missing values in the data set.  
Let's fill this values with the mean of the 5-minutes interval and create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data_filled <- data

for (i in 1:length(data_filled$steps)){
      if (is.na(data_filled$steps[i])){
            data_interval <- data_filled$interval[i]
            mean_interval <- as.numeric(steps_interval[names(steps_interval) == data_interval])
            data_filled$steps[i] <- mean_interval
      }
}
```

Make a histogram of the total number of steps taken each day:


```r
new_steps_day <- tapply(data_filled$steps, data_filled$date, sum)
hist(new_steps_day, main='Histogram of total number os steps taken each day', xlab='N of steps per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day  


```r
mean(new_steps_day)
```

```
## [1] 10766.19
```

```r
median(new_steps_day)
```

```
## [1] 10766.19
```

Now the mean is **10766** steps and median is **10766** of the total number of steps per day.  

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

```r
weekend <- c('Saturday','Sunday')
data_filled$day <- 'weekday'

is_weekend <- weekdays(data_filled$date) %in% weekend
data_filled$day[is_weekend] <- 'weekend'

data_filled$day <- factor(data_filled$day)
```

Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(plyr)
library(ggplot2)
data_final <- ddply(data_filled, ~interval + day, summarise, mean = mean(steps))

qplot(interval, mean, data=data_final, facets = day~., geom='line', ylab='Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
