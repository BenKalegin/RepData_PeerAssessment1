

---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
unzip("activity.zip")
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

```r
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "numeric"))
activity$time <- sapply(activity$interval, function(x) as.POSIXct(x %/% 100 * 60 * 60 + x %% 100 * 60, origin = "1970-01-01", tz = "UTC"))
```


## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day
2. Make a histogram of the total number of steps taken each day


```r
stepsByDate <- aggregate(steps ~ date, activity, FUN = sum, na.action = na.omit)
hist(stepsByDate$steps, main = "Histogram of total steps per day", xlab ="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsByDate$steps)    
```

```
## [1] 10766.19
```

```r
median(stepsByDate$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsByTime <- aggregate(steps ~ time, activity, FUN = mean, na.action = na.omit)
stepsByTime$time <- as.POSIXct(stepsByTime$time, origin = "1970-01-01", tz = "UTC")
plot(stepsByTime$steps ~  stepsByTime$time, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
format(stepsByTime$time[which(stepsByTime$steps == max(stepsByTime$steps))], "%R")
```

```
## [1] "08:35"
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to impute hourly average. 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity$hour <- sapply(activity$interval, function(x) x %/% 100)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))

library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activityFilled <- activity %>%
    group_by(hour) %>%
    mutate(
        steps = impute.mean(steps)
    )
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
stepsByDate <- aggregate(steps ~ date, activityFilled, FUN = sum, na.action = na.omit)
hist(stepsByDate$steps, main = "Histogram of total steps per day", xlab ="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
mean(stepsByDate$steps)    
```

```
## [1] 10766.19
```

```r
median(stepsByDate$steps)
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Impact is very low, less than 0.1%

## Are there differences in activity patterns between weekdays and weekends?


For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
library(timeDate)
activity$weekend <- as.factor(ifelse(isWeekday(activity$date, wday=1:5), "weekday", "weekend"))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
library(scales)
```

```
## Warning: package 'scales' was built under R version 3.2.5
```

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.2.5
```

```r
stepsByTime <- aggregate(steps ~ weekend + time, activity, FUN = mean, na.action = na.omit)
stepsByTime$time <- as.POSIXct(stepsByTime$time, origin = "1970-01-01", tz = "UTC")
ggplot(stepsByTime, aes(x = time, y = steps)) + 
    geom_line() + 
    xlab("") + 
    scale_x_datetime(breaks = date_breaks("3 hour"), labels = date_format("%H:%M")) +     
    facet_wrap(~ weekend, scales = 'free_y', ncol = 1)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
