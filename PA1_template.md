# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(dplyr)
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

```r
steps <- group_by(activity,date) %>% summarize(steps = sum(steps))
mean(steps$steps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(steps$steps, na.rm=TRUE)
```

```
## [1] 10765
```

```r
hist(steps$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

## What is the average daily activity pattern?

```r
interval <- group_by(activity,interval) %>% 
    summarize(mean = mean(steps,na.rm=T))
with(interval, plot(mean~interval, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Interval with greatest average activity is 8:35am-8:40am

```r
arrange(interval,desc(mean))[1,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```


## Imputing missing values

For each missing value I substituted the mean activity found for that interval and day of week (Sunday through Saturday). First I calculated the day of the week for each date then the mean number of steps for each interval for each day of the week:

```r
activity$day <- weekdays(as.Date(activity$date))
stepsIntDay <- group_by(activity, interval, day) %>% summarize(mean(steps, na.rm=T))
```
Then I assigned the values from 'activity' to the new 'filled' data frame and filled the empty values with the expected values from the stepsIntDay data frame.


```r
filled <- activity

## Are there differences in activity patterns between weekdays and weekends?
```
