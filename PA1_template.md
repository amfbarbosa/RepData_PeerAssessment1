
# Reproducible Research: Peer Assessment 1

## Adjuste enviroment


```r
Sys.setlocale('LC_TIME', 'C')        # Set time to English standard
```

```
## [1] "C"
```

## Loading and preprocessing the data


```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
 1. Make a histogram of the total number of steps taken each day


```r
aggstepsDay <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(aggstepsDay$steps, names.arg=aggstepsDay$date, xlab="Date", ylab="Total of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

 2. Calculate and report the mean and median total number of steps taken per day


```r
firstmean <- mean(aggstepsDay$steps)
print(firstmean)
```

```
## [1] 10766.19
```

```r
firstmedian <-median(aggstepsDay$steps)
print(firstmedian)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
aggstepsInter <- aggregate(steps ~ interval, data=activity, FUN=mean)
plot(aggstepsInter, type="l", xlab="5-minute interval", ylab="Average of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
aggstepsInter$interval[which.max(aggstepsInter$steps)]
```

```
## [1] 835
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the 
mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy is to use the means for the 5-minute intervals as fillers for missing values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activity <- merge(activity, aggstepsInter, by="interval", suffixes=c("",".y"))
nas <- is.na(activity$steps)
activity$steps[nas] <- activity$steps.y[nas]
activity <- activity[,c(1:3)]
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
aggstepsDay2 <- aggregate(steps ~ date, data=activity, FUN=sum)
barplot(aggstepsDay2$steps, names.arg=aggstepsDay2$date, xlab="Date", ylab="Total of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

Calculate and report the mean and median total number of steps taken per day. 


```r
secmean <- mean(aggstepsDay2$steps)
print(secmean)
```

```
## [1] 10766.19
```

```r
secmedian <- median(aggstepsDay2$steps)
print(secmedian)
```

```
## [1] 10766.19
```
Calculate differ from the estimates from the first part of the assignment.


```r
secmean - firstmean
```

```
## [1] 0
```

```r
secmedian - firstmedian
```

```
## [1] 1.188679
```

Conclusion: The missing data have a low impact  when estimating the total number of steps per day.

## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
activity$daytype <- as.factor(sapply(activity$date, daytype))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
par(mfrow=c(2,1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=activity,
                            subset=activity$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l", main=type, xlab="5-minute interval", ylab="Average of Steps")
}
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

