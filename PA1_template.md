---
title: "Markdown-Project1"
author: "Manuela Escobar"
date: "04-October-2020"
output: 
  html_document:
    keep_md: true
---
## Project instructions
1. Code for reading in the dataset and/or processing the data
2. Histogram of the total number of steps taken each day
3. Mean and median number of steps taken each day
4. Time series plot of the average number of steps taken
5. The 5-minute interval that, on average, contains the maximum number of steps
6. Code to describe and show a strategy for imputing missing data
7. Histogram of the total number of steps taken each day after missing values are imputed
8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

Development

##### 1. Code for reading in the dataset and/or processing the data


```r
# Understanding data
dim(activity)
```

```
## [1] 17568     3
```

```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# Missing data an date format
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
summary(activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

##### 2. Histogram of the total number of steps taken each day

```r
#ActStepDay
ActStepDay <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
#Plotting
qplot(ActStepDay, xlab = "Number of steps taken each day", main = "Total number of steps taken each day", binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

##### 3. Mean and median number of steps taken each day

```r
  #Mean
    ActStepDayMean <- mean(ActStepDay)
    ActStepDayMean
```

```
## [1] 9354.23
```

```r
    #Median
    ActStepDayMedian <- median(ActStepDay)
    ActStepDayMedian
```

```
## [1] 10395
```

##### 4. Time series plot of the average number of steps taken

```r
MeanStep5min <- aggregate(x=list(meanSteps=activity$steps), by=list(interval=activity$interval), FUN=mean, na.rm=TRUE)
ggplot(data = MeanStep5min, aes(x=interval, y=meanSteps)) + geom_line() + xlab("Intervals of 5 min") + ylab("Mean steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

##### 5. The 5-minute interval that, on average, contains the maximum number of steps

```r
AveStep5min <- aggregate(data=activity,steps~date+interval,FUN="mean")
MaxAveStep5min <- aggregate(data=AveStep5min,steps~interval,FUN="max")
max(MaxAveStep5min$steps)
```

```
## [1] 806
```

```r
b <- as.data.frame(MaxAveStep5min)
b[b[2] == max(b$steps)]
```

```
## [1] 615 806
```

##### 6. Code to describe and show a strategy for imputing missing data



```r
AmountMissingValues <- length(which(is.na(activity$steps)))
NoMissingValues <- impute(data = activity, object = NULL, method = "median/mode")
sum(is.na(NoMissingValues$steps))
```

```
## [1] 0
```

##### 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
ImputedStepsDay <- tapply(NoMissingValues$steps, NoMissingValues$date, sum)
qplot(ImputedStepsDay, xlab='Imputed total steps per day', main = "Total number of imputrd steps taken each day", binwidth=500)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

##### 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
NoMissingValues$dateType <-  ifelse(as.POSIXlt(NoMissingValues$date)$wday %in% c(0,6), 'weekend', 'weekday')
Number <- aggregate(steps ~ interval + dateType, data=NoMissingValues, mean)
ggplot(Number, aes(interval, steps)) +geom_line() + facet_grid(dateType ~ .) + xlab("5-minute interval") + ylab("avarage number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
