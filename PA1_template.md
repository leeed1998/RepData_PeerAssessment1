Coursera Reproducible Research Assignment 1 - 7/19/2014
========================================================

## Set Directory and Read in Data


```r
setwd("C:/Cousera/Course5_Reproducible Research")
ActivityData <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?
## Make a histogram of the total number of steps taken each day
## Calculate and report the mean and median total number of steps taken per day

## Summarize to each day by summing steps per day - omitting NA and Histogram


```r
StepByDay <- aggregate(steps ~ date, data=ActivityData, FUN=sum,na.action = na.omit)
library (ggplot2)
ggplot (StepByDay,aes(date)) 	+ geom_histogram(aes(weight=steps, fill =..count..),binwidth=.75) + xlab("Date") + theme(axis.text.x=element_text(angle = 90)) + ylab("Steps") + ggtitle("Total Steps Taken Per Day")
```

![plot of chunk histogram for Steps](figure/histogram for Steps.png) 

## Mean and Median per day


```r
MeanStepByDay <- aggregate(steps ~ date, data=ActivityData, FUN=mean,na.action = na.omit)
MedStepByDay <- aggregate(steps ~ date, data=ActivityData, FUN=median,na.action = na.omit)

MeanStepByDay
```

```
##          date   steps
## 1  2012-10-02  0.4375
## 2  2012-10-03 39.4167
## 3  2012-10-04 42.0694
## 4  2012-10-05 46.1597
## 5  2012-10-06 53.5417
## 6  2012-10-07 38.2465
## 7  2012-10-09 44.4826
## 8  2012-10-10 34.3750
## 9  2012-10-11 35.7778
## 10 2012-10-12 60.3542
## 11 2012-10-13 43.1458
## 12 2012-10-14 52.4236
## 13 2012-10-15 35.2049
## 14 2012-10-16 52.3750
## 15 2012-10-17 46.7083
## 16 2012-10-18 34.9167
## 17 2012-10-19 41.0729
## 18 2012-10-20 36.0938
## 19 2012-10-21 30.6285
## 20 2012-10-22 46.7361
## 21 2012-10-23 30.9653
## 22 2012-10-24 29.0104
## 23 2012-10-25  8.6528
## 24 2012-10-26 23.5347
## 25 2012-10-27 35.1354
## 26 2012-10-28 39.7847
## 27 2012-10-29 17.4236
## 28 2012-10-30 34.0938
## 29 2012-10-31 53.5208
## 30 2012-11-02 36.8056
## 31 2012-11-03 36.7049
## 32 2012-11-05 36.2465
## 33 2012-11-06 28.9375
## 34 2012-11-07 44.7326
## 35 2012-11-08 11.1771
## 36 2012-11-11 43.7778
## 37 2012-11-12 37.3785
## 38 2012-11-13 25.4722
## 39 2012-11-15  0.1424
## 40 2012-11-16 18.8924
## 41 2012-11-17 49.7882
## 42 2012-11-18 52.4653
## 43 2012-11-19 30.6979
## 44 2012-11-20 15.5278
## 45 2012-11-21 44.3993
## 46 2012-11-22 70.9271
## 47 2012-11-23 73.5903
## 48 2012-11-24 50.2708
## 49 2012-11-25 41.0903
## 50 2012-11-26 38.7569
## 51 2012-11-27 47.3819
## 52 2012-11-28 35.3576
## 53 2012-11-29 24.4688
```

```r
MedStepByDay 
```

```
##          date steps
## 1  2012-10-02     0
## 2  2012-10-03     0
## 3  2012-10-04     0
## 4  2012-10-05     0
## 5  2012-10-06     0
## 6  2012-10-07     0
## 7  2012-10-09     0
## 8  2012-10-10     0
## 9  2012-10-11     0
## 10 2012-10-12     0
## 11 2012-10-13     0
## 12 2012-10-14     0
## 13 2012-10-15     0
## 14 2012-10-16     0
## 15 2012-10-17     0
## 16 2012-10-18     0
## 17 2012-10-19     0
## 18 2012-10-20     0
## 19 2012-10-21     0
## 20 2012-10-22     0
## 21 2012-10-23     0
## 22 2012-10-24     0
## 23 2012-10-25     0
## 24 2012-10-26     0
## 25 2012-10-27     0
## 26 2012-10-28     0
## 27 2012-10-29     0
## 28 2012-10-30     0
## 29 2012-10-31     0
## 30 2012-11-02     0
## 31 2012-11-03     0
## 32 2012-11-05     0
## 33 2012-11-06     0
## 34 2012-11-07     0
## 35 2012-11-08     0
## 36 2012-11-11     0
## 37 2012-11-12     0
## 38 2012-11-13     0
## 39 2012-11-15     0
## 40 2012-11-16     0
## 41 2012-11-17     0
## 42 2012-11-18     0
## 43 2012-11-19     0
## 44 2012-11-20     0
## 45 2012-11-21     0
## 46 2012-11-22     0
## 47 2012-11-23     0
## 48 2012-11-24     0
## 49 2012-11-25     0
## 50 2012-11-26     0
## 51 2012-11-27     0
## 52 2012-11-28     0
## 53 2012-11-29     0
```

## What is the average daily activity pattern?


```r
ActivityByDay <- aggregate(steps~interval, data=ActivityData, FUN=mean,na.action = na.omit)
ggplot (ActivityByDay ,aes(interval,steps)) + geom_line() + xlab("Interval") + theme(axis.text.x=element_text(angle = 90)) + ylab("Ave Steps") + ggtitle("Average Daily Activity")
```

![plot of chunk average daily activity](figure/average daily activity.png) 

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps


```r
MaxInterval <- max(ActivityByDay$interval)
MaxRow <- which.max(ActivityByDay$steps)
MaxStep <- ActivityByDay$steps[MaxRow]

MaxStep
```

```
## [1] 206.2
```

```r
MaxInterval
```

```
## [1] 2355
```

## Imputing missing values - Calculate and report the total number of missing values in the dataset
Using Not complete cases

```r
Missing <- sum(!complete.cases(ActivityData))
Missing
```

```
## [1] 2304
```

## Create a new dataset that is equal to the original dataset but with the missing data filled in. 
## My strategy is to replace the NA with a total average
First, get the overall mean steps, then find the Row that has NA, replace the NA with the Mean

```r
MeanStep <- mean(ActivityByDay$steps)
ActivityImpute <- ActivityData
NARow <- is.na(ActivityData$steps)
ActivityImpute[NARow,1] <-  MeanStep 
```

## Summarize to each day by summing steps per day - with new NA


```r
StepByDayImp <- aggregate(steps ~ date, data=ActivityImpute, FUN=sum)
library (ggplot2)
ggplot (StepByDayImp ,aes(date)) + geom_histogram(aes(weight=steps, fill =..count..),binwidth=.75) + xlab("Date") + theme(axis.text.x=element_text(angle = 90)) + ylab("Steps") + ggtitle("Total Steps Taken Per Day")
```

![plot of chunk new Summary stpe and histogram](figure/new Summary stpe and histogram.png) 

# Mean and Median per day


```r
MeanStepByDayImp <- aggregate(steps ~ date, data=ActivityImpute, FUN=mean,na.action = na.omit)
MedStepByDayImp <- aggregate(steps ~ date, data=ActivityImpute, FUN=median,na.action = na.omit)

MeanStepByDayImp
```

```
##          date   steps
## 1  2012-10-01 37.3826
## 2  2012-10-02  0.4375
## 3  2012-10-03 39.4167
## 4  2012-10-04 42.0694
## 5  2012-10-05 46.1597
## 6  2012-10-06 53.5417
## 7  2012-10-07 38.2465
## 8  2012-10-08 37.3826
## 9  2012-10-09 44.4826
## 10 2012-10-10 34.3750
## 11 2012-10-11 35.7778
## 12 2012-10-12 60.3542
## 13 2012-10-13 43.1458
## 14 2012-10-14 52.4236
## 15 2012-10-15 35.2049
## 16 2012-10-16 52.3750
## 17 2012-10-17 46.7083
## 18 2012-10-18 34.9167
## 19 2012-10-19 41.0729
## 20 2012-10-20 36.0938
## 21 2012-10-21 30.6285
## 22 2012-10-22 46.7361
## 23 2012-10-23 30.9653
## 24 2012-10-24 29.0104
## 25 2012-10-25  8.6528
## 26 2012-10-26 23.5347
## 27 2012-10-27 35.1354
## 28 2012-10-28 39.7847
## 29 2012-10-29 17.4236
## 30 2012-10-30 34.0938
## 31 2012-10-31 53.5208
## 32 2012-11-01 37.3826
## 33 2012-11-02 36.8056
## 34 2012-11-03 36.7049
## 35 2012-11-04 37.3826
## 36 2012-11-05 36.2465
## 37 2012-11-06 28.9375
## 38 2012-11-07 44.7326
## 39 2012-11-08 11.1771
## 40 2012-11-09 37.3826
## 41 2012-11-10 37.3826
## 42 2012-11-11 43.7778
## 43 2012-11-12 37.3785
## 44 2012-11-13 25.4722
## 45 2012-11-14 37.3826
## 46 2012-11-15  0.1424
## 47 2012-11-16 18.8924
## 48 2012-11-17 49.7882
## 49 2012-11-18 52.4653
## 50 2012-11-19 30.6979
## 51 2012-11-20 15.5278
## 52 2012-11-21 44.3993
## 53 2012-11-22 70.9271
## 54 2012-11-23 73.5903
## 55 2012-11-24 50.2708
## 56 2012-11-25 41.0903
## 57 2012-11-26 38.7569
## 58 2012-11-27 47.3819
## 59 2012-11-28 35.3576
## 60 2012-11-29 24.4688
## 61 2012-11-30 37.3826
```

```r
MedStepByDayImp 
```

```
##          date steps
## 1  2012-10-01 37.38
## 2  2012-10-02  0.00
## 3  2012-10-03  0.00
## 4  2012-10-04  0.00
## 5  2012-10-05  0.00
## 6  2012-10-06  0.00
## 7  2012-10-07  0.00
## 8  2012-10-08 37.38
## 9  2012-10-09  0.00
## 10 2012-10-10  0.00
## 11 2012-10-11  0.00
## 12 2012-10-12  0.00
## 13 2012-10-13  0.00
## 14 2012-10-14  0.00
## 15 2012-10-15  0.00
## 16 2012-10-16  0.00
## 17 2012-10-17  0.00
## 18 2012-10-18  0.00
## 19 2012-10-19  0.00
## 20 2012-10-20  0.00
## 21 2012-10-21  0.00
## 22 2012-10-22  0.00
## 23 2012-10-23  0.00
## 24 2012-10-24  0.00
## 25 2012-10-25  0.00
## 26 2012-10-26  0.00
## 27 2012-10-27  0.00
## 28 2012-10-28  0.00
## 29 2012-10-29  0.00
## 30 2012-10-30  0.00
## 31 2012-10-31  0.00
## 32 2012-11-01 37.38
## 33 2012-11-02  0.00
## 34 2012-11-03  0.00
## 35 2012-11-04 37.38
## 36 2012-11-05  0.00
## 37 2012-11-06  0.00
## 38 2012-11-07  0.00
## 39 2012-11-08  0.00
## 40 2012-11-09 37.38
## 41 2012-11-10 37.38
## 42 2012-11-11  0.00
## 43 2012-11-12  0.00
## 44 2012-11-13  0.00
## 45 2012-11-14 37.38
## 46 2012-11-15  0.00
## 47 2012-11-16  0.00
## 48 2012-11-17  0.00
## 49 2012-11-18  0.00
## 50 2012-11-19  0.00
## 51 2012-11-20  0.00
## 52 2012-11-21  0.00
## 53 2012-11-22  0.00
## 54 2012-11-23  0.00
## 55 2012-11-24  0.00
## 56 2012-11-25  0.00
## 57 2012-11-26  0.00
## 58 2012-11-27  0.00
## 59 2012-11-28  0.00
## 60 2012-11-29  0.00
## 61 2012-11-30 37.38
```

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and 
“weekend” indicating whether a given date is a weekday or weekend day.

## add the weekday into dataset
Using Find and Replace - found on web hack - then column bind


```r
Wkend <- weekdays(as.Date(ActivityImpute$date), abbreviate = TRUE)
Wkend <- sub("^Mon|^Tue|^Wed|^Thu|^Fri", "weekday", Wkend ,  perl = TRUE)
Wkend <- sub("^Sat|^Sun", "weekend", Wkend ,  perl = TRUE)

ActivityImputeW <- cbind(ActivityImpute,Wkend)
```

## What is the average daily activity pattern?
Same graph as above but with facet - remember the + Wkend column for the added aggregation

```r
ActivityImpByDay <- aggregate(steps~interval + Wkend, data=ActivityImpute , FUN=mean)
ggplot (ActivityImpByDay ,aes(interval,steps)) + geom_line() + facet_grid(Wkend ~ .) + xlab("Interval") + theme(axis.text.x=element_text(angle = 90)) + ylab("Ave Steps") + ggtitle("Average Daily Activity") 
```

![plot of chunk facet line graph](figure/facet line graph.png) 
