---
output: html_document
---
# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
library(lattice)
source("formatters.R")  # Functions for pretty printing
unzip("activity.zip")
data1 <- read.csv("activity.csv")
summary(data1)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 12.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##  NA's   :2304    (Other)   :15840
```
## What is mean total number of steps taken per day?
Compute total number of steps taken each day:

```r
totDailySteps <- with(data1, by(steps, date, sum))
```

```r
hist(totDailySteps, breaks=7)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Compute mean daily steps:

```r
print2(meanDailyStepsRaw <- mean(totDailySteps, na.rm=TRUE))
```

```
## [1] 10766.19
```
Compute median daily steps:

```r
print2(medianDailyStepsRaw <- as.numeric(median(totDailySteps, na.rm=TRUE)))
```

```
## [1] 10765.00
```

## What is the average daily activity pattern?
Compute average number of steps taken for each 5-minute interval:

```r
avgStepsPerInterval <- with(data1, tapply(steps, interval, mean, na.rm=TRUE))
```

Plot the average number of steps per 5-min. interval by the 5-min. interval.


```r
plot(0:287, avgStepsPerInterval, type='l', xlab='', xaxt='n',
     ylab="Average number of steps in 5-min interval", 
     cex.lab=1.2)
xtics <- seq(0, 288, by=12)
xtics2 <- seq(6, 287, by=12)
xlabs <- sprintf("%04d", 100 * xtics / 12)
xaxis <- axis(side=1, at=xtics, labels=xlabs, las=3, tck=-.034)
xaxis2 <- axis(side=1, at=xtics2, labels=F, tck=-.025)
rug(1:287, ticksize=-.017)
mtext("5-min interval (24-hr time)", side=1, line=4, cex=1.2)
title("Average Daily Number of Steps Per 5-min Interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

Determine which interval has the highest average number of steps:


```r
# Highest average number of steps in any interval
print2(maxSteps <- max(avgStepsPerInterval))
```

```
## [1] 206.17
```

```r
# Interval with the highest average number of steps
print2(maxStepInterval <- names(which(avgStepsPerInterval == maxSteps)))
```

```
## [1] 835
```
The maximum average number of steps per interval is **206.17** and occurs 
in interval **0835**.

## Imputing missing values

The results of `summary data1` at the top of the page show that that `steps` is
the only variable with missing values.  This is verified below:


```r
apply(data1, 2, anyNA)  # TRUE only for variables with at least 1 NA
```

```
##    steps     date interval 
##     TRUE    FALSE    FALSE
```


```r
# Number of missing values for *steps* variable
stepsNAs <- is.na(data1$steps)
(countOfStepsNAs <- length(stepsNAs[stepsNAs]))
```

```
## [1] 2304
```

Thus there are **2304** missing values in the dataset, all occurring
in the variable `steps`.

We will impute missing values of `steps` for an `interval` by replacing the
missing value with the mean of the non-missing `steps` for that interval accross
all days in the dataset.


```r
# Get a vector of length==nrow(data1) whose values are the mean number of steps
# for each inverval
aveSteps <- with(data1, ave(steps, interval, FUN=function(x) mean(x, na.rm=TRUE)))

# Duplicate data1
data2 <- data1

# Replace missing values in new dataset
data2$steps[is.na(data2$steps)] <- aveSteps[is.na(data2$steps)]

summary(data2)
```

```
##      steps               date          interval   
##  Min.   :  0.0   2012-10-01:  288   Min.   :   0  
##  1st Qu.:  0.0   2012-10-02:  288   1st Qu.: 589  
##  Median :  0.0   2012-10-03:  288   Median :1178  
##  Mean   : 37.4   2012-10-04:  288   Mean   :1178  
##  3rd Qu.: 27.0   2012-10-05:  288   3rd Qu.:1766  
##  Max.   :806.0   2012-10-06:  288   Max.   :2355  
##                  (Other)   :15840
```
Compute total number of steps taken each day in the imputed dataset:

```r
totDailySteps2 <- with(data2, by(steps, date, sum))
```

```r
hist(totDailySteps2, breaks=7)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

Effect of missing value impuation on mean and median number of steps per day:

```r
# Mean before imputation
print2(meanDailyStepsRaw)
```

```
## [1] 10766.19
```

```r
# Mean after imputation
meanDailyStepsImputed <- mean(totDailySteps2)
print2(meanDailyStepsImputed)
```

```
## [1] 10766.19
```

```r
# Difference
print2(dMean <- meanDailyStepsImputed - meanDailyStepsRaw)
```

```
## [1] 0.00
```

```r
# Median before imputation
print2(medianDailyStepsRaw)
```

```
## [1] 10765.00
```

```r
# Median after imputation
medianDailyStepsImputed <- median(totDailySteps2)
names(medianDailyStepsImputed) <- NULL
print2(medianDailyStepsImputed)
```

```
## [1] 10766.19
```

```r
# Difference
(dMedian <- round(medianDailyStepsImputed - medianDailyStepsRaw, 2))
```

```
## [1] 1.19
```

Imputing missing values for `steps` had the following effects on the mean and
median number of steps per day:
* Mean: no effect
* Median: increase of 1.19

## Are there differences in activity patterns between weekdays and weekends?


```r
# Create factor "weekday" with levels "weekday" and "weekend"
day <- weekdays(as.POSIXct(data2$date), abbreviate=TRUE)
data2$weekday <- factor(ifelse(day %in% c("Sat", "Sun"), "weekend", "weekday"))

# Create a dataset for the average step data
avg.data <- data.frame(with(data2, aggregate(steps, list(interval, weekday), mean)))
names(avg.data) <- c('interval', 'weekday', 'steps')

# Plot the average number of steps per day for each interval, averagesd separately 
# over all weekdays and all weekend days, vs. the interval
avg.data$x <- 0:287
xticks3 <- seq(0, 288, by=6)
lenx <- length(xticks3)
xlabs3 <- numeric(lenx)
xlabs3[1:lenx %% 2 == 1] <- as.character(sprintf("%04d", xtics*100/12))
xlabs3[1:lenx %% 2 == 0] <- ''
xyplot(steps ~ x | weekday, data=avg.data, type='l', layout=c(1,2),
       xlab=list("5-min Interval (24-hr time)", cex=1.2), 
       ylab=list("Average daily number of steps in 5-min interval", cex=1.2), 
       scales=list(cex=1.05, x=list(at=xticks3, rot=90, labels=xlabs3)),
       par.strip.text=list(cex=1.15))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

The figure, above, reveals differences between the weekend and weekday activity
patterns.  On weekdays, activity begins around 0530, and there is a marked peak,
about one hour in duration, centered around 0830.  On weekends, activity begins
later (around 0700), the 0830 peak is less prominent, and activity levels
throught the remainder of the day tend to be higher than on weekdays.

