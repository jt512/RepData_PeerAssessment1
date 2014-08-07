# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
## system("unzip activity.zip")
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

```r
# Function to print numbers to decimal places by default
print2 <- function(x, decimals=2) noquote(format(x, nsmall=decimals))
```

## What is mean total number of steps taken per day?
Compute total number of steps taken each day:

```r
totDailySteps <- with(data1, by(steps, date, sum))
```

```r
hist(totDailySteps, breaks=7)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

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
avgStepsPerInterval <- with(data1, by(steps, interval, mean, na.rm=TRUE))
```

Plot the average number of steps per 5-min. interval by the 5-min. interval.


```r
xvar <- as.numeric(names(avgStepsPerInterval)) 
yvar <- avgStepsPerInterval
plot(xvar, yvar, type='l', xlab="5-minute inteval", ylab="Average number of steps")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Determine which interval has the highest average number of steps:


```r
print2(maxSteps <- max(avgStepsPerInterval))
```

```
## [1] 206.17
```

```r
print2(maxStepInterval = names(which(avgStepsPerInterval == maxSteps)))
```

```
## Error: unused argument (maxStepInterval = names(which(avgStepsPerInterval
## == maxSteps)))
```
The maximum average number of steps per interval is **206.1698** and occurs in interval **835**.

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

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 

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
print2(meanDailyStepsImputed - meanDailyStepsRaw)
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
round(medianDailyStepsImputed - medianDailyStepsRaw, 2)
```

```
## [1] 1.19
```

Imputing missing values for `steps` had no effect on the mean steps per day and
increased the median by 1.19.
## Are there differences in activity patterns between weekdays and weekends?
