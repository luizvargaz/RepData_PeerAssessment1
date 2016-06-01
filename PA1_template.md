# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

1. Load the data (i.e. read.csv())

```r
temp <- tempfile()
download.file('https://raw.githubusercontent.com/luizvargaz/RepData_PeerAssessment1/master/activity.zip', temp, mode = 'wb')
unzip(temp, 'activity.csv')
rawData  <- read.csv('activity.csv', header = T)
head(rawData, 3)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
transRawData <- transform(rawData, date = as.Date(date))
str(transRawData)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

```r
na <- is.na(transRawData$steps)
transRawData$esNA <- na
data <- transRawData[transRawData$esNA != 'TRUE',]
nrow(data)
```

```
## [1] 15264
```

1. Calculate the total number of steps taken per day

```r
dataDay <- aggregate(steps ~ date, data, FUN = 'sum')
print(dataDay)
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
library(scales) 
histSteps <- qplot(dataDay$steps, geom = 'histogram') + theme(aspect.ratio = 1)
print(histSteps)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
library(data.table)
dataDayT <- data.table(data)
#dataDayT[,list(mean = mean(steps), median = median(steps)), by = date]
dataDayT[,list(mean = mean(steps), median = median(steps))]
```

```
##       mean median
## 1: 37.3826      0
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
dataSteps <- aggregate(steps ~ interval, data, FUN = 'mean')
graphTimeSteps <- qplot(interval, steps, data = dataSteps, geom = 'line') + theme(aspect.ratio = 1)
print(graphTimeSteps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
apply(dataSteps, MARGIN = 2, function(x) max(x, na.rm = TRUE))
```

```
##  interval     steps 
## 2355.0000  206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
length(which(is.na(rawData$steps)))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
fillNa <- function(dataFrame){
        vectorinterval <- unique(dataFrame$interval)
        count <- 0
        for(i in vectorinterval){
                subsetInterval <- dataFrame[dataFrame$interval == i,]
                medianSteps <- mean(subsetInterval$steps, na.rm=TRUE)
                subsetInterval$steps[which(is.na(subsetInterval$steps))] <- medianSteps
                
                if(count == 0){
                        cleanData <- subsetInterval
                }else{
                        cleanData <- rbind(cleanData, subsetInterval)
                }
                
                count <- count + 1
                
        }
        newCleanData <- cleanData
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
newData <- fillNa(transRawData)
str(newData)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.72 0 0 47 0 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-02" ...
##  $ interval: int  0 0 0 0 0 0 0 0 0 0 ...
##  $ esNA    : logi  TRUE FALSE FALSE FALSE FALSE FALSE ...
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
library(ggplot2)
library(scales)
stepsByDay <- aggregate(steps ~ date, newData, FUN = 'sum')
newHistSteps <- qplot(steps, data = stepsByDay, geom = 'histogram') + theme(aspect.ratio = 1)
print(newHistSteps)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
newDataDayT <- data.table(newData)
#newDataDayT[,list(mean=mean(steps), median=median(steps)),by=date]
newDataDayT[,list(mean=mean(steps), median=median(steps))]
```

```
##       mean median
## 1: 37.3826      0
```



## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
dataDays <- newData
dataDays$day <- weekdays(newData$date)
days <- unique(dataDays$day)

dataDaysWeekend <- dataDays[dataDays$day == 'Saturday' | dataDays$day == 'Sunday',]
lenWeekend <- nrow(dataDaysWeekend)
vectorWeekend <- rep('weekend', lenWeekend)
dataDaysWeekend$week <- vectorWeekend

dataDaysWeekday <- dataDays[dataDays$day == 'Tuesday' | dataDays$day == 'Thursday' | dataDays$day == 'Wednesday' | dataDays$day == 'Friday' | dataDays$day == 'Monday',]
lenWeekday <- nrow(dataDaysWeekday)
vectorWeekday <- rep('weekday', lenWeekday)
dataDaysWeekday$week <- vectorWeekday

dataWeek <- rbind(dataDaysWeekend, dataDaysWeekday)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
d <- aggregate(steps ~ interval + week, dataWeek, FUN = 'mean')
library(lattice)
xyplot(steps ~ interval | factor(week),
       data = d,
       type = 'l', 
       lwd = c(2, 1),
       col.line = 'black',
       layout=(c(1,2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


