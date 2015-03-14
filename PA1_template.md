# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
rawData <- read.csv("activity.csv")
data <- rawData[complete.cases(rawData),]
```

## What is mean total number of steps taken per day?


```r
hist(sapply(levels(data$date), function(l) {sum(subset(data, date == l)[,1])} ), main = "", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
mean(sapply(levels(data$date), function(l) {sum(subset(data, date == l)[,1])} ))
```

```
## [1] 9354.23
```

```r
median(sapply(levels(data$date), function(l) {sum(subset(data, date == l)[,1])} ))
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
sumPerInterval <- sapply(unique(data$interval), function(i) {sum(subset(data, interval == i)[,1])})
plot(unique(data$interval), sumPerInterval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
which(sumPerInterval == max(sumPerInterval))
```

```
## [1] 104
```

## Imputing missing values


```r
sum(!complete.cases(rawData))
```

```
## [1] 2304
```

Describe and show with code a strategy for imputing missing data:
Using mean for the day to fill in missing values as suggested in assigment.


```r
missingData <- rawData[!complete.cases(rawData),]
x <- sapply(levels(missingData$date), function(l) {mean(subset(data, date == l)[,1])} )
x[is.nan(x)] <- 0
library(sqldf)
```

```
## Loading required package: gsubfn
## Loading required package: proto
## Loading required package: RSQLite
## Loading required package: DBI
```

```r
for(i in seq(1, length(x))) {
  name <- names(x[i]);
  step <- x[i];
  rawData <- fn$sqldf(c("update rawData set steps = $step where date like '$name' and steps is null", "select * from main.rawData"));
}
```

```
## Loading required package: tcltk
```

```r
hist(sapply(levels(rawData$date), function(l) {sum(subset(rawData, date == l)[,1])} ), main = "", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
mean(sapply(levels(rawData$date), function(l) {sum(subset(rawData, date == l)[,1])} ))
```

```
## [1] 9354.23
```

```r
median(sapply(levels(rawData$date), function(l) {sum(subset(rawData, date == l)[,1])} ))
```

```
## [1] 10395
```
What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
sumPerInterval <- sapply(unique(rawData$interval), function(i) {sum(subset(rawData, interval == i)[,1])})
plot(unique(rawData$interval), sumPerInterval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
which(sumPerInterval == max(sumPerInterval))
```

```
## [1] 104
```

```r
sum(data$steps)
```

```
## [1] 570608
```

```r
sum(rawData$steps)
```

```
## [1] 570608
```

## Are there differences in activity patterns between weekdays and weekends?


```r
rawData[,2] <- as.Date(rawData[,2], "%Y-%m-%d")
rawData$weekday <- weekdays(rawData[,2])
weekdays <- unique(weekdays(rawData[,2]))
rawDataInWeekDay <- subset(rawData, weekday == weekdays[1] | weekday == weekdays[2] | weekday == weekdays[3] | weekday == weekdays[4] | weekday == weekdays[5])
rawDataInWeekend <- subset(rawData, weekday == weekdays[6] | weekday == weekdays[6])

par(mfrow = c (2,1), oma=c(0,1,0,0))
par(mar=c(0,3,4,4))
plot(rawDataInWeekend$interval, rawDataInWeekend$steps, type = "l", ylab = "", xlab = "", col = "blue", xaxt = "n", yaxt = "n")
axis(2, at = NULL, labels = FALSE); axis(3, at = NULL, labels = FALSE); axis(4);
par(mar=c(4,3,0,4))
plot(rawDataInWeekend$interval, rawDataInWeekend$steps, type = "l", ylab = "", xlab = "Interval", col = "blue")
axis(1); axis(4, at = NULL, labels = FALSE);
mtext("Number of steps", 2, outer = TRUE)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
