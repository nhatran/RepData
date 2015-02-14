---
title: "PA1_template"
author: "Nha Tran"
date: "Friday, February 14, 2015"
output: html_document
---

# Assignment 1 for the Reproducible Data Course  


## 1) Load and process data

Data file "Activity monitoring data"
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


```r
library(knitr)
```

####  1.1) Load the data


```r
 alldata <- read.csv("activity.csv", header = TRUE)
```

####  1.2) Process the dataset


```r
 actdata <- na.omit(alldata)
```

## 2) The Total number of steps taken per day


```r
 sumdat <- aggregate(steps ~ date, data = actdata, sum)
```

####  2.1) Histogram of the total daily steps


```r
 hist(sumdat$steps, xlab = "Steps",breaks = 10, ylim = c(0,20),
           col = rgb(1,0,0,0.2), main = "Daily Total Steps")
```

![plot of chunk DailyTotalSteps](figures/DailyTotalSteps-1.png) 

```r
dev.off()
```

```
## null device 
##           1
```

The Mean for total number of steps taken per day (NA removed):
1.0766189 &times; 10<sup>4</sup>

The Median for total number of steps taken per day (NA removed):
10765


## 3) Average daily activity pattern

####  3.1) Plot of the 5 minutes interval and average steps taken 


```r
 intdat <- aggregate(steps ~ interval, data = actdata, mean)

 plot(intdat$interval, intdat$steps,main = "Average Daily Activity Pattern",
           type = "l", xlab = "5 mins Interval",ylab = "Average Steps")
```

![plot of chunk AveDailyActivity](figures/AveDailyActivity-1.png) 

####  3.2) Maximim number of Steps

The 5 mins interval containing the maximum number of steps 835, 206.1698113  


## 4) Imputing missing values

#### 4.1) Total NA in datasets

Total missing values in the dataset: 2304, 0, 0

####  4.2) Replace NA with the mean for that 5 mins interval


```r
mdat <- merge(alldata, intdat, "interval")
 names(mdat) <- c("interval","steps","date","stepmean") 
 mdat$steps[is.na(mdat$steps)] <- mdat$stepmean[is.na(mdat$steps)]
```

####  4.3) Histogram of the total number of Steps per day with NA filled


```r
 sumdat2 <- aggregate(steps ~ date, data = mdat, sum)
 hist(sumdat2$steps, xlab = "Steps",breaks = 10, col = rgb(0,0,1,0.2),
        main = "Daily Total Steps (missing data filled)")
```

![plot of chunk DailyTotStepwFill](figures/DailyTotStepwFill-1.png) 

####  4.4) The impact of imputing missing data

The mean for the total number of steps taken per day (NA filled)
1.0766189 &times; 10<sup>4</sup>

The Median for the total number of steps taken per day (NA filled)
1.0766189 &times; 10<sup>4</sup>

There is a difference between removing NA and filling the missing value with the mean value.  The mean value is the same but the median value is different.
We can also see the difference by plotting both case on the same plot


```r
 h1col <- rgb(1,0,0,0.2)
 h2col <- rgb(0,0,1,0.2)
 hist(sumdat$steps,breaks = 10, ylim =c(0, 30), col = h1col,
       xlab = "Steps", main = "Daily Total Steps" )
 hist(sumdat2$steps, breaks = 10, ylim = c(0, 30),
       add=T, col = h2col)
 legend('topleft',c("NA removed","NA filled"),
       fill = c(h1col, h2col),bty="n",border=NA)
```

![plot of chunk Compareplots](figures/Compareplots-1.png) 


## 5) Weekday and Weekend plots

####  5.1) Create new factor variable with weekday (0) and weekend (1)


```r
 intdat2 <- mdat
 intdat2$day <- weekdays(as.Date(intdat2$date))
 intdat2$daycat[intdat2$day=="Saturday" | intdat2$day=="Sunday"] <- 1
 intdat2$daycat[!(intdat2$day=="Saturday" | intdat2$day =="Sunday")] <- 0

 meandat2 <- aggregate(steps ~ interval + daycat, data = intdat2, mean)
```

####  5.2) Plot the average steps for weekday and weekend


```r
library(lattice)
 daycat.f <- factor(meandat2$daycat,levels = c(0,1),labels = c("Weekday", "Weekend"))
 xyplot(steps ~ interval | daycat.f, data = meandat2, layout = c(1,2),
       type = "l", main = "Average number of Steps on Weekday and Weekend",
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk WeekendWeekday](figures/WeekendWeekday-1.png) 





