
# Peer-graded Assignment: Course Project 1



### Data
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### Variables
The variables included in this dataset are:

1. steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
2. Date: The date on which the measurement was taken in YYYY-MM-DD format
3. Interval: Identifier for the 5-minute interval in which measurement was taken

### Loading and preprocessing the data

```r
path<-setwd("D:\\Reproducible Research")

library(downloader)
fileurl<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileurl,destfile="./courseproject.zip")
unzip("courseproject.zip")

library(data.table)
data_raw <- read.table("./activity.csv",sep=",",header=TRUE)
head(data_raw)
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

### What is mean total number of steps taken per day
For this part of the assignment, you can ignore the missing values in the dataset.

#### Calculate the total number of steps taken per day

```r
data_step<-aggregate(steps ~ date, data_raw, sum)
```

#### Make a histogram of the total number of steps taken each day

```r
hist(data_step$steps,main="Mean of Total Number of Steps Per Day",xlab="steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

#####Calculate and report the mean and median of the total number of steps taken per day
The mean of steps taken per day is 10766 steps and the median of steps taken per day is 10765 steps.Please see the calculation below.

```r
step_mean<-mean(data_step$steps)
step_median<-median(data_step$steps)
step_mean
```

```
## [1] 10766.19
```

```r
step_median
```

```
## [1] 10765
```

### What is the average daily activity pattern?
#### Make a time series plot 
Make a time series plot (i.e.TYPE="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
data_stepbyinterval<-aggregate(steps ~ interval, data_raw, mean)
with(data_stepbyinterval,plot(steps~interval,main="Avg Step Per Day by Interval",type="l"),width=480,height=480)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
The 835th interval contains the maximum number of steps, which is 206 steps.Please see the calculation below.

```r
data_stepbyinterval[which.max(data_stepbyinterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
There are 2304 missing values.Please see the calculation below.

```r
sum(is.na(data_raw))
```

```
## [1] 2304
```

#### Devise a strategy for filling in missing values. 
#### Create a new dataset with the missing data filled in.
The methodology is to find the missing value and replace it with the average number of steps in that particular interval.Please see the calculation below.

```r
data_New <- data_raw 
data_New$avgSteps<-data_stepbyinterval$steps
for (x in 1:nrow(data_New)) {
    if (is.na(data_New$steps[x])) {
        data_New$steps[x] <- data_New[which(data_New$interval[x] == data_stepbyinterval$interval), ]$avgSteps}}
sum(is.na(data_New))
```

```
## [1] 0
```

#### Make a histogram of the total number of steps taken each day 
Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
data_step_New<-aggregate(steps ~ date, data_New, sum)
sum(data_New$steps)
```

```
## [1] 656737.5
```

```r
hist(data_step_New$steps,main="New: Mean of Total Number of Steps Per Day",xlab="steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

```r
step_mean_new<-mean(data_step_New$steps)
step_median_new<-median(data_step_New$steps)
step_mean_new
```

```
## [1] 10766.19
```

```r
step_median_new
```

```
## [1] 10766.19
```

```r
mean_diff<-step_mean_new - step_mean
median_diff<-step_median_new - step_median
mean_diff
```

```
## [1] 0
```

```r
median_diff
```

```
## [1] 1.188679
```
The means are the same for imputed & nonimputed data. However, the median for the nonimupted data is 1.188679 smaller than imputed data.
Thus,the impact of the imputed data for the total daily number of steps are: imputed data has 86129 more steps than the nonimputed data.

```r
step_total_new<-sum(data_step_New$steps)-sum(data_step$steps)
step_total_new
```

```
## [1] 86129.51
```

### Are there differences in activity patterns between weekdays and weekends?
For this part the Weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#### Create a new factor variable in the dataset with two levels - "weekday" and "weekend".

```r
data_New$Weekflag<-weekdays(as.Date(data_New$date))
data_New$Weekflag<-ifelse(data_New$Weekflag %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"),"Weekday","Weekend")
```

#### Make a panel plot 
The plot should contain a time series plot (i.e. tyle="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
data_New_plot<-aggregate(steps ~ Weekflag+interval, data_New, mean)
library(lattice)
with(data_New_plot,xyplot(steps~interval|Weekflag,main="Average Steps Per Day by Interval",type="l",xlab="Interval",ylab="Steps",layout=c(1,2)),width=480,height=480)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)
