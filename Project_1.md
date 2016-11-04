Reproducible Research: Project 1
================
Bruce Brown
November 2, 2016

Introduction.
-------------

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and includes the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

-   date: The date on which the measurement was taken in YYYY-MM-DD format

-   interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.\*

The structure of this analysis follows the project instuctions as detailed on the course website and the GitHub repository, rdpeng/RepData\_PeerAssessment1.

Loading and preprocessing the data.
-----------------------------------

*1. Load the data (i.e. read.csv()).*

The dataset is downloaded to the working directory and imported to RStudio. R code is set to be readable.

``` r
activity <- read.csv("~/Desktop/Reproducible Research/Project 1/activity.csv")
```

Taking a look at the dataset.

``` r
head(activity)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
summary(activity)
```

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

*2. Process/transform the data (if necessary) into a format suitable for your analysis.*

Convert the date variable from factor to date.

``` r
activity$date <- as.Date(activity$date)
```

The required libraries are loaded for the analysis.

``` r
library(ggplot2)
library(dplyr)
```

What is mean total number of steps taken per day?
-------------------------------------------------

*For this part of the assignment, you can ignore the missing values in the dataset.*

The aggregate function is used to format the total number of steps by date.

``` r
TotalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```

We can inspect the result.

``` r
head(TotalSteps)
```

    ##         date steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

*1. Make a histogram of the total number of steps taken each day.*

``` r
hist(TotalSteps$steps, main = "Distribution of Total Steps/Day.", xlab = "Total steps per day", col = "blue", breaks = 20)
```

![](Project_1_files/figure-markdown_github/unnamed-chunk-8-1.png)

*2. Calculate and report the mean and median total number of steps taken per day.*

``` r
mean(TotalSteps$steps)
```

    ## [1] 10766.19

``` r
median(TotalSteps$steps)
```

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

Calculate the average daily activity pattern by interval and steps.

``` r
AvgDailyPattern <- activity %>% group_by(interval) %>% filter(!is.na(steps)) %>% summarise(avg_steps = mean(steps, na.rm=TRUE))
```

*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).*

``` r
ggplot(AvgDailyPattern, aes(x =interval , y=avg_steps)) + geom_line(color="blue", size=1) + labs(title = "Average Daily Pattern of Steps by Interval", x = "Interval", y = "Average Steps/Day")
```

![](Project_1_files/figure-markdown_github/unnamed-chunk-11-1.png)

*2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*

This value can be extracted from the AvgDailyPattern data and can also be seen in the plot above as data point (835, 206).

``` r
AvgDailyPattern[which.max(AvgDailyPattern$avg_steps),]
```

    ## # A tibble: 1 x 2
    ##   interval avg_steps
    ##      <int>     <dbl>
    ## 1      835  206.1698

Imputing missing values
-----------------------

*Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.*

*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs).*

``` r
sum(is.na(activity))
```

    ## [1] 2304

*2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*

Missing values in the original dataset, activity, are filled with mean values for the steps variable for the corresponding interval. The new dataset is labled NewActivity.

``` r
NewActivity <- activity
nas<- is.na(NewActivity$steps)
avg_interval<- tapply(NewActivity$steps, NewActivity$interval, mean, na.rm=TRUE, simplify = TRUE)
NewActivity$steps[nas] <- avg_interval[as.character(NewActivity$interval[nas])]
```

Check for missing values in the imputed dataset, NewActivity.

``` r
sum(is.na(NewActivity))
```

    ## [1] 0

No missing values detected in NewActivity dataset.

*4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

The aggregate function is used to format the total number of steps by date for the imputed dataset, NewActivity.

``` r
NewTotalSteps <- aggregate(steps ~ date, data = NewActivity, sum, na.rm = TRUE)
```

``` r
hist(NewTotalSteps$steps, main = "Steps/Day with Imputed Mean Data", xlab = "Steps/Day", col = "blue", breaks = 20)
```

![](Project_1_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
mean(NewTotalSteps$steps)
```

    ## [1] 10766.19

``` r
median(NewTotalSteps$steps)
```

    ## [1] 10766.19

We can see that the mean of steps for the imputed dataset has not changed from the original dataset. This would seem to be intuitive since we have included additional mean data for the 2,304 missing values. The median is now equal to the mean, also intuitive based on the additional mean values that were added. Therefore it appears this method of imputing missing values with mean values does not add meaningful insight into the data analysis regarding mean and median.

The frequency of steps/day has increased in the imputed dataset due to the additional step values which were added to the dataset.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

*1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.*

*2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).*

``` r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
NewActivity$daytype <- as.factor(sapply(NewActivity$date, daytype))
```

Inspect the dataset with new variable added.

``` r
head(NewActivity)
```

    ##       steps       date interval daytype
    ## 1 1.7169811 2012-10-01        0 weekday
    ## 2 0.3396226 2012-10-01        5 weekday
    ## 3 0.1320755 2012-10-01       10 weekday
    ## 4 0.1509434 2012-10-01       15 weekday
    ## 5 0.0754717 2012-10-01       20 weekday
    ## 6 2.0943396 2012-10-01       25 weekday

Redefine interval, as NewInterval, to recognize daytype in the new dataset.

``` r
NewInterval<- NewActivity%>%
      group_by(interval, daytype)%>%
      summarise(NewTotalSteps = mean(steps, na.rm=TRUE))
```

Use ggplot to create side by side graphs depicting number of steps by daytype.

``` r
ggplot(NewInterval, aes(x = interval , y = NewTotalSteps, color = daytype)) +
      geom_line() +
      labs(title = "Daily Steps by Daytype", x = "Interval", y = "Number of Steps") + 
      facet_wrap(~daytype, ncol = 1, nrow=2)
```

![](Project_1_files/figure-markdown_github/unnamed-chunk-22-1.png)

The graphical depiction of the data indicate an overall higher level of step activity on weekend days. This may be explained by the fact that the individual observed may be more active on weekends when not confined to an office or workplace as they might be on weekdays.

There is a noticeable spike in the weekday data which may reflect time of day activity, particulary observations at the beginning of a workday.
