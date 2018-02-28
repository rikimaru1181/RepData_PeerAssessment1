PA1\_template.rmd
================
vince
February 28, 2018

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.4.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.4.3

``` r
activities <- read.csv("activity.csv")
activities$date <- ymd(activities$date)

activities_per_day <- group_by(activities, date)
activities_per_interval <- group_by(activities, interval)
```

Histogram of the total number of steps taken each day:

``` r
items <- tapply(activities$steps, activities$date, FUN=sum)
barplot(items, las=2, cex.names = 0.5)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-3-1.png)

Mean and median number of steps taken each day. Time series plot of the average number of steps taken.

``` r
mean_and_median <- summarise(activities_per_day, mean=mean(steps, na.rm = TRUE), n=median(steps, na.rm = TRUE))
plot(mean_and_median$mean, type="l", col="red", ylab = "Steps", xlab = "Day", las=2, main="Average number of steps taken")
lines(mean_and_median$median, type="l", col="blue")
```

    ## Warning: Unknown or uninitialised column: 'median'.

``` r
legend("topright", legend=c("mean", "median"), pch=1, col=c("red", "blue"))
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

The 5-minute interval that, on average, contains the maximum number of steps

``` r
average_of_steps_by_interval <- summarise(activities_per_interval, average=mean(steps, na.rm = TRUE))
ranking_of_intervals <- arrange(average_of_steps_by_interval, desc(average))
print(ranking_of_intervals$interval[1])
```

    ## [1] 835

Code to describe and show a strategy for imputing missing data

-   Total number of missing values in the dataset

``` r
nas <- is.na(activities$steps)
total_empty <- sum(nas)
percentage_empty <- mean(nas)
# Total empty results
print(total_empty)
```

    ## [1] 2304

``` r
# Percentage
print(paste(sprintf("%.2f", percentage_empty * 100), "%", sep=""))
```

    ## [1] "13.11%"

-   Using average of that 5-minute to populate NAs

``` r
not_empty <- mutate(activities)
na_intervals <- not_empty$interval[nas]
average_na_intervals <- sapply(na_intervals, function(item) {average_of_steps_by_interval$average[average_of_steps_by_interval$interval==item]})
not_empty$steps[nas] = average_na_intervals
#average_of_steps_by_interval$average[average_of_steps_by_interval$interval==0]
```

Histogram of the total number of steps taken each day after missing values are imputed

``` r
not_empty_items <- tapply(not_empty$steps, not_empty$date, FUN=sum)

par(mfrow=c(2,1))
barplot(items, las=2, cex.names = 0.5, main="Histogram with missing values (NAs)")
barplot(not_empty_items, las=2, cex.names = 0.5, main="Histogram without missing values (NAs)")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

As we can see, missing values can change the final result of an analysis.

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

``` r
activities_with_days <- mutate(activities, weekday=weekdays(date))
activities_by_day_interval <- group_by(activities_with_days, weekday, interval)

results_per_day <- summarise(activities_by_day_interval, steps=mean(steps, na.rm=TRUE))
qplot(interval, steps, data=results_per_day, facets= weekday ~ .) + geom_line()
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-9-1.png)
