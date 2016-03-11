# Reproducible Research: Peer Assessment 1

## Initialise the environment

```r
library(dplyr)
library(lubridate)
library(ggplot2)
```

## Loading and preprocessing the data

```r
steps <- read.csv("activity.csv")
steps <- steps %>% mutate(date=ymd(date))
```

## What is mean total number of steps taken per day?

```r
steps_per_day  <- steps %>% group_by(date) %>% summarise(total=sum(steps,na.rm=TRUE))

mean_steps_per_day <- mean(steps_per_day$total,na.rm=TRUE)
median_steps_per_day <- median(steps_per_day$total,na.rm=TRUE)

ggplot(steps_per_day,aes(total))+
  geom_histogram(bins = 20)+
  geom_vline(aes(xintercept = mean_steps_per_day,color="Mean"))+
  geom_vline(aes(xintercept = median_steps_per_day,color="Median")) +
  scale_color_manual("Statistics", values = c("Mean" = "red", "Median" = "green"))
```

![](PA1_template_files/figure-html/mean_steps_per_day-1.png)

The mean number of steps per day are ``9354.2295082`` and the median number of steps per day are ``10395``.


## What is the average daily activity pattern?



## Inputing missing values



## Are there differences in activity patterns between weekdays and weekends?
