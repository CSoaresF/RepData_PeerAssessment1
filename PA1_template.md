---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
## Introduction
Analysis of data about personal movement using activity monitoring devices.   
The device collects dat at 5 minute intervals through out the day.   
The data consists of two months of data an anonymous individual collected during the months of October and November 2012 and include the number of steps taken in 5 minute interval each day.      
   
#### Dataset: Activity monitoring data [52k]   
*  The dataset is stored in a comma-separated-value (CSV) file.   
*  There are a total of 17,568 observations in this dataset.
*  url for download: https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip   
    
*  Variables of dataset:
*  __steps__: Number of steps in a 5 minute interval   
          (missing values are coded as NA)   
*  __date__: The date on wich the measurement was taken   
          format: YYYY-MM-DD   
*  __interval__: Identifier for the 5 minute interval in which measurement was taken   


## 1- Loading and preprocessing the data   

```r
# libraries
library(dplyr, ggplot2)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# work directory
setwd("C:/COURSERA5")
# if file is not exist in work directory,
# download and unzip
if(!file.exists("activity.csv")) {
    file <- "activity.zip"
    web <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(web, file)
    unzip(file)
}
# load spreadsheet to work space
activity <- read.csv("activity.csv")
#  view file structure
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
## 2- What is mean total number of steps taken per day?   

```r
# total steps by day
tot.steps.day <- activity %>%               # read activity,
    group_by(date) %>%                      # group by date,
    summarise(steps=sum(steps,na.rm=T)) %>% # summarise steps, ignoring NA,
    arrange(date)                           # and order by date

# returns the five first rows
head(tot.steps.day, 5)
```

```
## Source: local data frame [5 x 2]
## 
##         date steps
##       (fctr) (int)
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
```

```r
# quartis of total steps by day
summary(tot.steps.day$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```
*  The mean is 9,354 steps by day.   
*  The median is 10,400 steps by day.   


## 3- What is the average daily activity pattern?   

```r
# 
```



## 4- Imputing missing values   

```r
# 
```



## 5- Are there differences in activity patterns between weekdays and weekends?   
   
Weekends: Sat, Sun.    
Weekdays: Mon, Tues, Wed, Thurs, Fri.   

```r
# 
```
