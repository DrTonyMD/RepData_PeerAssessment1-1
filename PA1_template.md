Reproducible Reasearch - Week 2 Assignment
------------------------------------------

Loading and preprocessing the data
==================================

Read in data from file (activity.csv), add columns for Day of week and
POSIXct date/time format

    library(ggplot2)
    library(lattice)
    library(gridExtra)
    library(plyr)
    library(dplyr)
    setwd("C:/Users/Anthony/Documents/R/ReproducibleResearch")
    stepdata <- read.csv("activity.csv", header=TRUE)

    #Determine days of week, and add a column for POSIXct format of dates
    stepdata$day <- weekdays(as.Date(stepdata$date))
    stepdata$time <- as.POSIXct(stepdata$date, format="%Y-%m-%d")

    #Remove missing data
    stepdata1 <- stepdata[!is.na(stepdata$steps),]

What is mean total number of steps taken per day?
=================================================

    stepsPerDay <- aggregate(stepdata1$steps ~ stepdata1$date, FUN=sum)
    colnames(stepsPerDay) <- c("Date", "Steps")

    #Plot histogram of daily step frequencies
    hist(stepsPerDay$Steps, xlab="Steps", main="Histogram of Daily Step Frequencies Per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    #Calculate and report the mean and median of the total number of steps taken per day
    meanStepsPerDay <- as.integer(mean(stepsPerDay$Steps))
    medianStepsPerDay <- as.integer(median(stepsPerDay$Steps))
    paste("The mean number of steps per day was", meanStepsPerDay)

    ## [1] "The mean number of steps per day was 10766"

    paste("The median number of steps per day was", medianStepsPerDay)

    ## [1] "The median number of steps per day was 10765"

What is the average daily activity pattern?
===========================================

    #Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
    #Aggregate mean steps by interval
    interval5data <- aggregate(stepdata1$steps, by=list(interval=stepdata1$interval), FUN=mean)

    plot(interval5data$interval, interval5data$x, type="l", ylab="Average Steps in 5 min Intervals", xlab="5-min Interval", main="Average Steps in 5-min Intervals for All Data")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    #5-min interval with max # of steps
    maxSteps <- max(interval5data$x)
    maxStepsInterval <- interval5data$interval[interval5data$x==maxSteps]
    paste("The 5-minute interval with the most mean number of steps was",maxStepsInterval)

    ## [1] "The 5-minute interval with the most mean number of steps was 835"

    paste("The maximum mean number of steps in that interval was", maxSteps)

    ## [1] "The maximum mean number of steps in that interval was 206.169811320755"

Imputing missing values
=======================

    #Calculate and report the total number of missing values in the dataset
    missingValues <- nrow(stepdata) - nrow(stepdata1)
    missingValues

    ## [1] 2304

    #Devise a strategy for filling in all of the missing values in the dataset: Use the mean number of steps for each interval
    #Use mean step data per interval from interval5data
    imputed_stepdata <- stepdata
    for (i in 1:nrow(stepdata)){
         if(is.na(imputed_stepdata[i,"steps"])){
              imputed_stepdata[i,"steps"] <-interval5data$x[which(interval5data$interval==imputed_stepdata$interval[i],)]
         }
    }
    imputed_stepsPerDay <- aggregate(imputed_stepdata$steps ~ imputed_stepdata$date, FUN=sum)
    colnames(imputed_stepsPerDay) <- c("Date", "Steps")
    #Plot histogram of 
    hist(imputed_stepsPerDay$Steps, xlab="Steps", main = "Total Steps per Day with/without NAs", col="Black")
    hist(stepsPerDay$Steps, col="Grey", xlab="Steps", main = "Total Steps per Day with/without NAs", add=T)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    #
    imputed_meanStepsPerDay <- as.integer(mean(imputed_stepsPerDay$Steps))
    imputed_medianStepsPerDay <- as.integer(median(imputed_stepsPerDay$Steps))
    paste("The mean number of steps per day with NAs removed was", meanStepsPerDay)

    ## [1] "The mean number of steps per day with NAs removed was 10766"

    paste("The mean number of steps per day with NAs imputed was", imputed_meanStepsPerDay)

    ## [1] "The mean number of steps per day with NAs imputed was 10766"

    paste("The median number of steps per day with NAs removed was", medianStepsPerDay)

    ## [1] "The median number of steps per day with NAs removed was 10765"

    paste("The median number of steps per day with NAs imputed was", imputed_medianStepsPerDay)

    ## [1] "The median number of steps per day with NAs imputed was 10766"

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

    #Use imputed_stepdata, and change days to either weekday or weekend
    imputed_stepdata1 <- imputed_stepdata
    imputed_stepdata1$wkend <-  ifelse(imputed_stepdata1$day %in% c("Saturday","Sunday"),"Weekend","Weekday")
    imp_data_wkday <- filter(imputed_stepdata1,imputed_stepdata1$wkend=="Weekday")
    imp_data_wkend <- filter(imputed_stepdata1,imputed_stepdata1$wkend=="Weekend")

    #Calculate mean steps for each 5-min interval for weekends and weekdays
    mean5data_wkend <- aggregate(imp_data_wkend$steps, by=list(interval=imp_data_wkend$interval), FUN=mean)
    mean5data_wkday <- aggregate(imp_data_wkday$steps, by=list(interval=imp_data_wkday$interval), FUN=mean)

    #Plot steps per 5-min intervals for weekdays and wekends

    p_wkday <- qplot(interval, x,data=mean5data_wkday, geom="line")
    p_wkday <- p_wkday +  ggtitle("Average Steps in 5-min Intervals for Weekdays") + ylab("Avg Steps/5 min Ints")
    p_wkend <- qplot(interval, x,data=mean5data_wkend, geom="line")
    p_wkend <- p_wkend +  ggtitle("Average Steps in 5-min Intervals for Weekends") + ylab("Avg Steps/5 min Ints")

    grid.arrange(p_wkday,p_wkend, ncol=1) 

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-5-1.png)

There is a difference in the pattern of steps weekdays vs weekends, probably related to work/leisure time differences.
======================================================================================================================
