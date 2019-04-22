---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
library(plyr)
library(ggplot2)

unzip("activity.zip")
activity <- read.csv("activity.csv")

steps <- aggregate(activity$steps, by = list(activity$date), sum, na.rm=TRUE) 
names(steps) <- c("Date", "steps")

hist(steps$steps, main="Steps per day",col="green")

## What is mean total number of steps taken per day?
mean.steps <- mean(steps$steps) 
median.steps <- median(steps$steps)

## What is the average daily activity pattern?

#df of the mean and median number of steps taken, averaged across all days (y-axis)
tssteps <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm=TRUE)
tsstepsmed <- aggregate(activity$steps, by = list(activity$interval), median, na.rm=TRUE)

intsteps <- cbind(tssteps[], tsstepsmed$x)

#Tidy the df names and round the numbers
names(intsteps) = c("interval","mean.steps", "median.steps")
intsteps$mean.steps <- round(intsteps$mean.steps)
intsteps$median.steps <- round(intsteps$median.steps)

## load ggplot2
library(ggplot2)

ggplot(intsteps, aes(x = interval, y = mean.steps)) + geom_line()

##The 5-minute interval that, on average, contains the maximum number of steps
maxsteps <- intsteps$interval[intsteps$mean.steps == max(intsteps$mean.steps)]


## Imputing missing values

#find the missing values
na.steps <- subset(activity, is.na(steps))
num.NAs <-length(na.steps$steps)

#replace the NAs with the median number of steps for that period
nasteps <- data.frame(date=activity$date[is.na(activity$steps)], interval = activity$interval[is.na(activity$steps)], steps=intsteps[match(intsteps$interval, activity$interval[is.na(activity$steps)]),3])

# remove the NA's from the period
activity <- subset(activity, !is.na(steps))

# Append the median steps to the Activity DF
activity <- rbind(activity, nasteps)

#sum the number of steps each day into the dailysteps2 DF and get the mean and median 
dailysteps2 <- aggregate(activity$steps, by = list(activity$date), sum, na.rm=TRUE)
names(dailysteps2) <- c("Date", "steps")

# plotting histogram
qplot(steps, data = dailysteps2, geom="histogram", xlab = "Daily Number of Steps",binwidth=2500)

## calculate the mean and median after missing value inputed
mean.steps2 <- mean(dailysteps2$steps)  
median.steps2 <- median(dailysteps2$steps)

## Are there differences in activity patterns between weekdays and weekends?


#Add the Weekday/weekend identifier
activity$day <- ifelse(as.POSIXlt(as.Date(activity$date))$wday%%6 == 
                                    0, "weekend", "weekday")
activity$day <- factor(activity$day, levels = c("weekday", "weekend"))

steps.interval= aggregate(steps ~ interval + day, activity, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), data = steps.interval, aspect = 1/2, 
       type = "l")

