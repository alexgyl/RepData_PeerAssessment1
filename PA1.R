## Reproducible Research Course Project 1
library(lubridate)
library(dplyr)
## Code for reading in the dataset and/or processing the data
fulldata = read.csv("activity.csv",header = TRUE,sep = ",",na.strings = "NA")
fulldata = tbl_df(fulldata)
# Changing the date variable to appropriate format
fulldata$date = ymd(fulldata$date)

## Calculating total number of steps taken each day
sum_data = fulldata %>% select(steps,date) %>% group_by(date) %>% summarize(totalsteps = sum(steps,na.rm=TRUE))

## Histogram of the total number of steps taken each day
hist(sum_data$totalsteps,
     xlab = "Total Steps per day",
     breaks = 10,col = "red",
     main = "Histogram of the Total Number of Steps per day")

## Mean and median number of steps taken each day
meanSteps = mean(sum_data$totalsteps,na.rm = TRUE)
medianSteps = median(sum_data$totalsteps,na.rm = TRUE)

## Time series plot of the average number of steps taken
avg_data = fulldata %>% select(steps,interval) %>% group_by(interval) %>% summarize(avgsteps = mean(steps,na.rm=TRUE))
with(avg_data,
     plot(avgsteps~interval,
          type = "l",
          ylab = "Average No. of Steps",
          xlab = "Time Interval",
          main = "Average number of steps per time interval"))

## The 5-minute interval that, on average, contains the maximum number of steps
maxInterval = avg_data$interval[max(avg_data$avgsteps)]

## Code to describe and show a strategy for imputing missing data


## Histogram of the total number of steps taken each day after missing values are imputed


## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


## All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

