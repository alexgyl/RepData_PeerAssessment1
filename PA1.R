## Reproducible Research Course Project 1
library(lubridate)
library(dplyr)
library(chron)
library(ggplot2)
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
     breaks = 50,col = "red",
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
# Calculating total no. of NAs in data set
numNA = sum(is.na(fulldata$steps))
# To impute missing data, we will use the mean for the relavant 5min interval for the day
newdata = fulldata
imputeData = function(x1,x2){
  if(is.na(x1)){
    x1 = avg_data[avg_data[,1] == x2,2]
  }
  else
    x1 = x1
}
## This applies a custom function to the data and returns the mean of the interval if the original input is NA
t1 = as.vector(unlist(mapply(imputeData,newdata$steps,newdata$interval,SIMPLIFY = TRUE)))L
# Overwriting the original data with the new imputed data
newdata$steps = t1

## Histogram of the total number of steps taken each day after missing values are imputed
## Calculating total number of steps taken each day
sum_data_new = newdata %>% select(steps,date) %>% group_by(date) %>% summarize(totalsteps = sum(steps))

## Histogram of the total number of steps taken each day
hist(sum_data_new$totalsteps,
     xlab = "Total Steps per day",
     breaks = 50,col = "red",
     main = "Histogram of the Total Number of Steps per day")

## Mean and median number of steps taken each day after imputing values
meanStepsNew = mean(sum_data_new$totalsteps)
medianStepsNew = median(sum_data_new$totalsteps)


## Creating factor variable for weekday/weekend
newdata$weekday = is.weekend(newdata$date)
newdata$weekday = factor(ifelse(newdata$weekday,"weekend","weekday"))

## Summarizing data across 5-minute interval and weekday/weekend
sum_data_wk = newdata %>% select(steps,interval,weekday) %>% group_by(interval,weekday) %>% summarize(meansteps = mean(steps))
## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
ggplot(data = sum_data_wk, aes(x = interval,y = meansteps)) +
  geom_line() +
  facet_grid(weekday~.) + 
  xlab("5-minute Intervals") +
  ylab("Average No. of Steps")



