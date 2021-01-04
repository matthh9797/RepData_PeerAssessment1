## Project 1 - Reproducable Research 

## Q1. Download data and read data
# url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# download.file(url, destfile = "data/activity_data.zip")
# unzip("data/activity_data.zip", exdir = "data")

# Q1. Read and Process the data
activity <- read.csv("data/activity.csv")
head(activity)
str(activity)
# transform the variables
activity <- transform(activity, date = as.Date(date, "%Y-%m-%d"), interval = as.factor(interval))
# pluck out the variables
steps_na <- activity$steps
date <- activity$date
interval <- activity$interval

# Q2. What is the mean total of number of steps taken per day?
# Histogram of total steps taken by day
tot_na <- tapply(steps_na, date, sum, na.rm = T)
hist(tot_na, xlab = "Total Steps per Day", main = "Histogram of Total Number of Steps taken per Day")

# Q3. Mean and Median steps by day
mn0_date <- tapply(steps_na, date, mean, na.rm = T)
md0_date <- tapply(steps_na, date, median, na.rm = T)
## Maybe make a table?

# Q4. Time series of average steps taken by day
mn0_int <- tapply(steps_na, interval, mean, na.rm = T)
plot(names(mn0_int), mn0_int, type = "l", xlab = "Mean steps per 5-minute interval",
     main = "Time Series of Mean Number of Steps Taken per 5-minute Interval")
# Q5. The 5-minute interval that contains the max
# number of steps om average
which.max(mn0_int)

# Q6. Imputing missing values
sum(is.na(activity))
activity <- transform(activity, mean = mn0_int[interval])
steps <- steps_na
steps[is.na(steps)] <- activity$mean[is.na(steps)]
activity_nona <- activity
activity_nona$steps <- steps
sum(is.na(activity))
# create histogram
par(mfrow = c(1,2), oma = c(0,0,2,0), mar = c(5,4,2,1))
tot <- tapply(steps, date, sum, na.rm = T)
hist(tot_na, xlab = "Total number of steps", main = "Histogram with NA's")
hist(tot, xlab = "Total number of steps", main = "Histogram without NA's")
mtext("Comparison of total number of steps with NA's Filled in", outer = TRUE, cex = 1.5)

# mean and median
mn1_date <- tapply(steps, date, mean)
md1_date <- tapply(steps, date, mean)

# compaison plot
diff_mn <- mn0_date - mn1_date
diff_md <- md0_date - md1_date
barplot(diff_md)
barplot(diff_mn)

# Are there differences in activity patterns between
# weekdays and weekends?
weekend <- c("Saturday", "Sunday")
day <- weekdays(date)
is.weekend <- day %in% weekend
week <- as.numeric(is.weekend)
lbls <- c("Weekday", "Weekend")
# week is a factor variable with weekday or weekend
week <- factor(week, labels = lbls)
# plot weekday against weekend
activity$week <- week
# create mean week dataframe with the mean values classified by interval and weekday
library(dplyr)
mean_week <- activity %>% group_by(week, interval) %>% summarise(mean = mean(steps, na.rm = TRUE))

mean_wd <- mean_week$mean[mean_week$week == "Weekday"]
names(mean_wd) <- mean_week$interval[mean_week$week == "Weekday"]
mean_we <- mean_week$mean[mean_week$week == "Weekend"]
names(mean_we) <- mean_week$interval[mean_week$week == "Weekend"]
plot(names(mean_wd), mean_wd, type = "l", xlab = "5-minute Interval", ylab = "Mean Steps",
     main = "Mean Steps on a Weekday")
plot(names(mean_we), mean_we, type = "l", xlab = "5-minute Interval", ylab = "Mean Steps",
     main = "Mean Steps on the Weekend")
mtext("Comparison of Step Count on the Weekend or a Weekday", outer = TRUE, cex = 1.5)



