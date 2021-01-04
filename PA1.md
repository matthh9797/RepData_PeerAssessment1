Week 2 Project 1 - Reproducable Research 
======================================

## Loading and Preprocessing the Data

Read the data into R.

```{r echo=TRUE}
library(xtable)
activity <- read.csv("data/activity.csv")
head(activity)
```

Transform the data so that date is a date variable and interval is a factor variable. Pluck out variable names.

```{r echo = TRUE}
str(activity)
# transform the variables
activity <- transform(activity, date = as.Date(date, "%Y-%m-%d"), interval = as.factor(interval))
# pluck out the variables
steps_na <- activity$steps
date <- activity$date
interval <- activity$interval
```

## What is mean total number of steps taken per day?

Create a histogram of the total steps taken per day ignoring the missing values.

```{r echo=TRUE}
tot_na <- tapply(steps_na, date, sum, na.rm = T)
hist(tot_na, xlab = "Total Steps per Day", main = "Histogram of Total Number of Steps taken per Day")
```

Create variables of the mean and median steps per day ignoring missing values.

```{r echo=TRUE}
mn0_date <- tapply(steps_na, date, mean, na.rm = T)
md0_date <- tapply(steps_na, date, median, na.rm = T)
head(mn0_date)
head(md0_date)
```

## Q3. Average Daily Activity Pattern

This is a time-series of the average steps taken per day averaged across all days.

```{r echo=TRUE}
mn0_int <- tapply(steps_na, interval, mean, na.rm = T)
plot(names(mn0_int), mn0_int, type = "l", xlab = "Mean steps per 5-minute interval",
     main = "Time Series of Mean Number of Steps Taken per 5-minute Interval")
```

Which interval has the maximum number of steps on average.

```{r echo=TRUE}
which.max(mn0_int)
```

## Imput Missing Values

Calculate the total missing values in the dataset.

```{r echo=TRUE}
sum(is.na(activity))
```

Fill in the missing values of steps with the mean value of that interval

```{r echo=TRUE}
activity <- transform(activity, mean = mn0_int[interval])
steps <- steps_na
steps[is.na(steps)] <- activity$mean[is.na(steps)]
```

Create a dataframe which is the same as the orginal but with missing values filled in.

```{r echo=TRUE}
activity_nona <- read.csv("data/activity.csv")
activity_nona$steps <- steps
```

Make a histogram of the total number of steps taken per day and compare with the original dataset.

```{r echo=TRUE}
par(mfrow = c(1,2), oma = c(0,0,2,0), mar = c(5,4,2,1))
tot <- tapply(steps, date, sum, na.rm = T)
hist(tot_na, xlab = "Total number of steps", main = "Histogram with NA's")
hist(tot, xlab = "Total number of steps", main = "Histogram without NA's")
mtext("Comparison of total number of steps with NA's Filled in", outer = TRUE, cex = 1.5)
```

Create new mean and median vectors and compare with the original dataset by using a barplot. This shows that whilst the mean vactor has not changed the median value of the dataset has decreased.

```{r echo=TRUE}
# mean and median
mn1_date <- tapply(steps, date, mean)
md1_date <- tapply(steps, date, mean)
head(mn1_date)
head(md1_date)
# compaison plot
diff_mn <- mn0_date - mn1_date
diff_md <- md0_date - md1_date
par(mfrow = c(1,2), oma = c(0,0,2,0), mar = c(5,4,2,1))
barplot(diff_md)
barplot(diff_mn)
```

## Are there any difference in activity pattern from the Weekday to Weekend?

Create a new factor variable with two levels "weekday" and "weekend".

```{r echo=TRUE}
# weekdays and weekends?
weekend <- c("Saturday", "Sunday")
day <- weekdays(date)
is.weekend <- day %in% weekend
week <- as.numeric(is.weekend)
lbls <- c("Weekday", "Weekend")
# week is a factor variable with weekday or weekend
week <- factor(week, labels = lbls)
head(week)
```

Create a panel plot of the average number of steps taken on weekday's compared to the weekend. This plot shows that the person is more active on the weekend.

```{r echo=TRUE}
library(dplyr)
# plot weekday against weekend
activity$week <- week
# create mean week dataframe with the mean values classified by interval and weekday
mean_week <- activity %>% group_by(week, interval) %>% summarise(mean = mean(steps, na.rm = TRUE))
mean_wd <- mean_week$mean[mean_week$week == "Weekday"]
names(mean_wd) <- mean_week$interval[mean_week$week == "Weekday"]
mean_we <- mean_week$mean[mean_week$week == "Weekend"]
names(mean_we) <- mean_week$interval[mean_week$week == "Weekend"]
# make plot 
par(mfrow = c(1,2), oma = c(0,0,2,0), mar = c(5,4,2,1))
plot(names(mean_wd), mean_wd, type = "l", xlab = "5-minute Interval", ylab = "Mean Steps",
     main = "Mean Steps on a Weekday")
plot(names(mean_we), mean_we, type = "l", xlab = "5-minute Interval", ylab = "Mean Steps",
     main = "Mean Steps on the Weekend")
mtext("Comparison of Step Count on the Weekend or a Weekday", outer = TRUE, cex = 1.5)
```









