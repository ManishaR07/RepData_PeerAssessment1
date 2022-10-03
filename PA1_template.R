library(ggplot2)


#Loading the data
activity_data <- read.csv("activity.csv")
summary(activity_data)
str(activity_data)

#Making histogram
total_steps <- tapply(activity_data$steps,activity_data$date,sum,na.rm=TRUE)
qplot(total_steps,binwidth=1000,xlab="total number of steps taken each day")

#mean and median of total steps taken
mean(total_steps,na.rm=TRUE)
median(total_steps,na.rm=TRUE)

#Time Series Plot
data_avg <- aggregate(x=list(avg_steps=activity_data$steps), by=list(step_interval=activity_data$interval), 
                      FUN=mean, na.rm=TRUE)
ggplot(data=data_avg, aes(x=step_interval, y=avg_steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

#Max 5 minute interval
max_steps <- data_avg$step_interval[which.max(data_avg$avg_steps)]
max_steps

#Total missing value
colSums(is.na(activity_data))

#Imputing the missing values
activity_data$steps[is.na(activity_data$steps)] <- mean(activity_data$steps, na.rm = TRUE)
activity_new_data <- activity_data

#Histogram after imputing data
total_new_steps <- tapply(activity_new_data$steps,activity_new_data$date,sum,na.rm=TRUE)
qplot(total_new_steps,binwidth=1000,xlab="total number of steps taken each day")

#Mean and median after imputing data
mean(total_new_steps,na.rm = TRUE)
median(total_new_steps,na.rm = TRUE)

#Creating factor variable for weekday and weekend
day_type <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

activity_new_data$date <- as.Date(activity_new_data$date)
activity_new_data$day <- sapply(activity_new_data$date, FUN=day_type)
table(activity_new_data$day)

#Creating Time Series Plot
averages_new_data <- aggregate(steps ~ interval + day, data=activity_new_data, mean)
ggplot(averages_new_data, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")