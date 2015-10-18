# Set the working directory
setwd(normalizePath("C:\\Users\\jorda_000\\Documents\\GitHub\\RepData_PeerAssessment1", winslash="\\", mustWork=NA))

# Import data

activity <- read.csv("activity.csv")

# see what it looks like

head(activity)

# remove NA values from data set

activityclean <- data.frame(activity[!is.na(activity$steps),])

#use dplyr to calculate averages
library(dplyr)

#create a group according to date to calculate average steps by date
by_date <- group_by(activityclean, date)

#summarize data and put into a data frame, round to nearest whole number for better graphing
avg.days <- data.frame(summarise(by_date, round(mean(steps),0)))

#rename column header for clarity
colnames(avg.days) <- c("Date", "Steps.Per.Five.Minute.Interval")


#create a group according to interval to calculate average steps by interval
by_interval <- group_by(activityclean, interval)

#summarize data and put into a data frame, round to nearest whole number to impute data
avg.interval <- data.frame(summarise(by_interval, round(mean(steps),0)))

#rename column header for clarity
colnames(avg.interval) <- c("Interval", "Steps.Per.Interval")

#loop to replace missing values in original data set

#subset of data with missing values
activity.replace <- data.frame(activity[is.na(activity$steps),])

for (i in 1: length(activity.replace$interval))
{
  activity.replace[i,1] <- avg.interval[avg.interval$Interval == activity.replace[i,3],2]
}

#combine two data sets
activity.new <- rbind(activity.replace,activityclean)

#order the new combined data set by date and then interval
activity.new <- activity.new[order(activity.new$date, activity.new$interval),]

#Determine day of week of each date and create a new data frame
activity.new.weekdays <- data.frame(Date = activity.new$date, Interval = activity.new$interval,
                                    Steps = activity.new$steps, Day = weekdays(strptime(activity.new$date, format="%Y-%m-%d"))
)

#create a group according to days of the week
by_weekday <- group_by(activity.new.weekdays, Day, Interval)

#summarize data and put into a data frame, round to nearest whole number to impute data
avg.weekday <- data.frame(summarise(by_weekday, round(mean(Steps),0)))

#rename column header for clarity
colnames(avg.weekday) <- c("Day", "Interval", "Avg.Steps.Per.Five.Minute.Interval")

#load ggplot2
library(ggplot2)

#Create plots
plot.date <- qplot(x=Date, y=Steps.Per.Five.Minute.Interval, 
                   data=avg.days, geom="bar", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  labs(title="Mean Steps Per Five Minute Interval by Date")

plot.date.hist <- qplot(Steps.Per.Five.Minute.Interval, 
                   data=avg.days, geom="histogram") +  
  labs(title="Count of Days Grouped by Average Five Minute Interval Step Count")


# Create empty data frame
plot.avg.interval <- data.frame(Date = as.Date(character()), Mean.Steps=integer())

# loop through avg.interval to create time stamps for each interval
for (i in 1:length(avg.interval$Interval))
{
  if(nchar(avg.interval[i,1]) < 3)
  {
    plot.avg.interval.1 <- data.frame(Time = 
                                        strptime(avg.interval[i,1], format="%S"),
                                      Mean.Steps = 
                                        avg.interval[i,2])
    plot.avg.interval <- rbind(plot.avg.interval, plot.avg.interval.1)
  }
  else if(nchar(avg.interval[i,1]) < 4)
  {
    plot.avg.interval.2 <- data.frame(Time = 
                                        strptime(paste("0",avg.interval[i,1], sep=""), format="%H%S"),
                                      Mean.Steps = 
                                        avg.interval[i,2])
    plot.avg.interval <- rbind(plot.avg.interval, plot.avg.interval.2)
  }
  else
  {
    plot.avg.interval.3 <- data.frame(Time = 
                                        strptime(avg.interval[i,1], format="%H%S"),
                                      Mean.Steps = 
                                        avg.interval[i,2])
    plot.avg.interval <- rbind(plot.avg.interval, plot.avg.interval.3)
  }
}

plot.avg.interval.time <- ggplot(plot.avg.interval, aes(Time, Mean.Steps)) + 
  geom_line() + xlab("") + 
  ylab("Mean Steps per Interval") + 
  labs(title="Mean Step Count Troughout the Day")

plot.avg.day.interval <- qplot(x = Interval, y= Avg.Steps.Per.Five.Minute.Interval, data=avg.weekday) + geom_line() + facet_grid(Day ~ .) + xlab("") +
ylab("Mean Steps per Interval") +
labs(title="Mean Step Count Troughout the Day by Day of Week")

