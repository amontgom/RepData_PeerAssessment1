#Retrieve File from Internet + Processing
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile() #No need to store uncessary .zip files
download.file(fileUrl, destfile = temp, mode = "wb") #add a ", method = "curl"" if using a mac
dateDowloaded <- date()
dateDowloaded
unzip(temp, "activity.csv") #unzips the file, turns it into a readable .txt file
ActivityData <- read.csv("activity.csv", header = TRUE)
unlink(temp)

#Converts Factor-class dates into Date-class dates
library(lubridate)
ActivityDataDate <- ActivityData
ActivityDataDate$date<-ymd(ActivityData$date)

#Cleans the data of NA's
ActivityDataDateClean <- na.omit(ActivityDataDate)



#What is mean total number of steps taken per day?
#Totals per day
totalStepsPerDay <- c()
numDates <- length(unique(ActivityDataDateClean$date))
for(i in 1:numDates) {
    dates <- unique(ActivityDataDateClean$date)[i]
    totalStepsPerDay[i] <- sum(ActivityDataDateClean$steps[ActivityDataDateClean$date == dates])
}

#Create the total number of steps histogram
hist(totalStepsPerDay, xlab = "Total Steps Per Day", main = "Total Steps Per Day")

#Mean and median of the total
mean(totalStepsPerDay)
median(totalStepsPerDay)



#What is the average daily activity pattern?
#Mean per Interval
meanStepsPerInterval <- c()
numIntervals <- length(unique(ActivityDataDateClean$interval))
for(i in 1:numIntervals) {
    intervals <- unique(ActivityDataDateClean$interval)[i]
    meanStepsPerInterval[i] <- mean(ActivityDataDateClean$steps[ActivityDataDateClean$interval == intervals])
}

#Create the mean number of steps plot
plot(x = unique(ActivityDataDateClean$interval), y = meanStepsPerInterval,
     type = "l", xlab = "Interval (Minutes)", ylab = "Mean Steps",
     main = "Daily Activity Pattern")

#Interval with the maximum number of steps
unique(ActivityDataDateClean$interval)[which(meanStepsPerInterval == max(meanStepsPerInterval))]



#Imputing missing values
#Find # rows with NAs
nrow(ActivityDataDate[is.na(ActivityDataDate),]) #2304

#Fill the NA's with the mean-per-interval; aggregate() makes this easy
ActivityDataNAFilled <- as.data.frame(ActivityDataDate)
ActivityDataAggregate <- aggregate(steps ~ interval, ActivityDataDate, mean)
ActivityDataNAFilled$steps[is.na(ActivityDataNAFilled$steps)] <- ActivityDataAggregate$steps

#Imputed total number of steps calculation
totalStepsPerDayImputed <- c()
numDates1 <- length(unique(ActivityDataNAFilled$date))
for(i in 1:numDates1) {
    dates1 <- unique(ActivityDataNAFilled$date)[i]
    totalStepsPerDayImputed[i] <- sum(ActivityDataNAFilled$steps[ActivityDataNAFilled$date == dates1])
}

#Imputed total number of steps histogram plotter
hist(totalStepsPerDayImputed, xlab = "Total Steps Per Day", main = "(Imputed) Total Steps Per Day")

#Mean and median of the imputed total
mean(totalStepsPerDayImputed)
median(totalStepsPerDayImputed)



#Are there differences in activity patterns between weekdays and weekends?
#sets up two new columns, for day-of-week and weekend/day type
ActivityDataNAFilledDay <- as.data.frame(ActivityDataNAFilled)
ActivityDataNAFilledDay$day <- weekdays(ActivityDataNAFilled$date)
ActivityDataNAFilledDay$weekday <- weekdays(ActivityDataNAFilled$date)

#Sorts the rows by weekend or weekday
numDates2 <- length(ActivityDataNAFilledDay$date)
for(i in 1:numDates2) {
    if (ActivityDataNAFilledDay$day[i] == "Saturday" || ActivityDataNAFilledDay$day[i] == "Sunday") {
        ActivityDataNAFilledDay$weekday[i] <- "Weekend"
    }
    else {
        ActivityDataNAFilledDay$weekday[i] <- "Weekday"
    }
}

#Convert weekday from character to factor, needed for the aggregate
ActivityDataNAFilledDay$weekday <- as.factor(ActivityDataNAFilledDay$weekday)

#Aggregate once again proves useful, steps as interval to get average number of steps in an interval across all days
ADNAFDInterval <- aggregate(steps ~ interval+weekday, ActivityDataNAFilledDay, mean)

#Make the panel plot for weekdays/ends
library(ggplot2)
qplot(interval, steps, data=ADNAFDInterval, geom=c("line"), xlab="Interval", ylab="Number of steps", main="") + facet_wrap(~ weekday, ncol=1)



#Removes now-unnecessary data from memory
rm(ActivityData, ActivityDataAggregate, ActivityDataDate, ActivityDataDateClean, ActivityDataNAFilled, ActivityDataNAFilledDay, ADNAFDInterval)