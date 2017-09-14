#Retrieve File from Internet + Processing
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile() #No need to store uncessary .zip files
download.file(fileUrl, destfile = temp, mode = "wb")
#add a ", method = "curl"" if using a mac
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

#Total number of steps histogram
hist(totalStepsPerDay, xlab = "Total Steps Per Day", main = "Total Steps Per Day")

#Mean and median of the total
mean(totalStepsPerDay)
median(totalStepsPerDay)



#What is the average daily activity pattern?
#Plotter
meanStepsPerInterval <- c()
numIntervals <- length(unique(ActivityDataDateClean$interval))
for(i in 1:numIntervals) {
    intervals <- unique(ActivityDataDateClean$interval)[i]
    meanStepsPerInterval[i] <- mean(ActivityDataDateClean$steps[ActivityDataDateClean$interval == intervals])
}
plot(x = unique(ActivityDataDateClean$interval), y = meanStepsPerInterval,
     type = "l", xlab = "Interval (Minutes)", ylab = "Total Steps",
     main = "Daily Activity Pattern")

#Interval with the maximum number of steps
unique(ActivityDataDateClean$interval)[which(meanStepsPerInterval == max(meanStepsPerInterval))]



#Imputing missing values
#Find # rows with NAs
nrow(ActivityDataDate[is.na(ActivityDataDate),]) #2304

ActivityDataNAFilled <- as.data.frame(ActivityDataDate)
ActivityDataAggregate <- aggregate(steps ~ interval, ActivityDataDate, mean)
ActivityDataNAFilled$steps[is.na(ActivityDataNAFilled$steps)] <- ActivityDataAggregate$steps

totalStepsPerDay1 <- c()
numDates1 <- length(unique(ActivityDataNAFilled$date))
for(i in 1:numDates) {
    dates1 <- unique(ActivityDataNAFilled$date)[i]
    totalStepsPerDay1[i] <- sum(ActivityDataNAFilled$steps[ActivityDataNAFilled$date == dates1])
}

hist(totalStepsPerDay1, xlab = "Total Steps Per Day", main = "(Imputed) Total Steps Per Day")

