

## Getting and cleaning data Project


### Introduction and context

One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked in this project represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

 http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data for the project can be downloaded from: 
  
 https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
 
# Variable list and descriptions

YTrain Contains the YTrain dataset
XTrain Contains the XTrain dataset
TrainingSubjects contains the data from "UCI HAR Dataset/train/subject_train.txt"

YTest Contains the YTest data set
XTest Contains the XTest data set

TestsSubjects contains the data from "UCI HAR Dataset/test/subject_test.txt"

MergedData contains the data merged from both datasets

OnlyMeanData contains only the data with the mean values

OnlySTDData contains only the data with the STD values

TidyData contains the tidy dataset


### Project Tasks
This project includes one R script called run_analysis.R that does the following. 

* Merges the training and the test sets to create one data set.
* Extracts only the measurements on the mean and standard deviation for each measurement. 
* Uses descriptive activity names to name the activities in the data set
* Appropriately labels the data set with descriptive variable names. 
* From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Project Steps Explained

##Must load the packages to be used

packages <- c("data.table", "reshape2")

### We set the working directory

path <- getwd()

### load the required libraries
library(plyr)  
library(data.table) 
library(dplyr) 


### download the data set of the band information

dataFile <- "SamsungDataset.zip"
download.file("http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",dataFile)
unzip(dataFile) 


#get the features in the data set
Features <- read.table(unzip(dataFile, "UCI HAR Dataset/features.txt"))


### The following lines read the training data

YTrain <- read.table(unzip(dataFile, "UCI HAR Dataset/train/y_train.txt"))
XTrain <- read.table(unzip(dataFile, "UCI HAR Dataset/train/X_train.txt"))

#The following lines read the subject contained in the train dataset
TrainingSubjects <- read.table(unzip(dataFile, "UCI HAR Dataset/train/subject_train.txt"))



### The following lines read the testing data
YTest <- read.table(unzip(dataFile, "UCI HAR Dataset/test/y_test.txt"))
XTest <- read.table(unzip(dataFile, "UCI HAR Dataset/test/X_test.txt"))

#The following lines read the subject contained in the test dataset

TestsSubjects <- read.table(unzip(dataFile, "UCI HAR Dataset/test/subject_test.txt"))



### Transpose the features to get the proper column names

colnames(XTrain) <- t(Features[2])
colnames(XTest) <- t(Features[2])

### Get the data set to be merged

XTrain$activities <- YTrain[, 1]
XTrain$subjects <- TrainingSubjects[, 1]


XTest$activities <- YTest[, 1]
XTest$subjects <- TestsSubjects[, 1]


# Task 01: In this task we merge the training and the test sets to create merged one.

MergedData <- rbind(XTrain, XTest)

#find the duplicates and remove it
duplicated(colnames(MergedData))

#remove the duplicates
MergedData <- MergedData[, !duplicated(colnames(MergedData))]


# Task 02: Extract only the data that contains "mean" in its column name
Mean <- grep("mean()", names(MergedData), value = FALSE, fixed = TRUE)


OnlyMeanData <- MergedData[Mean]


# Task 02: Extract only the data that contains "std" in its column name

STD <- grep("std()", names(MergedData), value = FALSE)
OnlySTDData <- MergedData[STD]


# Task 03 Uses descriptive activity names to name the activities in the data set

### Change de column names to give proper names

MergedData$activities <- as.character(MergedData$activities)
MergedData$activities[MergedData$activities == 1] <- "Person is Walking"
MergedData$activities[MergedData$activities == 2] <- "Person is Walking Upstairs"
MergedData$activities[MergedData$activities == 3] <- "Person is Walking Downstairs"
MergedData$activities[MergedData$activities == 4] <- "Person is Sitting"
MergedData$activities[MergedData$activities == 5] <- "Person is Standing"
MergedData$activities[MergedData$activities == 6] <- "Person is Laying"

MergedData$activities <- as.factor(MergedData$activities)


# Task 04 Appropriately labels the data set with descriptive variable names.

names(MergedData)  

### Taks 04 Change the names of the features

names(MergedData) <- gsub("Acc", "Accelerator", names(MergedData))
names(MergedData) <- gsub("Mag", "Magnitude", names(MergedData))
names(MergedData) <- gsub("Gyro", "Gyroscope", names(MergedData))
names(MergedData) <- gsub("^t", "Time", names(MergedData))
names(MergedData) <- gsub("^f", "Frequency", names(MergedData))

### Taks 04 Change participants names

MergedData$subjects <- as.character(MergedData$subjects)
MergedData$subjects[MergedData$subjects == 1] <- "Person 1"
MergedData$subjects[MergedData$subjects == 2] <- "Person 2"
MergedData$subjects[MergedData$subjects == 3] <- "Person 3"
MergedData$subjects[MergedData$subjects == 4] <- "Person 4"
MergedData$subjects[MergedData$subjects == 5] <- "Person 5"
MergedData$subjects[MergedData$subjects == 6] <- "Person 6"
MergedData$subjects[MergedData$subjects == 7] <- "Person 7"
MergedData$subjects[MergedData$subjects == 8] <- "Person 8"
MergedData$subjects[MergedData$subjects == 9] <- "Person 9"
MergedData$subjects[MergedData$subjects == 10] <- "Person 10"
MergedData$subjects[MergedData$subjects == 11] <- "Person 11"
MergedData$subjects[MergedData$subjects == 12] <- "Person 12"
MergedData$subjects[MergedData$subjects == 13] <- "Person 13"
MergedData$subjects[MergedData$subjects == 14] <- "Person 14"
MergedData$subjects[MergedData$subjects == 15] <- "Person 15"
MergedData$subjects[MergedData$subjects == 16] <- "Person 16"
MergedData$subjects[MergedData$subjects == 17] <- "Person 17"
MergedData$subjects[MergedData$subjects == 18] <- "Person 18"
MergedData$subjects[MergedData$subjects == 19] <- "Person 19"
MergedData$subjects[MergedData$subjects == 20] <- "Person 20"
MergedData$subjects[MergedData$subjects == 21] <- "Person 21"
MergedData$subjects[MergedData$subjects == 22] <- "Person 22"
MergedData$subjects[MergedData$subjects == 23] <- "Person 23"
MergedData$subjects[MergedData$subjects == 24] <- "Person 24"
MergedData$subjects[MergedData$subjects == 25] <- "Person 25"
MergedData$subjects[MergedData$subjects == 26] <- "Person 26"
MergedData$subjects[MergedData$subjects == 27] <- "Person 27"
MergedData$subjects[MergedData$subjects == 28] <- "Person 28"
MergedData$subjects[MergedData$subjects == 29] <- "Person 29"
MergedData$subjects[MergedData$subjects == 30] <- "Person 30"
MergedData$subjects <- as.factor(MergedData$subjects)

# Task 05 creates a second, independent tidy data set with the average of each variable for each activity and each subject

MergedData.dt <- data.table(MergedData)
TidyData <- MergedData.dt[, lapply(.SD, mean), by = 'subjects,activities']
write.table(TidyData, file = "TidyData.txt", row.names = FALSE)


