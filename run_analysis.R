# README.md
# File: explains how all of the scripts work and how they are connected


# Create a temp. file name (eg tempfile())

tmp <- tempfile()

# Use download.file() to fetch the file into the temp. file

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",tmp)
unzip(tmp,list = TRUE)

# Use unz() to extract the target file from temp. file

data.X_test <- read.table(unz(tmp, "UCI HAR Dataset/test/X_test.txt"),sep="", header=FALSE)
data.Y_test <- read.table(unz(tmp, "UCI HAR Dataset/test/y_test.txt"),sep="", header=FALSE)
data.X_train <- read.table(unz(tmp, "UCI HAR Dataset/train/X_train.txt"),sep="", header=FALSE)
data.Y_train <- read.table(unz(tmp, "UCI HAR Dataset/train/y_train.txt"),sep="", header=FALSE)
descriptiveActivityNames <- read.table(unz(tmp, "UCI HAR Dataset/activity_labels.txt"),sep="", header=FALSE)
descriptiveVariableNames <- read.table(unz(tmp, "UCI HAR Dataset/features.txt"),sep="", header=FALSE)
subject_test <- read.table(unz(tmp, "UCI HAR Dataset/test/subject_test.txt"),sep="", header=FALSE)
subject_train <- read.table(unz(tmp, "UCI HAR Dataset/train/subject_train.txt"),sep="", header=FALSE)


# Add a column to the X's t distiguish between test
# and training sets in the merged data

library(dplyr)
data.X_test <- mutate(data.X_test, Set = factor("test"))
data.X_train <- mutate(data.X_train, Set = factor("train"))

# Bind the two Data Set together by stacking one on the other

boundDataX <- rbind(data.X_test, data.X_train)
boundDataY <- rbind(data.Y_test, data.Y_train)


# Append the combined Y data to the X DataSet as an
# addtional column and name it activity

boundDataX[["activity"]] <- boundDataY[,1]

# Append an additional column of activity names to
# facilitate tidying the Dataset

boundDataX$activityName <- factor(boundDataX$activity,
                    levels = descriptiveActivityNames[[1]],
                    labels = descriptiveActivityNames[[2]])

# The newly boundDataX now has columns with "variables"
# i.e. the codes for walking, sitting etc
# I spread the appended Y column over 6 columns by the factor levels asigned
# in the previous step This tidys up the data
# so that each column/variable contains all values that
# measure the same underlying attribute.

library(tidyr)
spreadDataX <- spread(data = boundDataX, key = activityName,value = activity)

# Note that the spread functions argument. The key values neccesitate
# the addition of the activityName variable

# Use the naming conventions set forth in the features.txt file
# after a few adjustments

# Add the index of the name to distinguish from duplicates in the list

s <-
  mapply(paste,  descriptiveVariableNames[[2]],descriptiveVariableNames[[1]],sep="__" )

# Remove '()' and then all of the special charcters to avoid conflicts
t<-gsub('\\()',"",s)

# Remove right parenthesis replace with double period
u<-gsub('\\(',"..",t)

# Remove left Parenthesis replace with double period
v<-gsub('\\)',"..",u)

# Remove dash replace with period
w<-gsub('-',".",v)

# Remove comma replace with  underscore
tidyNames<-gsub(',',"_",w)

# Change the names on the table

library(data.table)

setnames(spreadDataX, old = 1:561, new = tidyNames)
setnames(boundDataX, old = 1:561, new = tidyNames)

# Extract the means and the standard deviations of the features
# First find all of the mean and sd variables

library(reshape2)
meanAndSdNames <-
  tidyNames[sort(c(grep("mean", names(boundDataX)) ,grep("std", names(boundDataX))))]

# Select the found Names

firstTidyDataSet <- select(boundDataX, one_of(meanAndSdNames))


#Create csv file for mean data

write.csv(firstTidyDataSet, file = paste(getwd(),"/meanSdDataX.csv", sep = ""))

# Extract Just the mean data from the firstTidyDataSet

meanNames <-
  tidyNames[sort(c(grep("mean", names(boundDataX)) ))]

# Select the found Names

secondTidyDataSet <- select(firstTidyDataSet, one_of(meanNames))

# Bind the two subject Data Set together by stacking one on the other

boundDataSubject <- rbind(subject_test, subject_train)

# Append the combined subject_ data to the X DataSet as an
# addtional column and name it subject

secondTidyDataSet[["subject"]] <- boundDataSubject[,1]

# Append an additional column of activity names to
# melt by its value.  The ID variables (identifiers) subject and activityName
# are kept in rows, while all measured variables have been split into
# variable and value columns in this case it is the mean

secondTidyDataSet$activityName <- factor(boundDataX$activity,
                                  levels = descriptiveActivityNames[[1]],
                                  labels = descriptiveActivityNames[[2]])

# Group by activity then by subject and get a summary mean
# to produce the second independent tidy data set
# Returns the mean of each variable per acitvity per subject

meanMelt <-
  melt(secondTidyDataSet, id=c("subject", "activityName"), measure.vars = c(meanNames))

# Create the csv file for the second tidy data

write.csv(meanMelt, file = paste(getwd(),"/secondTidyDataSet.csv", sep = ""))




unlink(tmp)

