#------------------------------------------------------------
# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#-----------------------------------------------------------------------------

# Q1:Merges the training and the test sets to create one data set
# set working directory
setwd("./Rworkspace")
# download the documents
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "dataset.zip")
unzip("dataset.zip")

# reading train data
features <- read.table(file.path("UCI HAR Dataset","features.txt"),header=FALSE)
activityType <- read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
xTrain <- read.table("./UCI HAR Dataset/train/x_train.txt",header=FALSE)
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
colnames(activityType) <- c('activityId','activityType')
colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2] 
colnames(yTrain) <- "activityId"
trainingData = cbind(yTrain,subjectTrain,xTrain)

# reading test data
subjectTest <- read.table('./UCI HAR Dataset/test/subject_test.txt',header=FALSE)
xTest <- read.table('./UCI HAR Dataset/test/x_test.txt',header=FALSE)
yTest <- read.table('./UCI HAR Dataset/test/y_test.txt',header=FALSE)
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2] 
colnames(yTest) <- "activityId"
testData <- cbind(yTest,subjectTest,xTest)

# Combine training and test data 
finaldata <- rbind(trainingData,testData)

# Q2:Extracts only the measurements on the mean and standard deviation for each measurement
colNames <- colnames(finaldata)
logicalVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | 
                      grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & 
                      !grepl("mean..-",colNames) | grepl("-std..",colNames) & 
                      !grepl("-std()..-",colNames))
finaldata = finaldata[logicalVector==TRUE]

# Q3:Uses descriptive activity names to name the activities in the data set
finaldata <- merge(finaldata,activityType,by='activityId',all.x=TRUE)
colNames <- colnames(finaldata)

# Q4:Appropriately labels the data set with descriptive variable names
for (i in 1:length(colNames)) 
{
    colNames[i] = gsub("\\()","",colNames[i])
    colNames[i] = gsub("-std$","StdDev",colNames[i])
    colNames[i] = gsub("-mean","Mean",colNames[i])
    colNames[i] = gsub("^(t)","time",colNames[i])
    colNames[i] = gsub("^(f)","freq",colNames[i])
    colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
    colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
    colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
    colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
    colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
    colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
    colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}
colnames(finaldata) <- colNames

# Q5:Creates a second, independent tidy data set with the average of each variable for each activity and each subject 
finalDataNoActivityType <- finaldata[,names(finaldata) != 'activityType']
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType)!=c('activityId','subjectId')],
                      by=list(activityId=finalDataNoActivityType$activityId,
                              subjectId = finalDataNoActivityType$subjectId),mean)
tidyData <- merge(tidyData,activityType,by='activityId',all.x=TRUE)
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')
