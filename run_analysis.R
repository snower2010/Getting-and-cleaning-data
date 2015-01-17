#This file is used for analyzing the data of peer assessments of Getting and cleaning data
##Task 1
###read the data
setwd("F:/R_Script/practiceScript/GettingAndCleaningData/UCI_HAR_Dataset")
trainData <- read.table("./train/X_train.txt")
trainLable <- read.table("./train/y_train.txt")
trainSubject <- read.table("./train/subject_train.txt")
testData <- read.table("./test/X_test.txt")
testLable <- read.table("./test/y_test.txt")
testSubject <- read.table("./test/subject_test.txt")
###join data
joinData <- rbind(trainData,testData)
joinLabel <- rbind(trainLable,testLable)
joinSubject <- rbind(trainSubject,testSubject)

##Task 2
features <- read.table("./features.txt")
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
joinData <- joinData[, meanStdIndices]
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])
names(joinData) <- gsub("mean", "Mean", names(joinData))
names(joinData) <- gsub("std", "Std", names(joinData))
names(joinData) <- gsub("-", "", names(joinData))

## Task 3
activity <- read.table("./activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

##Task 4
names(joinSubject) <- "subject"
outData <- cbind(joinSubject, joinLabel, joinData)
write.table(outData, "task4.txt")

##Task 5
subjectLen <- length(table(joinSubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(outData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(outData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == outData$subject
    bool2 <- activity[j, 2] == outData$activity
    result[row, 3:columnLen] <- colMeans(outData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}
write.table(result, "task5.txt",row.name=FALSE)
