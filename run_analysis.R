url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- paste(getwd(), "/data.csv", sep="")
download.file(url, fileName)
datas = unzip(fileName)

mySfx <- function(x, sfx) {
  if (!(x %in% c("Subject","Activity"))) { paste(x,sfx, sep="") }
  else{x}
}

require(plyr)
pathfile <- file.path(getwd(),"UCI HAR Dataset")
testFilePath <- file.path(pathfile, "test")
trainFilePath <- file.path(pathfile, "train")
xtest <- read.table(file.path(testFilePath,"X_test.txt"))
ytest <- read.table(file.path(testFilePath,"Y_test.txt"))
subjectTest <- read.table(file.path(testFilePath,"subject_test.txt"))
xtrain <- read.table(file.path(trainFilePath,"X_train.txt"))
ytrain <- read.table(file.path(trainFilePath,"Y_train.txt"))
subjectTrain <- read.table(file.path(trainFilePath,"subject_train.txt"))
activityLabels <- read.table(file.path(pathfile, "activity_labels.txt"), col.names = c("Id", "Activity"))
featureLabels <- read.table(file.path(pathfile, "features.txt"),colClasses = c("character"))

trainData <- cbind(cbind(xtrain, subjectTrain), ytrain)
testData <- cbind(cbind(xtest, subjectTest), ytest)
sensorData <- rbind(trainData, testData)
sensorLabels <- rbind(rbind(featureLabels, c(562, "Subject")), c(563, "Id"))[,2]
names(sensorData) <- sensorLabels

sensorDataMeanStd <- sensorData[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(sensorData))]

sensorDataMeanStd <- join(sensorDataMeanStd, activityLabels, by = "Id", match = "first")
sensorDataMeanStd <- sensorDataMeanStd[,-1]

names(sensorDataMeanStd) <- gsub("([()])","",names(sensorDataMeanStd))
names(sensorDataMeanStd) <- make.names(names(sensorDataMeanStd))

finalData <- ddply(sensorDataMeanStd, c("Subject","Activity"), numcolwise(mean))
finalDataHeaders <- names(finalData)
finalDataHeaders <- sapply(finalDataHeaders, mySfx, ".mean")
names(finalData) <- finalDataHeaders

write.table(finalData, file = "result.txt", row.name=FALSE)
