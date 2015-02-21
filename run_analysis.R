url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fileName <- paste(getwd(), "/data.csv", sep="")
download.file(url, fileName)
datas = unzip(fileName)

mySfx <- function(x, sfx) {
  if (!(x %in% c("Subject","Activity"))) { paste(x,sfx, sep="") }
  else{x}
}

require(plyr)
pathfile<-file.path(getwd(),"UCI HAR Dataset")
testFilePath<-file.path(pathfile, "test")
trainFilePath<-file.path(pathfile, "train")
xtest<-read.table(file.path(testFilePath,"X_test.txt"))
ytest<-read.table(file.path(testFilePath,"Y_test.txt"))
subjecttest<-read.table(file.path(testFilePath,"subject_test.txt"))
xtrain<-read.table(file.path(trainFilePath,"X_train.txt"))
ytrain<-read.table(file.path(trainFilePath,"Y_train.txt"))
subjecttrain<-read.table(file.path(trainFilePath,"subject_train.txt"))
activityLabels<-read.table(file.path(pathfile, "activity_labels.txt"), col.names = c("Id", "Activity"))
featurelabels<-read.table(file.path(pathfile, "features.txt"),colClasses = c("character"))

traindata<-cbind(cbind(xtrain, subjecttrain), ytrain)
testdata<-cbind(cbind(xtest, subjecttest), ytest)
sensordata<-rbind(traindata, testdata)
sensorlabels<-rbind(rbind(featurelabels, c(562, "Subject")), c(563, "Id"))[,2]
names(sensordata)<-sensorlabels

sensordatameanstd <- sensordata[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(sensordata))]

sensordatameanstd <- join(sensordatameanstd, activityLabels, by = "Id", match = "first")
sensordatameanstd <- sensordatameanstd[,-1]

names(sensordatameanstd) <- gsub("([()])","",names(sensordatameanstd))
names(sensordatameanstd) <- make.names(names(sensordatameanstd))

finaldata<-ddply(sensordatameanstd, c("Subject","Activity"), numcolwise(mean))
finaldataheaders<-names(finaldata)
finaldataheaders<-sapply(finaldataheaders, mySfx, ".mean")
names(finaldata)<-finaldataheaders

write.table(finaldata, file = "result.txt", row.name=FALSE)
