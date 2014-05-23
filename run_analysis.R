## Function that build the tidy dataset
main <- function(){
    ## download the zip file
    downloadFile("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","Dataset.zip")
	
	## unzip file 
    unzipFile("Dataset.zip")
	
	## import labels
	labels <- read.table("./UCI HAR Dataset/activity_labels.txt", sep = "", col.names = c("labelcode","label"))
	
	## path of the testSet files 
	path_features    <- "./UCI HAR Dataset/features.txt"
	path_testSetX    <- "./UCI HAR Dataset/test/X_test.txt"
	path_testSetY    <- "./UCI HAR Dataset/test/y_test.txt"
	path_testSubject <- "./UCI HAR Dataset/test/subject_test.txt"
    ## Merge testSetX, testSetY and testSubject
	dataTest <- mergeDataset(path_testSetX,path_testSetY, path_testSubject, path_features)

	## path of the trainingSet files 	
	path_trainingSetX    <- "./UCI HAR Dataset/train/X_train.txt"
	path_trainingSetY    <- "./UCI HAR Dataset/train/y_train.txt"
	path_trainingSubject <- "./UCI HAR Dataset/train/subject_train.txt"
    ## Merge trainingSetX, trainingSetY and trainingSubject
	dataTraining <- mergeDataset(path_trainingSetX,path_trainingSetY, path_trainingSubject, path_features)

	## Merge dataTraining and dataTest
	dataset <- rbind(dataTraining, dataTest)

	## replace label codes with the label
	dataset = merge(labels, dataset, by.x="labelcode", by.y="labelcode")
	dataset <- dataset[,-1]

	## reshape the array
	library(reshape2)
	join <- melt(dataset, id = c("label", "subject"))

	## Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
	tidyDataset <- dcast(join, label + subject ~ variable, mean)

	## write tidy dataset to disk
	write.table(tidyDataset, file="UCI_HAR_Dataset.txt", quote=FALSE, row.names=FALSE, sep="\t")	
}

mergeDataset <- function(path_XdataSet,path_YdataSet, path_dataSubject, path_features){
	## Import features
	features <- read.table(path_features, sep = "",header = FALSE)
	# Computing the important features
	featureIndex <- grep("mean\\(|std\\(", features[,2])
	## Import data test and data training
	dataSetX <- read.table(path_XdataSet, col.names = features[,2], check.names=FALSE)
	dataSetX <- dataSetX[,featureIndex]
	dataSetY <- read.table(path_YdataSet, col.names = "labelcode")
	## Import test subject
	dataSubject <- read.table(path_dataSubject, col.names = "subject")
	## Merge dataSetX, dataSetY and testSubject
	dataTraining = cbind(dataSetY, dataSubject, dataSetX)
	dataTraining
}

## function for download files
downloadFile <- function(fileUrl, fileName){
  if (!file.exists(fileName)) {
      download.file(fileUrl, destfile = fileName)
  }
}
## function for unzip files
unzipFile <- function(fileUbication){
    unzip(fileUbication, overwrite = TRUE)
}
main()
