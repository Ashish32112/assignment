library(dplyr)

#Reading and merging all files into traindataset

traindata <- read.table("train/x_train.txt")
trainsubject <- read.table("train/subject_train.txt")
trainactivity <- read.table("train/y_train.txt")
traindataset <- cbind(trainsubject,trainactivity,traindata)

#Reading and merging all files into testdataset

testdata <- read.table("test/x_test.txt")
testsubject <- read.table("test/subject_test.txt")
testactivity <- read.table("test/y_test.txt")
testdataset <- cbind(testsubject,testactivity,testdata)

#merge traindataset and testdataset

merge_dataset <- rbind(traindataset, testdataset)

#extract features variables names from features file and apply on merged_dataset

featurenames <- read.table("features.txt")
names(merge_dataset) <- c("Subject", "Activity", as.character(featurenames[,2]))


#extracting only mean and std variables columns from merged_datset

required_names <- grep(".*mean.*|.*std.*", featurenames[,2])
final_dataset <- merge_dataset[,c(1,2,(required_names +2))]

#get activity labels

activitylabels <- read.table("activity_labels.txt")
final_dataset$Activity <- factor(final_dataset$Activity,
                                levels = activitylabels[,1],
                                labels = activitylabels[,2])

final_dataset$Subject <- factor(final_dataset$Subject)

#creating summarised dataset from final_dataset

summarise_dataset <- summarise_each(
        group_by(final_dataset, Subject, Activity),
        funs(mean)
)

names(summarise_dataset) <- c("Subject", "Activity",
                              paste(featurenames[,2][required_names],
                                rep("Avg.",length(required_names))))

#writing files to text files

write.table(final_dataset, "Clean_dataset.txt")
write.table(summarise_dataset, "Summary_dataset.txt")





