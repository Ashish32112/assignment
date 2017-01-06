library(stringr)
library(dplyr)

#Created platform independent paths

wd <- file.path("C:/Users/Ashish/LearningR/assignment4/")
testdir <- file.path("UCI HAR dataset/test")
traindir <- file.path("UCI HAR dataset/train")
signaldir <- file.path("Inertial Signals")
readfiles <- file.path("UCI HAR dataset")

#Function to convert signal txt files to dataframe 

text_to_df <- function(file){
        readtxt <- readLines(file)
        value_list <- str_extract_all(readtxt,
                                      "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?")
        value_list <- lapply(value_list, function(x) as.numeric(x))
        mean_vector <- sapply(value_list, mean)
        sd_vector <- sapply(value_list, sd)
        df <- data.frame(mean_vector, sd_vector)
        return(df)
}

#Function to create dataframe combining all signal files in given directory

create_df <- function(working_path = wd, data_path, signal_file_path = signaldir){
        files <- list.files(file.path(working_path,data_path,signal_file_path),
                            pattern = ".txt")
        if(data_path == traindir){
                subject <- readLines(file.path(working_path,
                                               data_path, "subject_train.txt"))
                activity <- readLines(file.path(working_path,
                                                data_path, "y_train.txt"))
                setwd(file.path(working_path,data_path,signal_file_path))
        }
        else if(data_path == testdir){
                subject <- readLines(file.path(working_path,
                                               data_path, "subject_test.txt"))
                activity <- readLines(file.path(working_path,
                                                data_path, "y_test.txt"))
                setwd(file.path(working_path,data_path,signal_file_path))
        }
        final_df <- text_to_df(files[1])
        final_df <- data.frame(subject, activity, final_df)
        for(file in files[2:length(files)]){
                df <- text_to_df(file)
                final_df <- cbind(final_df,df)
        }
        return(final_df)
}

#Merging data from test and train datasets

Merged_df <- rbind(
        create_df(wd, testdir, signaldir),
        create_df(wd, traindir, signaldir))[1:14]

# Extracting and applying activity labels to merged dataframe

activity_labels <- readLines(file.path(wd, readfiles , "activity_labels.txt"))
levels(Merged_df$activity) <- matrix(unlist(strsplit(activity_labels, " ")),
                          ncol = 2, byrow = TRUE)[,2]

rm('activity_labels')

#Extracting Variable names from features txt files.

variable_names <- readLines(file.path(wd, readfiles, "features.txt"))
variable_names <- str_extract_all(variable_names,
         "([t][BG][a-zA-Z]*[coy][-]?[ms][et][ad].*)", simplify = TRUE)
variable_names <- variable_names[variable_names != ""]

#Rearranging extraxted variable names according to merged dataframe
#applying the variables name to merged dataframe

variable_names <- variable_names[c(1,4,2,5,3,6,
                                     13,16,14,17,15,18)]
names(Merged_df) <- c("Subject", "Activity", variable_names)

rm('variable_names')

#Creating second dataset summariseing the merged dataset

grouped_data <- group_by(Merged_df, Subject, Activity)

Summary_df <- summarise_each(grouped_data, funs(mean))

names(Summary_df) <- c('Subject', 'Activity',
        paste(variable_names, rep("avg", length(variable_names)-2)))

rm('grouped_data')

#Writing data to files

setwd(wd)

write.table(Merged_df, "merged_dataset.txt",row.names = FALSE)
write.table(Summary_df, "summary_dataset.txt", row.names = FALSE)



