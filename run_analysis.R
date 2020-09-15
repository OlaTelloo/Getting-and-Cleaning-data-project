# Install the necessary Packages
library(data.table)
library(dplyr)
library(stringr)

features <-  read.table("getting and cleaning data/UCI HAR Dataset/features.txt") %>%
  rename(n = V1,
         functions = V2)

activities <- read.table("getdata_projectfiles_UCI HAR Dataset_new/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt") %>%
  rename(code = V1,
         activity = V2)



X_train <- read.table("getdata_projectfiles_UCI HAR Dataset_new/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",
                      col.names = features$functions) 


Y_train <- read.table("getdata_projectfiles_UCI HAR Dataset_new/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/Y_train.txt") %>%
  rename("code" = V1)


Sub_train <- read.table("getdata_projectfiles_UCI HAR Dataset_new/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt") %>%
  rename("subject" = V1)

X_test <- read.table("getdata_projectfiles_UCI HAR Dataset_new/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt",
                     col.names = features$functions)

Y_test <- read.table("getdata_projectfiles_UCI HAR Dataset_new/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/Y_test.txt") %>%
  rename("code" = V1)

Sub_test <- read.table("getdata_projectfiles_UCI HAR Dataset_new/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt") %>%
  rename("subject" = V1)

X_df <- rbind(X_train, X_test)
Y_df <- rbind(Y_train, Y_test)

Subject <- rbind(Sub_test, Sub_train)
All_data <- cbind(Subject, Y_df, X_df)


TidyData <- All_data %>%
  select(subject,
         code,
         contains("mean"),
         contains("std")) %>%
  left_join(.,
            activities,
            by = "code")

TidyData <- TidyData %>%
  mutate(code = activity) %>%
  select(-activity)


#renamed the data set's columns with descriptive variable names
names(TidyData) <- case_when(str_detect(names(TidyData),"Acc" ) ~ gsub("Acc", "Accelerometer", names(TidyData)),
                             str_detect(names(TidyData),"Gyro" ) ~ gsub("Gyro", "Gyroscope", names(TidyData)),
                             str_detect(names(TidyData),"BodyBody" ) ~ gsub("BodyBody", "Body", names(TidyData)),
                             str_detect(names(TidyData),"Mag" ) ~ gsub("Mag", "Magnitude", names(TidyData)),
                             str_detect(names(TidyData),"^t" ) ~ gsub("^t", "Time", names(TidyData)),
                             str_detect(names(TidyData),"^f" ) ~ gsub("^f", "Frequency", names(TidyData)),
                             str_detect(names(TidyData),"tBody" ) ~ gsub("tBody", "TimeBody", names(TidyData)),
                             str_detect(names(TidyData),"-mean()" ) ~ gsub("-mean()", "Mean", names(TidyData)),
                             str_detect(names(TidyData),"-std()" ) ~ gsub("-std()", "STD", names(TidyData)),
                             str_detect(names(TidyData),"-freq()" ) ~ gsub("-freq()", "Frequency", names(TidyData)),
                             str_detect(names(TidyData),"angle" ) ~ gsub("angle", "Angle", names(TidyData)),
                             str_detect(names(TidyData),"gravity" ) ~ gsub("gravity", "Gravity", names(TidyData)),
                             TRUE ~ names(TidyData))

#Step 4: creates a second, independent tidy data set with the average of each variable for each activity and each subject.
Output <- TidyData %>% 
  group_by(subject, code) %>%
  summarise_all(funs(mean))

