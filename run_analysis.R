test <- read.table("getdata_projectfiles_UCI HAR Dataset/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt")

train <- read.table("getdata_projectfiles_UCI HAR Dataset/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt")

col_names <- read.table("getdata_projectfiles_UCI HAR Dataset/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt") %>%
  select(V2) %>%
  rename(Measurement = V2)

df_all <- rbind.data.frame(test, train)

Mean <- c()
Std <- c()
Average <- c()

for (i in 1:561) {
  Mean_value <- mean(df_all[,i])
  Mean <- c(Mean, Mean_value)

  Std_value <- sd(df_all[,i])
  Std <- c(Std, Std_value)
  
  Average_value <- sd(df_all[,i])
  Average <- c(Average, Average_value)
  }

output <- cbind.data.frame(col_names, Mean, Std, Average)


write.table(output,
            "Analysis.txt",
            row.names = F)
