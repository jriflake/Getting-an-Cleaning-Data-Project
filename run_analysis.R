run_analysis <- function(){
  library(data.table)
  library(dplyr)
  library(tidyr)
  ## Merge results
 Tot <-merge(train,test,all = TRUE) 
 ## add column names
 colnames(Tot)<- tnames$V2
 ## filter to mean and std columns
 MSTot <- Tot[,grepl("mean()|std()",names(Tot))]
 MSTot2 <- MSTot[,!grepl("meanFreq" , names(MSTot))]
 ## merge activity files
 tactive <- rbind(activities,teactivities)
 ## merge subject files
 subjects <- rbind(subtrain,subtest)
 ## add column name to subject file
 colnames(subjects) <- "subject"
 ## associate activity names with the numbers
 activenames <- merge(tactive,act_labels,by = "V1")
 colnames(activenames)<- c("Act_number","Activity_Name")
 ## add activity and subject columns to results data
 MSTot3 <- cbind(MSTot2,activenames$Activity_Name)
 MSTot4 <- cbind(subjects$subject,MSTot3)
 names(MSTot4)[1] <- "subject"
 names(MSTot4)[68] <- "activity"
 ## create average data set by subject and activity
 MSTot5 <- MSTot4 %>% group_by(activity,subject)%>% summarize_all(mean)
 ## clean up column names
 colnames(MSTot5)<- gsub("-","",names(MSTot5))
 colnames(MSTot5)<- gsub("[()]","",names(MSTot5))
 MSTot5 
 
 
}