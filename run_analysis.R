run_analysis <- function(){
  library(data.table)
  library(dplyr)
  library(tidyr)
  
 Tot <-merge(train,test,all = TRUE) 
 colnames(Tot)<- tnames$V2
 MSTot <- Tot[,grepl("mean()|std()",names(Tot))]
 MSTot2 <- MSTot[,!grepl("meanFreq" , names(MSTot))]
 tactive <- rbind(activities,teactivities)
 subjects <- rbind(subtrain,subtest)
 colnames(subjects) <- "subject"
 activenames <- merge(tactive,act_labels,by = "V1")
 colnames(activenames)<- c("Act_number","Activity_Name")
 MSTot3 <- cbind(MSTot2,activenames$Activity_Name)
 MSTot4 <- cbind(subjects$subject,MSTot3)
 names(MSTot4)[1] <- "subject"
 names(MSTot4)[68] <- "activity"
 MSTot5 <- MSTot4 %>% group_by(activity,subject) %>% ddply(.(activity,subject),numcolwise(mean)) %>% arrange(activity,desc(subject))
 MSTot5
 
}