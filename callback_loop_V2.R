rm(list = ls())


library(RODBC)
library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(lubridate)
library(openxlsx)

all_file_paths <- tibble(folders=list.files(path = "W:/STI/WinCati"))
folder_paths <- str_subset(all_file_paths$folders, pattern = "[.]", negate = TRUE)
do.call(file.remove, list(list.files("C:/Users/perry86/University of Florida/BEBR - Survey - Shift Supervision - Shift Supervision/Callbacks", full.names = TRUE)))

#https://www.r-bloggers.com/skip-errors-in-r-loops-by-not-writing-loops/

fun <- function(folder_path_input){
  
  surveyodbc <- odbcConnect(folder_path_input, uid="guest", pwd="guest")
  
  sample <- sqlQuery(surveyodbc, "SELECT Priority, SampleId, ContactDate, ContactTime, Wave, NumofRefusal FROM dba.sample")
  
  sample2 <- sample %>% filter(Priority<3, ContactDate==Sys.Date()) %>%  select(SampleId, ContactDate, ContactTime, Wave, NumofRefusal) %>% 
    mutate(ContactTime = if_else(as.POSIXlt(ContactTime)$hour < 9, ContactTime + 43200, 
                                 if_else(as.POSIXlt(ContactTime)$hour > 21 , ContactTime - 43200, ContactTime))) %>% arrange(ContactTime)
  
  sample2$ContactTime <- format(sample2$ContactTime, "%I:%M %p")

    sample_final <- sample2 %>% select(Record_Number=SampleId, Callback_Time=ContactTime, Wave, Number_of_Refusals=NumofRefusal) 
  sample_final$Wave[sample_final$Wave==3] <- "Spanish" 
  
  if(nrow(sample_final) > 0){
  
  write.xlsx(sample_final, paste("C:/Users/perry86/University of Florida/BEBR - Survey - Shift Supervision - Shift Supervision/Callbacks/",folder_path_input, " Today's Callbacks.xlsx", sep=""), na="",  sheetName = paste(" ", folder_path_input, "Callbacks"))
    } 

  return(sample_final)
  
}  

fun_possibly <- possibly(fun, otherwise = "No connection")

result <- map(folder_paths, fun_possibly)
names(result) <- folder_paths


print("Code Complete")
