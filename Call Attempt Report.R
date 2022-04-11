rm(list = ls())


library(RODBC)
library(RMySQL)
library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(lubridate)
library(openxlsx)

current_day <- weekdays(Sys.Date())


#MySQL connection to Scheduler
Scheduler <- dbConnect(MySQL(), user='survey_ro', password='readonly', dbname='scheduler_db', host = 'scheduler.bebr.ufl.edu')

SchedTables <- tibble(tables=dbListTables(Scheduler))

#Monday - Thursday calls
if (current_day != "Monday") {
AllCalls <- dbGetQuery(Scheduler, "SELECT * FROM attempt_recording  WHERE DATE(attempt_start_time) = DATE_ADD(CURDATE(), INTERVAL -1 DAY)")

AttemptInfo <- dbGetQuery(Scheduler, "SELECT job_name, sample_id, attempt_number, disposition, interviewer_id, note FROM attempt 
                          WHERE DATE(start_time) = DATE_ADD(CURDATE(), INTERVAL -1 DAY)")
} else {
#Friday Saturday Sunday calls

AllCalls <- dbGetQuery(Scheduler, "SELECT * FROM attempt_recording  WHERE DATE(attempt_start_time) BETWEEN DATE_ADD(CURDATE(), INTERVAL -3 DAY) AND DATE_ADD(CURDATE(), INTERVAL -1 DAY)")

AttemptInfo <- dbGetQuery(Scheduler, "SELECT job_name, sample_id, attempt_number, disposition, interviewer_id, note FROM attempt 
                          WHERE DATE(start_time) BETWEEN DATE_ADD(CURDATE(), INTERVAL -3 DAY) AND DATE_ADD(CURDATE(), INTERVAL -1 DAY)")
}

#Other SQL Pulls
ActiveSurveys <- dbGetQuery(Scheduler, "SELECT ID, NAME 
                    FROM survey
                    WHERE STATUS = 1")

Employee <- dbGetQuery(Scheduler, "SELECT FIRST_NAME, LAST_NAME, ID FROM employee WHERE STATUS = 1") 


#datamerging

FilterCalls <- subset(AllCalls, AllCalls$job_id %in% ActiveSurveys$ID ) %>% left_join(ActiveSurveys, by = c("job_id" = "ID")) %>% 
  select(NAME, sample_id, phone_number, attempt_number, call_recording, attempt_start_time )

FilterCalls <- separate(FilterCalls, attempt_start_time, c("year","month", "day", "hour", "minute", "second"), extra = "drop", remove = FALSE, convert = TRUE)

FilterCallsAttemptInfo <- left_join(FilterCalls, AttemptInfo, by = c("NAME" = "job_name", 
                                                                     "sample_id" = "sample_id", 
                                                                     "attempt_number" = "attempt_number")) %>% 
                                                                      rename(record_number = sample_id, survey = NAME)
  
FilterCallsAttemptInfoEmployee <- left_join(FilterCallsAttemptInfo, Employee, by = c("interviewer_id" = "ID")) %>% 
    unite(int_name, FIRST_NAME, LAST_NAME, sep = " ") %>% 
  unite(date, month, day, year, sep = "/") %>% 
  unite(time, hour, minute, second, sep = ":")

FinalCallAttempt <- select(FilterCallsAttemptInfoEmployee, int_name, survey, record_number, phone_number, date, time, attempt_number, disposition, call_recording, note) %>% 
  arrange(int_name, date, time)

#make the recording a link
class(FinalCallAttempt$call_recording) <- "hyperlink"

#write to file
write.xlsx(FinalCallAttempt, paste("Q:/sursup/Evaluations/Call Attempt Reports/Call Attempt Report - ", Sys.Date()-1, ".xlsx", sep=""), overwrite = TRUE)

comps_per_survey <- FinalCallAttempt %>% filter(disposition == 1100) %>% group_by(survey) %>% summarize(completes = n())

View(comps_per_survey)
