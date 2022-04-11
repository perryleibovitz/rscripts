rm(list = ls())


library(RODBC)
library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(lubridate)
library(openxlsx)
library(RMySQL)


Scheduler <- dbConnect(MySQL(), user='survey_ro', password='readonly', dbname='scheduler_db', host = 'scheduler.bebr.ufl.edu')
Employee <- dbGetQuery(Scheduler, "SELECT FIRST_NAME, LAST_NAME, ID FROM employee WHERE STATUS = 1") 

all_file_paths_csi_suffix <- tibble(folders=list.files(path = "Y:/3144", recursive=TRUE)) %>% 
  separate(folders, c("year", "month", "day", "trash", "station", "CombinedPhone", "suffix", "filetype"), remove = FALSE) %>% 
  mutate(filepath = c(paste("Y:\\3144\\", year, "\\", month, "\\", day, "\\", trash, "-", station, "-", CombinedPhone, "-", suffix, ".", filetype, sep= ""))) %>% 
  unite(date, month, day, sep = "") %>% 
  select(filepath, date, CombinedPhone)

all_file_paths_tchip_suffix <- tibble(folders=list.files(path = "Y:/3135", recursive=TRUE)) %>% 
  separate(folders, c("year", "month", "day", "trash", "station", "CombinedPhone", "suffix", "filetype"), remove = FALSE) %>% 
  mutate(filepath = c(paste("Y:\\3135\\", year, "\\", month, "\\", day, "\\", trash, "-", station, "-", CombinedPhone, "-", suffix, ".", filetype, sep= ""))) %>% 
  unite(date, month, day, sep = "") %>% 
  select(filepath, date, CombinedPhone)

all_file_paths_tchpa_suffix <- tibble(folders=list.files(path = "Y:/3136", recursive=TRUE)) %>% 
  separate(folders, c("year", "month", "day", "trash", "station", "CombinedPhone", "suffix", "filetype"), remove = FALSE) %>% 
  mutate(filepath = c(paste("Y:\\3136\\", year, "\\", month, "\\", day, "\\", trash, "-", station, "-", CombinedPhone, "-", suffix, ".", filetype, sep= ""))) %>% 
  unite(date, month, day, sep = "") %>% 
  select(filepath, date, CombinedPhone)

all_file_paths_catch_suffix <- tibble(folders=list.files(path = "Y:/3130", recursive=TRUE)) %>% 
  separate(folders, c("year", "month", "day", "trash", "station", "CombinedPhone", "suffix", "filetype"), remove = FALSE) %>% 
  mutate(filepath = c(paste("Y:\\3130\\", year, "\\", month, "\\", day, "\\", trash, "-", station, "-", CombinedPhone, "-", suffix, ".", filetype, sep= ""))) %>% 
  unite(date, month, day, sep = "") %>% 
  select(filepath, date, CombinedPhone)

all_file_paths_noproject <- tibble(folders=list.files(path = "Y:/no-project/2022/02", recursive=TRUE)) %>% 
  separate(folders, c("day", "trash", "station", "CombinedPhone", "filetype"), remove = FALSE) %>% 
  mutate(month = "02", filepath = c(paste("Y:\\no-project\\2022\\02\\", day, "\\", trash, "-", station, "-", CombinedPhone, ".", filetype, sep= ""))) %>% 
  unite(date, month, day, sep = "") %>% 
  select(filepath, date, CombinedPhone)

all_file_paths <- rbind(all_file_paths_csi_suffix, all_file_paths_tchip_suffix, all_file_paths_tchpa_suffix, all_file_paths_catch_suffix, all_file_paths_noproject)


CSIodbc <- odbcConnect("C2202", uid="guest", pwd="guest")
TCHIPodbc <- odbcConnect("TCHIP21", uid="guest", pwd="guest")
TCHPAodbc <- odbcConnect("TCHPA21", uid="guest", pwd="guest")
CATCHodbc <- odbcConnect("CATCH22", uid="guest", pwd="guest")


sample_csi <- sqlQuery(CSIodbc, "SELECT * FROM dba.sample") %>% 
  filter(LastDisposition == 1100) %>% 
  select(SampleId, RespondentNumber, AreaCode, PhoneNumber, AssignedInterviewerId) %>% 
  unite(CombinedPhone, AreaCode, PhoneNumber, sep = "") %>% 
  rename(RecordNumber = SampleId) %>% mutate(survey = "CSI")

sample_tchip <- sqlQuery(TCHIPodbc, "SELECT * FROM dba.sample") %>% 
  filter(LastDisposition == 1100) %>% 
  select(SampleId, RespondentNumber, AreaCode, PhoneNumber, AssignedInterviewerId) %>% 
  unite(CombinedPhone, AreaCode, PhoneNumber, sep = "") %>% 
  rename(RecordNumber = SampleId) %>% mutate(survey = "TCHIP")

sample_tchpa <- sqlQuery(TCHPAodbc, "SELECT * FROM dba.sample") %>% 
  filter(LastDisposition == 1100) %>% 
  select(SampleId, RespondentNumber, AreaCode, PhoneNumber, AssignedInterviewerId) %>% 
  unite(CombinedPhone, AreaCode, PhoneNumber, sep = "") %>% 
  rename(RecordNumber = SampleId) %>% mutate(survey = "TCHPA")

sample_catch <- sqlQuery(CATCHodbc, "SELECT * FROM dba.sample") %>% 
  filter(LastDisposition == 1100) %>% 
  select(SampleId, RespondentNumber, AreaCode, PhoneNumber, AssignedInterviewerId) %>% 
  unite(CombinedPhone, AreaCode, PhoneNumber, sep = "") %>% 
  rename(RecordNumber = SampleId) %>% mutate(survey = "CATCH")

sample <- rbind(sample_csi, sample_tchip, sample_tchpa, sample_catch)

as.numeric(all_file_paths$date)

FirstJoin <- left_join(all_file_paths, sample, by = "CombinedPhone") %>% filter(!is.na(RecordNumber)) %>% group_by(RecordNumber) %>% arrange(desc(filepath)) %>% filter(filepath == first(filepath))

# udf_response <- sqlQuery(surveyodbc, "SELECT * FROM dba.udf") %>% filter(FieldNumber == 7) %>% #SampleID is record number; FieldNumber 7 = Response to Q1
#   select(-FieldNumber) %>% rename(response = FieldData)
# 
# udf_ichp <- sqlQuery(surveyodbc, "SELECT * FROM dba.udf") %>% subset(SampleId %in% udf_response$SampleId) %>% filter(FieldNumber == 3) %>% 
#   select(-FieldNumber) %>% rename(ICHP_ID = FieldData)
# 
udf_done_csi <- sqlQuery(CSIodbc, "SELECT * FROM dba.udf") %>% filter(FieldNumber == 7) %>% 
  select(-FieldNumber) %>% rename(AlreadyEntered = FieldData, RecordNumber = SampleId)

udf_done_tchip <- sqlQuery(TCHIPodbc, "SELECT * FROM dba.udf") %>% filter(FieldNumber == 16) %>% 
  select(-FieldNumber) %>% rename(AlreadyEntered = FieldData, RecordNumber = SampleId)

udf_done_tchpa <- sqlQuery(TCHPAodbc, "SELECT * FROM dba.udf") %>% filter(FieldNumber == 13) %>% 
  select(-FieldNumber) %>% rename(AlreadyEntered = FieldData, RecordNumber = SampleId)

udf_done_catch <- sqlQuery(CATCHodbc, "SELECT * FROM dba.udf") %>% filter(FieldNumber == 8) %>% 
  select(-FieldNumber) %>% rename(AlreadyEntered = FieldData, RecordNumber = SampleId)

udf_done <- rbind(udf_done_csi, udf_done_tchip, udf_done_tchpa, udf_done_catch)


# 
# udf <- left_join(udf_ichp, udf_response, by = "SampleId") %>% left_join(udf_done, by = "SampleId") %>% rename(RecordNumber = SampleId)
# 
SecondJoin <- left_join(FirstJoin, udf_done, by = "RecordNumber") %>%  arrange(AssignedInterviewerId, date, filepath) %>% filter(is.na(AlreadyEntered)) %>% select(-AlreadyEntered, -RespondentNumber)

ThirdJoin <- left_join(SecondJoin, Employee, by = c("AssignedInterviewerId" = "ID")) %>% unite(Interviewer, FIRST_NAME, LAST_NAME, sep = " ")

class(ThirdJoin$filepath) <- "hyperlink"

write.xlsx(ThirdJoin, "Q:/sursup/Evaluations/Call Attempt Reports/Completes/Completes.xlsx")
