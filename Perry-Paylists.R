rm(list = ls())

library(openxlsx)
library(RMySQL)
library(plyr) #this library has the round_any function
library(tidyverse)
library(janitor)
library(rmarkdown)

Payperiod_start <- Sys.Date()-18
Payperiod_stop <- Sys.Date()-5
  
UF_Paylists <- read.xlsx("C:/Users/perry86/Downloads/Final Payroll Listing Excel by Processor 2022-02-19T00_48_38.068Z.xlsx", startRow = 3) %>% filter(Earnings.Dept == 16980300, Earns.Code != "INP")

Totals <- UF_Paylists %>% group_by(Empl.ID, Name) %>% summarize(paylist_total_hours = sum(Actual.Hours)) %>% arrange(Name)

Scheduler <- dbConnect(MySQL(), user='survey_ro', password='readonly', dbname='scheduler_db', host = 'scheduler.bebr.ufl.edu')

SchedTables <- tibble(tables=dbListTables(Scheduler))

Employee <- dbGetQuery(Scheduler, "SELECT FIRST_NAME, LAST_NAME, ID, STUDENT_ID, FTE FROM employee") 

Scheduler_Time_Records_Payweek <- dbGetQuery(Scheduler, "SELECT * from work_detail WHERE DATE(START_TIMESTAMP) BETWEEN DATE_ADD(CURDATE(), INTERVAL -18 DAY) AND DATE_ADD(CURDATE(), INTERVAL -5 DAY)") %>% 
  left_join(Employee, by = c("EMPLOYEE_ID" = "ID")) %>% unite(FULL_NAME, LAST_NAME, FIRST_NAME, sep = ",") %>% mutate(int_time = (difftime(END_TIMESTAMP, START_TIMESTAMP) / 3600)) %>% 
  separate(START_TIMESTAMP, c("year","month", "day", "hour", "minute", "second"), extra = "drop", remove = FALSE, convert = TRUE) %>% unite(date, month, day, year, sep = "/") %>% 
  rename(UFID = STUDENT_ID)

Scheduler_Time_Records_Payweek$FULL_NAME[Scheduler_Time_Records_Payweek$UFID == 12569194] <- "Paiva Dantas Maciel,Imna"

Scheduler_Time_Records_Payweek$int_time <- as.numeric(Scheduler_Time_Records_Payweek$int_time) 

Scheduler_Time_Records_Payweek_By_Date <- Scheduler_Time_Records_Payweek %>%  group_by(UFID, FULL_NAME, date, FTE, PAY_RATE) %>% summarize(Scheduler_Hours_Raw = sum(int_time))

Scheduler_Time_Records_Payweek_By_Date$Scheduler_Hours_Rounded <- round_any(Scheduler_Time_Records_Payweek_By_Date$Scheduler_Hours_Raw, .25)

Scheduler_Time_Records_Payweek_By_Date$FULL_NAME2 <- Scheduler_Time_Records_Payweek_By_Date$FULL_NAME

Scheduler_Time_Records_Payweek_Final <- Scheduler_Time_Records_Payweek_By_Date %>% group_by(FULL_NAME) %>% summarize(Scheduler_Hours_Final = sum(Scheduler_Hours_Rounded)) 


Final <- cbind(Totals, Scheduler_Time_Records_Payweek_Final) %>% mutate(difference = paylist_total_hours - Scheduler_Hours_Final)

#Incentives - Run when the paycheck has incentives.
# Incentives <- read.xlsx("C:/Users/perry86/Downloads/Final Payroll Listing Excel by Processor 2021-10-15T23_54_51.310Z.xlsx", startRow = 3) %>% filter(Earnings.Dept == 16980300, Earnings.Code == "INP")
# Incentive_Totals <- Incentives %>% group_by(Empl.ID, Name) %>% summarize(paylist_total_hours = sum(Total.Gross)) %>% arrange(Name)
# Sent_to_Stef <- read.xlsx("Q:/admin/Incentive/2021/5 Aug_Sep/Int_Totals_Aug_Sep21.xlsx", sheet="Payroll") %>% select(UFID, Total_Amount)
# Sent_to_Stef$UFID <- as.character(Sent_to_Stef$UFID)
# Final_Incentives <- left_join(Incentive_Totals, Sent_to_Stef, by = c("Empl.ID" = "UFID")) %>% mutate(difference = paylist_total_hours - Total_Amount)
  
Markdown_Scheduler_Time_Records_Payweek_Final <- Scheduler_Time_Records_Payweek_By_Date %>% group_by(FULL_NAME2) %>% 
  group_map(~ .x  %>% adorn_totals("row", fill = "", name = "Total",,,, -FTE, -PAY_RATE)) %>% bind_rows() 


render("Paylists-Markdown.Rmd", output_file = paste("Q:/admin/Perry/Timecards ", Sys.Date()+4, ".html", sep = ""))
