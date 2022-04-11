#Assigning interviewers to available stations
#Written by Perry Leibovitz 6/3/20


# Libraries -----------------------------------------------------------------
#If using R for the first time, copy and paste the below into the "Console" quadrant (bottom left)
#without the pound sign to install the necessary libraries. tidyverse might take a little bit
#of time because it is large

#install.libraries("tidyverse")
#install.libraries("tidylog")
#install.libraries("openxlsx")
#install.libraries("readr")

#removes tables from the environment
rm(list = ls())

#loading libraries - these allow you to use functions in the code
library(tidyverse)
library(tidylog, warn.conflicts = FALSE)
library(openxlsx)
library(readr)


# Loading CSVs----------------

#Loading the eve shift assignment report saved as assignments.csv from Scheduler
#IMPORTANT! Scheduler appends to the existing file instead of deleting it.

Assignments <- read_csv("Q:/sursup/SHIFT SUPERVISION/Remote Dialing Stuff/assignments.csv") %>% select(Employee)

#Loading the master list of employees and stations they are assigned to
MasterSeats <- read.xlsx("Q:/sursup/SHIFT SUPERVISION/Remote Dialing Stuff/seats.xlsx") 

# Assigning available seats -----------------------


#Stations without an interviewer currently assigned to it
Open_Stations <- filter(MasterSeats, is.na(Interviewer.Name)) %>% select(PSComputerName)

#Stations where the interviewer is not assigned tonight
#or without an interviewer currently assigned
Available_Stations <- anti_join(MasterSeats, Assignments, by = c("Interviewer.Name" = "Employee"))  

#Combining the assignment report with corresponding stations
Shift <- left_join(Assignments, MasterSeats, by = c("Employee" = "Interviewer.Name")) 

#Number of interviewers that don't have a station plus various repetitive stuff I don't
#understand why we need to make the code work.
Needed <- sum(is.na(Shift$PSComputerName))
Open <- nrow(Open_Stations)
X <- Needed - Open

#If statement - If there are more unassigned stations than there are interviewers that 
#need a station this will randomly select an open station (Open_Stations)

if(X <= 0) {
Shift$PSComputerName[is.na(Shift$PSComputerName)] <- sample(Open_Stations$PSComputerName, Needed) 

#If there are more interviewers that need a station than unassigned stations, this will 
#randomly select a station from the list of employees not working tongiht (Available_Stations)
} else {

Shift$PSComputerName[is.na(Shift$PSComputerName)] <- sample(Available_Stations$PSComputerName, Needed)

}

#Sorting employee name by ABC order
Shift <- Shift %>% arrange(Employee)
#Cleaning up and saving new files -------------

#New master list of employees and stations, updated from above
#Joining the shift assignments to the old master list
NewMasterSeats <- left_join(MasterSeats, Shift, by = "PSComputerName") 

#Adding the interviewer names of people not working tonight to the proper column
NewMasterSeats$NewEmployee <- if_else(is.na(NewMasterSeats$Employee), NewMasterSeats$Interviewer.Name,NewMasterSeats$Employee)


#Keeping appropriate column and renaming it
NewMasterSeats <- select(NewMasterSeats, PSComputerName, NewEmployee) %>% rename(Interviewer.Name = NewEmployee)

#adding <br> html tag
Shift$htmlbreak <- "<br>" 

#saving tonight's shift assignments
write.xlsx(Shift, "Q:/sursup/SHIFT SUPERVISION/Remote Dialing Stuff/shiftassignments.xlsx")

#saving new masterlist of seats
write.xlsx(NewMasterSeats, "Q:/sursup/SHIFT SUPERVISION/Remote Dialing Stuff/seats_test.xlsx") 

#deleting the assignment report (see reason above)
unlink("Q:/sursup/SHIFT SUPERVISION/Remote Dialing Stuff/assignments.csv")

print("Code complete")
