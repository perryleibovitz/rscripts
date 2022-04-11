#NCQA Sample Pull - Written by Perry Leibovitz
# This code sorts and selects sample according to NCQA HEDIS Guidelines.
# Make sure your source spreadsheet has a header row with labels "firstname", "lastname", "dob"(MMDDYYYY), and "address"
# Each year, you have to change in the code
#
#   the OrigFile path (line 21)
#   variables (lines 26-28; lines 66+67)
#   the sort ("arrange" in R terms) from ascending/descending (line 48).
#   write AllFile path (line 96)

rm(list = ls())

library(openxlsx)
library(tidyverse)
library(readxl)


###General Sample Pull###

#reading in the original xlsx file in dataframe OrigFile
OrigFile <- read.xlsx("Q:/winwork/NCQA 2021/Sample/CMS/CMS_dedup.xlsx")


#setting variables. Only edit "User entered"
'%notin%' <- Negate('%in%')
MRSS<- 1650 #User entered
oversample <- .00 #User entered
RAND	<- 0.18 #User entered

EM <-	nrow(OrigFile) #formula
FSS <- ceiling(MRSS + (MRSS * oversample)) #formula
N <- floor(EM/FSS)  #formula
START <- ifelse(round(RAND * N) < 1, 1, round(RAND * N)) #formula - ifelse to round up to at least 1

#making a vector called member of values 1 through FSS
member <- 1:FSS

#loop for index number being selected for each member; member is updated from 1:FSS to the index we want
for(i in 1:FSS){
  
  member[i] <- round(START + ((i - 1) * (EM/FSS)))
  
  
}


#sorting the file and storing in new dataframe SortedFile
SortedFile <- OrigFile %>% arrange(last, first, month, day, year, address_city)

#adding a column named "Index" to SortedFile with values 1 to how many rows there are in SortedFile
SortedFile$Index <- 1:nrow(SortedFile) 

#filtering SortedFile where Index and member match and storing in new dataframe SelectedFile
SelectedFile <- SortedFile %>% filter(Index %in% member)

#creating CCC_OrigFile dataframe of unselected members of the general sample pull
CCC_OrigFile <- SortedFile %>% filter(Index %notin% member)

#Exporting to Excel the SelectedFile dataframe
#write.xlsx(SelectedFile, "Q:/winwork/NCQA 2020/NCQA Systematic Sample Test 2020/NCQA2020_sample_pulled.xlsx")


###CCC Sample Pull###


CCC_MRSS<- 1840 #User entered
CCC_oversample <- .0 #User entered
CCC_EM <-	CCC_OrigFile %>% filter(CCCFlag==2) %>% nrow() #Formula

CCC_FSS <- ceiling(CCC_MRSS + (CCC_MRSS * CCC_oversample)) #formula
CCC_N <- floor(CCC_EM/CCC_FSS)  #formula
CCC_START <- ifelse(round(RAND * CCC_N) < 1, 1, round(RAND * CCC_N)) #formula


#making a vector called member of values 1 through FSS
CCC_member <- 1:CCC_FSS

#loop for index number being selected for each member; member is updated from 1:FSS to the index we want
for(i in 1:CCC_FSS){
  
  CCC_member[i] <- round(CCC_START + ((i - 1) * (CCC_EM/CCC_FSS)))
  
}
#sorting the file and storing in new dataframe SortedFile
CCC_SortedFile <- CCC_OrigFile %>% filter(CCCFlag==2) %>% arrange(last, first, month, day, year, address_city)

#adding a column named "Index" to SortedFile with values 1 to how many rows there are in SortedFile
CCC_SortedFile$CCC_Index <- 1:nrow(CCC_SortedFile) 

#filtering SortedFile where Index and member match and storing in new dataframe SelectedFile
CCC_SelectedFile <- CCC_SortedFile %>% filter(CCC_Index %in% CCC_member)

All_SelectedFile <- bind_rows(SelectedFile, CCC_SelectedFile)

#Exporting to Excel the SelectedFile dataframe
write.xlsx(All_SelectedFile, "Q:/winwork/NCQA 2021/Sample/CMS/CMS_selected.xlsx")