#NCQA Sample Pull - Written by Perry Leibovitz + James StCharles
# This code sorts and selects sample according to NCQA HEDIS Guidelines.
# Make sure your source spreadsheet has a header row with labels "firstname", "lastname", "dob"(MMDDYYYY), and "address"
# Each year, you have to change:
#
#   the OrigFile path (line 21)
#   variables (lines 26-28)
#   the sort ("arrange" in R terms) from ascending/descending (line 48).
#   write SelectedFile path (line 60)

rm(list = ls())

library(openxlsx)
library(tidyverse)
library(readxl)
library(readr)

###General Sample Pull###

#reading in the original xlsx file in dataframe OrigFile
OrigFile <- read_fwf("Q:/winwork/NCQA 2022/Systematic Sampling Test Files/CAHPS Sample Frame Test Deck.txt", 
                     fwf_cols(col1 = c(1, 112), firstname = c(113, 137), middlename = c(138), lastname = c(139, 163), 
                              gender = c(164), dob = c(165, 172), address = c(173, 641)), trim_ws = FALSE)


#setting variables. Only edit "User entered"
'%notin%' <- Negate('%in%')
MRSS<- 1100 #User entered
oversample <- .05 #User entered
RAND	<- 0.77 #User entered
EM <-	nrow(OrigFile) #formula
FSS <- ceiling(1100 + (1100 * oversample)) #formula
N <- floor(EM/FSS)  #formula
START <- ifelse(round(RAND * N) < 1, 1, round(RAND * N)) #formula - ifelse to round up to at least 1

#making a vector called member of values 1 through FSS
member <- 1:FSS

#loop for index number being selected for each member; member is updated from 1:FSS to the index we want
for(i in 1:FSS){
  
  member[i] <- round(START + ((i - 1) * (EM/FSS)))
  
  
}


#sorting the file and storing in new dataframe SortedFile
SortedFile <- OrigFile %>% arrange(desc(lastname), desc(firstname), desc(dob), desc(address))

#adding a column named "Index" to SortedFile with values 1 to how many rows there are in SortedFile
SortedFile$Index <- 1:nrow(SortedFile) 

#filtering SortedFile where Index and member match and storing in new dataframe SelectedFile
SelectedFile <- SortedFile %>% filter(Index %in% member) %>% select(-Index)

#creating CCC_OrigFile dataframe of unselected members of the general sample pull
#CCC_OrigFile <- SortedFile %>% filter(Index %notin% member)

#Matching the original text file (might need editing next time)
SelectedFile <- unite(SelectedFile, mlname, middlename:lastname, sep = "", remove = TRUE) %>% 
  unite(gdobadd, gender:address, sep = "", remove = TRUE) 

#Exporting to Excel the SelectedFile dataframe
#write.xlsx(SelectedFile, "Q:/winwork/NCQA 2021/NCQA Systematic Sample Test 2021/NCQA2021_sample_pulled.xlsx")

write.table(SelectedFile, "Q:/winwork/NCQA 2022/Systematic Sampling Test Files/NCQA2022_sample_pulled.txt")

# ###CCC Sample Pull###
# 
# 
# CCC_MRSS<- 1100 #User entered
# CCC_oversample <- .03 #User entered
# CCC_EM <-	nrow(CCC_OrigFile) #Formula
# 
# CCC_FSS <- ceiling(1100 + (1100 * CCC_oversample)) #formula
# CCC_N <- floor(CCC_EM/CCC_FSS)  #formula
# CCC_START <- round(RAND * CCC_N) #formula
# 
# 
# #making a vector called member of values 1 through FSS
# CCC_member <- 1:CCC_FSS
# 
# #loop for index number being selected for each member; member is updated from 1:FSS to the index we want
# for(i in 1:CCC_FSS){
#   
#   CCC_member[i] <- round(CCC_START + ((i - 1) * (CCC_EM/CCC_FSS)))
#   
# }
# #sorting the file and storing in new dataframe SortedFile
# CCC_SortedFile <- CCC_OrigFile %>% filter(CCCFlag==2) %>% arrange(desc(lastname, firstname, dob, address))
# 
# #adding a column named "Index" to SortedFile with values 1 to how many rows there are in SortedFile
# CCC_SortedFile$CCC_Index <- 1:nrow(CCC_SortedFile) 
# 
# #filtering SortedFile where Index and member match and storing in new dataframe SelectedFile
# CCC_SelectedFile <- CCC_SortedFile %>% filter(CCC_Index %in% CCC_member)
# 
# #Exporting to Excel the SelectedFile dataframe
# write.xlsx(CCC_SelectedFile, "Q:/winwork/NCQA 2020/NCQA Systematic Sample Test 2020/NCQA2020_sample_pulled_CCC.xlsx")