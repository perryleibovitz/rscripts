###NCQA DEDUPLICATION-WRITTED BY PERRY LEIBOVITZ 1/17/20

##This code reads in an Excel spreadsheet and deduplicates according to the NCQA method
##User must edit the seed number and where the file reads in and out.
##Make sure your source spreadsheet has a header row with labels "first", "last", "month", "day", "year", "address_city", and "CCCFlag"

##If calendar year = even (e.g. 2022), arrange function needs to be desc() for all variables. (line 34)

#remove old environment
rm(list = ls())

#libraries needed
library(openxlsx)
library(tidyverse)
library(tidylog, warn.conflicts = FALSE)

#seed number - this changes each year
seed <- 4

#read in the original file
orig <- read.xlsx("Q:/winwork/NCQA 2022/Sample/FHK/FHK_orig.xlsx")

#sort the original file. 

#First, it is grouped by address to allow R to count how many times an address occurs (variable hh_size)
#Then, it is sorted according to NCQA methods. If the year is even (eg. 2020) the fields are sorted descending
#The next step creates a variable called index based on which instance of a duplicate each observation is in the sorted file.
#This step works because of the grouping.
#We also create a new variable called hh_selected which says tells us which member of the sorted household is selected.

sorted <- orig %>% 
  group_by(address_city) %>%
  mutate(hh_size = n()) %>% 
  arrange(desc(address_city), desc(day), desc(month), desc(year), desc(first)) %>% 
  mutate(index=1:n(), hh_selected= ifelse(hh_size < seed, seed%%hh_size+1, seed)) 

#New dataframe called Selection where we keep only the selected household members (matching index to hh_selected)
selection <- subset(sorted, index==hh_selected)

#write the dataframe selection to Excel
write.xlsx(selection, "Q:/winwork/NCQA 2022/Sample/FHK/FHK_dedup.xlsx")






#OLD NESTED IFELSE - UNNECESSARILY MESSY
#Selecting the sample member that survives deduplication

#The selected member of a household changes depending on the household size (variable hh_size)
#The formula for selecting members is "the remainder of seed divided the household size + 1"
#If the household size is equal to or greater than the seed number, than the member selected will equal the seed number
#This is why we created the variables "hh_size" and "index"; to match which member gets selected according to total household size
#We created a new variable called "selected" and go through each household size and match "index" to the formula result
#Each selected member gets a 1; unselected members get a 0

# selected <- mutate(sorted, 
#                    selected = ifelse(hh_size==1, 1, 
#                                      ifelse(hh_size==2 & index==seed%%2+1, 1, 
#                                             ifelse(hh_size==3 & index==seed%%3+1, 1,
#                                                    ifelse(hh_size==4 & index==seed%%4+1, 1,
#                                                           ifelse(hh_size==5 & index==seed%%5+1, 1, 
#                                                                  ifelse(hh_size==6 & index==seed%%6+1, 1,
#                                                                         ifelse(hh_size==7 & index==seed%%7+1, 1,
#                                                                                ifelse(hh_size==8 & index==seed%%8+1, 1,
#                                                                                       ifelse(hh_size==9 & index==seed%%9+1, 1,
#                                                                                              ifelse(hh_size>=10 & index==10,1,0)))))))))))
# 
# #The final step takes only the members that were selected and writes the data to an .xlsx file
# final <- selected %>% filter(selected==1) %>% write.xlsx("Q:/NIW/M Rashy/CAHPS_Child_FFS open_PLFINAL.xlsx")