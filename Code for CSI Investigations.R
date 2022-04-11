#Code for CSI Investigations. Created by Perry Leibovitz on 11/12/19

#This code allows the user to look at any range of CSI datasets to find out information about the population of Florida 
#based on past surveys

rm(list = ls()) # remove old environment i.e. proc datasets kill

#libraries needed
library(haven)
library(tidyverse)
library(dplyr)
library(janitor)

# directory to pull datasets from
setwd("C:/Export/work") 


# makes a list of filepaths ending with value of pattern
df <- list.files(path = "C:/Export/work", 
                 full.names = TRUE,
                 recursive = FALSE,
                 pattern = "*.sas7bdat") %>%    
      
      
  # converts list to a dataframe with one variable (value) containing the filepaths
      tbl_df() %>%                              
 
  # adds a variable (data) that reads the filepath from "value" as a SAS dataset and stores each month's dataset as a list   
      mutate(data = map(value, read_sas)) %>%   
                                                
  # unnests the list variable "data" creating CSI variables as columns with all of a month's data within the column  
      unnest_wider(data) %>%                    
                                                
  # keeps your choice of variables   
      select(value, HLTHCON) %>%
  
  # unnests each month's worth of data     
      unnest() # %>%                             
  
  # parameters to filter for
      #filter(COUNTY %in% c(6, 13, 44, 50), BFORFL > 51) 

  # Summary table with N + percents
tabyl(df$RRACE, sort = TRUE)      
