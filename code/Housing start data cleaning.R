#Econ 5029 data cleaning code. 
#1. The housing start data
  #1.1 The housing start data province quarterly
  #1.2 The housing start data CMA level monthly 


rm(list = ls())
#make sure everthing will be fine.)

#Step one Lode the all package that necessary. 
library (lubridate)    
library (cansim)       
library (OECD)        
library (WDI)          
library (fredr)        
library (mFilter)      
library (neverhpfilter)
library (tsbox)
library (RColorBrewer) #so sad they do not have colorful black 
library(plotly)
library(wesanderson)
library(writexl)
library(tidyverse)
library(readr)

#1.1 Data processing: The housing start data province quarterly
#####################################################################################
#lode the data
HS.raw<- read_csv("~/Library/CloudStorage/GoogleDrive-brownlovecake2009@gmail.com/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/Data/Housing start/34100135.csv")

#pivot the data
HS_pivot <- HS.raw %>% 
  pivot_wider(names_from = `Housing estimates`, values_from = VALUE)

#keep only the important variable.
HS_half_cook <- HS_pivot %>% 
  select("REF_DATE", "GEO", "Type of unit", "UOM", "Housing starts", "Housing under construction", "Housing completions")

#rename the variable to R friendly
HS_half_cook  <- HS_half_cook  %>% 
  rename( Type_of_unit = `Type of unit`,
         Housing_starts = `Housing starts`,
         Housing_completions =  `Housing completions`,
         Housing_under_construction = `Housing under construction`)


#remove the row with housing_starts, housing_under_construction and housing_completions are both NA. 
HS_na_remover <- HS_half_cook %>%
  group_by(REF_DATE, GEO, Type_of_unit, UOM) %>% 
  filter(!(is.na(Housing_starts) & is.na(Housing_under_construction) & is.na(Housing_completions))) %>%
  ungroup()

#now, we remove the data that is all the na for that part.


# Function to reshape data
reshape_housing_data <- function(data) {
  data %>%
    group_by(REF_DATE, GEO, Type_of_unit, UOM) %>%
    summarize(
      Housing_starts = ifelse(all(is.na(Housing_starts)), NA, max(Housing_starts, na.rm = TRUE)),
      Housing_under_construction = ifelse(all(is.na(Housing_under_construction)), NA, max(Housing_under_construction, na.rm = TRUE)),
      Housing_completions = ifelse(all(is.na(Housing_completions)), NA, max(Housing_completions, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    filter(!(is.na(Housing_starts) & is.na(Housing_under_construction) & is.na(Housing_completions)))  # 过滤掉全 NA 行
}


#futher cook 
HS_semi_cook  <- reshape_housing_data(HS_na_remover)


#The next step if to filter out each region? 
#its will be a double neshed loop?

#the number of region
n_geo <- unique(HS_semi_cook$GEO)
n_type <- unique(HS_semi_cook$Type_of_unit)

filtered_data_list <- list()
#the first loop 

for(i in n_geo) {
 
  for(j in n_type){
    semi_product <- HS_semi_cook  %>% 
      filter(GEO == i &Type_of_unit == j )
    
    if(nrow(semi_product) > 0) {
      filtered_data_list[[paste(i, j, sep = "_")]] <- semi_product
    }
    }
  }
  
filtered_data <- bind_rows(filtered_data_list, .id = "Region_Type")
head(filtered_data)

#its look good, Its the time to check the NA. 
#its the time to save it 

write.csv(filtered_data, "Housing_start_full.csv")



##################################################
#1.2 The housing start data CMA level monthly 
rm(list = ls())
#make sure everthing will be fine.

#lode the data
CMA.raw <- read_csv("~/Library/CloudStorage/GoogleDrive-brownlovecake2009@gmail.com/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/RAW_data/Housing_start_monthly/3410015401_databaseLoadingData-2.csv")


#keep only the important variable.
CMA_cook <- CMA.raw %>% 
  select("REF_DATE", "GEO", "Type of unit", "Housing estimates", "UOM", "VALUE" )


#rename the variable to R friendly
CMA_half_cook  <- CMA_cook %>% 
  rename( Type_of_unit = `Type of unit`,
          Housing_estimates = `Housing estimates`)

library(dplyr)

# Get unique values for the three filtering variables
n_geo <- unique(CMA_half_cook$GEO)
n_type <- unique(CMA_half_cook$Type_of_unit)
n_he <- unique(CMA_half_cook$Housing_estimates)

# Create an empty list to store filtered data
filtered_data_list <- list()

# Triple nested loop to iterate through all combinations
for(i in n_geo) {
  for(j in n_type) {
    for(k in n_he) {
      
      # Corrected filter condition
      semi_product <- CMA_half_cook  %>% 
        filter(GEO == i & Type_of_unit == j & Housing_estimates == k)
      
      # Store only if there is data
      if(nrow(semi_product) > 0) {
        filtered_data_list[[paste(i, j, k, sep = "_")]] <- semi_product
      }
      
    }
  }
}

# Combine all filtered data into a single data frame
filtered_data <- bind_rows(filtered_data_list, .id = "Region_Type")

# Print the first few rows
head(filtered_data)

write.csv(filtered_data, "Housing_start_CMA_montly.csv")






