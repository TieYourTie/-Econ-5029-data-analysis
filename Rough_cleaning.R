#Econ 5029 data cleaning code. 
#1. The housing start data
  #1.1 The housing start data (National quartly)
  #1.2 The housing start data (CMA monthly )

#2 Housing permit
  #Housing permit monthly (province and CMA)
  #https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=3410000301

#3 average construction cost 
# https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1810004601

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


################################################################################
#1.1 The housing start data (province quarterly)
################################################################################
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


################################################################################
#1.2 The housing start data (CMA monthly)
################################################################################


#Lode the data 
CMA.raw <- read_csv("H:/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/RAW_data/Housing_start_monthly/34100154.csv")

#pivot the data
CMA_pivot <- CMA.raw %>% 
  pivot_wider(names_from = `Housing estimates`, values_from = VALUE)

#keep only the important variable.
CMA_half_cook <- CMA_pivot %>% 
  select("REF_DATE", "GEO", "Type of unit", "UOM", "Housing starts", "Housing under construction", "Housing completions")


#rename the variable to R friendly
CMA_half_cook  <- CMA_half_cook  %>% 
  rename( Type_of_unit = `Type of unit`,
          Housing_starts = `Housing starts`,
          Housing_completions =  `Housing completions`,
          Housing_under_construction = `Housing under construction`)


#remove the row with housing_starts, housing_under_construction and housing_completions are both NA. 
CMA_na_remover <- CMA_half_cook %>%
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
CMA_semi_cook  <- reshape_housing_data(CMA_na_remover)


#The next step if to filter out each region? 
#its will be a double neshed loop?

#the number of region
n_geo <- unique(CMA_semi_cook$GEO)
n_type <- unique(CMA_semi_cook$Type_of_unit)

filtered_data_list <- list()
#the first loop 

for(i in n_geo) {
  
  for(j in n_type){
    semi_product <- CMA_semi_cook  %>% 
      filter(GEO == i &Type_of_unit == j )
    
    if(nrow(semi_product) > 0) {
      filtered_data_list[[paste(i, j, sep = "_")]] <- semi_product
    }
  }
}


filtered_data <- bind_rows(filtered_data_list, .id = "Region_Type")
head(filtered_data)


filtered_data_na_freee <- filtered_data %>% 
  group_by(REF_DATE, GEO, Type_of_unit, UOM) %>%
  filter(!(Housing_starts == 0 & Housing_under_construction == 0 & Housing_completions == 0))



write.csv(filtered_data_na_freee, "Housing_CMA_monthly.csv")

################################################################################
#2.0 The housing permit both province and CMA level
################################################################################
 #https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=3410000301
 #Archived - Building permits, values by activity sector (x 1,000) 1, 2 
 
#clean the enviroment 
rm(list = ls())


#Lode the data 
Housing_permit.raw <- read_csv("H:/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/Code/RAW_data/Housing_permit_1948-2017/3410000301_databaseLoadingData.csv")

#pivot the data
Housing_permit_pivot <- Housing_permit.raw %>% 
  pivot_wider(names_from = `Activity sector`, values_from = VALUE)

#keep only the important variable.
Housing_permit_cook <- Housing_permit_pivot %>% 
  select(
    "REF_DATE", 
    "GEO", 
    "UOM", 
    "Total residential and non-residential", 
    "Residential", 
    "Single Family", 
    "Multi -family", 
    "Non-residential", 
    "Industrial", 
    "Commercial", 
    "Institutional and governmental", 
    "Other non-specified, non-residential buildings"
  )




#rename the variable to R friendly
Housing_permit_half_cook  <- Housing_permit_cook  %>% 
  rename(
    Total_residential_and_non_residential = `Total residential and non-residential`,
    Single_Family = `Single Family`,
    Multi_family= `Multi -family`,
    Non_residential = `Non-residential`,
    Institutional_and_governmental = `Institutional and governmental`,
    Other_nonspecified_nonresidential_buildings = `Other non-specified, non-residential buildings`
  )



#remove the row with housing_starts, housing_under_construction and housing_completions are both NA. 
Housing_permit_na_remover <- Housing_permit_half_cook %>%
  group_by(REF_DATE, GEO, UOM) %>% 
  filter(!(is.na(Total_residential_and_non_residential) & 
             is.na(Residential) & 
             is.na(Single_Family) & 
             is.na(Multi_family) & 
             is.na(Non_residential) & 
             is.na(Industrial) & 
             is.na(Commercial) & 
             is.na( Institutional_and_governmental) & 
             is.na( Other_nonspecified_nonresidential_buildings))) %>%
  ungroup()


wow <- colnames(Housing_permit_na_remover)

for (i in wow) {
  print(unique(is.na(Housing_permit_na_remover[[i]])))
}


library(dplyr)

reshape_housing_data <- function(data) {
  num_cols <- setdiff(colnames(data), c("REF_DATE", "GEO", "UOM"))  # 选取数值列
  
  data %>%
    group_by(REF_DATE, GEO, UOM) %>%  # 按日期、地区和单位分组
    summarize(across(all_of(num_cols), ~ ifelse(all(is.na(.)), NA, max(., na.rm = TRUE)), .names = "{.col}"),
              .groups = "drop") %>%  # 计算分组内的最大值
    filter(!(is.na(Total_residential_and_non_residential) & 
               is.na(Residential) & 
               is.na(Single_Family) & 
               is.na(Multi_family) & 
               is.na(Non_residential) & 
               is.na(Industrial) & 
               is.na(Commercial) & 
               is.na(Institutional_and_governmental) & 
               is.na(Other_nonspecified_nonresidential_buildings)))
}




#futher cook 
Housing_permit_semi_cook  <- reshape_housing_data(Housing_permit_na_remover)





filtered_data_na_freee <- filtered_data %>% 
  group_by(REF_DATE, GEO, Type_of_unit, UOM) %>%
  filter(!(Housing_starts == 0 & Housing_under_construction == 0 & Housing_completions == 0))



write.csv(Housing_permit_semi_cook, "Housing_permit_all_cleaned.csv")

################################################################################
#3.0 The housing construction cost by CMA monthly? 
################################################################################


# Use either double backslashes or forward slashes in the file path
HCC.raw <- read_csv("H:/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/Code/RAW_data/Construction_wage/1810004601_databaseLoadingData.csv")

# Check if the data loaded correctly
head(HCC.raw)

#keep only relative variable
HCC.ts <- HCC.raw %>%
  select("REF_DATE", "GEO", "Construction trades",  "VALUE")


#rename the variable 
HCC.ts <- HCC.ts %>% 
  rename( Construction_trades = `Construction trades`)

HCC_p.ts <- HCC.ts %>%
  pivot_wider(
    names_from = Construction_trades,  # 将 "Construction_trades" 作为列名
    values_from = VALUE  # "VALUE" 作为新列的数据
  ) %>%
  arrange(REF_DATE)

HCC_half.ts <- HCC_p.ts %>%
  group_by(REF_DATE, GEO) %>%
  mutate(midwage = median(c_across(3:18), na.rm = TRUE))  # Calculate median wage across all trades


HCC_half_half <- HCC_half.ts %>% 
  select(REF_DATE, GEO, midwage)


HCC_half_half <- HCC_half_half %>% 
  group_by(REF_DATE, GEO)





################################################################################
#4.0 The CMA population data
################################################################################




################################################################################







