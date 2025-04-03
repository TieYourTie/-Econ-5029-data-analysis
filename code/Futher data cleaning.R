############################################################
#Econ 5029
#note: the code in this file has the content of trying different method of the
#re-calcuate the housing-supply-elastic
#note for abby:
  #here are some good time series code you can you
  #in the package forecast
    #using tslm() for hte time series lm
    #using checkresiduals() for check the residual
    #adf.test() for the adf() test


#things I need to do again
#############################################################
#Code update record
############################################################
#v0.2 Update the meraged code processing and add more data cleaning process.
#v0.3 now this data set including the housing permit
#v0.4 update the part of log-log difference
#v0.5 futher clean the code.
############################################################

###############################################################################
#Lode the package
###############################################################################
rm(list = ls())
#make sure everthing will be fine.)

#Step one Lode the all package that necessary. 
#(yes, I just copied it around)
library (lubridate)    
library (mFilter)      
library (neverhpfilter)
library (tsbox)
library(plotly)
library(wesanderson)
library(writexl)
library(cansim)
library(tidyverse)
library(readr)
library(forecast)
library(tseries)
library(ggplot2)
library(ggthemes)  # For Economist theme
library(dplyr)
library(readxl)
library(tidyr)
library(zoo)
library(tseries)  # This package helps us run the ADF test to check if data is stationary
library(purrr)    # This package helps us apply functions to multiple variables quickly


###############################################################################
#Step one lode the data
###############################################################################
#note: the policy rate and population data has been added in this dataset by using the cansim
#wow

#The housing price index
HPI.raw <- read_csv("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/HPI.csv")

#the housing supply
NHS.raw  <- read_csv("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/NHS_na_remover.csv")

#the population data
pd.raw  <- read_csv("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/pd.csv")

#housing_permit from 1948 to 2017
Housing_permit_one<- read_csv("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Housing_permit_all_cleaned.csv")

#
###############################################################################
#Processing the dictnoary
###############################################################################
#note: this section was design to create a dictnoary so we can
#aline all the data togther etter

#take out the list from the data
#HPI_list <- data.frame(GEO = unique(HPI.raw$GEO))
#NHS_list <- data.frame(GEO = unique(NHS.raw$GEO))
#pd_list <- data.frame(GEO = unique(pd.raw$GEO))

#combine it togther
#combined_list <- reduce(list(HPI_list, NHS_list, pd_list), full_join, by = "GEO")

#export it as exce.
#write.xlsx(combined_list, "combined_list.xlsx")

#note: after the mannally add the things
#then add it back 
#lode the cleaned dictionary
dictnoray <- read_excel("~/Documents/GitHub/-Econ-5029-data-analysis/combined_list.xlsx")

#note you need to keep the following:
#extracked the CMA
#extracked the Canada

#Further adjust for the dictnory.
# Remove empty dictionaries in Post_code
list_clean <- dictnoray %>%
  mutate(Post_code = case_when(
    GEO == "Canada" ~ "CA",
    
      # Assign J8T for all variations of Ottawa-Gatineau, Quebec part, Ontario/Quebec
        GEO == "Ottawa-Gatineau, Quebec part, Ontario/Quebec" ~ "J8T",
        GEO == "Ottawa - Gatineau, Quebec part, Ontario/Quebec" ~ "J8T",
        GEO == "Ottawa - Gatineau (CMA), Quebec part, Ontario/Quebec" ~ "J8T",
        
        # Assign K1G for all variations of Ottawa-Gatineau, Ontario part, Ontario/Quebec
        GEO == "Ottawa-Gatineau, Ontario part, Ontario/Quebec" ~ "K1G",
        GEO == "Ottawa - Gatineau, Ontario part, Ontario/Quebec" ~ "K1G",
        GEO == "Ottawa - Gatineau (CMA), Ontario part, Ontario/Quebec" ~ "K1G",
        
        # Assign K1A for all variations of Ottawa-Gatineau, Ontario/Quebec
        GEO == "Ottawa - Gatineau (CMA), Ontario/Quebec" ~ "K1A",
        GEO == "Ottawa-Gatineau, Ontario/Quebec" ~ "K1A",
        GEO == "Ottawa - Gatineau, Ontario/Quebec" ~ "K1A",
      
    
    TRUE ~ Post_code  # Keep existing values if no condition matches
  ))


#remove the empty (whitch is not the city)
list_clean <- list_clean %>% na.omit()



################################################################################
#Clean the housing permit data
################################################################################
#
# Table 34100007 - Building permits, dwelling units by type of structure, monthly - *Terminated*
# This table contains 1597 series, with data for Jan-1976 - Dec-2017 (not all combinations necessarily have data for all time periods), and was released on 08-Mar-2018.
# This table contains data described by the following dimensions (Not all combinations are available):
# Geography (116 items: Canada; Newfoundland and Labrador; Avalon Peninsula, Newfoundland and Labrador; South Coast-Burin Peninsula, Newfoundland and Labrador; Notre Dame-Central Bonavista Bay, Newfoundland and Labrador; ...)
# Type of structure (15 items: Total dwellings; Singles, includes mobile homes; Cottages; Doubles; Rows; ...)


#keep only the variables that matter 
Housing_permit_2017 <- Housing_permit_one %>% 
  select(REF_DATE, GEO, Residential)

#remove the row that is empty in the residential.
HP <- Housing_permit_2017 %>% 
  left_join(list_clean, by = "GEO")

#filter out the empty post_code 
HP <- HP %>% 
  filter(!is.na(Post_code))

#rename the variable 
HP <- HP %>%
  rename("HP_Residential" = "Residential" )

################################################################################
#IPPI
################################################################################
#Industrial product price index, by major product group, monthly 1, 2
#(https://www150.statcan.gc.ca/t1/tbl1/en/cv.action?pid=1810026501)

IPPI_raw <- "v1230995983" #Financial market statistics, last Wednesday unless otherwise stated, Bank of Canada, monthly
IPPI.st <- get_cansim_vector( IPPI_raw, start_time = "1900/01/01")
IPPI_year.st <- year( IPPI.st$REF_DATE[1])
IPPI_month.st <- month( IPPI.st$REF_DATE[1])
c(IPPI_year.st,  IPPI_month.st)

IPPI.ts<- ts( IPPI.st$VALUE, start = c( IPPI_year.st,  IPPI_month.st), freq = 12)


################################################################################
#the population data organized (Removed)
################################################################################
# #choice the variable
# pd <- pd.raw %>% select("REF_DATE", "GEO" , "VALUE")
# 
# #now its the time to processing the population data 
# pd <- pd %>%
#   mutate(REF_DATE = as.integer(REF_DATE))
# 
# # Step 1: Expand data to monthly for each region
# pd_monthly <- pd %>%
#   group_by(GEO) %>%  # Group by region
#   arrange(GEO, REF_DATE) %>%  # Ensure sorted order within each region
#   mutate(REF_DATE = as.yearmon(REF_DATE)) %>%  # Convert REF_DATE to yearmon format
#   complete(REF_DATE = seq(from = min(REF_DATE), to = max(REF_DATE), by = 1/12)) %>%  # Expand monthly
#   ungroup()  # Remove grouping for interpolation
# 
# # Step 2: Interpolate missing population values for each region
# pd_monthly <- pd_monthly %>%
#   group_by(GEO) %>%  # Group again for region-wise interpolation
#   mutate(VALUE = na.approx(VALUE, na.rm = FALSE)) %>%
#   ungroup()  # Remove grouping
# 
# # Step 3: Convert REF_DATE back to a Date format
# pd_monthly <- pd_monthly %>%
#   mutate(REF_DATE = as.Date(as.yearmon(REF_DATE), frac = 0))
# 
# # View results
# print(pd_monthly, n = 24)  # Show first 24 rows
# 

################################################################################
#The policy rate
################################################################################
#Financial market statistics, last Wednesday unless otherwise stated, Bank of Canada, monthly
PR_raw <- "v122530" 
PR.st <- get_cansim_vector( PR_raw, start_time = "1900/01/01")
PR_year.st <- year( PR.st$REF_DATE[1])
PR_month.st <- month( PR.st$REF_DATE[1])
c(PR_year.st,  PR_month.st)

PR.ts<- ts( PR.st$VALUE, start = c( PR_year.st,  PR_month.st), freq = 12)

autoplot(PR.ts) +
  ylab("Policy rate") +
  theme_fivethirtyeight() +
  xlab("Month") +
  ylab("rate") +
  ggtitle("Policy rate set by Bank of Canada ") +
  labs(subtitle = "from 2007 to 2023") +
  theme(axis.title = element_text())

################################################################################
#Combine data with dictionary
################################################################################
#The housing price index
HPI <- HPI.raw  %>% select("REF_DATE" , "GEO" , "Total" , "House", "Land")

#left join the HPI with the dictonary
HPI <- HPI %>% 
  left_join(list_clean, by = "GEO")

#filter out the empty post_code 
HPI <- HPI %>% 
  filter(!is.na(Post_code))

#create a new_row called the CMA

################################################################################
##combine the new housing supply with the housing permit
################################################################################
NHS <- NHS.raw %>% 
  select(REF_DATE, GEO, Total_units, Single_detached_units, Semi_detached_unitsn, Row_units, Apartment_and_other_units) %>% 
  rename(Semi_detached_units = Semi_detached_unitsn)

#left join the NHS  with the dictonary
NHS  <- NHS  %>% 
  left_join(list_clean, by = "GEO")

#filter out the empty post_code 
NHS <- NHS %>% 
  filter(!is.na(Post_code))

#combine those together
NHS_HPI <- NHS %>% 
  left_join(HPI, by = c("Post_code", "REF_DATE"))


#add the housing permit data 
NHS_HPI_HP <- NHS_HPI %>%
  left_join(HP, by = c("Post_code", "REF_DATE"))


# #the time for the left join
# pd_monthly <- pd_monthly %>% 
#   left_join(list_clean, by = "GEO")
# 
# #remove the na part from the post code
# pd_monthly <- pd_monthly %>% 
#   filter(!is.na(Post_code))


NHS_HPI_HP_ts <- NHS_HPI_HP %>%
  group_by(Post_code) %>%
  arrange(Post_code, REF_DATE) %>%
  mutate(REF_DATE = as.yearmon(REF_DATE))

#########
# 
# pd_monthly <- pd_monthly %>%
#   group_by(Post_code) %>%
#   arrange(Post_code, REF_DATE) %>%
#   mutate(REF_DATE = as.yearmon(REF_DATE))
# 
# #rename the variable 
# #pd_monthly <- pd_monthly %>% rename("population" = "VALUE")
# 
# 
# ##combine those together
# NHS_HPI_HP_HP_pd <- NHS_HPI_ts  %>% 
#   left_join(pd_monthly, by = c("Post_code", "REF_DATE"))



################################################################################
#add IPPI and policy rate
################################################################################
IPPI <- IPPI.st %>% 
  select(REF_DATE, VALUE)

#rename the variable
IPPI <- IPPI %>% 
  rename("IPPI" = "VALUE" )

# Convert REF_DATE to yearmon format
IPPI <- IPPI %>%
  mutate(REF_DATE = as.yearmon(REF_DATE, format = "%Y-%m-%d"))

#The policy rate
PR <- PR.st %>% 
  select(REF_DATE, VALUE)

#rename the variable
PR <- PR %>% 
  rename( "PR" = "VALUE")

# Convert REF_DATE to yearmon format
PR <- PR %>%
  mutate(REF_DATE = as.yearmon(REF_DATE, format = "%Y-%m-%d"))

#left_join time!
PR_IPPI <- left_join(PR,IPPI,  by = "REF_DATE")

#remove the missing value 
PR_IPPI <- PR_IPPI %>% na.omit(PR_IPPI)


# Merge IPPI and PR with wow_data
NHS_HPI_HP_PR_IPPI <-NHS_HPI_HP_ts%>%
  left_join(PR_IPPI, by = "REF_DATE") 

# Check results
head(NHS_HPI_HP_PR_IPPI)


################################################################################
#Further clearn the data
################################################################################

#remove the region name
all_data <- NHS_HPI_HP_PR_IPPI %>%
  filter(!is.na(Post_code))

all_data <- all_data %>%
  filter(!is.na(Total_units) & 
           !is.na(Single_detached_units) & 
           !is.na(Semi_detached_units) & 
           !is.na(Row_units) & 
           !is.na(Apartment_and_other_units))

all_data <- all_data %>%
  filter(!is.na(Total) & 
           !is.na(House) & 
           !is.na(Land))

all_data <- all_data %>%
  filter(!is.na(PR) & 
           !is.na(IPPI) )



#log the value
################################################################################
# #doing all the log transfermation for the data 
# all_data <- all_data %>%
#   rename("population" = "VALUE")

# Apply HP filter to log-transformed variables
wow_data <- all_data %>%
  group_by(Post_code) %>%
  mutate(
    log_Total_HPI = if_else(Total > 0, log(Total), 0),  
    Log_House_HPI = if_else(House > 0, log(House), 0),
    Log_Land_HPI = if_else(Land > 0, log(Land), 0),
    Log_Total_units_Supply = if_else(Total_units > 0, log(Total_units), 0),  
    Log_Single_detached_units = if_else(Single_detached_units > 0, log(Single_detached_units), 0),
    Log_Semi_detached_units = if_else(Semi_detached_units > 0, log(Semi_detached_units), 0),
    Log_Row_units = if_else(Row_units > 0, log(Row_units), 0),
    Log_Apartment_and_other_units = if_else(Apartment_and_other_units > 0, log(Apartment_and_other_units), 0),
    log_HP_Residential = if_else(HP_Residential > 0, log(HP_Residential), 0)
    ) 

wow_data <- wow_data %>% filter(!is.na(HP_Residential))

###############################################################################
#Try to the log and log?
###############################################################################
# # Filter cities with at least 20 observations
# filtered_data <- wow_data %>%
#   group_by(Post_code) %>%
#   filter(n() <= 20) %>%
#   ungroup()
# 
# # Define formulas
# formulas <- list(
#   "Single_detached" = Log_House_HPI ~ 0 + Log_Single_detached_units,
#   "Semi_detached"   = Log_House_HPI ~ 0 + Log_Semi_detached_units,
#   "Row"             = Log_House_HPI ~ 0 + Log_Row_units,
#   "Apartment_other" = Log_House_HPI ~ 0 + Log_Apartment_and_other_units
# )
# 
# # Create a dataframe to store coefficients
# coefficient_df <- data.frame()
# 
# # Run regressions
# for (pc in unique(filtered_data$Post_code)) {
#   city_data <- filtered_data %>% filter(Post_code == pc)
#   
#   for (type in names(formulas)) {
#     model <- lm(formulas[[type]], data = city_data)
#     coef_val <- coef(model)[[1]]
#     
#     # Save result
#     coefficient_df <- rbind(coefficient_df, data.frame(
#       Post_code = pc,
#       Type = type,
#       Coefficient = coef_val
#     ))
#   }
# }
# 
# 
# 
# # Function to plot each type
# plot_coefficients_by_type <- function(type_name, data) {
#   df <- data %>% filter(Type == type_name) %>%
#     arrange(desc(Coefficient))
#   
#   avg_coef <- mean(df$Coefficient, na.rm = TRUE)
#   
#   ggplot(df, aes(x = reorder(Post_code, Coefficient), y = Coefficient)) +
#     geom_bar(stat = "identity", fill = "steelblue") +
#     geom_hline(yintercept = avg_coef, color = "red", linetype = "dashed", linewidth = 1) +
#     coord_flip() +
#     labs(
#       title = paste("Coefficient of", type_name, "Units"),
#       subtitle = paste("Average Coefficient =", round(avg_coef, 3)),
#       x = "Post Code (City)",
#       y = "Log-Log Regression Coefficient"
#     ) +
#     theme_minimal()
# }
# 
# # Plot for each type
# plot_coefficients_by_type("Single_detached", coefficient_df)
# plot_coefficients_by_type("Semi_detached", coefficient_df)
# plot_coefficients_by_type("Row", coefficient_df)
# plot_coefficients_by_type("Apartment_other", coefficient_df)
# 





################################################################################
#remove the unwanted variable 
################################################################################
# Define variables to check
variables_to_check <- c("log_Total_HPI", "Log_House_HPI", "Log_Land_HPI", "Log_Total_units_Supply",
                        "Log_Single_detached_units", "Log_Semi_detached_units", "Log_Row_units",
                        "PR", "IPPI", "log_HP_Residential")

# Convert specified variables to numeric and remove rows with empty/NA values
wow_data <- wow_data %>%
  mutate(across(all_of(variables_to_check), as.numeric)) %>%  # Convert to numeric
  filter(if_any(all_of(variables_to_check), ~ !is.na(.) & . != ""))


################################################################################
#Stationary the data
################################################################################

#take out of the list in the post code
unique_post_codes <- unique(wow_data$Post_code)

# Store each Post_code's data in a list
post_code_data <- split(wow_data, wow_data$Post_code)


make_stationary_with_date <- function(data, variables) {
  
  # check if the date and post code
  if (!all(c("REF_DATE", "Post_code") %in% colnames(data))) {
    stop("Error: The dataset must have 'REF_DATE' and 'Post_code' columns!")
  }
  
  # create a list that store the stationary data
  stationary_data <- data %>% select(REF_DATE, Post_code)
  
  #create a list for the adf-list
  adf_results <- list()
  
  #go though every variable
  for (var in variables) {
    
    #only do the test for number
    if (!is.numeric(data[[var]])) {
      warning(paste("Skipping column:", var, "because it's not numeric!"))
      next
    }
    
    #remove the NA
    clean_var <- na.omit(data[[var]])
    
    #if there are not enough data then screaming for warning.
    if (length(clean_var) < 5) {
      warning(paste("Skipping column:", var, "because it has too few non-NA observations!"))
      next
    }
    
    # run the adf test
    test_result <- tryCatch(
      adf.test(clean_var, alternative = "stationary"),
      error = function(e) return(NULL)
    )
    
    #If the ADF test wrong 
    if (is.null(test_result) || is.na(test_result$p.value)) {
      warning(paste("ADF test failed for variable:", var))
      next
    }
    
    # save it
    adf_results[[var]] <- list(
      "ADF_Stat" = test_result$statistic,
      "P_Value" = test_result$p.value,
      "Stationary" = test_result$p.value < 0.05
    )
    
    # check if the data is stationary
    # if the things is not stationary
    if (test_result$p.value < 0.05) {
      stationary_data[[var]] <- data[[var]]
    } else {
      #then doing the first difference
      d1_var <- paste0("d_", var)
      stationary_data[[d1_var]] <- c(NA, diff(data[[var]]))
      
      #check if the stationary is long enough
      d1_clean_var <- na.omit(stationary_data[[d1_var]])
      if (length(d1_clean_var) >= 5) {
        d1_test_result <- tryCatch(
          adf.test(d1_clean_var, alternative = "stationary"),
          error = function(e) return(NULL)
        )
        
        if (!is.null(d1_test_result) && !is.na(d1_test_result$p.value)) {
          # save the reuslt of the adf
          adf_results[[d1_var]] <- list(
            "ADF_Stat" = d1_test_result$statistic,
            "P_Value" = d1_test_result$p.value,
            "Stationary" = d1_test_result$p.value < 0.05
          )
          
          # check the statonary for the adf
          if (d1_test_result$p.value < 0.05) {
            next 
          }
        } else {
          warning(paste("ADF test failed for first-differenced variable:", d1_var))
        }
      }
      
      # calcuate the second difference
      d2_var <- paste0("d2_", var)
      stationary_data[[d2_var]] <- c(NA, NA, diff(data[[var]], differences = 2))
      
      #check the if result of the second stationary is long enough
      d2_clean_var <- na.omit(stationary_data[[d2_var]])
      if (length(d2_clean_var) >= 5) {
        d2_test_result <- tryCatch(
          adf.test(d2_clean_var, alternative = "stationary"),
          error = function(e) return(NULL)
        )
        
        if (!is.null(d2_test_result) && !is.na(d2_test_result$p.value)) {
          # result in the second section
          adf_results[[d2_var]] <- list(
            "ADF_Stat" = d2_test_result$statistic,
            "P_Value" = d2_test_result$p.value,
            "Stationary" = d2_test_result$p.value < 0.05
          )
        } else {
          warning(paste("ADF test failed for second-differenced variable:", d2_var))
        }
      }
    }
  }
  
  # organized the adf result 
  adf_summary <- tibble(
    Variable = names(adf_results),
    ADF_Stat = map_dbl(adf_results, ~ .x$ADF_Stat),
    P_Value = map_dbl(adf_results, ~ .x$P_Value),
    Stationary = map_lgl(adf_results, ~ .x$Stationary)
  )
  
  #out put the ADF result.
  return(list("data" = stationary_data, "adf_results" = adf_summary))
}

# List of variables we want to check for stationarity
variables_to_check <- c("log_Total_HPI", "Log_House_HPI", "Log_Land_HPI", "Log_Total_units_Supply",
                        "Log_Single_detached_units", "Log_Semi_detached_units", "Log_Row_units",
                        "PR", "IPPI", "log_HP_Residential")




################################################################################
#Save the result 
################################################################################

# Create a list to store each processed dataset
stationary_datasets <- list()
adf_results_list <- list()

# Get unique post codes
post_codes <- unique(wow_data$Post_code)

# Loop through each post code and process data separately
for (i in post_codes) {
  
  # Filter the dataset for the current post code
  data_subset <- post_code_data[[i]]  # This selects the data for the given post code
  
  # Run the stationary processing function on the subset
  result <- make_stationary_with_date(data_subset, variables_to_check)
  
  # Save the processed dataset in a list using the post code as the key
  stationary_datasets[[i]] <- result$data
  
  # Save ADF test results in another list
  adf_results_list[[i]] <- result$adf_results
  
  # Dynamically assign a variable with the post code as the name
  assign(paste0("stationary_data_", i), result$data)
}


# Create a list to store each processed dataset
stationary_d_datasets <- list()
adf_d_results_list <- list()


# Loop through each post code and process data separately
for (i in post_codes) {
  
  # Filter the dataset for the current post code
  data_subset <- post_code_data[[i]]  # This selects the data for the given post code
  
  # Run the stationary processing function on the subset
  result <- make_stationary_with_date(data_subset, variables_to_check)
  
  # Save the processed dataset in a list using the post code as the key
  stationary_d_datasets[[i]] <- result$data
  
  # Save ADF test results in another list
  adf_d_results_list[[i]] <- result$adf_results
  
  # Dynamically assign a variable with the post code as the name
  assign(paste0("stationary_data_", i), result$data)
}


# Show an example: first few rows of the first dataset
head(stationary_datasets[[post_codes[1]]])  # Show first dataset processed

save(stationary_datasets, file = "stationary_data.RData")
#################






