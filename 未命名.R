#Readme ###################
#the new data d2 means twice difference and d mean only once difference.
#I will create code here that generate the VAR regression read data.
#because each variable are stationary or not really depend on the city, therefore I am generate 
#some really wild idea here.
#using a function to do the city specifc VAR regression, but since you did not tell me the order...
#wow 

####
#lode_the_package###########
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

#lode_the_data#########
#function#######
clean_column_names <- function(df) {
  # Get all column names
  col_names <- colnames(df)
  
  # Find all variables with "d_" and "d2_" prefixes
  d_vars <- grep("^d_", col_names, value = TRUE)
  d2_vars <- grep("^d2_", col_names, value = TRUE)
  
  # Remove "d2_" prefix to get the base variable names
  base_d2_vars <- sub("^d2_", "", d2_vars)
  
  # Find corresponding "d_" variables that should be removed
  remove_d_vars <- d_vars[d_vars %in% paste0("d_", base_d2_vars)]
  
  # Keep only columns that are NOT in remove_d_vars
  cleaned_df <- df[, !(col_names %in% remove_d_vars)]
  
  return(cleaned_df)
}

#######

#remove all the NA vlaue
clean <- lapply(stationary_datasets, na.omit)

#pick_the_variable
clean_select <- lapply(clean, clean_column_names)


# Define the folder where you want to save files
save_folder <- "clean_data_outputs/"  # Change this to your desired folder

# Create folder if it doesn't exist
if (!dir.exists(save_folder)) {
  dir.create(save_folder)
}

# Save each dataframe as a separate CSV file
lapply(names(clean_select), function(name) {
  write.csv(clean_select[[name]], file = paste0(save_folder, name, ".csv"), row.names = FALSE)
})

# Define the folder where you want to save the RData files
save_folder <- "clean_data_outputs/"  # Change this to your preferred directory

# Create folder if it doesn't exist
if (!dir.exists(save_folder)) {
  dir.create(save_folder)
}

ungroup(E2H)

E2H.df <- as.data.frame(E2H)

E2H.var <- E2H.df[, !colnames(E2H) %in% c("REF_DATE", "Post_code")]


var_selection <- VARselect(E2H, lag.max = 10)


# Estimate the VAR model with different lags
var_selection <- VARselect(E2H.var, lag.max = 10)

# View the suggested lags
print(var_selection)


optimal_lag <- var_selection$selection["AIC(n)"]  # Change "AIC(n)" to "BIC(n)" if preferred

# Fit the VAR model
var_model <- VAR(E2H.var, p = optimal_lag)

summary(var_model)

