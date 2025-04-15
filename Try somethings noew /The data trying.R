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

################################################################################

raw <- load("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_stational_datasset_20250325.RData")

#remove all the NA vlaue
clean <- lapply(raw , na.omit)

#pick_the_variable
 clean_select <- clean
#    \l
# apply(clean, clean_column_names)


#make sure every list_city need to have at least 20 col, remove city if its not working 
##########filter out city does not have enough data
clean_select <- filter_cities_by_rows(clean_select, row_min = 300, row_max = 311)

#print the outcome for the data does not have enough data 

row_counts <- sapply(clean_select, nrow)
cities_not_enough_data <- names(clean_select)[row_counts < 300]
#the following city does not have enough dataset 
"G8T" "J1C" "J8T" "L1G" "N1C" "V1P"
###########

###########run the city specific log-log regression

# --------------------------------------------------
# 1) Helper function: get_column_name
#    Given a data frame and a vector of possible column names (in order),
#    returns the first one that actually exists in the data.
#    If none is found, returns NULL.
# --------------------------------------------------
get_column_name <- function(df, possible_names) {
  found <- possible_names[possible_names %in% names(df)]
  if (length(found) > 0) {
    return(found[1])  # Return the first match
  } else {
    return(NULL)
  }
}

# --------------------------------------------------
# 2) Main function: run_regressions_for_city
#    Takes a single city’s data frame.
#    1. Identifies the correct Y variable (d2_Log_House_HPI or d_Log_House_HPI).
#    2. For each of the four specified X variables, tries the "d2_" version if present.
#    3. Fits lm(), extracts the coefficient, and stores in a results data frame.
# --------------------------------------------------
run_regressions_for_city <- function(df) {
  # Safely extract this city’s post_code (assuming it’s consistent within the city).
  city_post_code <- if ("Post_code" %in% names(df)) {
    # Use the first unique post code as the label for this city
    unique(df$Post_code)[1]
  } else {
    # If there's no Post_code column, use something else or just "Unknown"
    "Unknown"
  }
  
  # 2A) Identify the correct Y variable:
  #     Prefer d2_Log_House_HPI if it exists, otherwise use d_Log_House_HPI.
  y_var <- get_column_name(df, c("d2_Log_House_HPI", "d_Log_House_HPI"))
  # If neither is found, skip all regressions for this city:
  if (is.null(y_var)) {
    return(NULL)
  }
  
  # 2B) Define the set of possible X columns for each conceptual variable
  #     We look for "d2_" versions first, then "d_" or "Log_" versions next, etc.
  possible_x_vars <- list(
    # 1) "Total units supply"
    supply = c("d2_Log_Total_units_Supply", "d_Log_Total_units_Supply", "Log_Total_units_Supply"),
    # 2) "Single detached" units
    single = c("d2_Log_Single_detached_units", "d_Log_Single_detached_units", "Log_Single_detached_units"),
    # 3) "Semi detached" units
    semi   = c("d2_Log_Semi_detached_units", "d_Log_Semi_detached_units", "Log_Semi_detached_units"),
    # 4) "Row units"
    row    = c("d2_Log_Row_units", "d_Log_Row_units", "Log_Row_units")
  )
  
  # We'll store each regression's results here
  results_list <- list()
  
  # Loop over each X set
  for (model_tag in names(possible_x_vars)) {
    
    # Attempt to find the appropriate column in df
    x_var <- get_column_name(df, possible_x_vars[[model_tag]])
    
    # If none of the variants is found, skip this regression
    if (is.null(x_var)) next
    
    # Build the regression formula (no intercept):
    # e.g.  "d_Log_House_HPI ~ 0 + d_Log_Single_detached_units"
    formula_str <- paste0(y_var, " ~ 0 + ", x_var)
    model_formula <- as.formula(formula_str)
    
    # Fit the model
    fit <- lm(model_formula, data = df)
    
    # Extract coefficient info. Because we used "0 + x_var", there's exactly 1 coefficient
    sm <- summary(fit)
    coef_mat <- sm$coefficients
    
    # In case something is off or there's no data
    if (nrow(coef_mat) < 1) next
    
    # Extract stats for the single coefficient:
    est   <- coef_mat[1, 1]  # Estimate
    se    <- coef_mat[1, 2]  # Std. error
    tval  <- coef_mat[1, 3]  # t-value
    pval  <- coef_mat[1, 4]  # p-value
    
    # Create a small data frame with the relevant info
    one_result <- data.frame(
      post_code  = city_post_code,
      y_variable = y_var,
      x_variable = x_var,
      formula    = formula_str,
      estimate   = est,
      std_error  = se,
      statistic  = tval,
      p_value    = pval
    )
    
    # Add to our growing list
    results_list[[length(results_list) + 1]] <- one_result
  }
  
  # Combine all results for this city into a single data frame
  if (length(results_list) > 0) {
    city_results <- do.call(rbind, results_list)
    return(city_results)
  } else {
    return(NULL)
  }
}

# --------------------------------------------------
# 3) Apply to each city in your list "clean_select"
#    and combine results into one data frame
# --------------------------------------------------
library(dplyr)

all_cities_results <- lapply(clean_select, run_regressions_for_city) %>%
  bind_rows()

# Now, "all_cities_results" should hold one row per regression, 
# including the post code (if present) as well as coefficient estimates.
# You can view it with:
# View(all_cities_results)

library(dplyr)
library(ggplot2)

# --- 1) House HPI vs. Total Supply ---
chart_data_supply <- all_cities_results %>%
  # Keep only rows whose formula includes "Total_units_Supply"
  filter(grepl("Total_units_Supply", formula)) %>%
  # Sort by estimate (descending) to rank from highest to lowest
  arrange(desc(estimate))

ggplot(chart_data_supply, aes(x = reorder(post_code, estimate), y = estimate)) +
  geom_col() +
  coord_flip() +  # flip so cities go top-to-bottom
  labs(
    title = "d_Log_House_HPI ~ 0 + (d2/d)_Log_Total_units_Supply",
    x = "Post Code",
    y = "Coefficient Estimate"
  )

# --- 2) House HPI vs. Single-Detached Units ---
chart_data_single <- all_cities_results %>%
  filter(grepl("Single_detached_units", formula)) %>%
  arrange(desc(estimate))

ggplot(chart_data_single, aes(x = reorder(post_code, estimate), y = estimate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "d_Log_House_HPI ~ 0 + (d2/d)_Log_Single_detached_units",
    x = "Post Code",
    y = "Coefficient Estimate"
  )

# --- 3) House HPI vs. Semi-Detached Units ---
chart_data_semi <- all_cities_results %>%
  filter(grepl("Semi_detached_units", formula)) %>%
  arrange(desc(estimate))

ggplot(chart_data_semi, aes(x = reorder(post_code, estimate), y = estimate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "d_Log_House_HPI ~ 0 + (d2/d)_Log_Semi_detached_units",
    x = "Post Code",
    y = "Coefficient Estimate"
  )

# --- 4) House HPI vs. Row Units ---
chart_data_row <- all_cities_results %>%
  filter(grepl("Row_units", formula)) %>%
  arrange(desc(estimate))

ggplot(chart_data_row, aes(x = reorder(post_code, estimate), y = estimate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "d_Log_House_HPI ~ 0 + (d2/d)_Log_Row_units",
    x = "Post Code",
    y = "Coefficient Estimate"
  )


library(dplyr)
library(ggplot2)

# 1) Filter out the unwanted post_codes
excluded_codes <- c("G8T", "J1C", "J8T", "L1G", "N1C", "V1P")
filtered_results <- all_cities_results %>%
  filter(!post_code %in% excluded_codes)

# 2) Define a plotting function that returns a ggplot object
plot_regression_bar <- function(data, pattern, plot_title) {
  chart_data <- data %>%
    filter(grepl(pattern, formula)) %>%
    arrange(desc(estimate))
  
  p <- ggplot(chart_data, aes(x = reorder(city_name, estimate), y = estimate)) +
    geom_col() +
    coord_flip() +
    labs(
      title = plot_title,
      x = "City",
      y = "Coefficient Estimate"
    )
  return(p)
}

# 3) Create each chart as a ggplot object
p_supply <- plot_regression_bar(
  filtered_results,
  pattern    = "Total_units_Supply",
  plot_title = "House HPI vs. Total Units Supply"
)

p_single <- plot_regression_bar(
  filtered_results,
  pattern    = "Single_detached_units",
  plot_title = "House HPI vs. Single-Detached Units"
)

p_semi <- plot_regression_bar(
  filtered_results,
  pattern    = "Semi_detached_units",
  plot_title = "House HPI vs. Semi-Detached Units"
)

p_row <- plot_regression_bar(
  filtered_results,
  pattern    = "Row_units",
  plot_title = "House HPI vs. Row Units"
)

# 4) Save each chart to your working directory (PNG format)
ggsave("chart_house_vs_total_supply.png", p_supply, width = 8, height = 6, dpi = 300)
ggsave("chart_house_vs_single_detached.png", p_single, width = 8, height = 6, dpi = 300)
ggsave("chart_house_vs_semi_detached.png", p_semi, width = 8, height = 6, dpi = 300)
ggsave("chart_house_vs_row_units.png", p_row, width = 8, height = 6, dpi = 300)

# After running this code, you should have four PNGs saved in your working directory.

  
  
  
  



