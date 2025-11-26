#Readme ###################
#trying the difference lag difference from thee each city
###
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
library(broom)
library(writexl)
################################################################################

#lode the data set 
presentation_data  <- read_csv("Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/presentation_data_update.csv")

#######Data processing 

#remove the region that is not for city?
presentation_data <- presentation_data %>% # Remove rows where GEO is 'Canada'
  mutate(REF_DATE = format(my(REF_DATE), "%Y-%m"))  # Convert to "YYYY-MM" using lubridate

# Define the list of regions to filter out
region_list <- c(
  "Atlantic Region",
  "Newfoundland and Labrador",
  "Prince Edward Island",
  "Nova Scotia",
  "New Brunswick",
  "Ontario",
  "Prairie Region",
  "Manitoba",
  "British Columbia",
  "Quebec",
  "Saskatchewan",
  "Alberta",
  "Canada"
)

# Filter out the specified regions
PresentationData_filter <- presentation_data %>%
  filter(!(GEO %in% region_list))  # Removed extra closing parenthesis


#remove the na 
Presentation_no_na <- na.omit(PresentationData_filter)

#filte the outcome
geo_counts <- presentation_data %>%
  group_by(GEO) %>%
  summarise(count = n()) %>%
  arrange(count)  # Optional: sort to see smallest first

################################################################################
# Calculate log and change rates, handling zero values
################################################################################

graphy_wow <- Presentation_no_na  %>%
  mutate(
    log_Total_HPI = ifelse(Total_HPI > 0, log(Total_HPI), NA),  # Avoid log(0)
    log_Housing_Supply = ifelse(housing_supply > 0, log(housing_supply), NA),  # Avoid log(0)
    lag_population = ifelse(Fixed_Population > 0, log(Total_HPI), NA))
    

#remove the the row does not matter.
graphy_wow_owo <- graphy_wow %>%  select(REF_DATE, GEO, log_Total_HPI, log_Housing_Supply, lag_population)

#removce the na
graphy_wow_owo <- na.omit(graphy_wow_owo )

################################################################################
#The first try: lag housing supply 12 month to say what is going on.
########################################################

# #random try
# evil <- graphy_wow_owo %>% 
#   filter(GEO == "St. Catharines-Niagara")


# Step 1: 先为每个城市生成滞后12期的 log_Total_HPI
graphy_lagged <- graphy_wow_owo %>%
  group_by(GEO) %>%
  arrange(REF_DATE) %>%
  mutate(log_Housing_Supply12 = lag(log_Housing_Supply, 12)) %>%
  ungroup()

# Step 2: 过滤 NA，防止回归出错
graphy_lagged  <- graphy_lagged  %>%
  filter(!is.na(log_Housing_Supply12), !is.na(log_Housing_Supply))


# Step 2: 对每个城市进行回归：log_Housing_Supply ~ 0 + log_HPI_lag12
regression_results <- graphy_lagged %>%
  group_by(GEO) %>%
  do(tidy(lm(log_Housing_Supply12 ~ 0 + log_Total_HPI, data = .))) %>%
  ungroup()

# Step 3: 筛选出斜率项结果
regression_summary_two <- regression_results %>%
  filter(term == "log_Total_HPI") %>%
  select(GEO, estimate, std.error, statistic, p.value)

# Step 4: 查看结果
print(regression_summary_two)

#regression table
med_regression_two <- median(regression_summary_two$estimate)

#regression table low elastic
R_2_low <- regression_summary_two %>% 
  filter(estimate <= med_regression_two)

#regression table high elastic
R_2_high <- regression_summary_two %>% 
  filter(estimate >= med_regression_two )

################################################################################
#The first try: lag housing price 12 month to say what is going on.
########################################################

# #random try
# evil <- graphy_wow_owo %>% 
#   filter(GEO == "St. Catharines-Niagara")


# Step 1: 先为每个城市生成滞后12期的 log_Total_HPI
graphy_lagged <- graphy_wow_owo %>%
  group_by(GEO) %>%
  arrange(REF_DATE) %>%
  mutate(log_Total_HPI12 = lag(log_Total_HPI, 12)) %>%
  ungroup()

# Step 2: 过滤 NA，防止回归出错
graphy_lagged  <- graphy_lagged  %>%
  filter(!is.na(log_Total_HPI12), !is.na(log_Total_HPI))


# Step 2: 对每个城市进行回归：log_Housing_Supply ~ 0 + log_HPI_lag12
regression_results <- graphy_lagged %>%
  group_by(GEO) %>%
  do(tidy(lm(log_Total_HPI ~ 0 + log_Total_HPI12, data = .))) %>%
  ungroup()

# Step 3: 筛选出斜率项结果
regression_summary_one <- regression_results %>%
  filter(term == "log_Total_HPI12") %>%
  select(GEO, estimate, std.error, statistic, p.value)

# Step 4: 查看结果
print(regression_summary_one)

#regression table
med_regression_two <- median(regression_summary_two$estimate)

#regression table low elastic
R_2_low <- regression_summary_two %>% 
  filter(estimate <= med_regression_two)

#regression table high elastic
R_2_high <- regression_summary_two %>% 
  filter(estimate >= med_regression_two )



########################################################
####regression two with 12 lag (city specifc lag)
########################################################

# get_best_lag_supply <- function(df, lag_max = 20) {
#   df <- na.omit(df)
#   if (nrow(df) < 5) return(data.frame(best_lag = NA, max_corr = NA))
#   
#   # 滞后 housing supply，保持 HPI 不动
#   ccf_result <- ccf(df$log_Total_HPI, df$log_Housing_Supply, lag.max = lag_max, plot = FALSE)
#   
#   best_index <- which.max(abs(ccf_result$acf))
#   best_lag <- ccf_result$lag[best_index]
#   best_corr <- ccf_result$acf[best_index]
#   
#   return(data.frame(best_lag = best_lag, max_corr = best_corr))
# }
# 
# # 应用于所有城市
# lag_table_supply <- graphy_wow_owo %>%
#   arrange(GEO, REF_DATE) %>%
#   group_by(GEO) %>%
#   group_modify(~ get_best_lag_supply(.x)) %>%
#   ungroup()
# 
# print(lag_table_supply)
# 
# 
# 
# get_best_lag_supply <- function(df, lag_max = 20) {
#   df <- na.omit(df)
#   if (nrow(df) < 5) return(data.frame(best_lag = NA, max_corr = NA))
#   
#   # 滞后 housing supply，保持 HPI 不动
#   ccf_result <- ccf(df$log_Total_HPI, df$log_Housing_Supply, lag.max = lag_max, plot = FALSE)
#   
#   best_index <- which.max(abs(ccf_result$acf))
#   best_lag <- ccf_result$lag[best_index]
#   best_corr <- ccf_result$acf[best_index]
#   
#   return(data.frame(best_lag = best_lag, max_corr = best_corr))
# }
# 
# 
# 
# # 应用于所有城市
# lag_table_supply <- graphy_wow_owo %>%
#   arrange(GEO, REF_DATE) %>%
#   group_by(GEO) %>%
#   group_modify(~ get_best_lag_supply(.x)) %>%
#   ungroup()
# 
# print(lag_table_supply)
# regression_lagged_supply <- function(df, lag_df) {
#   results <- list()
#   
#   for (city in unique(df$GEO)) {
#     lag_val <- lag_df %>% filter(GEO == city) %>% pull(best_lag)
#     if (is.na(lag_val)) next
#     
#     df_city <- df %>%
#       filter(GEO == city) %>%
#       arrange(REF_DATE)
#     
#     # 滞后 housing supply，而不是 HPI
#     if (lag_val > 0) {
#       df_city <- df_city %>%
#         mutate(supply_lagged = lag(log_Housing_Supply, n = lag_val))
#     } else if (lag_val < 0) {
#       df_city <- df_city %>%
#         mutate(supply_lagged = lead(log_Housing_Supply, n = abs(lag_val)))
#     } else {
#       df_city <- df_city %>%
#         mutate(supply_lagged = log_Housing_Supply)
#     }
#     
#     df_city <- df_city %>%
#       drop_na(supply_lagged, log_Total_HPI)
#     
#     if (nrow(df_city) < 5) next
#     
#     # 回归：log(HPI) ~ lagged supply
#     model <- lm( supply_lagged ~ 0 + log_Total_HPI, data = df_city)
#     tidy_result <- tidy(model) %>% mutate(GEO = city)
#     
#     results[[city]] <- tidy_result
#   }
#   
#   bind_rows(results)
# }
# 
# # 执行滞后 supply 回归
# regression_results_supply <- regression_lagged_supply(graphy_wow_owo, lag_table_supply)
# 
# # 整理回归结果
# regression_summary_supply <- regression_results_supply %>%
#   filter(term == "supply_lagged") %>%
#   select(GEO, estimate, std.error, statistic, p.value)
# 
# # 查看结果
# print(regression_lagged_summary)

###############

# Step 1:  log_HPI = log_Total_HPI - log_Total_HPI_lag12
graphy_diff12 <- graphy_wow_owo %>%
  group_by(GEO) %>%
  arrange(REF_DATE) %>%
  mutate(
    delta12_log_HPI = log_Total_HPI - lag(log_Total_HPI, 12)
  ) %>%
  ungroup()

# Step 2: 过滤 NA，防止回归出错
graphy_filtered <- graphy_diff12 %>%
  filter(!is.na(delta12_log_HPI), !is.na(log_Housing_Supply))

# Step 3: 计算每个城市样本数
sample_sizes <- graphy_filtered %>%
  group_by(GEO) %>%
  summarise(n = n(), .groups = "drop")

# Step 4: log_Housing_Supply ~ 0 + delta12_log_HPI
regression_results <- graphy_filtered %>%
  group_by(GEO) %>%
  do(tidy(lm(log_Housing_Supply ~ 0 + delta12_log_HPI, data = .))) %>%
  ungroup()

# Step 5: 提取斜率和统计显著性指标
regression_summary_delta <- regression_results %>%
  filter(term == "delta12_log_HPI") %>%
  select(GEO, estimate, std.error, statistic, p.value)

# Step 6: 合并样本数
regression_summary_delta <- regression_summary_delta %>%
  left_join(sample_sizes, by = "GEO")


# Step 7: 查看结果
print(regression_summary_delta)

#regression table low elastic
R_12_median <- median(regression_summary_delta$estimate)

R_12_low <- regression_summary_delta %>%
  filter(estimate <= R_12_median)

#regression table highear elastic
R_12_high <- regression_summary_delta %>%
  filter(estimate > R_12_median)





# save to the excl.
write_xlsx(regression_summary_delta, "12_month_lag_difference.xlsx")
#write_xlsx(lag_table_supply, "lag_table.xlsx")
write_xlsx(regression_lagged_summary, "regression_lagged_summary.xlsx")
write_xlsx(regression_summary_two, "regression_summary_two.xlsx")
write_xlsx(regression_summary_one, "regression_summary_one.xlsx")

