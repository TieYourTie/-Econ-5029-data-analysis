#Readme ###################
#trying the difference lag difference from thee each city
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
################################################################################
presentation_data  <- read_csv("Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/presentation_data_update.csv")


# Filter out non-province names and format REF_DATE
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
graphy_wow <- Presentation_no_na  %>%
  mutate(
    log_Total_HPI = ifelse(Total_HPI > 0, log(Total_HPI), NA),  # Avoid log(0)
    log_Housing_Supply = ifelse(housing_supply > 0, log(housing_supply), NA),  # Avoid log(0)
    HPI_change_rate = ifelse(lag(Total_HPI) > 0, (Total_HPI - lag(Total_HPI)) / lag(Total_HPI), NA),  # Change rate, avoid division by zero
    housing_supply_change_rate = ifelse(lag(housing_supply) > 0, (housing_supply - lag(housing_supply)) / lag(housing_supply), NA)  # Same here
  )

################################################################################
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggplot2)

# Step 1: 筛选 St. John's 数据
df_stjohns <- graphy_wow %>%
  filter(GEO == "Windsor") %>%
  arrange(REF_DATE) %>%
  select(REF_DATE, GEO, Total_HPI, housing_supply, Interpolated_Population)

# Step 2: 网格搜索所有 lag 组合 (最多滞后12期)
max_lag <- 100
aic_results <- expand.grid(lag_hs = 0:max_lag, lag_pop = 0:max_lag)
aic_results$AIC <- NA  # 为每种组合创建 AIC 空位

# Step 3: 进行循环搜索
for (i in 1:nrow(aic_results)) {
  l1 <- aic_results$lag_hs[i]
  l2 <- aic_results$lag_pop[i]
  
  df_lagged <- df_stjohns %>%
    mutate(
      housing_supply_lag = dplyr::lag(housing_supply, l1),
      population_lag = dplyr::lag(Interpolated_Population, l2)
    ) %>%
    select(Total_HPI, housing_supply_lag, population_lag) %>%
    na.omit()
  
  if (nrow(df_lagged) > 10) {
    model <- lm(Total_HPI ~ housing_supply_lag + population_lag, data = df_lagged)
    aic_results$AIC[i] <- AIC(model)
  }
}

# Step 4: 找出 AIC 最小的 lag 组合
best_row <- aic_results[which.min(aic_results$AIC), ]
cat("✅ 最佳组合是：housing_supply 滞后", best_row$lag_hs, 
    "，population 滞后", best_row$lag_pop,
    "\n📉 最小 AIC：", best_row$AIC, "\n")

# Step 5: 用最佳组合拟合最终模型
df_final <- df_stjohns %>%
  mutate(
    housing_supply_lag = dplyr::lag(housing_supply, best_row$lag_hs),
    population_lag = dplyr::lag(Interpolated_Population, best_row$lag_pop)
  ) %>%
  select(Total_HPI, housing_supply_lag, population_lag) %>%
  na.omit()

final_model <- lm(Total_HPI ~ 0 + housing_supply_lag + population_lag, data = df_final)
summary(final_model)

# Step 6: 可视化（可选）—— AIC 热力图
library(ggplot2)
ggplot(aic_results, aes(x = lag_hs, y = lag_pop, fill = AIC)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "AIC for each lag combination",
       x = "Lag of housing_supply", 
       y = "Lag of Interpolated_Population")






