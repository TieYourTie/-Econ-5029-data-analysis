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
  
############################################################
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
############################################################
#Step one lode the data
################################################################################
#The housing price index
HPI.raw <- read_csv("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/HPI.csv")

#the housing supply
NHS.raw  <- read_csv("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/NHS_na_remover.csv")

#the population data
pd.raw  <- read_csv("~/Documents/GitHub/-Econ-5029-data-analysis/Cleaned_data/Presentation_use_data/pd.csv")

#The general producer price index 

################################################################################

#OK, What I need to check if the data is working or not? 
  #what is the frquence? 
  #Does it seasonal adjust? 
  #remove the province and keep only the CMA level.
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

autoplot(IPPI.ts) +
  ylab("Policy rate") +
  theme_fivethirtyeight() +
  xlab("Month") +
  ylab("rate") +
  ggtitle("Policy rate set by Bank of Canada ") +
  labs(subtitle = "from 2007 to 2023") +
  theme(axis.title = element_text())

################################################################################
#The policy rate
################################################################################
PR_raw <- "v122530" #Financial market statistics, last Wednesday unless otherwise stated, Bank of Canada, monthly
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
#Now, its the time to processing the list
################################################################################

# 提取唯一城市名并转换为数据框
HPI_list <- data.frame(GEO = unique(HPI.raw$GEO))
NHS_list <- data.frame(GEO = unique(NHS.raw$GEO))
pd_list <- data.frame(GEO = unique(pd.raw$GEO))

# 合并三个数据框，按GEO列合并
combined_list <- reduce(list(HPI_list, NHS_list, pd_list), full_join, by = "GEO")

# 输出为 Excel 文件
write.xlsx(combined_list, "combined_list.xlsx")

#lode the cleaned dictionary 
dictnoray <- read_excel("combined_list.xlsx")

#note you need to keep the following:
  #extracked the CMA
  #extracked the Canada

#remove the dictonary that is empty in the post_cost 
list_clean <- dictnoray %>% 
  mutate(Post_code = ifelse(GEO == "Canada", "CA", Post_code),
         Post_code = ifelse(GEO == "Canada", "CA", Post_code) 

#remove the empty (whitch is not the city)
list_clean <- list_clean %>% na.omit()


################################################################################
#####1.Some smaller cleaning
################################################################################

#########
#The housing price index
HPI <- HPI.raw  %>% select("REF_DATE" , "GEO" , "Total" , "House", "Land")

#left join the HPI with the dictonary
HPI <- HPI %>% 
  left_join(list_clean, by = "GEO")

#filter out the empty post_code 
HPI <- HPI %>% 
  filter(!is.na(Post_code))

#create a new_row called the CMA

##########
#####
#pick the correct variable
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
#####
#now its the time to processing the population data 
pd.ts <- ts(pd.raw, frequency = 1 , from = min(Ref_date), to = max(Ref-date) )


#####
PR_m <- PR.raw  %>%
  mutate(REF_DATE = format(REF_DATE, "%Y-%m")) %>%  
  group_by(REF_DATE) %>%
  summarise(Policy_Rate = mean(VALUE, na.rm = TRUE))

#####
NHS <- NHS.raw   %>% 
  select(REF_DATE, GEO, Total_units)

###
pd <- pd.raw  %>%
  select(REF_DATE, GEO, VALUE)



################################################################################
#1.1rename_the_variable
################################################################################
#PR_m

NHS <- NHS %>%
  rename(housing_supply = Total_units)

pd <- pd %>% 
  rename(populaton = VALUE)

HPI <- HPI %>% 
  rename(Total_HPI = Total,
         House_HPI = House,
         Land_HPI = Land)

# Change the row name "Census metropolitan areas and census agglomerations of 50,000 and over" to "Canada"
NHS <- NHS %>%
  mutate(GEO = ifelse(GEO == "Census metropolitan areas and census agglomerations of 50,000 and over", 
                      "Canada", GEO))

# Display the updated dataset
print(NHS)


print(unique(NHS$GEO))

print(unique(pd $GEO))
print(unique(HPI$GEO))

################################################################################
#2. I came up with the better code, so ignore the folwing code 
################################################################################








#1.Date processing
  #1.1. stationary the data
  #1.2. stationary check
    #1.2.1 ADF test 
    #1.2.2 union root test 
#2. the following method will be used in the code file 
  #2.1. De-trend log value
  #2.2. Log-difference regression
  #2.3. autoregressive distributed lag (ARDL) model
############################################################
v1230995983
#load the data
raw <- read_csv("Code/Cleaned_data/Presentation_use_data/presentation_data_update.csv")

#check the data
View(presentation_data_update)

#further data cleaning
regions_to_remove <- c("Atlantic Region", "Region", "Newfoundland and Labrador", "Prince Edward Island", 
                       "Nova Scotia", "New Brunswick", "Quebec", "Ontario", "Prairie Region",
                       "Manitoba", "Saskatchewan", "Alberta", "British Columbia")

#filter out the data that is not city 
data <- raw %>% filter(!(GEO %in% regions_to_remove))

# Calculate log and change rates, handling zero values
graphy_wow <- data %>%
  group_by(GEO) %>%
  mutate(
    log_Total_HPI = if_else(Total_HPI > 0, log(Total_HPI), NA_real_),  # Avoid log(0), ensure numeric consistency
    log_Housing_Supply = if_else(housing_supply > 0, log(housing_supply), NA_real_),  # Avoid log(0)
    HPI_change_rate = if_else(lag(Total_HPI, default = NA_real_) > 0, 
                              (Total_HPI - lag(Total_HPI, default = NA_real_)) / lag(Total_HPI, default = NA_real_), 
                              NA_real_),  # Change rate, avoid division by zero
    housing_supply_change_rate = if_else(lag(housing_supply, default = NA_real_) > 0, 
                                         (housing_supply - lag(housing_supply, default = NA_real_)) / lag(housing_supply, default = NA_real_), 
                                         NA_real_)  # Same here
  ) %>%
  ungroup()  # Ensure no unexpected behavior from grouping

#check if the result be better? 
#no
############################################################
#transfer the data into the standard time-series data
#test run###################################################


#Take out the national data
national_level <- graphy_wow %>% filter(GEO == "Sudbury")

#transfer it into the time series data
nl <- national_level %>%
  mutate(REF_DATE = as.yearmon(REF_DATE, format = "%Y-%m"))

#remove the na part
nl <- nl %>% filter(!is.na(Total_HPI) & !is.na(housing_supply))

#transfer it into the time series data
nl$log_Housing_Supply <- ts(nl$log_Housing_Supply, start = c(year(min(nl$REF_DATE)), month(min(nl$REF_DATE))), frequency = 12)
nl$ts_total_hpi <- ts(nl$log_Total_HPI, start = c(year(min(nl$REF_DATE)), month(min(nl$REF_DATE))), frequency = 12)


#check the seasonality
#ggseasonplot(nl$log_Housing_Supply, year.labels = TRUE, year.labels.left = TRUE) +
 # ggtitle("Seasonal Plot of log_Housing_Supply") +
 # ylab("log_Housing_Supply") +
 # theme_minimal()

#ggseasonplot(nl$ts_total_hpi, year.labels = TRUE, year.labels.left = TRUE) +
 # ggtitle("Seasonal Plot of log_Total_HPI") +
 # ylab("log_Total_HPI") +
  #theme_minimal()


#conclution: there is no seasonality within the this datasheet
#######################################################################
#plot the ACF graphy

#check the result
#checkresiduals(nl$log_Total_HPI, lag.max = 100) + ggtitle("")
#checkresiduals(nl$log_Housing_Supply, lag.max = 100) + ggtitle("")

#conclution: the data is not stationaied
#now, its the time to apply the first time difference 

nl.sd <- nl %>% 
  select(REF_DATE, log_Total_HPI,log_Housing_Supply )

#take the data out for the difference
nl.sd.housing <- nl.sd %>% 
  select(REF_DATE, log_Housing_Supply )

#take the data out for the difference
nl.sd.hpi <- nl.sd %>% 
  select(REF_DATE, log_Total_HPI )

#difference
nl_sd_houing_twice <- diff(nl.sd.housing$log_Housing_Supply, lag = 1)
nl_sd_hpi_twice  <- diff(nl.sd.hpi$log_Total_HPI, lag = 1)

#check the residual 
checkresiduals(nl_sd_houing_twice)
checkresiduals(nl_sd_hpi_twice)

#run ADF test, its the stationary now! 
adf.test(nl_sd_houing_twice)
adf.test(nl_sd_hpi_twice)


# Ensure both series have the same length after differencing
min_length <- min(length(nl_sd_houing_twice), length(nl_sd_hpi_twice))

# Create a time series dataframe
data_ts <- ts(data.frame(
  housing = nl_sd_houing_twice[1:min_length],
  hpi = nl_sd_hpi_twice[1:min_length]
), start = start(nl_sd_houing_twice), frequency = frequency(nl_sd_houing_twice))

# Check the structure
head(data_ts)


# Fit the time series linear model
model <- tslm(housing ~ 0 + hpi, data = data_ts)

# View the model summary
summary(model)
#function####################################


estimate_tslm_by_region <- function(data) {
  # Get the unique list of GEO regions
  unique_regions <- unique(data$GEO)
  
  # Initialize an empty vector to store coefficients
  estimates <- numeric(length(unique_regions))
  
  # Loop through each GEO region
  for (i in seq_along(unique_regions)) {
    region <- unique_regions[i]
    
    # Filter data for the current region
    region_data <- data %>% filter(GEO == region)
    
    # Convert REF_DATE to year-month format
    region_data <- region_data %>%
      mutate(REF_DATE = as.yearmon(REF_DATE, format = "%Y-%m"))
    
    # Remove rows with NA values
    region_data <- region_data %>%
      filter(!is.na(Total_HPI) & !is.na(housing_supply))
    
    # Convert log-transformed variables into time series
    region_data$log_Housing_Supply <- ts(region_data$log_Housing_Supply, 
                                         start = c(year(min(region_data$REF_DATE)), month(min(region_data$REF_DATE))), 
                                         frequency = 12)
    
    region_data$ts_total_hpi <- ts(region_data$log_Total_HPI, 
                                   start = c(year(min(region_data$REF_DATE)), month(min(region_data$REF_DATE))), 
                                   frequency = 12)
    
    # Select relevant columns
    region_ts <- region_data %>% select(REF_DATE, log_Total_HPI, log_Housing_Supply)
    
    # Difference the time series to ensure stationarity
    housing_diff <- diff(region_ts$log_Housing_Supply, lag = 1)
    hpi_diff <- diff(region_ts$log_Total_HPI, lag = 1)
    
    # Ensure both series have the same length
    min_length <- min(length(housing_diff), length(hpi_diff))
    
    # Create a data frame for time series regression
    data_ts <- ts(data.frame(
      housing = housing_diff[1:min_length],
      hpi = hpi_diff[1:min_length]
    ), start = start(housing_diff), frequency = frequency(housing_diff))
    
    # Run the time series linear model (without intercept)
    model <- tslm(housing ~  0 + hpi, data = data_ts)
    
    # Extract the coefficient estimate for hpi
    estimates[i] <- coef(model)["hpi"]
  }
  
  # Create a matrix with region names
  estimate_matrix <- matrix(estimates, nrow = length(unique_regions), dimnames = list(unique_regions, "Estimate"))
  
  # Print the matrix
  print(estimate_matrix)
  
  return(estimate_matrix)
}

# Run the function on graphy_wow dataset
estimate_matrix <- estimate_tslm_by_region(graphy_wow)

#plot the graphy################################

# Convert the matrix to a dataframe
df_estimates <- as.data.frame(estimate_matrix)
df_estimates$City <- rownames(estimate_matrix)

# Calculate the national mean and variance
national_mean <- mean(df_estimates$Estimate)
national_sd <- sd(df_estimates$Estimate)

# Sort data from highest to lowest estimate
df_estimates <- df_estimates %>%
  arrange(desc(Estimate))

# Convert City to an ordered factor for ggplot
df_estimates$City <- factor(df_estimates$City, levels = df_estimates$City)

# Create the plot
ggplot(df_estimates, aes(x = City, y = Estimate, fill = Estimate)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Bar chart
  geom_hline(yintercept = national_mean, linetype = "dashed", color = "red", size = 1.2) + # Mean line
  geom_hline(yintercept = national_mean + national_sd, linetype = "dotted", color = "blue", size = 1) + # +1 SD
  geom_hline(yintercept = national_mean - national_sd, linetype = "dotted", color = "blue", size = 1) + # -1 SD
  
  # Labels for Mean and Standard Deviation
  annotate("text", x = length(df_estimates$City) - 2, y = national_mean + 0.5, 
           label = paste0("Mean: ", round(national_mean, 2)), color = "red", hjust = 1) +
  annotate("text", x = length(df_estimates$City) - 2, y = national_mean + national_sd + 0.5, 
           label = paste0("+1 SD: ", round(national_mean + national_sd, 2)), color = "blue", hjust = 1) +
  annotate("text", x = length(df_estimates$City) - 2, y = national_mean - national_sd - 0.5, 
           label = paste0("-1 SD: ", round(national_mean - national_sd, 2)), color = "blue", hjust = 1) +
  
  # Improve color mapping
  scale_fill_gradient2(low = "red", mid = "white", high = "darkgreen", midpoint = national_mean) +
  
  # Apply the Economist theme
  theme_economist() +
  
  # Improve text readability
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  
  # Labels
  labs(
    title = "Estimated Impact of HPI on Housing Supply by City",
    subtitle = "National mean and variance included",
    x = "City",
    y = "Estimated Coefficient"
  )

#####################

#Its the time to merge! 

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
library(stringr)




































