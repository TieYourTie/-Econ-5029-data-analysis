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


################################################################################
#####Lode the data 
################################################################################
HPI.raw <- read_csv("H:/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/Code/Cleaned_data/Presentation_use_data/HPI.csv")
NHS.raw  <- read_csv("H:/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/Code/Cleaned_data/Presentation_use_data/NHS_na_remover.csv")
pd.raw  <- read_csv("H:/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/Code/Cleaned_data/Presentation_use_data/pd.csv")
PR.raw  <- read_csv("H:/我的云端硬盘/Mac things/2025 winter/Econ 5029w/5029 project/Data cleaning processing/Code/Cleaned_data/Presentation_use_data/PR.csv")


################################################################################
#####1.Some smaller cleaning
################################################################################
HPI <- HPI.raw  %>% select("REF_DATE" , "GEO" , "Total" , "House", "Land")

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
#2. make sure the name in the GEO are the same 
################################################################################


# 统一城市名称函数
standardize_geo <- function(df) {
  df %>%
    mutate(GEO = str_replace_all(GEO, c(
      " \\(CMA\\)" = "",           # 删除 (CMA)
      " \\(CA\\)" = "",            # 删除 (CA)
      ", Ontario/Quebec" = "",     # 统一双省城市
      ", Quebec part, Quebec" = "", 
      ", Ontario part, Ontario" = "",
      ", Ontario" = "", 
      ", Quebec" = "",
      ", British Columbia" = "",
      ", Alberta" = "",
      ", Saskatchewan" = "",
      ", Manitoba" = "",
      ", Nova Scotia" = "",
      ", New Brunswick" = "",
      ", Newfoundland and Labrador" = "",
      ", Prince Edward Island" = "",
      " - " = "-",                 # 统一 ` - ` 为 `-`
      "Québec" = "Quebec",         # 统一特殊字符
      "St\\. " = "Saint "          # 统一 St. 为 Saint
    )))
}


# 应用于所有数据集
HPI <- standardize_geo(HPI)
NHS <- standardize_geo(NHS)
pd <- standardize_geo(pd)

# 确保标准化后 `GEO` 名称一致
print(unique(HPI$GEO))
print(unique(NHS$GEO))
print(unique(pd$GEO))




####################################################

# 进一步标准化城市名称
standardize_geo_v2 <- function(df) {
  df %>%
    mutate(GEO = str_replace_all(GEO, c(
      " - " = "-",              # 统一 ` - ` 为 `-`
      ", " = "-",               # 统一 `, ` 为 `-`
      "/Quebec" = "",           # 处理 `/Quebec`
      "/Ontario" = "",
      "/Saskatchewan" = "",
      "/British Columbia" = "",
      "/Manitoba" = "",
      "/Alberta" = "",
      " part" = ""              # 删除 `part`
    ))) %>%
    mutate(GEO = case_when(
      GEO == "Saint Catharines - Niagara" ~ "Saint Catharines-Niagara",
      GEO == "Saint John, Fredericton, and Moncton" ~ "Saint John",
      GEO == "Ottawa-Gatineau part" ~ "Ottawa-Gatineau",
      GEO == "Ottawa-Gatineau, Ontario part" ~ "Ottawa-Gatineau",
      GEO == "Ottawa-Gatineau, Quebec part" ~ "Ottawa-Gatineau",
      GEO == "Ottawa-Gatineau, Ontario/Quebec" ~ "Ottawa-Gatineau",
      GEO == "Belleville-Quinte West" ~ "Belleville",
      GEO == "Fort Saint John" ~ "Fort St. John",
      GEO == "Saint-Hyacinthe" ~ "Saint Hyacinthe",
      GEO == "Saint-Jean-sur-Richelieu" ~ "Saint Jean sur Richelieu",
      GEO == "Campbellton part" ~ "Campbellton",
      GEO == "Wood Buffalo" ~ "Fort McMurray",
      TRUE ~ GEO  # 其他情况保持不变
    ))
}

# 应用于所有数据集
HPI <- standardize_geo_v2(HPI)
NHS <- standardize_geo_v2(NHS)
pd <- standardize_geo_v2(pd)

# 确保标准化后 `GEO` 名称一致
print(unique(HPI$GEO))
print(unique(NHS$GEO))
print(unique(pd$GEO))


####

# 进一步优化城市名称
standardize_geo_v3 <- function(df) {
  df %>%
    mutate(GEO = str_replace_all(GEO, c(
      " - " = "-",              # 统一 ` - ` 为 `-`
      ", " = "-",               # 统一 `, ` 为 `-`
      "/Quebec" = "",           # 处理 `/Quebec`
      "/Ontario" = "",
      "/Saskatchewan" = "",
      "/British Columbia" = "",
      "/Manitoba" = "",
      "/Alberta" = "",
      " part" = "",             # 删除 `part`
      "Whitehorse-Yukon" = "Whitehorse",
      "Yellowknife-Northwest Territories" = "Yellowknife",
      "Saint John-Fredericton-and Moncton" = "Saint John",
      "Saint Catharines - Niagara" = "Saint Catharines-Niagara",
      "Ottawa-Gatineau part" = "Ottawa-Gatineau",
      "Ottawa-Gatineau, Ontario part" = "Ottawa-Gatineau",
      "Ottawa-Gatineau, Quebec part" = "Ottawa-Gatineau",
      "Belleville-Quinte West" = "Belleville",
      "Fort Saint John" = "Fort St. John",
      "Saint-Hyacinthe" = "Saint Hyacinthe",
      "Saint-Jean-sur-Richelieu" = "Saint Jean sur Richelieu",
      "Campbellton part" = "Campbellton",
      "Wood Buffalo" = "Fort McMurray"
    )))
}

# 应用于所有数据集
HPI <- standardize_geo_v3(HPI)
NHS <- standardize_geo_v3(NHS)
pd <- standardize_geo_v3(pd)

# 确保标准化后 `GEO` 名称一致
print(unique(HPI$GEO))
print(unique(NHS$GEO))
print(unique(pd$GEO))



####
# 创建标准化名称映射
standard_names <- c(
  "Saint Catharines-Niagara" = "St. Catharines-Niagara",
  "Montréal" = "Montreal",
  "Trois-Rivières" = "Trois Rivières",
  "Québec" = "Quebec",
  "Ottawa-Gatineau, Ontario/Quebec" = "Ottawa-Gatineau",
  "Greater Sudbury" = "Sudbury",
  "Saint John's" = "St. John's",
  "Kitchener-Cambridge-Waterloo" = "Kitchener-Waterloo",
  "Red Deer, Alberta" = "Red Deer",
  "Trois Rivières" = "Trois-Rivières",
  "Québec" = "Quebec",
  "Sudbury" = "Greater Sudbury",
  "Saint Catharines-Niagara" = "St. Catharines-Niagara",
  "Saint John's" = "St. John's",
  "Ottawa-Gatineau, Ontario/Quebec" = "Ottawa-Gatineau",
  "Saint Hyacinthe" = "Saint-Hyacinthe",
  "Saint Jean sur Richelieu" = "Saint-Jean-sur-Richelieu"

)



datasets <- list(HPI, NHS, pd)

for (i in seq_along(datasets)) {
  datasets[[i]]$GEO <- ifelse(datasets[[i]]$GEO %in% names(standard_names),
                              standard_names[datasets[[i]]$GEO],  # 替换成标准名称
                              datasets[[i]]$GEO)  # 否则保留原值
}

# 重新赋值
HPI <- datasets[[1]]
NHS <- datasets[[2]]
pd <- datasets[[3]]

# 打印检查
print(unique(HPI$GEO))
print(unique(NHS$GEO))
print(unique(pd$GEO))

#######


library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)  # 用于日期处理

# 假设 pd 是原始年度人口数据
monthly_population <- pd %>%
  # 生成每个 GEO（Canada + 各城市）对应的12个月数据
  tidyr::expand(GEO, REF_DATE, Month = 1:12) %>%
  # 连接年度人口数据
  left_join(pd, by = c("GEO", "REF_DATE")) %>%
  # 生成真正的月度时间格式 YYYY-MM
  mutate(Date = as.Date(paste(REF_DATE, Month, "01", sep = "-"))) %>%
  group_by(GEO) %>%
  mutate(
    # 1️⃣ 线性插值版本（平滑人口数据）
    Interpolated_Population = zoo::na.approx(populaton, rule = 2, na.rm = FALSE),
    # 2️⃣ 直接复制年度人口版本（每月使用相同的人口值）
    Fixed_Population = populaton
  ) %>%
  fill(Fixed_Population, .direction = "down") %>%  # 用年度值填充缺失月份
  ungroup()

# 选择需要的列
monthly_population <- monthly_population %>%
  select(Date, GEO, Interpolated_Population, Fixed_Population)

# 显示最终的数据
print(monthly_population)

monthly_population <- monthly_population %>% 
  rename(REF_DATE = Date)



#########

library(dplyr)
library(lubridate)

# 统一日期格式为 YYYY-MM
HPI$REF_DATE <- substr(as.character(HPI$REF_DATE), 1, 7)
NHS$REF_DATE <- substr(as.character(NHS$REF_DATE), 1, 7)
PR_m$REF_DATE <- substr(as.character(PR_m$REF_DATE), 1, 7)

# monthly_population 是 YYYY-MM-DD，需要转换为 YYYY-MM
monthly_population <- monthly_population %>%
  mutate(REF_DATE = format(REF_DATE, "%Y-%m"))

# 确保每个数据集的 GEO 处理一致
HPI <- HPI %>% distinct(REF_DATE, GEO, .keep_all = TRUE)
NHS <- NHS %>% distinct(REF_DATE, GEO, .keep_all = TRUE)
monthly_population <- monthly_population %>% distinct(REF_DATE, GEO, .keep_all = TRUE)
PR_m <- PR_m %>% distinct(REF_DATE, .keep_all = TRUE)  # PR_m 可能没有 GEO

# 执行合并操作，确保 GEO 列匹配
merged <- left_join(NHS, monthly_population, by = c("REF_DATE", "GEO"))

merged_semi <- left_join(merged, HPI, by = c("REF_DATE", "GEO"))

merged_semi_down <- left_join(merged_semi, PR_m, by = "REF_DATE")

# 显示最终合并的数据
print(merged_semi_down)

################################
write.csv(merged_semi_down, "presentation_data.csv")

##############################
#futher data cleaning






# 只保留城市，删除省份和区域
merged_semi_down <- merged_semi_down %>%
  filter(!GEO %in% c("Atlantic Region", "Newfoundland and Labrador", 
                     "Prince Edward Island", "Nova Scotia", "New Brunswick", 
                     "Quebec", "Ontario", "Prairie Region", "Manitoba", 
                     "Saskatchewan", "Alberta", "British Columbia")) 

# 查看筛选后的唯一 GEO 值
print(unique(merged_semi_down$GEO))





































