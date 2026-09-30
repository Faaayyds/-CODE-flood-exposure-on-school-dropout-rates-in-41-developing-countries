library(fixest)
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

# 1. Load Historical Panel Data
data <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx") 
data$country_province <- paste(data$Country, data$Province, sep = "_")
data$id <- as.numeric(as.factor(data$country_province))
data$Year <- as.numeric(data$Year)
data <- data[order(data$id, data$Year), ]

# 2. Model Specification and Fixed Effects Estimation
controls <- paste0(
  "hur_avg + pr_total + tas_avg + avg_age + female_ratio + ",
  "avg_Numberofhouseholdmembers + urban_population_share + electrification_rate + avg_Combinedwealthscore"
)

model <- feols(
  as.formula(paste("dropout_rate ~ flood_event_count +", controls, "| id + Year")),
  data = data,
  vcov = ~id
)

summary(model)

# 3. Parameter Extraction & Counterfactual Baseline Derivation
beta_est   <- coef(model)["flood_event_count"]
beta_se    <- sqrt(diag(vcov(model)))["flood_event_count"]
beta_lower <- beta_est - 1.96 * beta_se
beta_upper <- beta_est + 1.96 * beta_se

mean_out   <- mean(data$dropout_rate, na.rm = TRUE)
mean_flood <- mean(data$flood_event_count, na.rm = TRUE)

baseline_est   <- pmax(mean_out - beta_est * mean_flood, 0)
baseline_lower <- pmax(mean_out - beta_upper * mean_flood, 0) 
baseline_upper <- pmax(mean_out - beta_lower * mean_flood, 0) 

data <- data %>% mutate(
  Pred_dropout_est   = pmin(pmax(baseline_est + beta_est * flood_event_count, 0), 1),
  Pred_dropout_lower = pmin(pmax(baseline_lower + beta_lower * flood_event_count, 0), 1),
  Pred_dropout_upper = pmin(pmax(baseline_upper + beta_upper * flood_event_count, 0), 1),
  
  RR_est   = ifelse(baseline_est > 0, Pred_dropout_est / baseline_est, 1),
  RR_lower = ifelse(baseline_lower > 0, Pred_dropout_lower / baseline_lower, 1),
  RR_upper = ifelse(baseline_upper > 0, Pred_dropout_upper / baseline_upper, 1),
  
  PAF_est   = ifelse(Pred_dropout_est > 0, (Pred_dropout_est - baseline_est) / Pred_dropout_est, 0),
  PAF_lower = ifelse(Pred_dropout_lower > 0, (Pred_dropout_lower - baseline_lower) / Pred_dropout_lower, 0),
  PAF_upper = ifelse(Pred_dropout_upper > 0, (Pred_dropout_upper - baseline_upper) / Pred_dropout_upper, 0),
  
  PAF_est   = pmin(pmax(PAF_est, 0), 1),
  PAF_lower = pmin(pmax(PAF_lower, 0), 1),
  PAF_upper = pmin(pmax(PAF_upper, 0), 1),
  
  N_mics_dropout = dropout_school_count,
  
  N_mics_flood_est   = N_mics_dropout * PAF_est,
  N_mics_flood_lower = N_mics_dropout * PAF_lower,
  N_mics_flood_upper = N_mics_dropout * PAF_upper
)

# 4. Sub-national Historical Aggregation (Formulas 10-12)
result_provyear <- data %>%
  group_by(Country, Province, Year) %>%
  summarise(
    P_5_17 = sum(all_05_17, na.rm=T),
    N_mics_dropout = sum(N_mics_dropout, na.rm=T),
    N_mics_flood_est = sum(N_mics_flood_est, na.rm=T),
    PAF_est = mean(PAF_est, na.rm=T),
    .groups = "drop"
  )

result_prov <- data %>%
  group_by(Country, Province) %>%
  summarise(
    k_years = n_distinct(Year),
    sum_flood_count = sum(flood_event_count),
    sum_P_5_17 = sum(all_05_17, na.rm = TRUE),
    mean_P_5_17_rt = sum_P_5_17 / k_years,
    
    sum_dropout_count  = sum(dropout_school_count, na.rm = TRUE),
    sum_attended_count = sum(children_ever_attended_num, na.rm = TRUE), 
    
    sum_N_mics_flood_est   = sum(N_mics_flood_est, na.rm = TRUE),
    sum_N_mics_flood_lower = sum(N_mics_flood_lower, na.rm = TRUE),
    sum_N_mics_flood_upper = sum(N_mics_flood_upper, na.rm = TRUE),
    sum_N_mics_dropout     = sum(N_mics_dropout, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prov_mean_dropout_rate = ifelse(
      sum_attended_count > 0,
      sum_dropout_count / sum_attended_count,
      NA_real_
    ),
    
    PAF_r_est   = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_est / sum_N_mics_dropout, 0),
    PAF_r_lower = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_lower / sum_N_mics_dropout, 0),
    PAF_r_upper = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_upper / sum_N_mics_dropout, 0),
    
    AN_r_est   = round(PAF_r_est * sum_P_5_17, 3),
    AN_r_lower = round(PAF_r_lower * sum_P_5_17, 3),
    AN_r_upper = round(PAF_r_upper * sum_P_5_17, 3)
  )

# 5. National Historical Baseline Calculation (Formula 13)
result_country_hist <- result_prov %>%
  group_by(Country) %>%
  summarise(
    P_c_historical = sum(mean_P_5_17_rt, na.rm = TRUE),
    .groups = "drop"
  )

global_paf_summary <- data %>%
  summarise(
    total_countries     = n_distinct(Country),
    total_provinces     = n_distinct(Province),
    sum_N_mics_dropout  = sum(N_mics_dropout, na.rm = TRUE),
    sum_N_mics_flood_est   = sum(N_mics_flood_est, na.rm = TRUE),
    sum_N_mics_flood_lower = sum(N_mics_flood_lower, na.rm = TRUE),
    sum_N_mics_flood_upper = sum(N_mics_flood_upper, na.rm = TRUE),
    total_hist_P_517     = sum(all_05_17, na.rm = TRUE)
  ) %>%
  mutate(
    PAF_global_est   = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_est / sum_N_mics_dropout, 0),
    PAF_global_lower = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_lower / sum_N_mics_dropout, 0),
    PAF_global_upper = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_upper / sum_N_mics_dropout, 0),
    RR_global_est   = ifelse(PAF_global_est < 1, 1 / (1 - PAF_global_est), 1),
    RR_global_lower = ifelse(PAF_global_lower < 1, 1 / (1 - PAF_global_lower), 1),
    RR_global_upper = ifelse(PAF_global_upper < 1, 1 / (1 - PAF_global_upper), 1)
  )

# 6. Data Export
dir.create("D:/fe_R/newcode/results/", showWarnings = FALSE, recursive = TRUE)

write_xlsx(result_provyear,     "D:/fe_R/newcode/results/Attri_Province_Year.xlsx")
write_xlsx(result_prov,         "D:/fe_R/newcode/results/Attri_Province_Formula11_12.xlsx")
write_xlsx(global_paf_summary,  "D:/fe_R/newcode/results/Global_PAF_Summary.xlsx")