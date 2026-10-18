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

# 4. Sub-national Historical Aggregation (Formulas 10-12, 15, 16)
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
    
    mean_P_5_17_r = sum_P_5_17 / k_years,
    Pr_r_historical = mean(pr_total, na.rm = TRUE),
    
    mean_Base_dropout_est   = mean(baseline_est, na.rm = TRUE),
    mean_Base_dropout_lower = mean(baseline_lower, na.rm = TRUE),
    mean_Base_dropout_upper = mean(baseline_upper, na.rm = TRUE),
    
    sum_dropout_count  = sum(dropout_school_count, na.rm = TRUE),
    sum_attended_count = sum(children_ever_attended_num, na.rm = TRUE), 
    
    sum_N_mics_flood_est   = sum(N_mics_flood_est, na.rm = TRUE),
    sum_N_mics_flood_lower = sum(N_mics_flood_lower, na.rm = TRUE),
    sum_N_mics_flood_upper = sum(N_mics_flood_upper, na.rm = TRUE),
    sum_N_mics_dropout     = sum(N_mics_dropout, na.rm = TRUE),
    
    sum_base_dropout_pop_est   = sum(all_05_17 * baseline_est, na.rm = TRUE),
    sum_base_dropout_pop_lower = sum(all_05_17 * baseline_lower, na.rm = TRUE),
    sum_base_dropout_pop_upper = sum(all_05_17 * baseline_upper, na.rm = TRUE),
    
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
    
    AN_r_est   = round(PAF_r_est * sum_base_dropout_pop_est, 3),
    AN_r_lower = round(PAF_r_lower * sum_base_dropout_pop_lower, 3),
    AN_r_upper = round(PAF_r_upper * sum_base_dropout_pop_upper, 3)
  )

# 5. National & Global Historical Baseline Summary (Formula 13)
result_country_hist <- data %>%
  group_by(Country) %>%
  summarise(
    Kt = n_distinct(Year),
    P_c_historical = sum(all_05_17, na.rm = TRUE) / Kt,
    .groups = "drop"
  )

global_paf_summary <- data %>%
  summarise(
    total_countries         = n_distinct(Country),
    total_provinces         = n_distinct(Province),
    sum_N_mics_dropout      = sum(N_mics_dropout, na.rm = TRUE),
    sum_N_mics_flood_est    = sum(N_mics_flood_est, na.rm = TRUE),
    sum_N_mics_flood_lower  = sum(N_mics_flood_lower, na.rm = TRUE),
    sum_N_mics_flood_upper  = sum(N_mics_flood_upper, na.rm = TRUE),
    total_hist_P_517        = sum(all_05_17, na.rm = TRUE)
  ) %>%
  mutate(
    PAF_global_est   = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_est / sum_N_mics_dropout, 0),
    PAF_global_lower = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_lower / sum_N_mics_dropout, 0),
    PAF_global_upper = ifelse(sum_N_mics_dropout > 0, sum_N_mics_flood_upper / sum_N_mics_dropout, 0),
    RR_global_est    = ifelse(PAF_global_est < 1, 1 / (1 - PAF_global_est), 1),
    RR_global_lower  = ifelse(PAF_global_lower < 1, 1 / (1 - PAF_global_lower), 1),
    RR_global_upper  = ifelse(PAF_global_upper < 1, 1 / (1 - PAF_global_upper), 1)
  )

# 6. CMIP6 Climate Baseline & Sub-national SSP Scenario Projections
cmip6_future <- read_excel("D:/fe_R/newcode/CMIP6_merged_nex_gddp.xlsx") %>%
  filter(scenario %in% c("ssp126", "ssp245", "ssp585")) %>%
  rename(Year = year, Pr_r_T_SSP = pr_total)

ssp_pop_data <- read_excel("D:/fe_R/newcode/SSP_44countries_5_17_pivot.xlsx") %>%
  mutate(across(starts_with("SSP"), ~ .x * 1000))

ssp_prov_pred <- cmip6_future %>%
  left_join(ssp_pop_data, by = c("Country", "Year")) %>%
  left_join(result_country_hist, by = "Country") %>%
  left_join(result_prov, by = c("Country", "Province")) %>%
  filter(!is.na(P_c_historical) & P_c_historical > 0) %>%
  mutate(
    P_517_SSP = case_when(
      scenario == "ssp126" ~ mean_P_5_17_r * (SSP1 / P_c_historical),
      scenario == "ssp245" ~ mean_P_5_17_r * (SSP2 / P_c_historical),
      scenario == "ssp585" ~ mean_P_5_17_r * (SSP5 / P_c_historical),
      TRUE ~ mean_P_5_17_r
    ),
    
    PAF_SSP_est   = round(pmin(1.0, PAF_r_est   * (Pr_r_T_SSP / Pr_r_historical)), 5),
    PAF_SSP_lower = round(pmin(1.0, PAF_r_lower * (Pr_r_T_SSP / Pr_r_historical)), 5),
    PAF_SSP_upper = round(pmin(1.0, PAF_r_upper * (Pr_r_T_SSP / Pr_r_historical)), 5),
    
    AN_SSP_est   = round(P_517_SSP * mean_Base_dropout_est   * PAF_SSP_est, 3),
    AN_SSP_lower = round(P_517_SSP * mean_Base_dropout_lower * PAF_SSP_lower, 3),
    AN_SSP_upper = round(P_517_SSP * mean_Base_dropout_upper * PAF_SSP_upper, 3)
  )

# 7. Reshape and Export Clean Structured Tables
format_scenario_table <- function(df_input, target_year, target_scenario) {
  df_input %>%
    filter(Year == target_year, scenario == target_scenario) %>%
    select(
      Country,
      Province,
      P_517_SSP,
      PAF_SSP_est,
      PAF_SSP_lower,
      PAF_SSP_upper,
      AN_SSP_est,
      AN_SSP_lower,
      AN_SSP_upper
    ) %>%
    rename(
      P_5_17_SSP = P_517_SSP,
      PAF_est = PAF_SSP_est,
      PAF_lower = PAF_SSP_lower,
      PAF_upper = PAF_SSP_upper,
      AN_est = AN_SSP_est,
      AN_lower = AN_SSP_lower,
      AN_upper = AN_SSP_upper
    ) %>%
    arrange(Country, Province)
}

ssp126_2030 <- format_scenario_table(ssp_prov_pred, 2030, "ssp126")
ssp245_2030 <- format_scenario_table(ssp_prov_pred, 2030, "ssp245")
ssp585_2030 <- format_scenario_table(ssp_prov_pred, 2030, "ssp585")

ssp126_2050 <- format_scenario_table(ssp_prov_pred, 2050, "ssp126")
ssp245_2050 <- format_scenario_table(ssp_prov_pred, 2050, "ssp245")
ssp585_2050 <- format_scenario_table(ssp_prov_pred, 2050, "ssp585")

dir.create("D:/fe_R/newcode/results/", showWarnings = FALSE, recursive = TRUE)

write_xlsx(
  list(
    "SSP126_2030" = ssp126_2030,
    "SSP245_2030" = ssp245_2030,
    "SSP585_2030" = ssp585_2030
  ),
  path = "D:/fe_R/newcode/results/SSP_Projections_2030.xlsx"
)

write_xlsx(
  list(
    "SSP126_2050" = ssp126_2050,
    "SSP245_2050" = ssp245_2050,
    "SSP585_2050" = ssp585_2050
  ),
  path = "D:/fe_R/newcode/results/SSP_Projections_2050.xlsx"
)