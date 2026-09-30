library(fixest)
library(modelsummary)
library(data.table)
library(readxl)
library(car)

# 1. Read data
df <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx")

# 2. Data preprocessing
df$country_province <- paste(df$Country, df$Province, sep = "_")
df$id <- as.numeric(as.factor(df$country_province))
df$Year <- as.numeric(df$Year)
df <- df[order(df$id, df$Year), ]

# 3. Variable selection
all_vars <- c("dropout_rate", "if_flood", "if_flood_1", "if_flood_2", "if_flood_3",
              "flood_event_count", "hur_avg", "pr_total", "tas_avg",
              "avg_age", "female_ratio", "avg_Numberofhouseholdmembers",
              "urban_population_share", "electrification_rate", "avg_Combinedwealthscore")

# 4. Handle missing values
df_complete <- na.omit(df[, c(all_vars, "id", "Year")])


# 5. Linear Fixed Effects Models 
cat("\n========== Estimating Linear FE Models ==========\n")

# M1: Baseline (Climate controls only)
m1_linear <- feols(
  dropout_rate ~ if_flood + hur_avg + pr_total + tas_avg | id + Year,
  data = df_complete, cluster = ~id
)

# M2: + Household Demographics (Age, Female ratio, Household size)
m2_linear <- feols(
  dropout_rate ~ if_flood + hur_avg + pr_total + tas_avg +
    avg_age + female_ratio + avg_Numberofhouseholdmembers | id + Year,
  data = df_complete, cluster = ~id
)

# M3: Full Controls (+ Socioeconomic: Urban, Electricity, Wealth)
m3_linear <- feols(
  dropout_rate ~ if_flood + hur_avg + pr_total + tas_avg +
    avg_age + female_ratio + avg_Numberofhouseholdmembers +
    urban_population_share + electrification_rate + avg_Combinedwealthscore | id + Year,
  data = df_complete, cluster = ~id
)

# VIF Test
m3_ols <- lm(
  dropout_rate ~ if_flood + hur_avg + pr_total + tas_avg +
    avg_age + female_ratio + avg_Numberofhouseholdmembers +
    urban_population_share + electrification_rate + avg_Combinedwealthscore +
    factor(id) + factor(Year),
  data = df_complete
)

vif_results <- vif(m3_ols)
print(vif_results)

# M4: Lagged Flood Exposure (Full Controls)
m4_linear <- feols(
  dropout_rate ~ if_flood + if_flood_1 + if_flood_2 + if_flood_3 +
    hur_avg + pr_total + tas_avg +
    avg_age + female_ratio + avg_Numberofhouseholdmembers +
    urban_population_share + electrification_rate + avg_Combinedwealthscore | id + Year,
  data = df_complete, cluster = ~id
)

# M5: Flood Frequency / Event Count (Full Controls)
m5_linear <- feols(
  dropout_rate ~ flood_event_count + hur_avg + pr_total + tas_avg +
    avg_age + female_ratio + avg_Numberofhouseholdmembers +
    urban_population_share + electrification_rate + avg_Combinedwealthscore | id + Year,
  data = df_complete, cluster = ~id
)


# 6. Output Export Configuration

my_coef_map <- c(
  "if_flood"                      = "Flood exposure (if flood)",
  "if_flood_1"                    = "Flood exposure (lag 1)",
  "if_flood_2"                    = "Flood exposure (lag 2)",
  "if_flood_3"                    = "Flood exposure (lag 3)",
  "flood_event_count"             = "Flood Counts",
  "hur_avg"                       = "Relative humidity",
  "pr_total"                        = "Precipitation",
  "tas_avg"                       = "Temperature",
  "avg_age"                       = "Average age",
  "female_ratio"                  = "Female ratio",
  "avg_Numberofhouseholdmembers"  = "Household size",
  "urban_population_share"        = "Urban",
  "electrification_rate"          = "Electricity",
  "avg_Combinedwealthscore"       = "Wealth"
)

my_stars <- c("*" = .05, "**" = .01, "***" = .001)
my_notes <- c(
  "All models include province and year fixed effects.",
  "Clustered robust standard errors in parentheses.",
  "95% confidence intervals in brackets."
)

linear_list <- list(
  "M1: Climate"         = m1_linear, 
  "M2: +Demographics"   = m2_linear, 
  "M3: Full Controls"   = m3_linear,
  "M4: Lagged Flood"    = m4_linear, 
  "M5: Flood Frequency" = m5_linear
)

# Ensure output directory exists
if (!dir.exists("D:/fe_R/newcode/results")) {
  dir.create("D:/fe_R/newcode/results", recursive = TRUE)
}

# Export Results (HTML)
modelsummary(
  linear_list, 
  stars = my_stars, 
  statistic = c("std.error"), 
  conf_level = 0.95,
  gof_omit = "Adj|AIC|BIC|Log.Lik|RMSE", 
  coef_map = my_coef_map, 
  notes = my_notes,
  title = "Linear Fixed Effects Models: Flood Exposure and Dropout Rate",
  output = "D:/fe_R/newcode/results/linear_model_results.html"
)

# Export Results (Excel)
modelsummary(
  linear_list, 
  fmt = 4,
  stars = my_stars, 
  statistic = c("({std.error})", "[{conf.low}, {conf.high}]"),
  conf_level = 0.95,
  gof_omit = "Adj|AIC|BIC|Log.Lik|RMSE", 
  coef_map = my_coef_map, 
  notes = my_notes,
  title = "Linear Fixed Effects Models: Flood Exposure and Dropout Rate",
  output = "D:/fe_R/newcode/results/linear_model_results.xlsx"
)

cat("\nDone! Results saved to D:/fe_R/newcode/results/\n")