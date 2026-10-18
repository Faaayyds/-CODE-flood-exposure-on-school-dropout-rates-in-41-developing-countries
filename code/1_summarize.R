library(data.table)
library(readxl)
library(modelsummary)
library(openxlsx)
library(dplyr)

df <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx")
df <- as.data.table(df)

df[, Province := as.factor(Province)]
df[, Year := as.numeric(Year)]
setorder(df, Province, Year)

# 1. Dataset metadata and panel dimensions
n_obs  <- nrow(df)
n_prov <- length(unique(df$Province))
n_year <- length(unique(df$Year))

panel_info <- data.frame(
  Indicator = c("Observations", "Provinces", "Years"),
  Value = c(n_obs, n_prov, n_year)
)

# 2. Variable mapping and description definitions
coef_map_updated <- c(
  "dropout_rate"                   = "Dropout rate",
  "if_flood"                       = "Flood exposure (if flood)",
  "total_flood_months"             = "Flood exposure (flood months)",
  "flood_event_count"              = "Flood exposure (flood counts)",
  "hur_avg"                        = "Relative humidity", 
  "pr_total"                         = "Precipitation", 
  "tas_avg"                        = "Temperature",
  "avg_age"                        = "Average age", 
  "female_ratio"                   = "Female ratio",
  "avg_Numberofhouseholdmembers"   = "Household size", 
  "urban_population_share"         = "Urban",
  "electrification_rate"           = "Electricity", 
  "avg_Combinedwealthscore"        = "Wealth"
)

var_descriptions <- c(
  "dropout_rate"                   = "Ratio of School Dropout",
  "if_flood"                       = "Indicator for flood occurrence (1=Yes, 0=No)",
  "total_flood_months"             = "Total duration of flood in months",
  "flood_event_count"              = "Number of distinct flood events",
  "hur_avg"                        = "Average relative humidity (%)",
  "pr_total"                         = "Average precipitation (mm)",
  "tas_avg"                        = "Average temperature (°C)",
  "avg_age"                        = "Average age of cohort",
  "female_ratio"                   = "Ratio of female household members",
  "avg_Numberofhouseholdmembers"   = "Mean household size",
  "urban_population_share"         = "Proportion of urban population",
  "electrification_rate"           = "Proportion of households with electricity",
  "avg_Combinedwealthscore"        = "Aggregated wealth index score"
)

# 3. Descriptive statistics calculation logic
calc_stats <- function(var) {
  x <- df[[var]]
  mean_val <- mean(x, na.rm = TRUE)
  sd_val <- sd(x, na.rm = TRUE)
  
  display_name <- coef_map_updated[var]
  desc_text <- var_descriptions[var]
  
  if(var == "if_flood") {
    pct1 <- mean_val * 100
    summary_str <- sprintf("1 = %.1f%%\n0 = %.1f%%", pct1, 100 - pct1)
  } else {
    summary_str <- sprintf("%.3f (%.3f)", mean_val, sd_val)
    display_name <- paste0(display_name, ", Mean (SD)")
  }
  
  return(data.frame(
    Variable = display_name, 
    `Original Variable` = var,
    `Summary statistic` = summary_str, 
    Description = desc_text,
    stringsAsFactors = FALSE, 
    check.names = FALSE
  ))
}

vars_processed <- names(coef_map_updated)[names(coef_map_updated) %in% names(df)]
desc_df <- do.call(rbind, lapply(vars_processed, calc_stats))

cat_row <- function(name) {
  data.frame(Variable = name, `Original Variable` = "", `Summary statistic` = "", 
             Description = "", check.names = FALSE, stringsAsFactors = FALSE)
}

# 4. Final table assembly with categorical headers
desc_final <- rbind(
  cat_row("Current study status"),
  desc_df[desc_df$`Original Variable` == "dropout_rate", ],
  cat_row("Flood Exposure Variables"),
  desc_df[desc_df$`Original Variable` %in% c("if_flood", "total_flood_months", "flood_event_count"), ],
  cat_row("Climate Variables"),
  desc_df[desc_df$`Original Variable` %in% c("hur_avg", "pr_total", "tas_avg"), ],
  cat_row("Family Demographic variables"),
  desc_df[desc_df$`Original Variable` %in% c("avg_age", "female_ratio", "avg_Numberofhouseholdmembers"), ],
  cat_row("Socioeconomic variables"),
  desc_df[desc_df$`Original Variable` %in% c("urban_population_share", "electrification_rate", "avg_Combinedwealthscore"), ]
)

core_vars <- c("dropout_rate", "if_flood", "total_flood_months", "flood_event_count", "urban_population_share", "avg_Combinedwealthscore")
cor_matrix <- round(cor(df[, ..core_vars], use = "complete.obs"), 3)

group_table <- df %>%
  filter(!is.na(if_flood)) %>%
  group_by(if_flood) %>%
  summarise(Mean_dropout = mean(dropout_rate, na.rm = TRUE),
            SD_dropout = sd(dropout_rate, na.rm = TRUE), N = n())

# 5. Excel export with dynamic bold formatting for headers
wb <- createWorkbook()
addWorksheet(wb, "Table_Descriptive")
writeData(wb, "Table_Descriptive", desc_final)

header_indices <- which(desc_final$`Original Variable` == "" & desc_final$Variable != "") + 1
bold_style <- createStyle(textDecoration = "bold")

for(r in header_indices) {
  addStyle(wb, "Table_Descriptive", style = bold_style, rows = r, cols = 1:4, gridExpand = TRUE)
}

addWorksheet(wb, "Panel_Info")
writeData(wb, "Panel_Info", panel_info)
addWorksheet(wb, "Group_Comparison")
writeData(wb, "Group_Comparison", group_table)
addWorksheet(wb, "Correlation")
writeData(wb, "Correlation", cor_matrix, rowNames = TRUE)

save_path <- "D:/fe_R/newcode/results/1_summarize_code/Table_Descriptive.xlsx"
saveWorkbook(wb, save_path, overwrite = TRUE)