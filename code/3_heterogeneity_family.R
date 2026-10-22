library(fixest)
library(modelsummary)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(readxl)
library(openxlsx)
library(marginaleffects) # Must load this package to extract subgroup marginal effects

# 1. Load and preprocess data
df <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx")
df$country_province <- paste(df$Country, df$Province, sep = "_")
df$id <- as.numeric(as.factor(df$country_province))
df$Year <- as.numeric(df$Year)
df <- df[order(df$id, df$Year), ]

output_dir <- "D:/fe_R/newcode/results/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Control variables and subgroup configurations
all_controls <- c(
  "hur_avg", "pr_total", "tas_avg", "avg_age", "female_ratio", 
  "avg_Numberofhouseholdmembers", "urban_population_share", "electrification_rate", 
  "avg_Combinedwealthscore"
)

group_configs <- list(
  list(var = "avg_age", name_low = "Low average age", name_high = "High average age", category = "average age"),
  list(var = "female_ratio", name_low = "Low female ratio", name_high = "High female ratio", category = "female ratio"),
  list(var = "avg_Numberofhouseholdmembers", name_low = "Small household", name_high = "Large household", category = "household size"),
  list(var = "urban_population_share", name_low = "Low urban", name_high = "High urban", category = "urban"),
  list(var = "electrification_rate", name_low = "Low electricity", name_high = "High electricity", category = "electricity"),
  list(var = "avg_Combinedwealthscore", name_low = "Low wealth", name_high = "High wealth", category = "wealth")
)

# 3. Full-sample interaction model function and marginal effects extraction
all_models <- list()
results_list <- list()

for(config in group_configs) {
  median_val <- median(df[[config$var]], na.rm = TRUE)
  group_var_name <- paste0("grp_", config$var)
  
  # Construct binary group indicator based on median split
  df_temp <- df %>%
    mutate(!!group_var_name := factor(ifelse(.data[[config$var]] > median_val, "High", "Low"), levels = c("Low", "High")))
  
  # Remove the continuous grouping variable from controls to prevent collinearity with dummy
  current_controls <- setdiff(all_controls, config$var)
  
  # Construct full-sample interaction formula
  fml_str <- paste0(
    "dropout_rate ~ if_flood * ", group_var_name, " + ",
    paste(current_controls, collapse = " + "), " | id + Year"
  )
  fml <- as.formula(fml_str)
  
  # Estimate interaction model
  model_inter <- feols(fml, data = df_temp, cluster = ~id)
  
  # Extract estimated marginal effects (Slopes) for Low and High groups using marginaleffects
  slopes_df <- as.data.frame(avg_slopes(model_inter, variables = "if_flood", by = group_var_name))
  
  # Perform overall Wald test for interaction term
  inter_term <- paste0("if_flood:", group_var_name, "High")
  wald_test <- wald(model_inter, keep = inter_term)
  p_wald_val <- if (is.list(wald_test) && "p" %in% names(wald_test)) wald_test$p else as.numeric(wald_test["p"])
  
  # Construct standardized output table
  res_sub <- slopes_df %>%
    mutate(
      Category          = config$category,
      GroupType         = as.character(.data[[group_var_name]]),
      Group             = ifelse(GroupType == "Low", config$name_low, config$name_high),
      Estimate          = estimate,
      Std_Error         = std.error,
      CI_Lower          = conf.low,
      CI_Upper          = conf.high,
      P_Value           = p.value,
      Significance      = case_when(
        p.value < 0.001 ~ "***",
        p.value < 0.01  ~ "**",
        p.value < 0.05  ~ "*",
        TRUE ~ ""
      ),
      `Effect Size`     = paste0(sprintf("%.4f", estimate), ifelse(Significance != "", paste0(" ", Significance), "")),
      `Std Error`       = sprintf("%.4f", std.error),
      `95% CI`          = sprintf("[%.4f, %.4f]", conf.low, conf.high),
      `p-value`         = ifelse(p.value < 0.001, "<0.001", sprintf("%.4f", p.value)),
      `Wald Test (p-val)` = ifelse(p_wald_val < 0.001, "<0.001", sprintf("%.4f", p_wald_val)),
      P_Wald            = p_wald_val
    ) %>%
    select(Category, Group, GroupType, `Effect Size`, `Std Error`, `95% CI`, `p-value`, 
           `Wald Test (p-val)`, Estimate, Std_Error, CI_Lower, CI_Upper, P_Value, Significance, P_Wald)
  
  all_models[[paste0("Subgroup: ", config$category)]] <- model_inter
  results_list[[length(results_list) + 1]] <- res_sub
}

results_df <- bind_rows(results_list)

# 4. Forest plot plotting function
create_nature_forest_plot <- function(config, results_df) {
  subgroup_data <- results_df %>% 
    filter(Category == config$category) %>%
    mutate(point_color = ifelse(Significance != "", "#C72423", "black"))
  
  p_wald_val <- unique(subgroup_data$P_Wald)
  p_wald_label <- ifelse(p_wald_val < 0.001, "Wald test: p < 0.001", 
                         sprintf("Wald test: p = %.3f", p_wald_val))
  
  x_min <- min(subgroup_data$CI_Lower, na.rm = TRUE)
  x_max <- max(subgroup_data$CI_Upper, na.rm = TRUE)
  x_range <- ifelse(x_max == x_min, 0.1, x_max - x_min)
  x_limits <- c(x_min - 0.15 * x_range, x_max + 0.40 * x_range)
  
  to_sentence_case <- function(x) {
    paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  }
  plot_title_text <- to_sentence_case(config$category)
  
  p <- ggplot(subgroup_data, aes(x = Estimate, y = factor(GroupType, levels = c("Low", "High")))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.15, linewidth = 0.8, color = "black") +
    geom_point(aes(color = point_color), size = 5.5, shape = 19) +
    scale_color_identity() +
    geom_text(aes(x = CI_Upper + 0.03 * x_range, label = Significance), 
              size = 6, face = "bold", color = "#C72423", hjust = 0) +
    annotate("text", x = x_limits[2], y = 2.4, label = p_wald_label, 
             hjust = 1, size = 6, face = "italic", color = "gray20") +
    scale_x_continuous(labels = function(x) sprintf("%.2f", x), limits = x_limits) +
    scale_y_discrete(labels = c("Low" = paste0("Low ", tolower(config$category)), 
                                "High" = paste0("High ", tolower(config$category)))) +
    coord_cartesian(ylim = c(0.8, 2.6), clip = "off") +
    labs(title = plot_title_text, x = NULL, y = NULL) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
      panel.grid.major.x = element_line(color = "gray92", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 10, r = 15, b = 5, l = 5)
    )
  
  return(p)
}

# 5. Generate forest plots and save layout
all_nature_plots <- lapply(seq_along(group_configs), function(i) {
  create_nature_forest_plot(group_configs[[i]], results_df)
})

combined_nature <- wrap_plots(all_nature_plots, ncol = 2) +
  plot_annotation(
    caption = "Percentage-point change in dropout rate (95% CI)", 
    theme = theme(
      plot.caption = element_text(size = 19, color = "black", hjust = 0.5, margin = margin(t = 15)),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
    )
  )

ggsave(file.path(output_dir, "FIGURE_1_heterogeneity_wald_forest.png"),
       combined_nature, width = 15, height = 13, dpi = 300, bg = "white")

# 6. Export regression tables (HTML & Dual-Sheet Excel)
fmt_4dec <- function(x) sprintf("%.4f", x)

# Restore concise coefficient mapping (merging dummy main effects into "Group indicator (above median)")
coef_map_subgroups_concise <- c(
  "if_flood"                                        = "Flood exposure (if flood)",
  
  "grp_avg_ageHigh"                                 = "Group indicator (above median)",
  "if_flood:grp_avg_ageHigh"                        = "Flood × age group",
  
  "grp_female_ratioHigh"                            = "Group indicator (above median)",
  "if_flood:grp_female_ratioHigh"                   = "Flood × female ratio group",
  
  "grp_avg_NumberofhouseholdmembersHigh"           = "Group indicator (above median)",
  "if_flood:grp_avg_NumberofhouseholdmembersHigh"  = "Flood × household size group",
  
  "grp_urban_population_shareHigh"                  = "Group indicator (above median)",
  "if_flood:grp_urban_population_shareHigh"         = "Flood × urban group",
  
  "grp_electrification_rateHigh"                    = "Group indicator (above median)",
  "if_flood:grp_electrification_rateHigh"           = "Flood × electricity group",
  
  "grp_avg_CombinedwealthscoreHigh"                 = "Group indicator (above median)",
  "if_flood:grp_avg_CombinedwealthscoreHigh"        = "Flood × Wealth group"
)

# Export HTML table
modelsummary(
  all_models,
  coef_map = coef_map_subgroups_concise,
  fmt = fmt_4dec,
  statistic = c("std.error"),
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_omit = "Adj|AIC|BIC|Log.Lik",
  title = "Heterogeneity Interaction Models: Flood Effects Across Socio-Demographic Subgroups",
  notes = "Notes: Robust standard errors clustered at province level in parentheses. Controls and Fixed Effects are included in all models but omitted from display.",
  output = file.path(output_dir, "HETEROGENEITY_INTERACTION_MODELS_4DEC.html")
)

# Export Excel regression table (Sheet 1: Interactions only; Sheet 2: All variables)
df_sheet1 <- modelsummary(
  all_models,
  coef_map = coef_map_subgroups_concise,
  fmt = fmt_4dec,
  statistic = c("std.error", "[{conf.low}, {conf.high}]"),
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_omit = "Adj|AIC|BIC|Log.Lik",
  output = "data.frame"
)

df_sheet2 <- modelsummary(
  all_models,
  fmt = fmt_4dec,
  statistic = c("std.error", "[{conf.low}, {conf.high}]"),
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_omit = "Adj|AIC|BIC|Log.Lik",
  output = "data.frame"
)

wb_models <- createWorkbook()
addWorksheet(wb_models, "Interactions_Only")
writeData(wb_models, "Interactions_Only", df_sheet1)

addWorksheet(wb_models, "Full_Model_All_Variables")
writeData(wb_models, "Full_Model_All_Variables", df_sheet2)

setColWidths(wb_models, sheet = "Interactions_Only", cols = 1:ncol(df_sheet1), widths = "auto")
setColWidths(wb_models, sheet = "Full_Model_All_Variables", cols = 1:ncol(df_sheet2), widths = "auto")

saveWorkbook(
  wb_models, 
  file.path(output_dir, "HETEROGENEITY_INTERACTION_MODELS_4DEC.xlsx"), 
  overwrite = TRUE
)

# 7. Export summary table Excel with styling (CSV output removed)
excel_cols <- c("Category", "Group", "GroupType", "Effect Size", "Std Error", "95% CI", "p-value", "Wald Test (p-val)")
summary_table_excel <- results_df[, excel_cols]

summary_wb <- createWorkbook()
addWorksheet(summary_wb, "Family_Heterogeneity_Summary")

header_style <- createStyle(
  fontName = "Arial", fontSize = 11, textDecoration = "bold",
  bgFill = "#F2F2F2", halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderColour = "#D9D9D9"
)

cell_style_left <- createStyle(
  fontName = "Arial", fontSize = 10, halign = "left", valign = "center",
  border = "TopBottomLeftRight", borderColour = "#E0E0E0"
)

cell_style_right <- createStyle(
  fontName = "Arial", fontSize = 10, halign = "right", valign = "center",
  border = "TopBottomLeftRight", borderColour = "#E0E0E0"
)

writeData(summary_wb, "Family_Heterogeneity_Summary", summary_table_excel, headerStyle = header_style)

for(col in 1:ncol(summary_table_excel)) {
  style <- if(col %in% c(4, 5, 6, 7, 8)) cell_style_right else cell_style_left
  addStyle(summary_wb, sheet = "Family_Heterogeneity_Summary", style = style, 
           rows = 2:(nrow(summary_table_excel) + 1), cols = col, gridExpand = TRUE)
}
setColWidths(summary_wb, sheet = "Family_Heterogeneity_Summary", cols = 1:ncol(summary_table_excel), widths = "auto")

saveWorkbook(summary_wb, file.path(output_dir, "SUMMARY_FAMILY_INTERACTION_HETEROGENEITY.xlsx"), overwrite = TRUE)