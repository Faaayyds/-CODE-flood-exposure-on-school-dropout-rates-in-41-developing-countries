library(fixest)
library(modelsummary)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(patchwork)
library(marginaleffects) # Must load this package to automatically extract subgroup effects for interaction terms

# 1. Load and preprocess data
df <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx")
df <- as.data.table(df)
df$country_province <- paste(df$Country, df$Province, sep = "_")
df$id <- as.numeric(as.factor(df$country_province))
df$Year <- as.numeric(df$Year)
df <- df[order(df$id, df$Year), ]

output_dir <- "D:/fe_R/newcode/results/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Calculate dropout rates with range checks
vars_att <- c("attend_05_11_boy", "attend_05_11_girl", "attend_12_14_boy", "attend_12_14_girl", "attend_15_17_boy", "attend_15_17_girl")
vars_dr  <- c("dr_primary_male", "dr_primary_female", "dr_lower_male", "dr_lower_female", "dr_upper_male", "dr_upper_female")

df[, dr_primary_male   := drop_05_11_boy   / attend_05_11_boy]
df[, dr_primary_female := drop_05_11_girl  / attend_05_11_girl]
df[, dr_lower_male     := drop_12_14_boy   / attend_12_14_boy]
df[, dr_lower_female   := drop_12_14_girl  / attend_12_14_girl]
df[, dr_upper_male     := drop_15_17_boy   / attend_15_17_boy]
df[, dr_upper_female   := drop_15_17_girl  / attend_15_17_girl]

for(i in seq_along(vars_dr)){
  v_dr  <- vars_dr[i]
  v_att <- vars_att[i]
  
  df[[v_dr]] <- fifelse(
    !is.na(df[[v_att]]) & df[[v_att]] > 0 & 
      !is.na(df[[v_dr]])  & is.finite(df[[v_dr]]) & 
      df[[v_dr]] >= 0     & df[[v_dr]] <= 1, 
    df[[v_dr]], 
    NA_real_
  )
}

controls <- c(
  "hur_avg", "pr_total", "tas_avg", "avg_age", "avg_Numberofhouseholdmembers",
  "urban_population_share", "electrification_rate", "avg_Combinedwealthscore"
)

# 3. Reshape dataset to long panel format
id_vars <- c("id", "Year", "if_flood", controls)
df_long <- melt(df, id.vars = id_vars, measure.vars = vars_dr, variable.name = "subgroup", value.name = "dr_stacked")[!is.na(dr_stacked)]

df_long[, stage  := fifelse(grepl("primary", subgroup), "Primary (5-11)", 
                            fifelse(grepl("lower", subgroup), "Lower Sec (12-14)", "Upper Sec (15-17)"))]
df_long[, Sex := fifelse(grepl("male", subgroup) & !grepl("female", subgroup), "Male", "Female")]
df_long[, full_group := paste(stage, Sex, sep = " - ")]

df_long$stage      <- factor(df_long$stage, levels = c("Primary (5-11)", "Lower Sec (12-14)", "Upper Sec (15-17)"))
df_long$Sex        <- factor(df_long$Sex, levels = c("Female", "Male"))
df_long$full_group <- factor(df_long$full_group, levels = c(
  "Primary (5-11) - Female", "Primary (5-11) - Male",
  "Lower Sec (12-14) - Female", "Lower Sec (12-14) - Male",
  "Upper Sec (15-17) - Female", "Upper Sec (15-17) - Male"
))

# 4. Full-sample interaction model function and marginal effects extraction
run_interaction_analysis <- function(group_col, category_name) {
  # Construct unified full-sample regression model with interaction terms
  fml_str <- paste0("dr_stacked ~ if_flood * ", group_col, " + ", paste(controls, collapse = " + "), " | id + Year")
  model_inter <- feols(as.formula(fml_str), data = df_long, cluster = ~ id)
  
  # Extract estimated marginal effects (Slopes) for each subgroup using marginaleffects
  slopes_df <- as.data.frame(avg_slopes(model_inter, variables = "if_flood", by = group_col))
  
  # Perform overall Wald test for the joint significance of interaction terms
  wald_test <- wald(model_inter, keep = paste0("if_flood:", group_col))
  p_wald_val <- if (is.list(wald_test) && "p" %in% names(wald_test)) wald_test$p else as.numeric(wald_test["p"])
  
  # Construct standardized output table
  res_df <- slopes_df %>%
    mutate(
      Category          = category_name,
      Group             = as.character(.data[[group_col]]),
      GroupType         = Group,
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
      Subgroup          = Group,
      P_Wald            = p_wald_val
    ) %>%
    select(Category, Group, GroupType, `Effect Size`, `Std Error`, `95% CI`, `p-value`, 
           `Wald Test (p-val)`, Subgroup, Estimate, Std_Error, CI_Lower, CI_Upper, P_Value, Significance, P_Wald)
  
  return(list(model = model_inter, results = res_df))
}

# Run interaction models across three heterogeneity dimensions
res_stage    <- run_interaction_analysis("stage", "Educational Stage")
res_Sex      <- run_interaction_analysis("Sex", "Sex")
res_combined <- run_interaction_analysis("full_group", "Sex & Stage Combined")

all_inter_models <- list(
  "Educational Stage" = res_stage$model,
  "Sex"               = res_Sex$model,
  "Stage & Sex Combined" = res_combined$model
)

df_stage_res    <- res_stage$results
df_Sex_res      <- res_Sex$results
df_combined_res <- res_combined$results

# 5. Export regression tables (HTML & Dual-Sheet Excel)
fmt_4dec <- function(x) sprintf("%.4f", x)

# Coefficient map defined in logical order (control variables are excluded to omit them from output)
coef_map_children_ordered <- c(
  # Main effect of core independent variable
  "if_flood"                                       = "Flood exposure (if flood)",
  
  # 1. Educational Stage dummy variables and interaction terms
  "stageLower Sec (12-14)"                         = "Group indicator: Lower Sec (12-14)",
  "if_flood:stageLower Sec (12-14)"                = "Flood × Lower Sec",
  "stageUpper Sec (15-17)"                         = "Group indicator: Upper Sec (15-17)",
  "if_flood:stageUpper Sec (15-17)"                = "Flood × Upper Sec",
  
  # 2. Sex dummy variables and interaction terms
  "SexMale"                                        = "Group indicator: Male",
  "if_flood:SexMale"                               = "Flood × Male",
  
  # 3. Combined Group dummy variables and interaction terms
  "full_groupPrimary (5-11) - Male"                = "Group indicator: Primary Male",
  "if_flood:full_groupPrimary (5-11) - Male"       = "Flood × Primary Male",
  "full_groupLower Sec (12-14) - Female"           = "Group indicator: Lower Sec Female",
  "if_flood:full_groupLower Sec (12-14) - Female"   = "Flood × Lower Sec Female",
  "full_groupLower Sec (12-14) - Male"             = "Group indicator: Lower Sec Male",
  "if_flood:full_groupLower Sec (12-14) - Male"     = "Flood × Lower Sec Male",
  "full_groupUpper Sec (15-17) - Female"           = "Group indicator: Upper Sec Female",
  "if_flood:full_groupUpper Sec (15-17) - Female"   = "Flood × Upper Sec Female",
  "full_groupUpper Sec (15-17) - Male"             = "Group indicator: Upper Sec Male",
  "if_flood:full_groupUpper Sec (15-17) - Male"     = "Flood × Upper Sec Male"
)

# Export HTML table
modelsummary(
  all_inter_models,
  coef_map = coef_map_children_ordered, # Custom order with controls omitted
  fmt = fmt_4dec,
  statistic = c("std.error"),
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_omit = "Adj|AIC|BIC|Log.Lik",
  title = "Interaction Models: Flood Effects Across Children Subgroups",
  notes = "Notes: Robust standard errors clustered at province level in parentheses. Controls and Fixed Effects are included in all models but omitted from display.",
  output = file.path(output_dir, "CHILDREN_INTERACTION_MODELS.html")
)

# Export Excel regression tables (Sheet 1: Main effects and interactions only; Sheet 2: All variables)
df_sheet1 <- modelsummary(
  all_inter_models,
  coef_map = coef_map_children_ordered,
  fmt = fmt_4dec,
  statistic = c("std.error", "[{conf.low}, {conf.high}]"),
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_omit = "Adj|AIC|BIC|Log.Lik",
  output = "data.frame"
)

df_sheet2 <- modelsummary(
  all_inter_models,
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
  file.path(output_dir, "CHILDREN_INTERACTION_MODELS_TWO_SHEETS.xlsx"), 
  overwrite = TRUE
)

# 6. Export marginal effects summary Excel
excel_cols <- c("Category", "Group", "GroupType", "Effect Size", "Std Error", "95% CI", "p-value", "Wald Test (p-val)")

df_stage_excel    <- df_stage_res[, excel_cols]
df_Sex_excel      <- df_Sex_res[, excel_cols]
df_combined_excel <- df_combined_res[, excel_cols]
df_all_excel      <- bind_rows(df_stage_excel, df_Sex_excel, df_combined_excel)

summary_wb <- createWorkbook()

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

sheets <- list(
  "Educational_Stage"  = df_stage_excel,
  "Sex"                = df_Sex_excel,
  "Sex_Stage_Combined" = df_combined_excel,
  "All_Summaries"      = df_all_excel
)

for(s_name in names(sheets)) {
  addWorksheet(summary_wb, s_name)
  dt <- sheets[[s_name]]
  writeData(summary_wb, s_name, dt, headerStyle = header_style)
  
  for(col in 1:ncol(dt)) {
    style <- if(col %in% c(4, 5, 6, 7, 8)) cell_style_right else cell_style_left
    addStyle(summary_wb, sheet = s_name, style = style, rows = 2:(nrow(dt) + 1), cols = col, gridExpand = TRUE)
  }
  setColWidths(summary_wb, sheet = s_name, cols = 1:ncol(dt), widths = "auto")
}

saveWorkbook(summary_wb, file.path(output_dir, "SUMMARY_CHILDREN_INTERACTION_HETEROGENEITY.xlsx"), overwrite = TRUE)

# 7. Forest plot function
create_nature_forest_plot <- function(subgroup_data, title_text) {
  ordered_subgroups <- unique(subgroup_data$Subgroup)
  
  subgroup_data <- subgroup_data %>% 
    mutate(
      point_color = ifelse(Significance != "", "#C72423", "black"),
      Subgroup = factor(Subgroup, levels = rev(ordered_subgroups))
    )
  
  p_wald_val <- unique(subgroup_data$P_Wald)
  p_wald_label <- ifelse(p_wald_val < 0.001, "Wald test: p < 0.001", 
                         sprintf("Wald test: p = %.3f", p_wald_val))
  
  x_min <- min(subgroup_data$CI_Lower, na.rm = TRUE)
  x_max <- max(subgroup_data$CI_Upper, na.rm = TRUE)
  x_range <- ifelse(x_max == x_min, 0.1, x_max - x_min)
  x_limits <- c(x_min - 0.15 * x_range, x_max + 0.40 * x_range)
  
  num_groups <- nrow(subgroup_data)
  
  p <- ggplot(subgroup_data, aes(x = Estimate, y = Subgroup)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.4) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.15, linewidth = 0.8, color = "black") +
    geom_point(aes(color = point_color), size = 5.5, shape = 19) +
    scale_color_identity() +
    geom_text(
      aes(x = CI_Upper + 0.03 * x_range, label = Significance), 
      size = 6, face = "bold", color = "#C72423", hjust = 0
    ) +
    annotate(
      "text", x = x_limits[2], y = num_groups + 0.4, label = p_wald_label, 
      hjust = 1, size = 6, face = "italic", color = "gray20"
    ) +
    scale_x_continuous(labels = function(x) sprintf("%.3f", x), limits = x_limits) +
    scale_y_discrete(limits = rev(ordered_subgroups)) +
    coord_cartesian(ylim = c(0.8, num_groups + 0.6), clip = "off") +
    labs(title = title_text, x = "Percentage-point change in dropout rate (95% CI)", y = NULL) + 
    theme_classic() +
    theme(
      plot.title = element_text(size = 21, face = "bold", color = "black", hjust = 0),
      axis.text = element_text(size = 18, color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.title.x = element_text(size = 19, color = "black", margin = margin(t = 6)),
      panel.grid.major.x = element_line(color = "gray92", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 10)
    )
  return(p)
}

p1 <- create_nature_forest_plot(df_stage_res, "Educational stages")
p2 <- create_nature_forest_plot(df_Sex_res, "Sex")
p3 <- create_nature_forest_plot(df_combined_res, "Stage and Sex interaction")

# 8. Combine plots and export
p1_clean <- p1 + theme(axis.title.x = element_blank())
p2_clean <- p2 + theme(axis.title.x = element_blank())

p_combined <- (p1_clean / p2_clean / p3) + plot_layout(heights = c(3, 2, 6))

ggsave(
  filename = file.path(output_dir, "FIGURE_2_heterogeneity_wald_forest.png"),
  plot = p_combined,
  width = 10,
  height = 14,
  dpi = 300,
  bg = "white"
)