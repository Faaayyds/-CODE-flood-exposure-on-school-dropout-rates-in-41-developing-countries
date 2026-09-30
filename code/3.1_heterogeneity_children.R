library(fixest)
library(modelsummary)
library(data.table)
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(patchwork)

# 1. Load and preprocess data
df <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx")
df <- as.data.table(df)
df$country_province <- paste(df$Country, df$Province, sep = "_")
df$id <- as.numeric(as.factor(df$country_province))
df$Year <- as.numeric(df$Year)
df <- df[order(df$id, df$Year), ]

output_dir <- "D:/fe_R/newcode/results/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Calculate dropout rates with strict zero-denominator and range checks
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

df_long$stage  <- factor(df_long$stage, levels = c("Primary (5-11)", "Lower Sec (12-14)", "Upper Sec (15-17)"))
df_long$Sex <- factor(df_long$Sex, levels = c("Female", "Male"))

fml_str <- paste0("dr_stacked ~ if_flood + ", paste(controls, collapse = " + "), " | id + Year")

# 4. Fit subsample regressions and reorder model lists
models_stage    <- feols(as.formula(fml_str), data = df_long, split = ~ stage, cluster = ~ id)
models_Sex   <- feols(as.formula(fml_str), data = df_long, split = ~ Sex, cluster = ~ id)
models_combined <- feols(as.formula(fml_str), data = df_long, split = ~ full_group, cluster = ~ id)

clean_fixest_names <- function(model_list) {
  raw_names <- names(model_list)
  cleaned <- gsub("sample\\.var: [^;]+; sample: ", "", raw_names)
  names(model_list) <- cleaned
  return(model_list)
}

models_stage    <- clean_fixest_names(models_stage)
models_Sex   <- clean_fixest_names(models_Sex)
models_combined <- clean_fixest_names(models_combined)

desired_stage_order  <- c("Primary (5-11)", "Lower Sec (12-14)", "Upper Sec (15-17)")
desired_Sex_order <- c("Female", "Male")
desired_combined_order <- c(
  "Primary (5-11) - Female",
  "Primary (5-11) - Male",
  "Lower Sec (12-14) - Female",
  "Lower Sec (12-14) - Male",
  "Upper Sec (15-17) - Female",
  "Upper Sec (15-17) - Male"
)

models_stage    <- as.list(models_stage)[desired_stage_order]
models_Sex   <- as.list(models_Sex)[desired_Sex_order]
models_combined <- as.list(models_combined)[desired_combined_order]

all_sub_models  <- c(models_stage, models_Sex, models_combined)

# 5. Extract subsample estimates and perform Wald joint hypothesis tests
extract_subsample_results <- function(models_list, dim_name, group_names_map, group_type_map = NULL) {
  m_names <- names(models_list)
  
  res_list <- lapply(seq_along(models_list), function(i) {
    m <- models_list[[i]]
    raw_subgroup <- gsub("sample\\.var: [^;]+; sample: ", "", m_names[i])
    
    est <- coef(m)["if_flood"]
    se_val <- se(m)["if_flood"]
    
    pv_vec <- tryCatch(pvalue(m), error = function(e) NULL)
    p_val <- if (!is.null(pv_vec) && "if_flood" %in% names(pv_vec)) pv_vec["if_flood"] else 2 * (1 - pnorm(abs(est / se_val)))
    
    disp_subgroup <- if(!is.null(group_names_map) && raw_subgroup %in% names(group_names_map)) group_names_map[raw_subgroup] else raw_subgroup
    group_type    <- if(!is.null(group_type_map) && raw_subgroup %in% names(group_type_map)) group_type_map[raw_subgroup] else raw_subgroup
    
    sig_stars <- ifelse(p_val < 0.001, "***", ifelse(p_val < 0.01, "**", ifelse(p_val < 0.05, "*", "")))
    ci_low  <- est - 1.96 * se_val
    ci_high <- est + 1.96 * se_val
    
    data.frame(
      Category          = dim_name,
      Group             = disp_subgroup,
      GroupType         = group_type,
      `Effect Size`     = paste0(sprintf("%.4f", est), ifelse(sig_stars != "", paste0(" ", sig_stars), "")),
      `Std Error`       = sprintf("%.4f", se_val),
      `95% CI`          = sprintf("[%.4f, %.4f]", ci_low, ci_high),
      `p-value`         = sprintf("%.4f", p_val),
      Subgroup          = disp_subgroup,
      Estimate          = unname(est),
      Std_Error         = unname(se_val),
      CI_Lower          = unname(ci_low),
      CI_Upper          = unname(ci_high),
      P_Value           = unname(p_val),
      Significance      = sig_stars,
      stringsAsFactors  = FALSE,
      check.names       = FALSE
    )
  })
  
  df_res <- bind_rows(res_list)
  
  if(dim_name == "Educational Stage") {
    fit_inter <- feols(as.formula(paste0("dr_stacked ~ if_flood * stage + (", paste(controls, collapse=" + "), ")*stage | id + Year")), data = df_long, cluster = ~id)
    wald_test <- wald(fit_inter, keep = "if_flood:stage")
  } else if(dim_name == "Sex") {
    fit_inter <- feols(as.formula(paste0("dr_stacked ~ if_flood * Sex + (", paste(controls, collapse=" + "), ")*Sex | id + Year")), data = df_long, cluster = ~id)
    wald_test <- wald(fit_inter, keep = "if_flood:Sex")
  } else {
    fit_inter <- feols(as.formula(paste0("dr_stacked ~ if_flood * full_group + (", paste(controls, collapse=" + "), ")*full_group | id + Year")), data = df_long, cluster = ~id)
    wald_test <- wald(fit_inter, keep = "if_flood:full_group")
  }
  
  p_wald_val <- if (is.list(wald_test) && "p" %in% names(wald_test)) wald_test$p else as.numeric(wald_test["p"])
  df_res$`Wald Test (p-val)` <- sprintf("%.4f", p_wald_val)
  df_res$P_Wald               <- as.numeric(p_wald_val)
  
  return(df_res)
}

map_stage <- c(
  "Primary (5-11)"    = "Primary (5-11)", 
  "Lower Sec (12-14)" = "Lower sec (12-14)", 
  "Upper Sec (15-17)" = "Upper sec (15-17)"
)
type_stage <- c(
  "Primary (5-11)"    = "Primary", 
  "Lower Sec (12-14)" = "Lower sec", 
  "Upper Sec (15-17)" = "Upper sec"
)

map_Sex <- c(
  "Female" = "Female", 
  "Male"   = "Male"
)
type_Sex <- c(
  "Female" = "Female", 
  "Male"   = "Male"
)

map_combined <- c(
  "Primary (5-11) - female"    = "Primary female (5-11)",
  "Primary (5-11) - male"      = "Primary male (5-11)",
  "Lower Sec (12-14) - female" = "Lower sec female (12-14)",
  "Lower Sec (12-14) - male"   = "Lower sec male (12-14)",
  "Upper Sec (15-17) - female" = "Upper sec female (15-17)",
  "Upper Sec (15-17) - male"   = "Upper sec male (15-17)"
)
type_combined <- c(
  "Primary (5-11) - female"    = "Primary female",
  "Primary (5-11) - male"      = "Primary male",
  "Lower Sec (12-14) - female" = "Lower sec female",
  "Lower Sec (12-14) - male"   = "Lower sec male",
  "Upper Sec (15-17) - female" = "Upper sec female",
  "Upper Sec (15-17) - male"   = "Upper sec male"
)

df_stage_res    <- extract_subsample_results(models_stage, "Educational Stage", map_stage, type_stage)
df_Sex_res   <- extract_subsample_results(models_Sex, "Sex", map_Sex, type_Sex)
df_combined_res <- extract_subsample_results(models_combined, "Sex & Stage Combined", map_combined, type_combined)

# 6. Export regression tables in HTML and Excel formats
fmt_4dec <- function(x) sprintf("%.4f", x)

gof_mapping <- list(
  list("raw" = "nobs", "clean" = "Num.Obs.", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "R2", "fmt" = 3),
  list("raw" = "within.r.squared", "clean" = "R2 Within", "fmt" = 3)
)

modelsummary(
  all_sub_models,
  coef_map = c("if_flood" = "Flood Exposure (If Flood)"),
  fmt = fmt_4dec,
  statistic = c("std.error"),
  conf_level = 0.95,
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_map = gof_mapping,
  title = "Subsample Regression Analysis: Heterogeneity by Stage, Sex, and Interaction",
  notes = "Notes: Robust standard errors clustered at province level in parentheses. 95% Confidence Intervals in brackets.",
  output = file.path(output_dir, "SUBSAMPLE_HETEROGENEITY_REGRESSION_TABLE.html")
)

modelsummary(
  all_sub_models,
  coef_map = c("if_flood" = "Flood exposure (if flood)"),
  fmt = fmt_4dec,
  statistic = c("std.error", "[{conf.low}, {conf.high}]"),
  conf_level = 0.95,
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_map = gof_mapping,
  title = "Subsample Regression Analysis: Heterogeneity by Stage, Sex, and Interaction",
  notes = "Notes: Robust standard errors clustered at province level in parentheses. 95% Confidence Intervals in brackets.",
  output = file.path(output_dir, "SUBSAMPLE_HETEROGENEITY_REGRESSION_TABLE.xlsx")
)

excel_cols <- c("Category", "Group", "GroupType", "Effect Size", "Std Error", "95% CI", "p-value", "Wald Test (p-val)")

df_stage_excel    <- df_stage_res[, excel_cols]
df_Sex_excel   <- df_Sex_res[, excel_cols]
df_combined_excel <- df_combined_res[, excel_cols]
df_all_excel      <- bind_rows(df_stage_excel, df_Sex_excel, df_combined_excel)

summary_wb <- createWorkbook()

header_style <- createStyle(
  fontName = "Arial",
  fontSize = 11,
  textDecoration = "bold",
  bgFill = "#F2F2F2",
  halign = "center",
  valign = "center",
  border = "TopBottomLeftRight",
  borderColour = "#D9D9D9"
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
  "Educational_Stage"     = df_stage_excel,
  "Sex"                = df_Sex_excel,
  "Sex_Stage_Combined" = df_combined_excel,
  "All_Summaries"         = df_all_excel
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

saveWorkbook(summary_wb, file.path(output_dir, "SUMMARY_SUBSAMPLE_HETEROGENEITY_ALL.xlsx"), overwrite = TRUE)

# 7. Generate forest plots
create_nature_forest_plot <- function(subgroup_data, title_text, file_name) {
  
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
    labs(title = title_text, x = "Effect on dropout rate (95% CI)", y = NULL) +
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

p1 <- create_nature_forest_plot(df_stage_res, "Educational stages", "forest_plot_stage_nature.png")
p2 <- create_nature_forest_plot(df_Sex_res, "Sex", "forest_plot_Sex_nature.png")
p3 <- create_nature_forest_plot(df_combined_res, "Stage and Sex interaction", "forest_plot_combined_nature.png")

# 8. Combine forest plots and save output
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