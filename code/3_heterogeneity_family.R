library(fixest)
library(modelsummary)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(readxl)
library(kableExtra)

# 1. Data loading and preprocessing
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

# 核心修改点 1：将 category 统一修改为全小写，方便后续组装成仅首字母大写的句式
group_configs <- list(
  list(var = "avg_age", name_low = "Low average age", name_high = "High average age", category = "average age"),
  list(var = "female_ratio", name_low = "Low female ratio", name_high = "High female ratio", category = "female ratio"),
  list(var = "avg_Numberofhouseholdmembers", name_low = "Small household", name_high = "Large household", category = "household size"),
  list(var = "urban_population_share", name_low = "Low urban", name_high = "High urban", category = "urban"),
  list(var = "electrification_rate", name_low = "Low electricity", name_high = "High electricity", category = "electricity"),
  list(var = "avg_Combinedwealthscore", name_low = "Low wealth", name_high = "High wealth", category = "wealth")
)

# 3. Interaction estimation and Wald hypothesis testing
all_models <- list()
results_list <- list()

for(config in group_configs) {
  median_val <- median(df[[config$var]], na.rm = TRUE)
  group_var_name <- paste0("grp_", config$var)
  
  df_temp <- df %>%
    mutate(!!group_var_name := ifelse(.data[[config$var]] > median_val, 1, 0))
  
  current_controls <- setdiff(all_controls, config$var)
  
  fml_str <- paste0(
    "dropout_rate ~ if_flood * ", group_var_name, " + ",
    paste(current_controls, collapse = " + "), " | id + Year"
  )
  fml <- as.formula(fml_str)
  
  model_inter <- feols(fml, data = df_temp, cluster = ~id)
  
  coef_low <- coef(model_inter)["if_flood"]
  se_low   <- se(model_inter)["if_flood"]
  
  inter_term <- paste0("if_flood:", group_var_name)
  coef_high <- coef_low + coef(model_inter)[inter_term]
  
  vcov_mat <- vcov(model_inter)
  var_high <- vcov_mat["if_flood", "if_flood"] + 
    vcov_mat[inter_term, inter_term] + 
    2 * vcov_mat["if_flood", inter_term]
  se_high  <- sqrt(var_high)
  
  wald_res <- wald(model_inter, keep = inter_term)
  p_wald <- wald_res$p
  
  all_models[[config$var]] <- list(
    model = model_inter,
    p_wald = p_wald,
    config = config,
    median = median_val
  )
  
  p_low  <- 2 * (1 - pnorm(abs(coef_low / se_low)))
  p_high <- 2 * (1 - pnorm(abs(coef_high / se_high)))
  
  get_stars <- function(p) {
    if(p < 0.001) "***" else if(p < 0.01) "**" else if(p < 0.05) "*" else ""
  }
  
  res_sub <- data.frame(
    Group = c(config$name_low, config$name_high),
    Category = config$category,
    GroupType = c("Low", "High"),
    Coefficient = c(coef_low, coef_high),
    CI_lower = c(coef_low - 1.96 * se_low, coef_high - 1.96 * se_high),
    CI_upper = c(coef_low + 1.96 * se_low, coef_high + 1.96 * se_high),
    P_value = c(p_low, p_high),
    Significance = c(get_stars(p_low), get_stars(p_high)),
    P_Wald = p_wald,
    stringsAsFactors = FALSE
  )
  
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
  
  x_min <- min(subgroup_data$CI_lower, na.rm = TRUE)
  x_max <- max(subgroup_data$CI_upper, na.rm = TRUE)
  x_range <- x_max - x_min
  x_limits <- c(x_min - 0.15 * x_range, x_max + 0.40 * x_range)
  
  # 核心修改点 2：X 轴标题仅首字母大写
  x_title <- "Effect on dropout rate (95% CI)"
  
  # 辅助小函数：使字符串仅首字母大写
  to_sentence_case <- function(x) {
    paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  }
  
  plot_title_text <- to_sentence_case(config$category)
  
  p <- ggplot(subgroup_data, aes(x = Coefficient, y = factor(GroupType, levels = c("Low", "High")))) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.15, linewidth = 0.8, color = "black") +
    geom_point(aes(color = point_color), size = 5.5, shape = 19) +
    scale_color_identity() +
    geom_text(aes(x = CI_upper + 0.03 * x_range, label = Significance), 
              size = 6, face = "bold", color = "#C72423", hjust = 0) +
    annotate("text", x = x_limits[2], y = 2.4, label = p_wald_label, 
             hjust = 1, size = 6, face = "italic", color = "gray20") +
    scale_x_continuous(labels = function(x) sprintf("%.2f", x), limits = x_limits) +
    
    # 核心修改点 3：Y 轴刻度标签仅首字母大写 (如 "Low average age", "High average age")
    scale_y_discrete(labels = c("Low" = paste0("Low ", tolower(config$category)), 
                                "High" = paste0("High ", tolower(config$category)))) +
    coord_cartesian(ylim = c(0.8, 2.6), clip = "off") +
    labs(title = plot_title_text, x = x_title, y = NULL) +
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

# 5. Plot generation and multi-panel arrangement
all_nature_plots <- lapply(seq_along(group_configs), function(i) {
  create_nature_forest_plot(group_configs[[i]], results_df)
})

combined_nature <- wrap_plots(all_nature_plots, ncol = 2) +
  plot_annotation(
    theme = theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
  )

ggsave(file.path(output_dir, "FIGURE_1_heterogeneity_wald_forest.png"),
       combined_nature, width = 15, height = 13, dpi = 300, bg = "white")

# 6. Interaction models export (display only main effect and interaction terms, formatted to 4 decimals)
models_list <- lapply(all_models, function(x) x$model)
names(models_list) <- sapply(group_configs, function(x) paste0("Subgroup: ", x$category))

coef_map_subgroups <- c(
  "if_flood" = "Flood exposure (if flood)",
  "grp_avg_age" = "Group indicator (above median)",
  "if_flood:grp_avg_age" = "Flood × age group",
  
  "grp_female_ratio" = "Group indicator (above median)",
  "if_flood:grp_female_ratio" = "Flood × female ratio group",
  
  "grp_avg_Numberofhouseholdmembers" = "Group indicator (above median)",
  "if_flood:grp_avg_Numberofhouseholdmembers" = "Flood × household size group",
  
  "grp_urban_population_share" = "Group indicator (above median)",
  "if_flood:grp_urban_population_share" = "Flood × urban group",
  
  "grp_electrification_rate" = "Group indicator (above median)",
  "if_flood:grp_electrification_rate" = "Flood × electricity group",
  
  "grp_avg_Combinedwealthscore" = "Group indicator (above median)",
  "if_flood:grp_avg_Combinedwealthscore" = "Flood × Wealth group"
)

fmt_4dec <- function(x) sprintf("%.4f", x)

modelsummary(
  models_list,
  coef_map = coef_map_subgroups,
  fmt = fmt_4dec,
  statistic = c("std.error"),
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_omit = "Adj|AIC|BIC|Log.Lik",
  title = "Heterogeneity Interaction Models: Flood Effects Across Socio-Demographic Subgroups",
  notes = "Notes: Robust standard errors clustered at province level in parentheses. Controls and Fixed Effects are included in all models but omitted from display.",
  output = file.path(output_dir, "HETEROGENEITY_INTERACTION_MODELS_4DEC.html")
)

modelsummary(
  models_list,
  coef_map = coef_map_subgroups,
  fmt = fmt_4dec,
  statistic = c("std.error", "conf.int"),
  stars = c('*' = 0.05, '**' = 0.01, '***' = 0.001),
  gof_omit = "Adj|AIC|BIC|Log.Lik",
  title = "Heterogeneity Interaction Models: Flood Effects Across Socio-Demographic Subgroups",
  notes = "Notes: Robust standard errors clustered at province level in parentheses. 95% Confidence Intervals in brackets.",
  output = file.path(output_dir, "HETEROGENEITY_INTERACTION_MODELS_4DEC.xlsx")
)

# 7. Export summary table with 4 decimal places
summary_table_4dec <- results_df %>%
  mutate(
    `Std Error` = (CI_upper - CI_lower) / (2 * 1.96),
    `Effect Size` = sprintf("%.4f %s", Coefficient, Significance),
    `Coefficient` = sprintf("%.4f", Coefficient),
    `Std Error` = sprintf("%.4f", `Std Error`),
    `95% CI` = sprintf("[%.4f, %.4f]", CI_lower, CI_upper),
    `p-value` = ifelse(P_value < 0.001, "<0.001", sprintf("%.4f", P_value)),
    `Wald Test (p-val)` = ifelse(P_Wald < 0.001, "<0.001", sprintf("%.4f", P_Wald))
  ) %>%
  select(Category, Group, GroupType, `Effect Size`, `Std Error`, `95% CI`, `p-value`, `Wald Test (p-val)`)

write.csv(summary_table_4dec, file.path(output_dir, "SUMMARY_TABLE_WALD_4DEC.csv"), row.names = FALSE)