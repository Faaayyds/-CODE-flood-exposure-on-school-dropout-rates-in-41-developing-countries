# 1. Load required libraries
library(fixest)
library(modelsummary)
library(data.table)
library(readxl)
library(car)
library(ggplot2)
library(dplyr)
library(patchwork)

# 2. Data loading and preprocessing
df <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx")
df$country_province <- paste(df$Country, df$Province, sep = "_")
df$id <- as.numeric(as.factor(df$country_province))
df$Year <- as.numeric(df$Year)
df <- df[order(df$id, df$Year), ]

all_vars <- c("dropout_rate", "if_flood", "if_flood_1", "if_flood_2", "if_flood_3",
              "flood_event_count", "hur_avg", "pr_total", "tas_avg",
              "avg_age", "female_ratio", "avg_Numberofhouseholdmembers",
              "urban_population_share", "electrification_rate", "avg_Combinedwealthscore")

df_complete <- na.omit(df[, c(all_vars, "id", "Year")])

output_dir <- "D:/fe_R/newcode/results/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 3. Model estimation
m1_linear <- feols(dropout_rate ~ if_flood + hur_avg + pr_total + tas_avg | id + Year, data = df_complete, cluster = ~id)
m2_linear <- feols(dropout_rate ~ if_flood + hur_avg + pr_total + tas_avg + avg_age + female_ratio + avg_Numberofhouseholdmembers | id + Year, data = df_complete, cluster = ~id)
m3_linear <- feols(dropout_rate ~ if_flood + hur_avg + pr_total + tas_avg + avg_age + female_ratio + avg_Numberofhouseholdmembers + urban_population_share + electrification_rate + avg_Combinedwealthscore | id + Year, data = df_complete, cluster = ~id)
m4_linear <- feols(dropout_rate ~ if_flood + if_flood_1 + if_flood_2 + if_flood_3 + hur_avg + pr_total + tas_avg + avg_age + female_ratio + avg_Numberofhouseholdmembers + urban_population_share + electrification_rate + avg_Combinedwealthscore | id + Year, data = df_complete, cluster = ~id)
m5_linear <- feols(dropout_rate ~ flood_event_count + hur_avg + pr_total + tas_avg + avg_age + female_ratio + avg_Numberofhouseholdmembers + urban_population_share + electrification_rate + avg_Combinedwealthscore | id + Year, data = df_complete, cluster = ~id)

# 4. Coefficient mapping dictionary
my_coef_map <- c(
  "if_flood"          = "Flood exposure (if flood)",
  "if_flood_1"        = "Flood exposure (lag 1)",
  "if_flood_2"        = "Flood exposure (lag 2)",
  "if_flood_3"        = "Flood exposure (lag 3)",
  "flood_event_count" = "Flood counts"
)

# 5. Extraction function for plot data
extract_main_coefs <- function(model, model_name, target_vars) {
  co <- coef(model)
  se_val <- se(model)
  p_val <- pvalue(model)
  ci_val <- confint(model, level = 0.95)
  
  res_list <- list()
  for(v in target_vars) {
    if(v %in% names(co)) {
      p <- p_val[v]
      stars <- if(p < 0.001) "***" else if(p < 0.01) "**" else if(p < 0.05) "*" else ""
      
      res_list[[v]] <- data.frame(
        Model = model_name,
        Variable = v,
        Var_Mapped = my_coef_map[v],
        Estimate = co[v],
        Std_Error = se_val[v],
        CI_lower = ci_val[v, 1],
        CI_upper = ci_val[v, 2],
        P_value = p,
        Significance = stars,
        stringsAsFactors = FALSE
      )
    }
  }
  return(bind_rows(res_list))
}

# 6. Extract data for Panel A and Panel B
df_m1 <- extract_main_coefs(m1_linear, "M1: Climate", "if_flood")
df_m2 <- extract_main_coefs(m2_linear, "M2: +Demographics", "if_flood")
df_m3 <- extract_main_coefs(m3_linear, "M3: Full Controls", "if_flood")
df_m4 <- extract_main_coefs(m4_linear, "M4: Lagged Flood", c("if_flood", "if_flood_1", "if_flood_2", "if_flood_3"))
df_m5 <- extract_main_coefs(m5_linear, "M5: Flood Frequency", "flood_event_count")

panel_a_data <- bind_rows(df_m1, df_m2, df_m3) %>%
  mutate(
    Model = factor(Model, levels = rev(c("M1: Climate", "M2: +Demographics", "M3: Full Controls"))),
    point_color = ifelse(Significance != "", "#C72423", "black")
  )

panel_b_data <- bind_rows(df_m4, df_m5) %>%
  mutate(
    Var_Mapped = factor(Var_Mapped, levels = rev(unname(my_coef_map))),
    point_color = ifelse(Significance != "", "#C72423", "black")
  )

# 7. Plotting Panel A
x_min_a <- min(panel_a_data$CI_lower)
x_max_a <- max(panel_a_data$CI_upper)
x_range_a <- x_max_a - x_min_a
x_limits_a <- c(x_min_a - 0.1 * x_range_a, x_max_a + 0.3 * x_range_a)

p_a <- ggplot(panel_a_data, aes(x = Estimate, y = Model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.15, linewidth = 0.8, color = "black") +
  geom_point(aes(color = point_color), size = 5, shape = 19) +
  scale_color_identity() +
  geom_text(aes(x = CI_upper + 0.05 * x_range_a, label = Significance), 
            size = 6, face = "bold", color = "#C72423", hjust = 0, vjust = 0.75) +
  scale_x_continuous(labels = function(x) sprintf("%.3f", x), limits = x_limits_a) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Effect on dropout rate (95% CI)",
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0),
    axis.text = element_text(size = 13, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 13, color = "black", margin = margin(t = 6)),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

# 8. Plotting Panel B
x_min_b <- min(panel_b_data$CI_lower)
x_max_b <- max(panel_b_data$CI_upper)
x_range_b <- x_max_b - x_min_b
x_limits_b <- c(x_min_b - 0.1 * x_range_b, x_max_b + 0.3 * x_range_b)

p_b <- ggplot(panel_b_data, aes(x = Estimate, y = Var_Mapped)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.15, linewidth = 0.8, color = "black") +
  geom_point(aes(color = point_color), size = 5, shape = 19) +
  scale_color_identity() +
  geom_text(aes(x = CI_upper + 0.05 * x_range_b, label = Significance), 
            size = 6, face = "bold", color = "#C72423", hjust = 0, vjust = 0.75) +
  scale_x_continuous(labels = function(x) sprintf("%.3f", x), limits = x_limits_b) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Effect on dropout rate (95% CI)",
    y = NULL
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 15, face = "bold", color = "black", hjust = 0),
    axis.text = element_text(size = 13, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 13, color = "black", margin = margin(t = 6)),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  )

# 9. Combine and save figure
main_forest_combined <- p_a / plot_spacer() / p_b + 
  plot_layout(heights = c(1, 0.1, 1.4)) +
  plot_annotation(
    # title = "Figure: Main Fixed Effects Models of Flood Exposure on Education Dropout Rate",
    theme = theme(plot.title = element_text(size = 17, face = "bold", hjust = 0.5))
  )

ggsave(
  file.path(output_dir, "FIGURE_MAIN_MODELS_FOREST.png"),
  main_forest_combined, 
  width = 11, 
  height = 9, 
  dpi = 300, 
  bg = "white"
)

cat("\nMain model forest plot generated and saved to: ", file.path(output_dir, "FIGURE_MAIN_MODELS_FOREST.png"), "\n")