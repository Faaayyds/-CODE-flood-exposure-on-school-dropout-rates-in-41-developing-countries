# 1. Load required libraries
library(fixest)
library(modelsummary)
library(data.table)
library(readxl)
library(car)
library(ggplot2)
library(dplyr)

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

# 4. Coefficient mapping dictionary (专用于 M4 滞后期及 M5)
my_coef_map <- c(
  "if_flood_1"        = "M4: Lagged flood (Lag 1)",
  "if_flood_2"        = "M4: Lagged flood (Lag 2)",
  "if_flood_3"        = "M4: Lagged flood (Lag 3)",
  "flood_event_count" = "M5: Flood counts"
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

# 6. Extract data and assign Y_Label directly
df_m1 <- extract_main_coefs(m1_linear, "M1", "if_flood") %>% mutate(Y_Label = "M1: Climate")
df_m2 <- extract_main_coefs(m2_linear, "M2", "if_flood") %>% mutate(Y_Label = "M2: +Demographics")
df_m3 <- extract_main_coefs(m3_linear, "M3", "if_flood") %>% mutate(Y_Label = "M3: Full controls")

# M4 中剔除 "if_flood"，只保留 3 个滞后项
df_m4 <- extract_main_coefs(m4_linear, "M4", c("if_flood_1", "if_flood_2", "if_flood_3")) %>% 
  mutate(Y_Label = my_coef_map[Variable])

df_m5 <- extract_main_coefs(m5_linear, "M5", "flood_event_count") %>% 
  mutate(Y_Label = my_coef_map[Variable])

# 定义自上而下的严格显示顺序
y_levels <- c(
  "M1: Climate",
  "M2: +Demographics",
  "M3: Full controls",
  "M4: Lagged flood (Lag 1)",
  "M4: Lagged flood (Lag 2)",
  "M4: Lagged flood (Lag 3)",
  "M5: Flood counts"
)

all_forest_data <- bind_rows(df_m1, df_m2, df_m3, df_m4, df_m5) %>%
  mutate(
    Y_Label = factor(Y_Label, levels = rev(y_levels)),
    point_color = ifelse(Significance != "", "#C72423", "black")
  )

# 7. Plotting Single Combined Forest Plot
x_min <- min(all_forest_data$CI_lower)
x_max <- max(all_forest_data$CI_upper)
x_range <- x_max - x_min
x_limits <- c(x_min - 0.1 * x_range, x_max + 0.3 * x_range)

p_single_forest <- ggplot(all_forest_data, aes(x = Estimate, y = Y_Label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, linewidth = 0.8, color = "black") +
  geom_point(aes(color = point_color), size = 4.5, shape = 19) +
  scale_color_identity() +
  geom_text(aes(x = CI_upper + 0.04 * x_range, label = Significance), 
            size = 6, face = "bold", color = "#C72423", hjust = 0, vjust = 0.75) +
  scale_x_continuous(labels = function(x) sprintf("%.3f", x), limits = x_limits) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Percentage-point change in dropout rate (95% CI)",
    y = NULL
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 12, color = "black", margin = margin(t = 8)),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15)
  )

# 8. Save figure
ggsave(
  file.path(output_dir, "FIGURE_MAIN_MODELS_FOREST_SINGLE.png"),
  p_single_forest, 
  width = 9.5, 
  height = 6.5, 
  dpi = 300, 
  bg = "white"
)

cat("\nCombined single forest plot generated and saved to: ", file.path(output_dir, "FIGURE_MAIN_MODELS_FOREST_SINGLE.png"), "\n")