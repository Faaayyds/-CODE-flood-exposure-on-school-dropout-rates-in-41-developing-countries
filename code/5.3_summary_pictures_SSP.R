library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(purrr)

options(warn = -1)

# 1. Path Configuration and Environment Setup
path_2030 <- "D:/fe_R/newcode/results/4.2_SSP_RCP/SSP_Projections_2030.xlsx"
path_2050 <- "D:/fe_R/newcode/results/4.2_SSP_RCP/SSP_Projections_2050.xlsx"
output_dir <- "D:/fe_R/newcode/results/"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

scenario_map <- c(
  "SSP126" = "SSP1-2.6",
  "SSP245" = "SSP2-4.5",
  "SSP585" = "SSP5-8.5"
)

# Robust data loading function extracting both population and dropout counts
load_ssp_data <- function(file_path, year_label) {
  if (!file.exists(file_path)) return(NULL)
  
  available_sheets <- excel_sheets(file_path)
  
  map_df(names(scenario_map), function(prefix) {
    sheet_name <- paste0(prefix, "_", year_label)
    
    if (sheet_name %in% available_sheets) {
      df <- read_excel(file_path, sheet = sheet_name)
      
      an_col <- if ("AN_est" %in% names(df)) "AN_est" else names(df)[grepl("AN_", names(df))][1]
      pop_col <- if ("P_5_17_SSP" %in% names(df)) "P_5_17_SSP" else names(df)[grepl("P_5_17", names(df))][1]
      
      df %>%
        transmute(
          Country = as.character(Country),
          Province = as.character(Province),
          Total_Pop = as.numeric(.data[[pop_col]]),
          Dropout_Count = as.numeric(.data[[an_col]]),
          Year = as.character(year_label),
          Scenario = scenario_map[[prefix]]
        )
    } else {
      NULL
    }
  })
}

tryCatch({
  
  # 2. Ingestion Across Designated Sheets
  df_2030 <- load_ssp_data(path_2030, "2030")
  df_2050 <- load_ssp_data(path_2050, "2050")
  
  raw_df <- bind_rows(df_2030, df_2050)
  
  if (nrow(raw_df) == 0) {
    stop("No data extracted. Verify sheet names and target columns.")
  }
  
  # 3. Data Transformation
  df_clean <- raw_df %>%
    mutate(
      Scenario = factor(Scenario, levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"))
    ) %>%
    filter(!is.na(Dropout_Count) & !is.na(Total_Pop))
  
  # Publication-ready theme (legend moved to bottom)
  clean_theme <- theme_minimal(base_size = 14) +
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8), 
      axis.line = element_blank(), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      axis.text = element_text(color = "black", size = 16),
      axis.title = element_text(size = 18),
      legend.position = "bottom", 
      legend.margin = margin(t = 10), 
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      plot.title = element_blank(),
      strip.text = element_text(size = 18, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.8)
    )
  
  # Aggregated summary data
  ssp_summary_df <- df_clean %>%
    group_by(Scenario, Year) %>%
    summarise(
      Dropout_Count = sum(Dropout_Count, na.rm = TRUE),
      Total_Pop = sum(Total_Pop, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 4. Chart 1: Population vs. Dropout Comparison
  comparison_df <- ssp_summary_df %>%
    pivot_longer(
      cols = c(Total_Pop, Dropout_Count),
      names_to = "Metric",
      values_to = "Value"
    ) %>%
    mutate(
      Metric = factor(
        Metric, 
        levels = c("Total_Pop", "Dropout_Count"),
        labels = c("Total Population (Ages 5-17)", "Predicted Dropout Children")
      )
    )
  
  p_pop_vs_dropout <- ggplot(comparison_df, aes(x = Scenario, y = Value, fill = Year)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.55) +
    geom_text(
      aes(label = comma(round(Value))),
      position = position_dodge(width = 0.7),
      vjust = -0.5,
      size = 4.2,
      fontface = "bold"
    ) +
    facet_wrap(~ Metric, scales = "free_y") +
    scale_fill_manual(values = c("2030" = "#52A992", "2050" = "#DE4F3A")) +
    scale_y_continuous(labels = label_comma(), expand = expansion(mult = c(0, 0.18))) +
    labs(
      x = "SSP Scenario",
      y = "Number of Children",
      fill = "Year"
    ) +
    clean_theme +
    theme(
      panel.spacing = unit(2, "lines"),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 15, unit = "pt")
    )
  
  ggsave(
    file.path(output_dir, "Pop_vs_Dropout_Comparison_2030_2050.png"), 
    p_pop_vs_dropout, 
    width = 18, 
    height = 7.5, 
    dpi = 600, 
    bg = "white"
  )
  
  # 5. Chart 2: Temporal Trajectory Slopechart
  p_slope <- ggplot(ssp_summary_df, aes(x = Year, y = Dropout_Count, group = Scenario, color = Scenario)) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 4) +
    scale_color_manual(values = c("SSP1-2.6" = "#52A992", "SSP2-4.5" = "#FEBA2C", "SSP5-8.5" = "#DE4F3A")) +
    scale_y_continuous(labels = label_comma()) +
    labs(
      x = "Year",
      y = "Predicted Dropout Children",
      color = "Scenario"
    ) +
    clean_theme +
    theme(
      panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 15, unit = "pt")
    )
  
  ggsave(
    file.path(output_dir, "SSP_Trajectory_2030_2050.png"), 
    p_slope, 
    width = 8.5, 
    height = 7, 
    dpi = 600, 
    bg = "white"
  )
  
}, error = function(e) {
  message("Execution halted: ", e$message)
}, finally = {
  options(warn = 0)
})