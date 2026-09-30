library(readxl)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)

# 1. Data Input and Full Outer Join
df_edu <- read_excel("D:/fe_R/newcode/edupanel_41countries.xlsx")
df_flood <- read_excel("D:/fe_R/newcode/flood_data.xlsx")

edu_yearly <- df_edu %>%
  group_by(Year) %>%
  summarise(
    total_dropout = sum(dropout_school_count, na.rm = TRUE),
    total_attended = sum(children_ever_attended_num, na.rm = TRUE),
    dropout_rate = total_dropout / total_attended,
    .groups = "drop"
  )

flood_yearly <- df_flood %>%
  group_by(Year) %>%
  summarise(
    total_flood = sum(flood_event_count, na.rm = TRUE),
    .groups = "drop"
  )

# Use full_join to retain all flood years, non-sampled edu years become NA
df_yearly <- full_join(edu_yearly, flood_yearly, by = "Year") %>%
  arrange(Year)

print(df_yearly)

# 2. Dual-Axis Plot Projection with Complete Flood Background
scale_factor <- max(df_yearly$total_flood, na.rm = TRUE) / max(df_yearly$dropout_rate, na.rm = TRUE)

p_double_axis <- ggplot(df_yearly, aes(x = Year)) +
  # Flood Frequency (Full Time Series)
  geom_line(aes(y = total_flood / scale_factor, color = "Flood Counts"), linewidth = 1.0, na.rm = TRUE) +
  geom_point(aes(y = total_flood / scale_factor, color = "Flood Counts"), size = 2.0, na.rm = TRUE) +
  # Dropout Rate (Non-continuous, filtered NA for geom_line to connect available points)
  geom_line(
    data = filter(df_yearly, !is.na(dropout_rate)),
    aes(y = dropout_rate, color = "Dropout Rate"),
    linewidth = 1.2
  ) +
  geom_point(
    data = filter(df_yearly, !is.na(dropout_rate)),
    aes(y = dropout_rate, color = "Dropout Rate"),
    size = 3.0
  ) +
  scale_x_continuous(breaks = seq(min(df_yearly$Year), max(df_yearly$Year), by = 2)) +
  scale_y_continuous(
    name = "Dropout Rate",
    labels = percent_format(accuracy = 0.1),
    sec.axis = sec_axis(~ . * scale_factor, name = "Flood Counts")
  ) +
  scale_color_manual(
    values = c("Dropout Rate" = "#A339AD", "Flood Counts" = "#7FA4B5"),
    name = "Metric"
  ) +
  labs(
    title = "Annual Dropout Rate vs Complete Flood Frequency Sequence",
    x = "Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_double_axis)

# 3. Faceted Panel Plot Transformation
df_long <- df_yearly %>%
  select(Year, dropout_rate, total_flood) %>%
  pivot_longer(
    cols = c(dropout_rate, total_flood),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric_name = factor(
      case_when(
        metric == "dropout_rate" ~ "Dropout Rate",
        metric == "total_flood" ~ "Flood Counts"
      ),
      levels = c("Dropout Rate", "Flood Counts")
    )
  )

custom_labels <- function(x) {
  if (length(x) == 0) return(character(0))
  if (max(x, na.rm = TRUE) <= 1) {
    return(percent(x, accuracy = 0.1))
  } else {
    return(comma(x))
  }
}

p_faceted <- ggplot(df_long, aes(x = Year, y = value, color = metric_name)) +
  geom_line(
    data = filter(df_long, !is.na(value)),
    linewidth = 1.1
  ) +
  geom_point(
    data = filter(df_long, !is.na(value)),
    size = 2.5
  ) +
  facet_wrap(~ metric_name, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(min(df_yearly$Year), max(df_yearly$Year), by = 2)) +
  scale_color_manual(values = c("Dropout Rate" = "#A339AD", "Flood Counts" = "#7FA4B5")) +
  scale_y_continuous(labels = custom_labels) +
  labs(
    title = "Annual Dropout Rate and Complete Flood Frequency Panel View",
    x = "Year",
    y = "Value"
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_faceted)

# 4. File Export Pipeline
dir.create("D:/fe_R/newcode/results", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "D:/fe_R/newcode/results/dropout_vs_flood_double_axis.png",
  plot = p_double_axis,
  width = 11.5,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "D:/fe_R/newcode/results/dropout_vs_flood_faceted.png",
  plot = p_faceted,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  filename = "D:/fe_R/newcode/results/dropout_vs_flood_chart.pdf",
  plot = p_double_axis,
  width = 11,
  height = 6
)