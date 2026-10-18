library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

# 1. Load sub-national dataset
prov_data <- read_excel("D:/fe_R/newcode/results/4_PAF/Attri_Province_Formula11_12.xlsx")

# 2. Data processing for Lorenz curve 
lorenz_prov <- prov_data %>%
  filter(!is.na(AN_r_est) & !is.na(sum_P_5_17)) %>%
  filter(sum_P_5_17 > 0) %>%
  mutate(per_capita_burden = AN_r_est / sum_P_5_17) %>%
  arrange(per_capita_burden) %>%
  mutate(
    cum_pop = cumsum(sum_P_5_17),
    cum_attr = cumsum(AN_r_est),
    p_pop = cum_pop / sum(sum_P_5_17),
    p_attr = cum_attr / sum(AN_r_est)
  )

# 3. Gini coefficient estimation
n <- nrow(lorenz_prov)
p_pop_lag <- c(0, lorenz_prov$p_pop[-n])
p_attr_lag <- c(0, lorenz_prov$p_attr[-n])
gini_prov <- 1 - sum((lorenz_prov$p_pop - p_pop_lag) * (lorenz_prov$p_attr + p_attr_lag))

# 4. Define origin coordinates
plot_data <- data.frame(
  p_pop = c(0, lorenz_prov$p_pop),
  p_attr = c(0, lorenz_prov$p_attr)
)

# 5. Visualization setup
p <- ggplot(plot_data, aes(x = p_pop, y = p_attr)) +
  geom_line(color = "#2c3e50", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, color = "grey50", linetype = "dashed") +
  annotate("text", x = 0.57, y = 0.35, 
           label = paste0("Gini coefficient: ", round(gini_prov, 3)), 
           size = 7, fontface = "bold", color = "#e74c3c") +
  scale_x_continuous(labels = percent, limits = c(0, 1), expand = c(0, 0)) +
  scale_y_continuous(labels = percent, limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title = "Lorenz curve of flood-attributed dropouts",
    x = "Cumulative proportion of child population",
    y = "Cumulative proportion of flood-attributed dropouts"
  ) +
  theme_classic(base_size = 15) +
  theme(
    text = element_text(family = "Arial"),
    plot.title = element_text(face = "bold", size = 23, hjust = 0.5),
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 21, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.8),
    plot.margin = margin(t = 20, r = 25, b = 15, l = 15, unit = "pt")
  )

# 6. Reset device to avoid potential deadlock issues
graphics.off() 

# 7. Export figure
ggsave(
  filename = "D:/fe_R/newcode/results/Lorenz_Curve.png", 
  plot = p, 
  device = "png",
  width = 9,      
  height = 10,     
  dpi = 600       
)

cat("Execution completed: Lorenz curve exported successfully.\n")