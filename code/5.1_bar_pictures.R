library(tidyverse)
library(readxl) 
library(scales)

# 1. Read and clean data
df <- read_excel("D:/fe_R/newcode/results/4_PAF_SSP/Attri_Province_Formula11_12.xlsx", sheet = 1)

df_clean <- df %>% 
  mutate(
    prov_mean_dropout_rate = as.numeric(prov_mean_dropout_rate),
    sum_flood_count = as.numeric(sum_flood_count),
    PAF_r_est = as.numeric(PAF_r_est),
    AN_r_est = as.numeric(AN_r_est),
    Province_Label = Province
  )

# 2. Filter and sort Top 10 data
# 2.1 Education Top 10
cutoff_edu <- df_clean %>%
  slice_max(order_by = prov_mean_dropout_rate, n = 10, with_ties = FALSE) %>%
  pull(prov_mean_dropout_rate) %>% min()

df_top10_edu <- df_clean %>%
  filter(prov_mean_dropout_rate >= cutoff_edu) %>%
  mutate(Province_Label = reorder(Province_Label, prov_mean_dropout_rate))

# 2.2 Flood Top 10
cutoff_flood <- df_clean %>%
  slice_max(order_by = sum_flood_count, n = 10, with_ties = FALSE) %>%
  pull(sum_flood_count) %>% min()

df_top10_flood <- df_clean %>%
  filter(sum_flood_count >= cutoff_flood) %>%
  mutate(Province_Label = reorder(Province_Label, sum_flood_count))

# 2.3 PAF Top 10
cutoff_paf <- df_clean %>% 
  slice_max(order_by = PAF_r_est, n = 10, with_ties = FALSE) %>% 
  pull(PAF_r_est) %>% min()

df_top10_paf <- df_clean %>% 
  filter(PAF_r_est >= cutoff_paf) %>% 
  mutate(Province_Label = reorder(Province_Label, PAF_r_est))

# 2.4 Attributable Children Top 10（添加 is_top4 标记前5大数值）
cutoff_an <- df_clean %>% 
  slice_max(order_by = AN_r_est, n = 10, with_ties = FALSE) %>% 
  pull(AN_r_est) %>% min()

df_top10_an <- df_clean %>% 
  filter(AN_r_est >= cutoff_an) %>% 
  arrange(desc(AN_r_est)) %>% 
  mutate(
    is_top4 = row_number() <= 4, # 前5名标记为 TRUE，后5名为 FALSE
    Province_Label = reorder(Province_Label, AN_r_est)
  )

# 3. Separate theme definitions 
# p1, p2
theme_front <- theme_classic(base_size = 15) + 
  theme(
    text = element_text(family = "Arial"),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15, unit = "pt"),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 18),
    axis.text.y = element_text(color = "black", size = 16, hjust = 1),
    axis.text.x = element_text(color = "black", size = 16),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

# p3, p4
theme_back <- theme_classic(base_size = 15) + 
  theme(
    text = element_text(family = "Arial"),
    plot.margin = margin(t = 15, r = 25, b = 15, l = 15, unit = "pt"),
    plot.title = element_text(face = "bold", size = 28, hjust = 0.5),
    axis.title = element_text(size = 26),
    axis.text.y = element_text(color = "black", size = 24, hjust = 1),
    axis.text.x = element_text(color = "black", size = 24),
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA)
  )


# 4. Plotting and saving
# Plot 1: Dropout Ratio
p1 <- ggplot(df_top10_edu, aes(x = prov_mean_dropout_rate, y = Province_Label)) +
  geom_bar(stat = "identity", fill = "#9924A4", linewidth = 0.5, alpha = 0.9, width = 0.5) +
  geom_text(
    aes(label = round(prov_mean_dropout_rate, 3)), 
    hjust = -0.15, size = 6, color = "black"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = label_number()) +
  labs(x = "Dropout ratio", y = "Subnational regions", title = "Top 10 subnational regions by dropout rate") +
  theme_front

ggsave("D:/fe_R/newcode/results/Single_Top10_Education.png", plot = p1,
       width = 8, height = 7, dpi = 300, bg = "white")


# Plot 2: Flood Events
p2 <- ggplot(df_top10_flood, aes(x = sum_flood_count, y = Province_Label)) +
  geom_bar(stat = "identity", fill = "#407993", linewidth = 0.5, alpha = 0.9, width = 0.5) +
  geom_text(
    aes(label = comma(sum_flood_count)), 
    hjust = -0.5, size = 6, color = "black"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = label_number()) +
  labs(x = "Total flood counts", y = "Subnational Regions", title = "Top 10 subnational regions by flood counts") +
  theme_front

ggsave("D:/fe_R/newcode/results/Single_Top10_Flood.png", plot = p2,
       width = 8, height = 7, dpi = 300, bg = "white")


# Plot 3: PAF Data
p3 <- ggplot(df_top10_paf, aes(x = PAF_r_est, y = Province_Label)) +
  geom_bar(stat = "identity", fill = "#407993", linewidth = 0.5, alpha = 0.9, width = 0.6) +
  geom_text(
    aes(label = round(PAF_r_est, 3)), 
    hjust = 1.15, size = 7.5, color = "white", fontface = "bold"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05)), labels = label_number()) +
  labs(x = "PAF", y = "Subnational regions", title = "Top 10 subnational regions by PAF") +
  theme_back

ggsave("D:/fe_R/newcode/results/Single_Top10_PAF.png", plot = p3, 
       width = 8, height = 11, dpi = 300, bg = "white")


# Plot 4: Attributable Children (前5个大值在柱内白色，后5个小值在柱外黑色)
p4 <- ggplot(df_top10_an, aes(x = AN_r_est, y = Province_Label)) +
  geom_bar(stat = "identity", fill = "#9924A4", linewidth = 0.5, alpha = 0.9, width = 0.6) +
  # 图层 1：前 4 个最大值（白色，加粗，位于柱子内部右侧）
  geom_text(
    data = filter(df_top10_an, is_top4),
    aes(label = comma(round(AN_r_est))), 
    hjust = 1.1, size = 7, color = "white", fontface = "bold"
  ) +
  # 图层 2：后 6 个较小值（黑色，常规体，位于柱子外部右侧）
  geom_text(
    data = filter(df_top10_an, !is_top4),
    aes(label = comma(round(AN_r_est))), 
    hjust = -0.15, size = 7, color = "black"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.18)), # 恢复右侧留白空间，防止外侧文本截断
    labels = label_comma()
  ) +
  labs(x = "Attributable children", y = "Subnational regions", title = "Top 10 subnational regions by attributable children") +
  theme_back

ggsave("D:/fe_R/newcode/results/Single_Top10_Attributable_Children.png", plot = p4, 
       width = 11, height = 11, dpi = 300, bg = "white")