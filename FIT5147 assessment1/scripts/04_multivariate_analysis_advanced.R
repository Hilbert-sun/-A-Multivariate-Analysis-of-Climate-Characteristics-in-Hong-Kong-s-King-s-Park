# =========================
# FIT5147 DEP
# 04_multivariate_analysis_advanced.R
# =========================

rm(list = ls())

# -------------------------
# 0. 加载包
# -------------------------
packages <- c("ggplot2", "dplyr")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(ggplot2)
library(dplyr)

# -------------------------
# 1. 读取数据
# -------------------------
climate <- read.csv("data_clean/daily_KP_all_merged.csv", stringsAsFactors = FALSE)
climate$Date <- as.Date(climate$Date)

# 创建季节变量
climate$Season <- ifelse(climate$Month %in% c(12, 1, 2), "Winter",
                         ifelse(climate$Month %in% c(3, 4, 5), "Spring",
                                ifelse(climate$Month %in% c(6, 7, 8), "Summer", "Autumn")))

climate$Season <- factor(climate$Season, levels = c("Winter", "Spring", "Summer", "Autumn"))

# -------------------------
# 2. 相关性与回归
# -------------------------
cor_sun_gsr <- cor(climate$SUN, climate$GSR, use = "complete.obs")
cor_rf_gsr  <- cor(climate$RF, climate$GSR, use = "complete.obs")
cor_rh_gsr  <- cor(climate$RH, climate$GSR, use = "complete.obs")

model_linear <- lm(GSR ~ SUN + RF + RH, data = climate)

cat("===== CORRELATIONS =====\n")
cat("SUN vs GSR:", round(cor_sun_gsr, 3), "\n")
cat("RF vs GSR :", round(cor_rf_gsr, 3), "\n")
cat("RH vs GSR :", round(cor_rh_gsr, 3), "\n\n")

cat("===== MULTIPLE LINEAR REGRESSION =====\n")
print(summary(model_linear))

capture.output(
  summary(model_linear),
  file = "outputs/summary_stats/multivariate_multiple_regression.txt"
)

# -------------------------
# 3. 高级图1：多变量散点图
# x = SUN
# y = GSR
# size = RF
# color = RH
# -------------------------
p1 <- ggplot(climate, aes(x = SUN, y = GSR)) +
  geom_point(aes(size = RF, color = RH), alpha = 0.45) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Multivariate Relationship Between Sunshine Hours and Solar Radiation",
    subtitle = "Point size represents rainfall and point colour represents relative humidity",
    x = "Sunshine Hours (SUN)",
    y = "Solar Radiation (GSR)",
    size = "Rainfall (RF)",
    color = "Relative Humidity (RH)"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "outputs/figures/fig5_multivariate_scatter_advanced.png",
  plot = p1,
  width = 12,
  height = 8,
  dpi = 300
)

# -------------------------
# 4. 高级图2：按季节分面的散点图
# 可以显示不同季节关系是否不同
# -------------------------
p2 <- ggplot(climate, aes(x = SUN, y = GSR)) +
  geom_point(aes(size = RF, color = RH), alpha = 0.40) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
  facet_wrap(~ Season, ncol = 2) +
  labs(
    title = "Seasonal Variation in the Relationship Between Sunshine Hours and Solar Radiation",
    subtitle = "Faceted by season, with rainfall encoded by size and humidity encoded by colour",
    x = "Sunshine Hours (SUN)",
    y = "Solar Radiation (GSR)",
    size = "Rainfall (RF)",
    color = "Relative Humidity (RH)"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "outputs/figures/fig6_multivariate_scatter_by_season.png",
  plot = p2,
  width = 12,
  height = 9,
  dpi = 300
)

# -------------------------
# 5. 高级图3：LOESS平滑曲线
# 看是否存在非线性趋势
# -------------------------
p3 <- ggplot(climate, aes(x = SUN, y = GSR)) +
  geom_point(alpha = 0.20, size = 1.2) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1) +
  labs(
    title = "Non-linear Relationship Between Sunshine Hours and Solar Radiation",
    subtitle = "LOESS smoothing highlights the overall trend",
    x = "Sunshine Hours (SUN)",
    y = "Solar Radiation (GSR)"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "outputs/figures/fig7_loess_sun_gsr.png",
  plot = p3,
  width = 10,
  height = 7,
  dpi = 300
)

# -------------------------
# 6. 高级图4：按季节的相关性摘要
# 每个季节分别计算 SUN 与 GSR 的相关性
# -------------------------
season_cor <- climate %>%
  group_by(Season) %>%
  summarise(
    Correlation_SUN_GSR = cor(SUN, GSR, use = "complete.obs"),
    Mean_RF = mean(RF, na.rm = TRUE),
    Mean_RH = mean(RH, na.rm = TRUE),
    .groups = "drop"
  )

print(season_cor)

write.csv(
  season_cor,
  "outputs/summary_stats/seasonal_correlation_summary.csv",
  row.names = FALSE
)

p4 <- ggplot(season_cor, aes(x = Season, y = Correlation_SUN_GSR)) +
  geom_col() +
  labs(
    title = "Seasonal Correlation Between Sunshine Hours and Solar Radiation",
    x = "Season",
    y = "Correlation Coefficient"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  filename = "outputs/figures/fig8_seasonal_correlation_bar.png",
  plot = p4,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\nAdvanced multivariate analysis completed successfully.\n")