# =========================
# FIT5147 DEP
# 03_seasonal_analysis.R
# =========================

rm(list = ls())

# 读取数据
climate <- read.csv("data_clean/daily_KP_all_merged.csv", stringsAsFactors = FALSE)

# -------------------------
# 1. 计算月平均值
# -------------------------
monthly_data <- aggregate(
  cbind(GSR, RF, SUN, RH) ~ Month,
  data = climate,
  FUN = mean
)

print(monthly_data)

# 保存月平均数据
write.csv(monthly_data,
          "outputs/summary_stats/monthly_average_data.csv",
          row.names = FALSE)

# -------------------------
# 2. 月均值折线图
# -------------------------
png("outputs/figures/fig2_monthly_patterns.png", width = 1200, height = 800)

matplot(
  monthly_data$Month,
  monthly_data[, c("GSR", "RF", "SUN", "RH")],
  type = "o",
  pch = 1:4,
  lty = 1,
  lwd = 2,
  col = c("orange", "blue", "darkgreen", "red"),
  xlab = "Month",
  ylab = "Monthly Mean Value",
  main = "Monthly Patterns of GSR, RF, SUN and RH"
)

legend(
  "topright",
  legend = c("GSR", "RF", "SUN", "RH"),
  col = c("orange", "blue", "darkgreen", "red"),
  pch = 1:4,
  lwd = 2,
  bty = "n"
)

dev.off()

# -------------------------
# 3. 相关性矩阵
# -------------------------
cor_data <- climate[, c("GSR", "RF", "SUN", "RH")]
cor_matrix <- cor(cor_data, use = "complete.obs")

print(cor_matrix)

write.csv(cor_matrix,
          "outputs/summary_stats/correlation_matrix.csv")

# -------------------------
# 4. 相关性热力图
# -------------------------
png("outputs/figures/fig3_correlation_heatmap.png", width = 800, height = 800)

image(
  1:ncol(cor_matrix),
  1:nrow(cor_matrix),
  t(cor_matrix[nrow(cor_matrix):1, ]),
  axes = FALSE,
  xlab = "",
  ylab = "",
  main = "Correlation Heatmap of Climate Variables",
  col = heat.colors(20)
)

axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix))
axis(2, at = 1:nrow(cor_matrix), labels = rev(rownames(cor_matrix)))

# 加数值标签
for (i in 1:nrow(cor_matrix)) {
  for (j in 1:ncol(cor_matrix)) {
    text(
      j,
      nrow(cor_matrix) - i + 1,
      round(cor_matrix[i, j], 2)
    )
  }
}

dev.off()

# -------------------------
# 5. GSR 月度箱线图
# -------------------------
png("outputs/figures/fig4_monthly_gsr_boxplot.png", width = 1200, height = 800)

boxplot(
  GSR ~ Month,
  data = climate,
  col = "lightblue",
  xlab = "Month",
  ylab = "Daily Solar Radiation (MJ/m²)",
  main = "Monthly Distribution of Solar Radiation"
)

dev.off()

cat("Seasonal analysis completed.\n")