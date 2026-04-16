# =========================
# FIT5147 DEP
# 02_trend_analysis.R
# =========================

rm(list = ls())

# 读取清洗后的合并数据
climate <- read.csv("data_clean/daily_KP_all_merged.csv", stringsAsFactors = FALSE)

# 转换日期
climate$Date <- as.Date(climate$Date)

# 按年份聚合
annual_data <- aggregate(cbind(GSR, RF) ~ Year, data = climate, FUN = mean)

# 查看结果
print(head(annual_data))
print(summary(annual_data))

# 线性趋势模型
gsr_lm <- lm(GSR ~ Year, data = annual_data)
rf_lm  <- lm(RF ~ Year, data = annual_data)

summary(gsr_lm)
summary(rf_lm)

# 保存趋势统计结果
write.csv(annual_data, "outputs/summary_stats/annual_trend_data.csv", row.names = FALSE)

# -------------------------
# 画图：双轴趋势图
# -------------------------
png("outputs/figures/fig1_annual_trends.png", width = 1200, height = 800)

par(mar = c(5, 5, 4, 5))

plot(
  annual_data$Year, annual_data$GSR,
  type = "l",
  lwd = 2,
  xlab = "Year",
  ylab = "Annual Mean Solar Radiation (MJ/m²)",
  main = "Annual Trends of Solar Radiation and Rainfall at King's Park"
)

abline(gsr_lm, lty = 2)

par(new = TRUE)

plot(
  annual_data$Year, annual_data$RF,
  type = "l",
  lwd = 2,
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = range(annual_data$RF, na.rm = TRUE)
)

abline(rf_lm, lty = 2)

axis(side = 4)
mtext("Annual Mean Rainfall (mm)", side = 4, line = 3)

legend(
  "topleft",
  legend = c("GSR", "GSR Trend", "RF", "RF Trend"),
  lty = c(1, 2, 1, 2),
  lwd = c(2, 1, 2, 1),
  bty = "n"
)

dev.off()

cat("Trend analysis completed.\n")

# 标准化两个变量
annual_data$GSR_scaled <- scale(annual_data$GSR)
annual_data$RF_scaled  <- scale(annual_data$RF)

png("outputs/figures/fig1_standardized_trends.png", width = 1200, height = 800)

plot(
  annual_data$Year,
  annual_data$GSR_scaled,
  type = "o",
  pch = 16,
  col = "blue",
  lwd = 2,
  ylim = range(c(annual_data$GSR_scaled, annual_data$RF_scaled)),
  xlab = "Year",
  ylab = "Standardized Value",
  main = "Standardized Annual Trends of Solar Radiation and Rainfall"
)

lines(
  annual_data$Year,
  annual_data$RF_scaled,
  type = "o",
  pch = 17,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Solar Radiation (GSR)", "Rainfall (RF)"),
  col = c("blue", "red"),
  pch = c(16, 17),
  lwd = 2,
  bty = "n"
)

dev.off()