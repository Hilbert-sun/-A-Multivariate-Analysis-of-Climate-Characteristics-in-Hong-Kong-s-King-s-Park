# =========================
# FIT5147 DEP
# 01_data_cleaning.R
# =========================

rm(list = ls())

# -------------------------
# 1. 自定义清洗函数
# -------------------------
clean_climate_data <- function(file_path, value_name) {
  
  df <- read.csv(
    file_path,
    skip = 2,
    header = TRUE,
    stringsAsFactors = FALSE
  )
  
  # 查看原始列名
  cat("\nReading file:", file_path, "\n")
  print(colnames(df))
  print(head(df))
  
  # 有些文件列名可能不完全一致，统一取前5列
  df <- df[, 1:5]
  colnames(df) <- c("Year", "Month", "Day", "Value", "Completeness")
  
  # 处理缺失值
  df$Value[df$Value == "***"] <- NA
  df$Completeness[df$Completeness == "***"] <- NA
  df$Completeness[df$Completeness == ""] <- NA
  
  # 转换类型
  df$Year  <- as.integer(df$Year)
  df$Month <- as.integer(df$Month)
  df$Day   <- as.integer(df$Day)
  df$Value <- as.numeric(df$Value)
  
  # 日期列
  df$Date <- as.Date(
    paste(df$Year, df$Month, df$Day, sep = "-"),
    format = "%Y-%m-%d"
  )
  
  # 改变量名
  colnames(df)[colnames(df) == "Value"] <- value_name
  
  # 调整顺序
  df <- df[, c("Date", "Year", "Month", "Day", value_name, "Completeness")]
  
  return(df)
}

# -------------------------
# 2. 读取四个原始数据集
# -------------------------
gsr <- clean_climate_data("data_raw/daily_KP_GSR_ALL.csv", "GSR")
rf  <- clean_climate_data("data_raw/daily_KP_RF_ALL.csv", "RF")
sun <- clean_climate_data("data_raw/daily_KP_SUN_ALL.csv", "SUN")
rh  <- clean_climate_data("data_raw/daily_KP_RH_ALL.csv", "RH")

# -------------------------
# 3. 检查结果
# -------------------------
cat("\n===== HEAD =====\n")
print(head(gsr))
print(head(rf))
print(head(sun))
print(head(rh))

cat("\n===== SUMMARY =====\n")
print(summary(gsr))
print(summary(rf))
print(summary(sun))
print(summary(rh))

# -------------------------
# 4. 保留完整记录
# -------------------------
gsr_clean <- subset(gsr, Completeness == "C" & !is.na(GSR))
rf_clean  <- subset(rf,  Completeness == "C" & !is.na(RF))
sun_clean <- subset(sun, Completeness == "C" & !is.na(SUN))
rh_clean  <- subset(rh,  Completeness == "C" & !is.na(RH))

# -------------------------
# 5. 输出清洗前后行数
# -------------------------
cat("\n===== ROW COUNTS =====\n")
cat("GSR raw:", nrow(gsr), " | clean:", nrow(gsr_clean), "\n")
cat("RF raw :", nrow(rf),  " | clean:", nrow(rf_clean), "\n")
cat("SUN raw:", nrow(sun), " | clean:", nrow(sun_clean), "\n")
cat("RH raw :", nrow(rh),  " | clean:", nrow(rh_clean), "\n")

# -------------------------
# 6. 保存单独清洗文件
# -------------------------
write.csv(gsr_clean, "data_clean/daily_KP_GSR_clean.csv", row.names = FALSE)
write.csv(rf_clean,  "data_clean/daily_KP_RF_clean.csv", row.names = FALSE)
write.csv(sun_clean, "data_clean/daily_KP_SUN_clean.csv", row.names = FALSE)
write.csv(rh_clean,  "data_clean/daily_KP_RH_clean.csv", row.names = FALSE)

# -------------------------
# 7. 合并四个数据集
# -------------------------
merged_data <- merge(
  gsr_clean[, c("Date", "Year", "Month", "Day", "GSR")],
  rf_clean[, c("Date", "RF")],
  by = "Date"
)

merged_data <- merge(
  merged_data,
  sun_clean[, c("Date", "SUN")],
  by = "Date"
)

merged_data <- merge(
  merged_data,
  rh_clean[, c("Date", "RH")],
  by = "Date"
)

# 排序
merged_data <- merged_data[order(merged_data$Date), ]

# 重新生成年月日
merged_data$Year  <- as.integer(format(merged_data$Date, "%Y"))
merged_data$Month <- as.integer(format(merged_data$Date, "%m"))
merged_data$Day   <- as.integer(format(merged_data$Date, "%d"))

# 调整列顺序
merged_data <- merged_data[, c("Date", "Year", "Month", "Day", "GSR", "RF", "SUN", "RH")]

# -------------------------
# 8. 检查合并结果
# -------------------------
cat("\n===== MERGED DATA =====\n")
print(head(merged_data))
print(summary(merged_data))
cat("Merged row count:", nrow(merged_data), "\n")

# -------------------------
# 9. 导出总表
# -------------------------
write.csv(merged_data, "data_clean/daily_KP_all_merged.csv", row.names = FALSE)

cat("\nData cleaning and merging completed successfully.\n")