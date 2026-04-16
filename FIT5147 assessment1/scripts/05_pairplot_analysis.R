# =========================
# FIT5147 DEP
# 05_pairplot_analysis.R
# =========================

rm(list = ls())

packages <- c("GGally", "ggplot2")
installed <- packages %in% rownames(installed.packages())

if (any(!installed)) {
  install.packages(packages[!installed])
}

library(GGally)
library(ggplot2)

# 读取数据
climate <- read.csv("data_clean/daily_KP_all_merged.csv", stringsAsFactors = FALSE)

# 只保留分析变量
plot_data <- climate[, c("GSR", "RF", "SUN", "RH")]

# 删除缺失值
plot_data <- na.omit(plot_data)

# 创建 pair plot
p <- ggpairs(
  plot_data,
  title = "Scatterplot Matrix of Climate Variables",
  upper = list(continuous = wrap("cor", size = 5)),
  lower = list(continuous = wrap("points", alpha = 0.2, size = 0.5)),
  diag = list(continuous = wrap("densityDiag", alpha = 0.5))
)

ggsave(
  filename = "outputs/figures/fig9_pairplot_matrix.png",
  plot = p,
  width = 12,
  height = 12,
  dpi = 300
)

cat("Pairplot analysis completed.\n")