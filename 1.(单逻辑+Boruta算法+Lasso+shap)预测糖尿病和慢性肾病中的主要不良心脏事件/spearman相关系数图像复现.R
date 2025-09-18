# 简单热图的制作----------------------------------------------------------------
# install.packages("dplyr")      # 用于数据处理
# install.packages("corrplot")   # 一个强大的相关性热图包
# install.packages("ggcorrplot") # 另一个基于ggplot2的热图包

# 加载需要的包
library(dplyr)
library(corrplot)
library(ggcorrplot)
# 设置随机种子以保证结果可复现
set.seed(42)

# 创建一个模拟的数据框
df_original <- data.frame(
  Age = round(rnorm(100, mean = 65, sd = 10)),  # 连续变量
  SBP = round(rnorm(100, mean = 140, sd = 15)), # 连续变量
  eGFR = round(rnorm(100, mean = 70, sd = 20)),   # 连续变量
  
  # 二元分类变量 (存储为字符或因子)
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Hypertension = sample(c("Yes", "No"), 100, replace = TRUE),
  
  # 有序分类变量 (存储为有序因子)
  Diabetes_Severity = factor(
    sample(c("Mild", "Moderate", "Severe"), 100, replace = TRUE),
    levels = c("Mild", "Moderate", "Severe"),
    ordered = TRUE
  ),
  
  # 名义分类变量 (存储为普通因子)
  Blood_Type = factor(sample(c("A", "B", "AB", "O"), 100, replace = TRUE))
)

# 查看原始数据的前几行和结构
print("----------- 原始数据框 -----------")
head(df_original)
str(df_original)
# 定义一个函数来处理数据框
process_for_correlation <- function(df) {
  df_processed <- df %>%
    mutate(
      # 对二元变量进行明确的 0/1 编码
      Gender = ifelse(Gender == "Male", 1, 0),
      Hypertension = ifelse(Hypertension == "Yes", 1, 0),
      
      # 对所有因子(factor)变量进行强制数值转换
      # as.numeric() 会提取因子的内部整数编码
      # WARNING: 这对于名义变量(如Blood_Type)的解释是武断的！
      across(where(is.factor), as.numeric)
    )
  return(df_processed)
}

# 应用处理函数
df_numeric <- process_for_correlation(df_original)

# 查看处理后的数据框，现在所有列都是数值型了
print("----------- 处理后的纯数值数据框 -----------")
head(df_numeric)
str(df_numeric)

# 我们可以检查一下转换结果
# Diabetes_Severity: Mild=1, Moderate=2, Severe=3 (因为我们定义了顺序)
# Blood_Type: A=1, AB=2, B=3, O=4 (这个顺序是基于字母顺序，是任意的！)
# 计算斯皮尔曼相关系数矩阵
# use = "pairwise.complete.obs" 用于处理可能存在的缺失值
cor_matrix <- cor(df_numeric, method = "spearman", use = "pairwise.complete.obs")

# 查看相关性矩阵的前几行前几列
print("----------- 斯皮尔曼相关性矩阵 -----------")
round(cor_matrix, 2)
# 设置绘图区域
# par(mfrow = c(1, 1)) # 确保只有一个图

# 使用corrplot绘制热图
# hclust: 按层次聚类结果对变量重排序，使相似的变量聚在一起
# tl.col: 调整文本标签颜色
# addCoef.col: 调整相关系数的颜色
# type = "lower": 只显示矩阵的下三角部分
print("----------- 绘制热图 -----------")
corrplot(cor_matrix,
         method = "color",       # 使用颜色填充
         type = "lower",         # 只显示下三角
         order = "hclust",       # 按聚类结果排序
         tl.col = "black",       # 文本标签颜色
         tl.srt = 45,            # 文本标签旋转角度
         addCoef.col = "black",  # 添加相关系数
         number.cex = 0.7,       # 系数文字大小
         cl.cex = 0.7,           # 图例文字大小
         title = "Spearman Correlation Heatmap",
         mar = c(0,0,1,0)         # 调整边界
)
# *(这是一个占位符，实际运行代码会生成一个热图)*
  
  ### 结论与反思
"
  通过以上代码，我们成功地重现了论文作者可能采取的步骤：

1.  **数据准备**: 他们从一个混合了多种变量类型的数据集开始。
2.  **强制数值化**: 他们将所有非数值变量（二元的、有序的、甚至无序的）都转换成了数值。这是能够计算出一个统一相关性矩阵的**关键前提**。
3.  **计算与可视化**: 在纯数值数据集上计算斯皮尔曼相关性，并用热图进行可视化。

**您的疑惑最终得到了解答**：代码之所以能运行，是因为在计算前进行了**强制类型转换**。这种做法在探索性分析中为了方便而被使用，但我们在解读结果时必须极度小心：

*   对于**连续-连续**、**连续-有序**、**连续-二元**变量间的相关性，结果是可靠的。
*   对于涉及到**名义分类变量**的相关性，其数值和正负号可能完全是其内部编码顺序（如字母顺序）造成的**人为假象**，不具有真实的统计学意义。

希望这个从构建数据到最终出图的完整过程，能够彻底解答您的疑问。
"
# 生成组合式热图----------------------------------------------------------------
# --- 准备工作：加载包 ---
library(dplyr)
library(corrplot)
library(RColorBrewer)

# --- 第1步 到 第3步：准备数据和相关性矩阵 ---
# (代码与之前完全相同，此处省略以保持简洁)
# 确保你已经运行了之前的代码，并得到了 cor_matrix 和 custom_colors 这两个对象
set.seed(42)
df_original <- data.frame(
  Age = round(rnorm(100, mean = 65, sd = 10)),
  SBP = round(rnorm(100, mean = 140, sd = 15)),
  eGFR = round(rnorm(100, mean = 70, sd = 20)),
  Gender = sample(c("Male", "Female"), 100, replace = TRUE),
  Hypertension = sample(c("Yes", "No"), 100, replace = TRUE),
  Diabetes_Severity = factor(
    sample(c("Mild", "Moderate", "Severe"), 100, replace = TRUE),
    levels = c("Mild", "Moderate", "Severe"),
    ordered = TRUE
  ),
  Blood_Type = factor(sample(c("A", "B", "AB", "O"), 100, replace = TRUE))
)
process_for_correlation <- function(df) {
  df_processed <- df %>%
    mutate(
      Gender = ifelse(Gender == "Male", 1, 0),
      Hypertension = ifelse(Hypertension == "Yes", 1, 0),
      across(where(is.factor), as.numeric)
    )
  return(df_processed)
}
df_numeric <- process_for_correlation(df_original)
cor_matrix <- cor(df_numeric, method = "spearman", use = "pairwise.complete.obs")
custom_colors <- colorRampPalette(rev(RColorBrewer::brewer.pal(n = 11, name = "RdBu")))(200)


# --- 第4步：使用两步corrplot()叠加法，实现最终效果 ---

# (可选) 防止标签因超出边界而被剪掉
par(xpd = TRUE)

# 1. 绘制“画布”：上半部分的扇形图 + 完整的坐标轴和对角线
corrplot(cor_matrix,
         method = "pie",          # 使用扇形图
         type = "upper",          # 只绘制上半部分
         order = "hclust",        # 按聚类结果排序
         
         # --- 关键：建立完整的框架 ---
         tl.pos = "tp",           # 'tp' 表示标签显示在顶部 (Top) 和左侧 (implied by default)
         diag = TRUE,             # !!! 绘制对角线，补全框架 !!!
         
         # --- 美学设置 ---
         col = custom_colors,
         tl.col = "black",
         tl.srt = 45,
         title = "Spearman Correlation Heatmap",
         mar = c(0,0,1,0)
)

# 2. 在画布上添加下半部分：颜色背景 + 数字
corrplot(cor_matrix,
         add = TRUE,              # !!! 关键：将此图添加到已存在的绘图上 !!!
         type = "lower",          # 只操作下半部分
         method = "color",        # 主要方法是绘制颜色背景
         order = "hclust",        # !!! 必须使用和上一步完全相同的排序 !!!
         col = custom_colors,
         # --- 关键：在颜色上添加数字 ---
         addCoef.col = "black",   # 在色块上添加黑色的相关系数值
         number.cex = 0.8,        # 数字的大小
         
         # --- 清理不必要的元素 ---
         diag = FALSE,            # 不重复绘制对角线
         tl.pos = "n",            # 不重复绘制标签
         cl.pos = "n"             # 不重复绘制颜色图例
)

