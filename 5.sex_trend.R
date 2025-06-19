#设置工作环境
setwd("C:/Users/Administrator/Desktop/WORK/2.bl-pr/2.analysis/6.subgroup_time")                #设置工作目录


#################################################################################################
####时间趋势差异分析
library(dplyr)
library(broom)
data <- read.csv("time_male.csv",header = T)

#然后，您可以按照以下步骤进行批量差异分析：
# 假设您的数据框为 df，第一列是分组变量，其他列是要进行差异分析的变量
library(tidyverse)

# 使用t函数转置数据框
#df_transposed <- as.data.frame(t(df))

#colnames(df_transposed) <- df_transposed[1,]
#df <- df_transposed[-1,]

##对数据进行批量t检验
#加载dplyr，rstatix和reshape2这三个R包
library(rstatix)
library(reshape2)

# 假设您的数据框为df，第一列是分组，其它列是指标

# 定义一个函数，对每列指标进行T检验
df <- data
t_test <- function(col) {
  group1 <- col[df$group == "before"]  # 第一组数据
  group2 <- col[df$group == "after"]  # 第二组数据
  
  # 进行T检验
  result <- t.test(group1, group2)
  
  return(list(p_value = result$p.value, statistic = result$statistic))
}

# 选择第2到第5列进行T检验
cols_to_test <- df[, 2:23]

# 对每列指标进行T检验
results <- apply(cols_to_test, 2, t_test)

# 将结果整理成数据框
results_df <- do.call(rbind, lapply(results, as.data.frame))

write.csv(results_df, "male_pvalue.csv")


###################################################################################################
########差异图
library("gghalves")
library("ggsignif")
library("tidyverse")
library("devtools")
library("Rmisc")
library("remarkdown")
library("readr")
library("dplyr")
library("ggplot2")
library("lattice")
library("plyr")
library("PupillometryR")
library(ggrain)


#####################################################################################
####RBC 
rt <- df
# 根据箱线图法去除离群值
Q1 <- quantile(rt$RBC, 0.25)
Q3 <- quantile(rt$RBC, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$RBC >= lower_limit & data$RBC <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, RBC, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("RBC") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (10^12/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)


#####################################################################################
####MON 
# 根据箱线图法去除离群值
Q1 <- quantile(rt$MON, 0.25)
Q3 <- quantile(rt$MON, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$MON >= lower_limit & data$MON <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, MON, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("MON") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (10^9/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)


#####################################################################################
####LYM# 
# 根据箱线图法去除离群值
Q1 <- quantile(rt$LYM, 0.25)
Q3 <- quantile(rt$LYM, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$LYM >= lower_limit & data$LYM <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, LYM, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("LYM") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines")) +  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (10^9/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####TBIL
# 根据箱线图法去除离群值
Q1 <- quantile(rt$TBIL, 0.25)
Q3 <- quantile(rt$TBIL, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$TBIL >= lower_limit & data$TBIL <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, TBIL, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("TBIL") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines")) +  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (umol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####PALB
# 根据箱线图法去除离群值
Q1 <- quantile(rt$PALB, 0.25)
Q3 <- quantile(rt$PALB, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$PALB >= lower_limit & data$PALB <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, PALB, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("PALB") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines")) +  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (mg/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####UREA
# 根据箱线图法去除离群值
Q1 <- quantile(rt$UREA, 0.25)
Q3 <- quantile(rt$UREA, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$UREA >= lower_limit & data$UREA <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, UREA, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("UREA") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (mmol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)


#####################################################################################
####UA
# 根据箱线图法去除离群值
Q1 <- quantile(rt$UA, 0.25)
Q3 <- quantile(rt$UA, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$UA >= lower_limit & data$UA <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, UA, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("UA") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines")) +  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (μmol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####TG
# 根据箱线图法去除离群值
Q1 <- quantile(rt$TG, 0.25)
Q3 <- quantile(rt$TG, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$TG >= lower_limit & data$TG <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, TG, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("TG") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (IU/ml)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####K
# 根据箱线图法去除离群值
Q1 <- quantile(rt$K, 0.25)
Q3 <- quantile(rt$K, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$K >= lower_limit & data$K <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, K, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("K") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("steelblue", "darkgreen")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (mmol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



############################################################################################################
####################################################################
######################################
###female
data <- read.csv("time_female.csv",header = T)

# 使用t函数转置数据框
#df_transposed <- as.data.frame(t(df))

#colnames(df_transposed) <- df_transposed[1,]
#df <- df_transposed[-1,]

# 假设您的数据框为df，第一列是分组，其它列是指标

# 定义一个函数，对每列指标进行T检验
df <- data
t_test <- function(col) {
  group1 <- col[df$group == "before"]  # 第一组数据
  group2 <- col[df$group == "after"]  # 第二组数据
  
  # 进行T检验
  result <- t.test(group1, group2)
  
  return(list(p_value = result$p.value, statistic = result$statistic))
}

# 选择第2到第5列进行T检验
cols_to_test <- df[, 2:23]

# 对每列指标进行T检验
results <- apply(cols_to_test, 2, t_test)

# 将结果整理成数据框
results_df <- do.call(rbind, lapply(results, as.data.frame))

write.csv(results_df, "female_pvalue.csv")


###################################################################################################
########差异图

rt <- df
#####################################################################################
####MON 
# 根据箱线图法去除离群值
Q1 <- quantile(rt$MON, 0.25)
Q3 <- quantile(rt$MON, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$MON >= lower_limit & data$MON <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, MON, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("MON") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (10^9/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####TBIL
# 根据箱线图法去除离群值
Q1 <- quantile(rt$TBIL, 0.25)
Q3 <- quantile(rt$TBIL, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$TBIL >= lower_limit & data$TBIL <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, TBIL, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("TBIL") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (umol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)


#####################################################################################
####TBA
# 根据箱线图法去除离群值
Q1 <- quantile(rt$TBA, 0.25)
Q3 <- quantile(rt$TBA, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$TBA >= lower_limit & data$TBA <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, TBA, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("TBA") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (umol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)




#####################################################################################
####PALB
# 根据箱线图法去除离群值
Q1 <- quantile(rt$PALB, 0.25)
Q3 <- quantile(rt$PALB, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$PALB >= lower_limit & data$PALB <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, PALB, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("PALB") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (mg/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####UREA
# 根据箱线图法去除离群值
Q1 <- quantile(rt$UREA, 0.25)
Q3 <- quantile(rt$UREA, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$UREA >= lower_limit & data$UREA <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, UREA, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("UREA") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (mmol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)


#####################################################################################
####UA
# 根据箱线图法去除离群值
Q1 <- quantile(rt$UA, 0.25)
Q3 <- quantile(rt$UA, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$UA >= lower_limit & data$UA <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, UA, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("UA") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (μmol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



#####################################################################################
####TG
# 根据箱线图法去除离群值
Q1 <- quantile(rt$TG, 0.25)
Q3 <- quantile(rt$TG, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$TG >= lower_limit & data$TG <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, TG, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("TG") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (IU/ml)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)


#####################################################################################
####Na
# 根据箱线图法去除离群值
Q1 <- quantile(rt$Na, 0.25)
Q3 <- quantile(rt$Na, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$Na >= lower_limit & data$Na <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, Na, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("Na") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (mmol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)
#####################################################################################
####K
# 根据箱线图法去除离群值
Q1 <- quantile(rt$K, 0.25)
Q3 <- quantile(rt$K, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$K >= lower_limit & data$K <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, K, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("K") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = TRUE) +
  scale_fill_manual(values = c("lightpink", "darkorchid")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))+  # 设置两组箱型图间的距离为原来的一半
  ylab("Count (mmol/L)")+  # 修改纵坐标名称为"count"
  theme(axis.title.y = element_text(size = 12, face = "bold"))  # 修改纵坐标标题的字体大小和粗细
# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)




