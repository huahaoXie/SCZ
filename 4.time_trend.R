#设置工作环境
setwd("C:/Users/Administrator/Desktop/WORK/2.bl-pr/2.analysis/5.time_trend")                #设置工作目录
data <- read.csv("time_matchdata.csv",header = T)
View(data)
View(data)
df <- data[,-c(2,3)]
###################################################################################################
########差异图
library("gghalves")
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
#####################################################################################################
####MCHC
rt <- df
rt$group <- factor(rt$group,levels=c('before','after'))

# 根据箱线图法去除离群值
Q1 <- quantile(rt$MCHC, 0.25)
Q3 <- quantile(rt$MCHC, 0.75)
IQR <- Q3 - Q1
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR
rt_filtered <- data[data$MCHC >= lower_limit & data$MCHC <= upper_limit, ]
# 将"group"变量转换为factor，并指定factor的顺序
rt_filtered$group <- factor(rt_filtered$group, levels = c("before", "after"))
# 创建图表，使用过滤后的数据
p <- ggplot(rt_filtered, aes(group, MCHC, fill = group)) +
  geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("MCHC time trend") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = T) +
  scale_fill_manual(values = c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))  # 设置两组箱型图间的距离为原来的一半

# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)



p <- ggplot(rt_filtered, aes(group, MCHC, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  #geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  #geom_rain(rain.side = 'f1x1', alpha = 0.8) +
  theme_minimal() +
  scale_fill_brewer(palette = 'Dark2') +
  guides(fill = 'none', color = 'none') +
  ggtitle("MCHC time trend") +
  geom_signif(comparisons = list(c('before', 'after')), map_signif_level = T) +
  scale_fill_manual(values = c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none') +
  theme(panel.grid = element_blank(),  # 去除网格线
        plot.title = element_text(size = 14, face = "bold"),  # 设置标题字体大小和粗细
        axis.title = element_text(size = 12),  # 设置坐标轴标题字体大小
        axis.text = element_text(size = 10),  # 设置坐标轴刻度字体大小
        panel.spacing = unit(0.25, "lines"))  # 设置两组箱型图间的距离为原来的一半

# 调整图表边距
p1 <- p + theme(plot.margin = unit(rep(4, 4), 'cm'))

# 输出美化后的图表
print(p1)


ggplot(rt_filtered, aes(x = group, y = MCHC, fill = group)) +
  geom_flat_violin(aes(fill = group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(time)-.15, y = score, colour = group),position = position_jitter(width = .05, height = 0), size = 1, shape = 20)+
  geom_boxplot(aes(x = time, y = score, fill = group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure 10: Repeated Measures Factorial Rainclouds") + theme_classic()
