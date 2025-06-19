
library(MatchIt)
library(dplyr)
library(tidyverse)  
library(ggplot2)
library(ggthemes)
library(ggsci)

#设置工作环境
setwd("C:/Users/Administrator/Desktop/WORK/2.bl-pr/2.analysis/2.PSM")                #设置工作目录

mydata <- read.csv("matrix.csv",header = T)
df = mydata
attach(df)
#summary(df$age)

# 计算不同组间的平均值和标准差
summary_stats <- df %>%
  group_by(bl_group) %>%
  summarise(
    mean_value = mean(Age, na.rm = TRUE),
    sd_value = sd(Age, na.rm = TRUE)
  )

# 输出结果
print(summary_stats)

# 使用group_by()和 summarise()函数查看不同分组下的四分位数
result <- df %>%
  group_by(bl_group) %>%
  summarise(
    Q1 = quantile(age, 0.25),
    median = median(age),
    Q3 = quantile(age, 0.75),
    mean = mean(age)
  )

# 输出结果
print(result)


# 使用group_by()和 count()函数对不同分组进行计数
result <- df %>%
  group_by(bl_group, mood_stabilizer) %>%
  count()

# 输出结果
print(result)

#统计分析
# 使用t.test()函数进行独立样本 t 检验
t_test_result <- t.test(age ~ bl_group, data = df)
print(t_test_result)

# 使用wilcox.test()函数进行 Wilcoxon 秩和检验
wilcox_test_result <- wilcox.test(age ~ bl_group, data = df)
print(wilcox_test_result)


# 使用chisq.test()函数进行卡方检验
chisq_test_result <- chisq.test(table(df$sex, df$bl_group))
print(chisq_test_result)
#install.packages("psych")
#library(psych)


############################### 倾向性评分匹配 ############################
m = matchit(bl_group ~ Age+ Sex+ ECT+
              Typical_antipsychotics+	Atypical_antipsychotics+	Mood_stabilizer+	
              Antiepileptic+ X5_HT+	Transmitter_inhibitors+	
              Benzodiazepine+	Non_benzodiazepines, data = df,
            method="nearest", #匹配方法——最近邻匹配
            caliper=0.2, #设置卡钳值为0.1，可能要多次尝试确定最佳值
            ratio = 1:1)   #replace=F 匹配公式 1:1最近邻

summary(m)

df2 = match.data(m)   # 匹配出来的新数据集

# 使用group_by()和 summarise()函数查看不同分组下的四分位数
result <- df2 %>%
  group_by(bl_group) %>%
  summarise(
    Q1 = quantile(age, 0.25),
    median = median(age),
    Q3 = quantile(age, 0.75),
    mean = mean(age)
  )
# 输出结果
print(result)

# 使用group_by()和 count()函数对不同分组进行计数
result <- df2 %>%
  group_by(bl_group, sex) %>%
  count()
# 输出结果
print(result)


df2 <- read.csv("bl_matched.csv",header = T)
#统计分析
# 使用t.test()函数进行独立样本 t 检验
t_test_result <- wilcox.test(age ~ bl_group, data = df2)
print(t_test_result)
# 使用chisq.test()函数进行卡方检验
chisq_test_result <- chisq.test(table(df2$ECT, df2$bl_group))
print(chisq_test_result)

write.csv(df2, "bl_matched.csv")

df <- read.csv("bl_matched.csv",header = T)
# 计算不同组间的平均值和标准差
summary_stats <- df %>%
  group_by(bl_group) %>%
  summarise(
    mean_value = mean(age, na.rm = TRUE),
    sd_value = sd(age, na.rm = TRUE)
  )

# 输出结果
print(summary_stats)
# 计算倾向性评分
# 匹配前
library(MatchIt)
ps_model <- matchit(bl_group ~ Age+ Sex+ ECT+
                      Typical_antipsychotics+	Atypical_antipsychotics+	Mood_stabilizer+	
                      Antiepileptic+	X5_HT+	Transmitter_inhibitors+	
                      Benzodiazepine+	Non_benzodiazepines, data = df, method = "nearest",
                      distance="glm", m.order= 
                      "random",ratio=1,caliper=0.03,replace=FALSE)

summary(ps_model)

plot(summary(ps_model))

# 匹配后
ps_model <- matchit(bl_group ~ age+ sex+ ECT+
                      typical_antipsychotics+	atypical_antipsychotics+	mood_stabilizer+	
                      antiepileptic+	X5_HT+	transmitter_inhibitors+	
                      benzodiazepine+	non_benzodiazepines, data = df2, method = "nearest",
                      distance="glm", m.order= 
                      "random",ratio=1,caliper=0.03,replace=FALSE)

summary(ps_model)
library(cobalt)

love.plot(ps_model,binary = "std",stats = c("mean.diffs"),
          threshold = c(.1), var.order = "unadjusted", line = TRUE)


#####################################################################################################
########################################
# 加载所需的包
library(ggplot2)
library(ggsignif) #添加显著性

data <- read.csv("bl_sex1.csv",header = T)
data2 <- read.csv("bl_sex2.csv",header = T)

# Load necessary libraries
library(ggplot2)

# Create the data frame
data <- data.frame(
  group = c("Control", "Impulsivity"),
  Percentage = c(37.3, 62.7),
  Label = c("Control(37.3%)", "Impulsivity(62.7%)")
)
# Create the plot with donut chart, ring border, and gap at connection
p <- ggplot(data, aes(x = 1, y = Percentage, fill = group)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(vjust = 0), color = "black", size = 1.5) +  # 加粗边框线条
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white")) +  # Set background to white
  annotate("text", x = 0, y = 0, label = "823", size = 6, color = "black") +
  #geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = c("#FFCC66", "#66CCFF"))  # Soft colors

# Display the plot
print(p)

p <- ggplot(data, aes(x = 1, y = Percentage, fill = Sex)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(vjust = 0), color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white")) +  # Set background to white
  annotate("text", x = 0, y = 0, label = "882", size = 6, color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = c("#6699CC", "#CC99CC"))  # Soft colors



p <- ggplot(data, aes(x = 1, y = Percentage, fill = group)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(vjust = 0), color = "black", size = 1.5) +  # 加粗边框线条
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white")) +  # Set background to white
  annotate("text", x = 0, y = 0, label = "673", size = 6, color = "black", fontface = "bold") +  # 加粗中心文字
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 5, fontface = "bold") +  # 加粗标签文字
  scale_fill_manual(values = c("#6699CC", "#CC99CC"))  # Soft colors
# Save the plot with 20% reduction in size and white background
ggsave("donut_chart.png", plot = p, width = 4.8, height = 4.8, units = "in", dpi = 300)  # 20% reduction

# Display the plot
print(p)

# Create the data frame
data <- data.frame(
  group = c("Control", "Impulsivity"),
  Percentage = c(45.6, 54.4),
  Label = c("Control(45.6%)", "Impulsivity(54.4%)")
)

p <- ggplot(data, aes(x = 1, y = Percentage, fill = group)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(vjust = 0), color = "black", size = 1.5) +  # 加粗边框线条
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white")) +  # Set background to white
  annotate("text", x = 0, y = 0, label = "673", size = 6, color = "black") +
  #geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = c("#6699CC", "#CC99CC"))  # Soft colors

# Display the plot
print(p)
# Create the plot with donut chart, ring border, and gap at connection
p <- ggplot(data, aes(x = 1, y = Percentage, fill = group)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(vjust = 0), color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right", panel.background = element_rect(fill = "white")) +  # Set background to white
  annotate("text", x = 0, y = 0, label = "673", size = 6, color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = c("#6699CC", "#CC99CC"))  # Soft colors




# Save the plot with 20% reduction in size and white background
ggsave("donut_chart2.png", plot = p, width = 4.8, height = 4.8, units = "in", dpi = 300)  # 20% reduction

# Display the plot
print(p)

# 创建环形图
ggplot(data, aes(x = 1, y = Percentage, fill = group)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right") +
  annotate("text", x = 0, y = 0, label = "678", size = 6, color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = c("#6699CC", "#CC99CC"))  # 柔和的颜色


# 创建环形图
ggplot(data, aes(x = 1, y = Percentage, fill = group)) +
  geom_bar(width = 1, stat = "identity", position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  theme(legend.position = "right") +
  annotate("text", x = 0, y = 0, label = "678", size = 6, color = "black") +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "black", size = 4) +
  scale_fill_manual(values = c("#FFCC66", "#66CCFF"))  # 柔和的颜色



####绘制性别统计条形图
data <- read.csv("bl_sex1.csv",header = T)
# 将percent_weight列转换为数值型
data$percentage <- as.numeric(data$percentage)

library(ggplot2)
library(RColorBrewer)

# 创建一个颜色调色板
colors <- brewer.pal(9, "Set3")
#### 绘制性别统计条形图

p <- ggplot(data, aes(x = sex, y = percentage, fill = group)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +  # 缩小条形图宽度50%
  labs(title = "Unadjusted",  # 标题居中
       x = "Sex", y = "percentage(%)") +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题居中
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),  # 添加横坐标和纵坐标轴
        axis.ticks = element_line(color = "black"))

p + geom_text(aes(label = paste0(round(percentage), "%"), group = group),
              position = position_stack(vjust = 0.5), size = 3, color = "black")


#### 绘制性别统计条形图
data <- read.csv("bl_sex2.csv", header = TRUE)
data$percentage <- as.numeric(data$percentage)

library(ggplot2)
library(RColorBrewer)

# 创建一个颜色调色板
colors <- brewer.pal(9, "Pastel1")  # 使用柔和的颜色调色板

p <- ggplot(data, aes(x = sex, y = percentage, fill = group)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +  # 条形图宽度
  labs(title = "Adjusted",  # 标题居中
       x = "Sex", y = "percentage(%)") +
  scale_fill_manual(values = colors) +  # 使用不同颜色
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),  # 标题居中
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),  # 添加横坐标和纵坐标轴
        axis.ticks = element_line(color = "black"))

p + geom_text(aes(label = paste0(round(percentage), "%"), group = group),
              position = position_stack(vjust = 0.5), size = 3, color = "black")


#3.将两个性别条形图合并显示：
#library(gridExtra)
# 将两个图合并显示
#grid.arrange(p, p2, ncol = 2)

####################### 绘制倾向性评分的分布图 #########################


# 匹配前的分布
ggplot(df, aes(x = propensity_score, fill = as.factor(bl_group))) +  
  geom_density(alpha = 0.5) + # boundary参数确保条形图从0开始  
  scale_fill_lancet(name = "Violence risk", labels = c("No", "Yes"))+
  theme_minimal() + # 使用简洁的主题  
  theme(legend.text = element_text(size = 12,),legend.title = element_text(size=13),
        legend.position = c(0.85, 0.85), # 调整图例位置  
        axis.title = element_text(size = 16), # 调整坐标轴标题字体大小  
        axis.text = element_text(size = 14),
        plot.title = element_text(size=16)) + # 调整坐标轴文本字体大小  
  labs(x = "Propensity score", y = "Density", fill = "Violence risk") +  
  ggtitle("Propensity score distribution before matching")



# 匹配后的分布  
ggplot(df2, aes(x = propensity_score, fill = as.factor(bl_group))) +  
  geom_density(alpha = 0.5) + # boundary参数确保条形图从0开始  
  scale_fill_lancet(name = "Violence risk", labels = c("No", "Yes"))+
  theme_minimal() + # 使用简洁的主题  
  theme(legend.text = element_text(size = 12,),legend.title = element_text(size=13),
        legend.position = c(0.85, 0.85), # 调整图例位置  
        axis.title = element_text(size = 16), # 调整坐标轴标题字体大小  
        axis.text = element_text(size = 14),
        plot.title = element_text(size=16)) + # 调整坐标轴文本字体大小  
  labs(x = "Propensity score", y = "Density", fill = "Violence risk") +  
  ggtitle("Propensity score distribution after matching")

#设置工作环境
setwd("C:/Users/Administrator/Desktop/WORK/2.bl-pr/2.analysis/lmer")                #设置工作目录


############################ 变量处理 ######################################
# 所有涉及到模型的操作，需要对分类变量进行独热编码
# 有个很恶心的bug，R语言在读取和保存字符串时会时不时出现错误，字符串会出现“制表符”
# 一种办法是手工去去除每个分类变量的
# 另一种办法是用gsub
# 加载匹配后的数据集

mydata <- read.csv("bl_lmer.csv",header = T)  # 这里读取线性混合效应填补后的数据集

library(glmnet)
library(foreign)


df = mydata
attach(df)


# 示例
table(KET)
table(gsub("\t", "", KET))

table(UGLU)


# 独热编码（针对分类数≥3个的变量）
vars_to_onehot = c('UGLU', 'KET', 'PRO', 'BIL')
df[vars_to_onehot] <- lapply(df[vars_to_onehot], function(x) gsub("\t| $", "", x))   # 去除所有需要onehot变量的制表符与空格
attach(df)
table(KET)   # 我们发现制表符不见了，这样所有分类变量都是“干净的”


df_onehot <- df[, !(names(df) %in% vars_to_onehot)]  

for (col in vars_to_onehot) {  
  df[[col]] <- factor(df[[col]])
  # 使用model.matrix进行One-Hot编码  
  onehot_matrix <- model.matrix(~ df[[col]] - 1)  
  # 移除截距列（如果有的话），通常第一列是截距  
  onehot_matrix <- onehot_matrix[, -1]    
  # 重命名编码后的列，确保列名有意义  
  new_col_names <- paste(col, levels(df[[col]]), sep="_")  
  colnames(onehot_matrix) <- new_col_names[-1]  # 去除对应于截距的列名  
  # 将编码后的数据添加到df_onehot数据框中  
  df_onehot <- cbind(df_onehot, onehot_matrix)  
}

# 这个函数遍历上面定义的需要onehot的变量，并进行onehot
# 以UGLU为例，原始值中为阴性、1阳、2阳、3阳、4阳，那么就创建UGLU_阴性、UGLU_1阳...
# UGLU_阴性中为1则代表UGLU中的值为阴性...


# 将onehot后的数据集赋值为我们正在用的数据集
df = df_onehot
attach(df)


###################### LASSO回归 ##################################

# 只有LASSO回归的系数不为0，证明变量对自杀风险有预测价值，才进入后续的分析，即筛选掉无显著意义的变量
# 这里不用做敏感性分析





# 处理进入模型的自变量和因变量
# 输入的多分类变量因为onehot后的
# 这里挑了几个变量作为演示，需要根据索引把所有变量纳入，当然也有不用打字的方法，前期建议老老实实打字
data <- as.data.frame(df)

y<-as.matrix(data[,2])
x<-as.matrix(data[,c(5:86)])



f1 = glmnet(x, y, family="binomial", nlambda=100, alpha=1) 

print(f1)

plot(f1, xvar="lambda", label=TRUE)


cvfit=cv.glmnet(x,y)
plot(cvfit)



cvfit$lambda.min
cvfit$lambda.1se #求出最小值一个标准误的λ值


l.coef2<-coef(cvfit$glmnet.fit,s=0.00608916,exact = F)
l.coef1<-coef(cvfit$glmnet.fit,s=0.02458204,exact = F)
l.coef1
l.coef2



################################ 单因素回归 #########################################

# 除了上述多因素随机效应逻辑回归外，还做一个单因素回归分析
# 这里的变量是分别逐个单独拟合的
# 每个lasso系数不为0的变量跑一次

independent_variables <- c("HGB", "RBC", "RDW_CV","BAS","MON","PLT","P_LCR","PALB","HBsAb",
                           "ALP","TBIL","TBA","GLO","ALB","UREA","UA","APOB","K","GLU","KET_阴性")   # 替换为lasso回归后的变量
or_results <- data.frame(Variable = character(), 
                         OR = numeric(), CI_Lower = numeric(), 
                         CI_Upper = numeric(), p_value = numeric(), stringsAsFactors = FALSE)  # 存储采集到的OR
df$y = df$bl_group

#install.packages ("broom")
library(broom)
library(dplyr)
# 循环遍历每个自变量  
for (var in independent_variables) {  
  # 确保变量存在于数据框中  
  if (var %in% names(df)) {  
    # 建立单因素逻辑回归模型  
    formula <- as.formula(paste("y ~", var))  
    model <- glm(formula, data = df, family = binomial)  
    
    # 使用 broom 包获取模型系数并计算 OR 值及 CI  
    tidy_model <- tidy(model, conf.int = TRUE, conf.level = 0.95)  
    
    # 提取 OR 值及其 95% CI  
    or_row <- tidy_model %>%    
      filter(term == var) %>%    
      mutate(OR = exp(estimate),    
             CI_Lower = exp(conf.low),    
             CI_Upper = exp(conf.high),    
             p_value = p.value) %>%  # 添加 p 值列  
      select(term, OR, CI_Lower, CI_Upper, p_value)  # 选择需要的列  
    
    # 变量名重命名为 Variable  
    names(or_row)[names(or_row) == "term"] <- "Variable"  
    
    # 将结果追加到 or_results 数据框  
    or_results <- rbind(or_results, or_row)  
  } else {  
    cat("Variable", var, "does not exist in the dataframe.\n")  
  }  
}

or_results

# 指定路径将OR结果保存，随后填入表中，这里的OR是单变量的
write.csv(or_results, "or_results.csv")   


# 多因素模型分析
model <- glm(bl_group~HGB+RBC+RDW_CV+BAS+MON+PLT+P_LCR+PALB+HBsAb+ALP+TBIL+TBA+GLO+ALB+
               UREA+UA+APOB+K+GLU+KET_阴性, data=df, family=binomial())

fitSum<-summary(model)
ResultMul<-c()#准备空向量，用来储存结果
ResultMul<-rbind(ResultMul,fitSum$coef)
OR<-exp(fitSum$coef[,'Estimate'])
ResultMul<-cbind(ResultMul,cbind(OR,exp(confint(model))))

ResultMul

write.csv(ResultMul,file="Mul_log.csv")

# 性别与各指标之间的交互分析
library(rms)
library(interactions)

#HGB
mydata$sex<-as.factor(mydata$sex)
mydata$KET<-as.factor(mydata$KET)

fit2 <- lm(bl_group ~ HGB + sex + HGB * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = HGB,  # 自变量
              modx = sex)    # 调节变量

#RBC
fit2 <- lm(bl_group ~ RBC + sex + RBC * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = RBC,  # 自变量
              modx = sex)    # 调节变量

#RDW_CV
fit2 <- lm(bl_group ~ RDW_CV + sex + RDW_CV * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = RDW_CV,  # 自变量
              modx = sex)    # 调节变量

#BAS
fit2 <- lm(bl_group ~ BAS + sex + BAS * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = BAS,  # 自变量
              modx = sex)    # 调节变量

#MON
fit2 <- lm(bl_group ~ MON + sex + MON * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = MON,  # 自变量
              modx = sex)    # 调节变量

#PLT
fit2 <- lm(bl_group ~ PLT + sex + PLT * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = PLT,  # 自变量
              modx = sex)    # 调节变量

#P_LCR
fit2 <- lm(bl_group ~ P_LCR + sex + P_LCR * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = P_LCR,  # 自变量
              modx = sex)    # 调节变量

#PALB
fit2 <- lm(bl_group ~ PALB + sex + PALB * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = PALB,  # 自变量
              modx = sex)    # 调节变量

#HBsAb
fit2 <- lm(bl_group ~ HBsAb + sex + HBsAb * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = HBsAb,  # 自变量
              modx = sex)    # 调节变量

#ALP
fit2 <- lm(bl_group ~ ALP + sex + ALP * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = ALP,  # 自变量
              modx = sex)    # 调节变量

#TBIL
fit2 <- lm(bl_group ~ TBIL + sex + TBIL * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = TBIL,  # 自变量
              modx = sex)    # 调节变量

#TBA
fit2 <- lm(bl_group ~ TBA + sex + TBA * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = TBA,  # 自变量
              modx = sex)    # 调节变量

#GLO
fit2 <- lm(bl_group ~ GLO + sex + GLO * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = GLO,  # 自变量
              modx = sex)    # 调节变量

#ALB
fit2 <- lm(bl_group ~ ALB + sex + ALB * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = ALB,  # 自变量
              modx = sex)    # 调节变量

#UREA
fit2 <- lm(bl_group ~ UREA + sex + UREA * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = UREA,  # 自变量
              modx = sex)    # 调节变量

#UA
fit2 <- lm(bl_group ~ UA + sex + UA * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = UA,  # 自变量
              modx = sex)    # 调节变量

#APOB
fit2 <- lm(bl_group ~ APOB + sex + APOB * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = APOB,  # 自变量
              modx = sex)    # 调节变量

#K
fit2 <- lm(bl_group ~ K + sex + K * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = K,  # 自变量
              modx = sex)    # 调节变量

#GLU
fit2 <- lm(bl_group ~ GLU + sex + GLU * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = GLU,  # 自变量
              modx = sex)    # 调节变量

#KET
fit2 <- lm(bl_group ~ KET + sex + KET * sex, data = mydata)
summary(fit2)

interact_plot(fit2,
              pred = KET,  # 自变量
              modx = sex)    # 调节变量


# 创建数据
rates <- c(59.0, 62.7, 54.4)
groups <- c("Total", "Male", "Female")

# 创建颜色向量
colors <- c("#84A5C7", "#F18484", "#F8AD64")

# 创建条形图
barplot(rates, names.arg = groups, col = colors, ylim = c(0, 70),
        main = "Impulsivity Rates by Gender", ylab = "Occurrence Rate (%)", xlab = "Sex",
        border = "black", space = 1.0, cex.names = 0.9, font.axis = 2, font.lab = 2)

# 添加0轴的横坐标轴线
abline(h = 0, col = "black")

