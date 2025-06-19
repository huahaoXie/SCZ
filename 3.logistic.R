#设置工作环境
setwd("C:/Users/Administrator/Desktop/WORK/2.bl-pr/2.analysis/5.logistic")                #设置工作目录

#加载数据
data=read.csv("data.csv",header = T)          #读取文件

# 查看变量名称
names(data)

# 查看基本统计信息
summary(data)

# 查看变量类型
str(data)

# 标明连续型变量和分类型变量
contin_vars <- c("BAS", "MON", "PDW", "TBIL","TBA","GLO","ALB","UA","HDL_C","Na","K")
discre_vars <- c( "UT", "KET")

# 处理分类变量
data$bl_group <- factor(data$bl_group, levels=c(0, 1), 
                          labels = c("无暴力风险", "暴力风险"))

data[discre_vars] <- lapply(data[discre_vars], factor)


#Step 2 单因素分析

# 对连续变量进行 t 检验
# 对分类变量进行卡方检验
library(tableone)
table1 <- CreateTableOne(vars=c(contin_vars, discre_vars),
                         data=data,
                         factorVars=discre_vars,
                         strata="bl_group", addOverall=FALSE)
result1 <- print(table1, showAllLevels=FALSE)
write.csv(result1, "result1.csv")

# 单因素 logistic
# 如果存在分类变量，需要设置哑变量

# 自变量: 连续变量

model <- glm(bl_group~BAS, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))


model <- glm(bl_group~MON, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~PDW, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~TBIL, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~TBA, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~GLO, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~ALB, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~UA, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~HDL_C, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~Na, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~K, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~KET, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))

model <- glm(bl_group~UT, data=data, family=binomial())
summary(model)
summary(model)$coefficients
# 计算 OR 及其可信区间
exp(cbind("OR"=coef(model), confint(model)))


# 多因素模型分析
model <- glm(bl_group~BAS+MON+PDW+TBIL+TBA+GLO+ALB+
               PALB+UA+HDL_C+Na+K+UT+KET, data=data, family=binomial())

fitSum<-summary(model)
ResultMul<-c()#准备空向量，用来储存结果
ResultMul<-rbind(ResultMul,fitSum$coef)
OR<-exp(fitSum$coef[,'Estimate'])
ResultMul<-cbind(ResultMul,cbind(OR,exp(confint(model))))

ResultMul

write.csv(ResultMul,file="Mul_log.csv")



# 更细节地优化一波
# 创建森林图 (forest plot) 并存储在 fig3_1 变量中

res_mul_cox <- read.csv("Mul_forest.csv",header = T)  
res_mul_cox <- as.data.frame(res_mul_cox)
res_mul_cox
#                                   p.value  HR  low high
# Age                               2.8e-09 1.0 1.00  1.0
# IDH_statusWildtype                1.6e-06 3.0 1.90  4.7
# MGMT_promoter_statusUn-methylated 9.5e-02 1.3 0.95  1.8
# StageWHO III                      2.1e-04 2.2 1.40  3.3
# StageWHO IV                       2.1e-07 4.1 2.40  6.9


library(forestplot)


fig <- forestplot(
  res_mul_cox[, c(1, 2, 6)],           # 需要显示在森林图中的列
  mean = res_mul_cox[, 3],             # 均值列（HR），它将显示为森林图的小方块或其他形状哈哈哈哈哈
  lower = res_mul_cox[, 4],            # 95%置信区间的下限数据列
  upper = res_mul_cox[, 5],            # 95%置信区间的上限数据列
  zero = 1,                        # 均值为1时的参考线，也就是零线
  boxsize = 0.2,                   # 方框的大小
  graph.pos = "right",             # 森林图在右侧显示
  hrzl_lines = list(               # 水平线样式的设置
    "1" = gpar(lty = 1, lwd = 2),  # 均值线
    "2" = gpar(lty = 2),           # 下限和上限之间的虚线
    "16" = gpar(lwd = 2, lty = 1, columns = c(1:4)) # 下限和上限线
  ),
  graphwidth = unit(.25, "npc"),   # 森林图的宽度
  #xlab = "\n 插播一条广告！\n 要知道出品，必属精品！", # x轴标签
  xticks = c(-0.5, 1, 3, 5), # x轴刻度
  # 判断是否为汇总行，汇总行就是连续变量或者分类变量名所在的行，可以加粗让它显眼一点，好看的！
  is.summary = c(T, F, F, F, F, F, F, F, F, F, F, F,F, F, F),  
  txt_gp = fpTxtGp(                # 文本样式的设置
    label = gpar(cex = 0.8),       # 标签的大小
    ticks = gpar(cex = 1),         # 刻度标记的大小
    xlab = gpar(cex = 0.9),        # x轴标签的大小
    title = gpar(cex = 1.2)        # 标题的大小
  ),
  lwd.zero = 1,                    # 均值线的宽度
  lwd.ci = 1.5,                    # 置信区间线的宽度
  lwd.xaxis = 2,                   # x轴线的宽度
  lty.ci = 1.5,                    # 置信区间线的样式
  ci.vertices = F,                 # 是否显示置信区间的顶点
  ci.vertices.height = 0.2,        # 置信区间顶点的高度
  clip = c(0.1, 8),                # x轴的截断范围
  ineheight = unit(2, 'mm'),       # 森林图内线的高度
  line.margin = unit(2, 'mm'),     # 森林图线的边距
  colgap = unit(16, 'mm'),          # 森林图中方框的间距
  fn.ci_norm = "fpDrawDiamondCI",  # 置信区间的形状（这里是钻石形）
  title = "多因素logistic回归森林图",   # 森林图的标题
  col = fpColors(                  # 颜色设置
    box = "blue4",                 # 方框颜色
    lines = "blue4",               # 线条颜色
    zero = "black"                 # 均值为0时的颜色
  )
)

fig
