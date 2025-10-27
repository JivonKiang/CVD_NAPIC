####---导入数据---####
#读取package
{library(survival)
  library(rms)
  library(tidyverse)
  library(caret)
  library(glmnet)
  library(openxlsx)
  library(data.table)
  library(tidyverse)
  library(caret)
  library(openxlsx)
  library(data.table)
  library(mlbench)
  library(klaR)
  library(tidyverse)
  library(caret)
  library(glmnet)
  library(openxlsx)
  library(data.table)
  library(mlbench)
  library(ROCR)
  library(pROC)
  library(multipleROC)
  library(pROC)
  library(DALEX)
  library(randomForest)
  library(dplyr)
  library(survival)
  library(survminer)
}
##读取数据
rm(list=ls())
library(openxlsx)
#载入数据
# 读取数据
data1 <- read.xlsx("Result/插补后去除na的数据.xlsx")
data1 <- data1[,-18]###删除最后一列常数项
####绘制基线调查表####
library(autoReg)
library(moonBook)
library(rrtable)
colnames(data1)
data1$Status <- as.factor(data1$Status)
data1$Sex <- as.factor(data1$Sex)
data1$marital_status <- as.factor(data1$marital_status)
data1$ethnicity <- as.factor(data1$ethnicity)
summary(data1)
moonbook_result <- mytable(Status~., data=data1, digits=2, method=3,
                           #3表示首先执行Shapiro-Wilk检验，由检验结果决定按正态还是非正态对数据进行分析。
                           catMethod=0, show.total=TRUE)

table2docx(moonbook_result[["res"]],"Result/moonbook_Baseline.docx")
if(! dir.exists("Result")){dir.create("Result")}
write.xlsx(moonbook_result[["res"]],"Result/moonbook_Baseline.xlsx")

#subgroup_result <- mytable(科室~., data, digits=2, method=3,
                           #3表示首先执行Shapiro-Wilk检验，由检验结果决定按正态还是非正态对数据进行分析。
#                           catMethod=0, show.total=TRUE)
#table2docx(subgroup_result,"Result/subgroup_result.docx")
#write.xlsx(subgroup_result,"Result/科室.xlsx")

#逻辑模型拟合
#data$出院方式 <- as.factor(data$出院方式)
#data <- data[,-(5:6)]
#data <- data[,-18]
#data <- data[,-1]###去除SEQN
library(survival)

mod <- coxph(Surv(Time, Status) ~ .,
             data = data1#, id = PatientID
             )
summary(mod)
#多元逻辑回归
#多元逻辑回归 <- autoReg(mod,threshold=0.05)
#write.xlsx(多元逻辑回归,"Result/多元逻辑回归.xlsx")

#单变量和多元逻辑回归（单变量向后选择）
单变量和多元逻辑回归 <- autoReg(mod,
                      uni=T,#逻辑值，是否执行单变量回归。默认为FALSE。
                      #multi=T,#逻辑值，是否执行多变量回归。默认为TRUE。
                      final=T,#逻辑值，是否执行逐步向后消除法。默认为FALSE。
                      #imputed=T,#逻辑值，是否执行多重插补。默认为FALSE。
                      threshold=0.05#,#数值，用于自动选择变量的p值阈值。默认为0.2。
                      #keepstats=T,#逻辑值，是否保留统计数据。默认为FALSE。
                      #showstats=T#,逻辑值，是否显示描述性统计数据。默认为TRUE。
                      )
write.xlsx(单变量和多元逻辑回归,"Result/单变量和多元逻辑回归.xlsx")
#查看模型的统计量
模型的统计量 <- gaze(mod)
write.xlsx(模型的统计量,"Result/模型的统计量.xlsx")
#模型可视化-绘制森林图
modelPlot(mod)
#保存图片
# 1. 创建画布
png( 
  filename = "Result/Forest plot.jpg", # 文件名称
  width = 7000,           # 宽
  height = 4000,          # 高
  units = "px",          # 单位
  bg = "white",          # 背景颜色
  res = 600)              # 分辨率
# 2. 绘图
modelPlot(final_mod)
# 3. 关闭画布
dev.off()
