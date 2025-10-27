# 安装并加载所需包
#install.packages("MatchIt")
#install.packages("cobalt")
library(MatchIt)
library(cobalt)
library(openxlsx)
library(autoReg)
library(survival)
rm(list = ls())
data <- read.xlsx("CHARLS.xlsx")
str(data)
# 获取第三列之后的所有列名
colnames <- colnames(data)[-c(1, 2)]
colnames(data)[-c(1, 2)] <- paste0("var", 3:ncol(data))

# 假设你想将列名从var3到var29的列转换为因子型变量
factor_columns <- paste0("var", 4:16)
# 将这些列转换为因子型变量
data[factor_columns] <- lapply(data[factor_columns], factor)
summary(data)

fit <- coxph(Surv(Time, as.numeric(Status)) ~ var3 + var4 + var5  + var7 + var8 + var9 + var10 
                     + var11 + var12 + var13 + var14 + var15 + var16 + var17 + var18 + var19 + var20 
                     + var21 + var22 + var23 + var24 + var25 + var26 + var27 + var28 + var29, data = data)

# 拟合Cox比例风险模型
fit <- glm(formula,data=data,family="binomial")

# 查看模型摘要
summary(fit)
#autoReg解析
autoReg(fit)


# 计算倾向得分
ps_model <- glm(Status ~ age + gender + income, data = data, family = binomial())

# 进行匹配
m.out <- matchit(treatment ~ age + gender + income, data = data, method = "nearest", distance = "glm")

# 匹配后的平衡性检验
summary(m.out)
bal.tab(m.out, thresholds = c(m = 0.1, v = 2))

# 提取匹配后的数据
matched_data <- match.data(m.out)

# 进行后续分析，例如使用t.test进行独立样本t检验
t.test(result_variable ~ treatment, data = matched_data)

