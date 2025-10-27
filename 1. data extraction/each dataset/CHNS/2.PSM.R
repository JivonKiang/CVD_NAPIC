#### PSM前数据准备 ####
# 加载必要的库
library(MatchIt)
library(openxlsx)
library(dplyr)

rm(list = ls())

# 读取数据
data <-read.xlsx("original data/等待插补的数据.xlsx")
####PSM####
# 数据清洗，移除有缺失值的行
colnames(data)[4] <- c("Sex")
data <- data %>%
  filter(!is.na(Age), !is.na(Sex))
# 使用subset函数去掉Time列中小于等于0的行
data <- subset(data, Time > 0)
write.xlsx(data, "original data/matched_before_data.xlsx")###

# 检查列名是否存在
colnames(data)

# 计算倾向性评分模型
psModel <- glm(Status ~ Age + Sex, 
               family = binomial(link = "logit"), 
               data = data)

# 确定实验组的数量
n_treatment <- sum(data$Status == "1")

# 执行匹配
m.out <- matchit(Status ~ Age + Sex, 
                 data = data, 
                 method = "nearest", ratio = 30)

# 查看匹配结果
summary(m.out)

# 提取匹配后的数据
final_data <- match.data(m.out)

# 将Status变量转换为因子
final_data$Status <- as.factor(final_data$Status)

# 查看匹配后的数据摘要
summary(final_data)
# 保存匹配后的数据到Excel文件
write.xlsx(final_data, "original data/matched_data.xlsx")
