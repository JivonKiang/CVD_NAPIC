#### PSM前数据准备 ####
# 加载必要的库
library(MatchIt)
library(openxlsx)
library(dplyr)

rm(list = ls())

# 读取数据
data <- read.xlsx("original data/替换好变量名的数据.xlsx", sheet = "随访时间清洗好了")
data[,4:16] <- data.frame(lapply(data[,4:16], function(x) ifelse(is.na(x), "99_NA", x)))
# 对col1列进行处理
data$Drinking <- ifelse(is.na(data$Drinking), "99_NA", data$Drinking)
# 对Sex变量进行处理，将缺失值替换为"99_NA"
data$Sex <- ifelse(data$Sex == 1, "Male",
                   ifelse(data$Sex == 2, "Female",data$Sex))

# 对Marital.status变量进行处理，将缺失值替换为"99_NA"
data$Marital.status <- ifelse(data$Marital.status == 1, "1_married ",
                              ifelse(data$Marital.status == 3, "3_partnered ",
                                     ifelse(data$Marital.status == 4, "4_separated",
                                            ifelse(data$Marital.status == 5, "5_divorced ",
                                                   ifelse(data$Marital.status == 7, "7_widowed ",
                                                          ifelse(data$Marital.status == 8, "8_never marriedd",data$Marital.status))))))

# 对Education.level变量进行处理，将缺失值替换为"99_NA"
data$Education.level <- ifelse(data$Education.level == 1, "1_No formal education illiterate",
                               ifelse(data$Education.level == 2, "2_Did not finish primary school but capa",
                                      ifelse(data$Education.level == 3, "3_Sishu",
                                             ifelse(data$Education.level == 4, "4_Elementary school",
                                                    ifelse(data$Education.level == 5, "5_Middle school",
                                                           ifelse(data$Education.level == 6, "6_High school",
                                                                  ifelse(data$Education.level == 7, "7_Vocational school",
                                                                         ifelse(data$Education.level == 8, "8_Two/Three Year College/Associate degree",
                                                                                ifelse(data$Education.level == 9, "9_Four Year College/Bachelor's degree",
                                                                                       ifelse(data$Education.level == 10, "10_Post-graduated(Master/PhD)",data$Education.level))))))))))

# 对Hukou.status变量进行处理，将缺失值替换为"99_NA"
data$Hukou.status <- ifelse(data$Hukou.status == 1, "1_Agricultural hukou",
                            ifelse(data$Hukou.status == 2, "2_Non-agricultural hukou",
                                   ifelse(data$Hukou.status == 3, "3_Unified residence hukou",
                                          ifelse(data$Hukou.status == 4, "4_Do not have hukou",data$Hukou.status))))

# 对Smoking变量进行处理，将缺失值替换为"99_NA"
data$Smoking <- ifelse(data$Smoking == 0, "0_No",
                       ifelse(data$Smoking == 1, "1_Yes",data$Smoking))

# 对Drinking变量进行处理，将缺失值替换为"99_NA"
data$Drinking <- ifelse(data$Drinking == 0, "0_None",
                        ifelse(data$Droking == 1, "1_Less than once a month",
                               ifelse(data$Drinking == 2, "2_Once a month",
                                      ifelse(data$Drinking == 3, "3_2 to 3 daysa month",
                                             ifelse(data$Drinking == 4, "4_Once a week",
                                                    ifelse(data$Drinking == 5, "5_2 to 3 days a week",
                                                           ifelse(data$Drinking == 6, "6_4 to 6 days a week",
                                                                  ifelse(data$Drinking == 7, "7_Daily",
                                                                         ifelse(data$Drinking == 8, "8_Twice a day",
                                                                                ifelse(data$Drinking == 9, "9_More than twice a day",data$Drinking))))))))))

# 对Cancer变量进行处理，将缺失值替换为"99_NA"
data$Cancer <- ifelse(data$Cancer == 0, "0_No",
                      ifelse(data$Cancer == 1, "1_Yes",data$Cancer))

# 对Diabetes变量进行处理，将缺失值替换为"99_NA"
data$Diabetes <- ifelse(data$Diabetes == 0, "0_No",
                        ifelse(data$Diabetes == 1, "1_Yes",data$Diabetes))

# 对Stroke变量进行处理，将缺失值替换为"99_NA"
data$Stroke <- ifelse(data$Stroke == 0, "No",
                      ifelse(data$Stroke == 1, "1_Yes",data$Stroke))

# 对Heart.disease变量进行处理，将缺失值替换为"99_NA"
data$Heart.disease <- ifelse(data$Heart.disease == 0, "0_No",
                             ifelse(data$Heart.disease == 1, "1_Yes",data$Heart.disease))

####PSM####
# 数据清洗，移除有缺失值的行
data <- data %>%
  filter(!is.na(Age), !is.na(Sex))
# 使用subset函数去掉Time列中小于等于0的行
data <- subset(data, Time > 0)
data$Drinking <- ifelse(is.na(data$Drinking), "99_NA", data$Drinking)
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
final_data$Drinking <- ifelse(is.na(final_data$Drinking), "99_NA", final_data$Drinking)
# 保存匹配后的数据到Excel文件
write.xlsx(final_data, "original data/matched_data.xlsx")
