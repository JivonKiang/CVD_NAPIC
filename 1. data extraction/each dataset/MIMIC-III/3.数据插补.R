####数据提取1####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
library(VIM)
library(pacman)
p_load(tidyverse) # 数据科学套件
p_load(mice) # 缺失值分析包
p_load(VIM)
rm(list=ls())
library(openxlsx)
varechem <- read.xlsx("original data/matched_data.xlsx")
#varechem <- varechem %>% filter(!is.na(varechem$`triglycerides`))
#varechem <- varechem %>% filter(!is.na(varechem$`ldl_cholesterol`))
varechem <- varechem %>% filter(!is.na(varechem$`glucose`))
varechem <- varechem %>% filter(!is.na(varechem$`hdl_cholesterol`))
varechem <- varechem %>% filter(!is.na(varechem$`weight_kg`))
# 数据检视
varechem %>% glimpse()
# 自定义缺失率计算函数
var_na_ratio <- function(x){return(mean(is.na(x)))}
# 变量缺失率
var_na_ratio <- apply(varechem, 2, var_na_ratio)
var_na_ratio1 <- as.data.frame(var_na_ratio)
rownames <- rownames(var_na_ratio1)
var_na_ratio2 <- cbind(var_na_ratio1,rownames)
var_na_ratio3 <- var_na_ratio2[which(var_na_ratio2$var_na_ratio<0.3),]#缺失率要低于0.3
eligble_var <- var_na_ratio3[,2]#提取合格变量名称
library(tidyverse)
varechem_select <- varechem %>% dplyr::select(eligble_var)#匹配到原始数据中
#保存筛选的合格变量数据集
if(! dir.exists("Result")){dir.create("Result")}
saveRDS(varechem_select,file = 'Result/varechem_select.rds') #保存 rds
###将变量名替换为var_1，再备份一下原本的名称，留着再替换回来

# 1. 存储原始变量名称
old_colnames <- colnames(varechem_select)

# 2. 将变量名更改为index_1到index_33
new_colnames <- paste0("index_", 1:20)
colnames(varechem_select) <- new_colnames

# 3. 进行多重插补法补全数据
library(mice)
imp_data <- mice(varechem_select, 
                 method = "rf", 
                 m = 5, 
                 printFlag = F)

# 4. 储存完整数据
completedData <- complete(imp_data, 1)

# 5. 恢复原始变量名称
colnames(completedData) <- old_colnames

sum(is.na(completedData))#统计缺失值总数
if(! dir.exists("Result")){dir.create("Result")}
write.xlsx(completedData,"Result/插补后的数据.xlsx")
na_omit <- na.omit(completedData)
write.xlsx(completedData,"Result/插补后去除na的数据.xlsx")
###打开最后一个xlsx，进行指标的处理
completedData$Status <- as.factor(completedData$Status)
summary(completedData)
