####数据提取1####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("original data/mortality/final_mortality.rds")

# 设置数据路径
path <- "original data/DEMO"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN",
                    "RIDAGEYR",
                    "RIAGENDR",
                    "DMDMARTL",
                    "DMDEDUC2",
                    "RIDRETH1")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-1.rds') #保存 rds


####数据提取2####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-1.rds")

# 设置数据路径
path <- "original data/SMQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN","SMQ040")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-2.rds') #保存 rds





####数据提取3####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-2.rds")

# 设置数据路径
path <- "original data/ALQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN","ALQ120Q")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-3.rds') #保存 rds






####数据提取4####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-3.rds")

# 设置数据路径
path <- "original data/PAQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[,c("SEQN","PAQ500")]
###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(list(final_mortality,merged),
                   by = c("SEQN"),
                   type = 'full')
merged <- merged[!duplicated(merged$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged,file = '提取数据/提取数据-4.rds') #保存 rds



####数据提取5####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-4.rds")

# 设置数据路径
path <- "original data/RDQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","RDQ100")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-5.rds') #保存 rds




####数据提取6####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-5.rds")

# 设置数据路径
path <- "original data/BPQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","BPQ100C")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-6.rds') #保存 rds




####数据提取7####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-6.rds")

# 设置数据路径
path <- "original data/DIQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","DIQ010")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-7.rds') #保存 rds





####数据提取8####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-7.rds")

# 设置数据路径
path <- "original data/MCQ"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","MCQ220",
                     "MCQ160F",
                     "MCQ160C",
                     "MCQ160B",
                     "MCQ160E",
                     "MCQ160D")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-8.rds') #保存 rds





####数据提取9####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-8.rds")

# 设置数据路径
path <- "original data/BMX"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","BMXHT",
                     "BMXWT",
                     "BMXBMI",
                     "BMXWAIST")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-9.rds') #保存 rds




####数据提取10####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-9.rds")

# 设置数据路径
path <- "original data/BPX"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","BPXSY3",
                     "BPXDI3")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-10.rds') #保存 rds



####数据提取11####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-10.rds")

# 设置数据路径
path <- "original data/Glycohemoglobin"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
merged0 <- merged[,c("SEQN","LBXGH")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-11.rds') #保存 rds



####数据提取12####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-11.rds")

# 设置数据路径
path <- "original data/Cholesterol"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXTC",
                     "LBDHDL",
                     "LBDLDL",
                     "LBXTR"
                     )]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-12.rds') #保存 rds



####数据提取13####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-12.rds")

# 设置数据路径
path <- "original data/CRP"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXCRP")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-13.rds') #保存 rds



####数据提取14####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-13.rds")

# 设置数据路径
path <- "original data/Glucose"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXGLU")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-14.rds') #保存 rds



####数据提取15####
library(foreign)#读取xpt
library(tidyverse)#文件整理
library(dplyr)
library(openxlsx)
library(plyr)
library(nhanesA)
library(RNHANES)
rm(list=ls())
final_mortality <- readRDS("提取数据/提取数据-14.rds")

# 设置数据路径
path <- "original data/UA"
# 获取目录中的所有文件名，并返回完整的文件路径
fileNames <- list.files(path, full.names = TRUE)
# 使用lapply读取所有XPT文件
data <- lapply(fileNames, function(x) {
  tryCatch({
    read.xport(x, header = TRUE)
  }, error = function(e) {
    message("Error reading file: ", x)
    NULL  # 如果读取失败，返回NULL
  })
})

###去掉没有SEQN的文件之后重新读取list，然后使用join all合并所有文件进行后续分析
merged <- join_all(data,
                   by = c("SEQN"),
                   type = 'full')
colnames(merged)
merged0 <- merged[,c("SEQN","LBXSUA")]
merged1 <- join_all(list(final_mortality,merged0),
                    by = c("SEQN"),
                    type = 'full')
merged1 <- merged1[!duplicated(merged1$SEQN),]
if(! dir.exists("提取数据")){dir.create("提取数据")}
saveRDS(merged1,file = '提取数据/提取数据-15.rds') #保存 rds
library(openxlsx)
write.xlsx(merged1,"提取数据/final data.xlsx")
###导出之后，xlsx中处理一下

####数据清洗为标准格式####
rm(list=ls())
data <- readRDS('提取数据/提取数据-15.rds')
data <- subset(data,eligstat=='1')
# 使用mutate和ifelse函数来创建新列CVD_Death，并根据条件标记1或0
library(dplyr)
data <- data %>% filter(ucod_leading %in% c(1, 5))
data <- data[,-(4:8)]
data <- data[,-(1:2)]
colnames(data)[2:33] <- c("Time","Age",
                          "Gender",
                          "Marital status",
                          "Education level",
                          "Race",
                          "Smoking",
                          "Days of drinking in the last year",
                          "Activity comparison last year",
                          "Chest sound wheezy during exercise",
                          "Now increasing exercise",
                          "Diabetes",
                          "Cancer",
                          "Stroke",
                          "Coronary heart disease",
                          "Congestiveheart failure",
                          "Myocardial infarction/heart attack",
                          "Angina pectoris",
                          "Standing Height (cm)",
                          "Weight (kg)",
                          "BMI (kg/m2) ",
                          "Waist Circumference (cm)",
                          "Systolic blood pressure (mmHg)",
                          "Diastolic blood pressure  (mmHg)",
                          "Glycated haemoglobin/Glyco hemoglobin/Glycohemoglobin (%)",
                          "Cholesterol (mg/dL)",
                          "HDL-cholesterol (mg/dL)",
                          "LDL-cholesterol (mg/dL)",
                          "Triglycerides (mg/dL)",
                          "C-reactive protein (mg/dL)",
                          "Glucose (mg/dL)",
                          "Uric acid (mg/dL)")

####二分类变量替换####
#colnames(data) <- gsub(" ", ".", colnames(data))
library(dplyr)

# 假设你的数据框名为data

# Gender列替换
data <- data %>%
  mutate(Gender = case_when(
    Gender == 1 ~ "1_Male",
    Gender == 2 ~ "2_Female",
    TRUE ~ as.character(Gender) # 保持其他值不变
  ))

# Marital Status列替换
marital_status_map <- c("1" = "1_Married", "2" = "2_Widowed", "3" = "3_Divorced", 
                        "4" = "4_Separated", "5" = "5_Never married", 
                        "6" = "6_Living with partner", "77" = "77_Refused", 
                        "99" = "99_Don't know")
data <- data %>%
  mutate(`Marital status` = marital_status_map[as.character(`Marital status`)])

# Education level列替换
education_level_map <- c("1" = "1_Less Than 9th Grade", 
                         "2" = "2_9-11th Grade (Includes 12th grade with no diploma)", 
                         "3" = "3_High School Grad/GED or Equivalent", 
                         "4" = "4_Some College or AA degree", 
                         "5" = "5_College Graduate or above", 
                         "7" = "7_Refused", "9" = "9_Don't Know")
data <- data %>%
  mutate(`Education level` = education_level_map[as.character(`Education level`)])

# Race列替换
race_map <- c("1" = "1_Mexican American", "2" = "2_Other Hispanic", 
              "3" = "3_Non-Hispanic White", "4" = "4_Non-Hispanic Black", 
              "5" = "5_Other Race - Including Multi-Racial")
data <- data %>%
  mutate(Race = race_map[as.character(Race)])

# Smoking列替换
smoking_map <- c("1" = "1_Every day", "2" = "2_Some days", "3" = "3_Not at all", 
                 "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Smoking` = smoking_map[as.character(`Smoking`)])

# Days of drinking in the last year列去除777和999的行
#data <- data %>%
#  filter(`Days of drinking in the last year` != 777 & `Days of drinking in the last year` != 999)

# Activity comparison last year列替换
activity_map <- c("1" = "1_more active", "2" = "2_less active, or", 
                  "3" = "3_About the same?", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Activity comparison last year` = activity_map[as.character(`Activity comparison last year`)])

# Chest sound wheezy during exercise列替换
wheezy_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Chest sound wheezy during exercise` = wheezy_map[as.character(`Chest sound wheezy during exercise`)])

# Now increasing exercise列替换
exercise_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Now increasing exercise` = exercise_map[as.character(`Now increasing exercise`)])

# Diabetes列替换
diabetes_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Diabetes` = diabetes_map[as.character(`Diabetes`)])

# Cancer列替换
cancer_map <- c("1" = "1_Yes","2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Cancer` = cancer_map[as.character(`Cancer`)])

# Stroke列替换
stroke_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Stroke` = stroke_map[as.character(`Stroke`)])

# Coronary heart disease列替换
chd_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Coronary heart disease` = chd_map[as.character(`Coronary heart disease`)])

# Congestive heart failure列替换
chf_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Congestiveheart failure` = chf_map[as.character(`Congestiveheart failure`)])

# Myocardial infarction/heart attack列替换
mi_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Myocardial infarction/heart attack` = mi_map[as.character(`Myocardial infarction/heart attack`)])

# Angina pectoris列替换
angina_map <- c("1" = "1_Yes", "2" = "2_No", "7" = "7_Refused", "9" = "9_Don't know")
data <- data %>%
  mutate(`Angina pectoris` = angina_map[as.character(`Angina pectoris`)])

# 将第4列到第19列中的NA替换为"99_NA"
for (i in 4:19) {
  data[[i]] <- ifelse(is.na(data[[i]]), "99_NA", data[[i]])
}

saveRDS(data,"Result/清洗但未插补的数据.RDS")
