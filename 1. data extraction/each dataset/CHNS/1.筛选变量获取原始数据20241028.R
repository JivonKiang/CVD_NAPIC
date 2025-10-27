library(openxlsx)
library(dplyr)
library(stringr)
library(haven)
rm(list = ls())
data1 <- read_sas("老版数据20231126//sas7bdat//hlth_12.sas7bdat")
data2 <- read_sas("老版数据20231126//sas7bdat//biomarker_09.sas7bdat")
data3 <- read_sas("老版数据20231126//sas7bdat//MAST_pub_12.sas7bdat")
#data4 <- read_sas("老版数据20231126//sas7bdat//en_00.sas7bdat")
data5 <- read_sas("老版数据20231126//sas7bdat//wed_12.sas7bdat")
data6 <- read_sas("老版数据20231126//sas7bdat//educ_12.sas7bdat")
data7 <- read_sas("老版数据20231126//sas7bdat//pexam_pub_12.sas7bdat")
data8 <- read_sas("老版数据20231126//sas7bdat//pact_12.sas7bdat")

data8 <- data8[,c("IDind","WAVE","U142_MN",
                  "U141_MN",
                  "U140_MN")]
colnames(data8)[2] <- c("WAVE1")

data7 <- data7[,c("IDind","WAVE",
                  "U25","U41","U24W","U24A","U24L",
                  "U24J","HEIGHT","WEIGHT","U10","SYSTOL3",
                  "DIASTOL3")]
colnames(data7)[2] <- c("WAVE2")

data6 <- data6[,c("IDind","WAVE","A12")]
colnames(data6)[2] <- c("WAVE3")

data5 <- data5[,c("IDind","wave","S1")]
colnames(data5)[2] <- c("WAVE4")
#data4 <- data4[,c("IDind","WAVE","A12")]

colnames(data3)[1] <- c("IDind")
data3 <- data3[,c("IDind",#"WAVE",
                  "WEST_DOB_Y",
                  "DOD",
                  "CAUSE",
                  "NATIONALITY","GENDER")]

data2 <- data2[,c("IDind","wave",
                  "TG_MG","HDL_C","LDL_C","HS_CRP","TC_MG",
                  "GLUCOSE_MG","UA")]
colnames(data2)[2] <- c("WAVE5")

data1 <- data1[,c("IDind","wave","M40","M24B_7")]
colnames(data1)[2] <- c("WAVE6")

library(dplyr)
library(plyr)
#data1 <- subset(data1,wave=="2009")

data <- left_join(data2,data1,by="IDind")
data <- left_join(data,data3,by="IDind")
#data <- left_join(data,data4,by="IDind")
data <- left_join(data,data5,by="IDind")
data <- left_join(data,data6,by="IDind")
data <- left_join(data,data7,by="IDind")
data <- left_join(data,data8,by="IDind")
saveRDS(data,"original data/data.RDS")
rm(list = ls())
data <- readRDS("original data/data.RDS")
colnames(data)

library(dplyr)
library(purrr)
interview_year <- data[,c("IDind","WAVE1","WAVE2","WAVE3","WAVE4","WAVE5","WAVE6")]
# 提取每个样本的Endyear（wave中的最大值）
# 计算每一行的最大值和最小值
max_values <- apply(interview_year[,-1], 1, max, na.rm = TRUE)
min_values <- apply(interview_year[,-1], 1, min, na.rm = TRUE)
# 将结果合并到一个新的数据框中
sample_interview <- data.frame(IDind = interview_year$IDind,Startyear = min_values,
                     Endyear = max_values)
saveRDS(sample_interview,"sample_interview.RDS")

# 对IDind进行去重，保留Startyear的最小值和Endyear的最大值
result <- sample_interview %>%
  group_by(IDind) %>%
  summarise(
    Startyear = min(Startyear, na.rm = TRUE),
    Endyear = max(Endyear, na.rm = TRUE)
  )
saveRDS(result,"sample_interview_result.RDS")

# 去除可能存在的重复行（根据IDind、Startyear和Endyear判断）
sample_interview <- distinct(sample_interview)

#sample_interview <- na.omit(sample_interview)
saveRDS(sample_interview,"sample_interview.RDS")
sample_interview <- readRDS("sample_interview.RDS")
data_without_interview <- data[data$`WAVE1`=='2009' & 
                                 data$`WAVE2`=='2009' & 
                                 data$`WAVE3`=='2009' & 
                                 data$`WAVE4`=='2009' & 
                                 data$`WAVE5`=='2009' & 
                                 data$`WAVE6`=='2009',]
saveRDS(data_without_interview,"data_without_interview.RDS")
data_without_interview <- readRDS("data_without_interview.RDS")
data_without_interview <- subset(data_without_interview,IDind!='NA')
#data_without_interview <- data[,-c(2,5,13,15,17,27)]
data <- left_join(data_without_interview,sample_interview,by='IDind')
colnames(data)
whole_data <- data[,c("IDind","Startyear" ,"Endyear" ,
                      "M40",
                      
                      "WEST_DOB_Y",
                      "DOD",
                      "CAUSE",
                      
                      
                      
                      
                      "GENDER",
                      "S1",
                      "A12",
                      "NATIONALITY",
                      "U25",
                      "U41",
                      "U142_MN",
                      "U141_MN",
                      "U140_MN",
                      "U24W",
                      "U24A",
                      "U24L",
                      
                      "M24B_7",
                      
                      "U24J",
                      
                      "HEIGHT",
                      "WEIGHT",
                      
                      "U10",
                      "SYSTOL3",
                      "DIASTOL3",
                      
                      "TG_MG",
                      "HDL_C",
                      "LDL_C",
                      "HS_CRP",
                      "TC_MG",
                      "GLUCOSE_MG",
                      "UA")]
colnames(whole_data) <- c("IDind","Startyear" ,"Endyear" ,
                          "DOCTOR'S DIAGNOSIS OF ILLENESS/INJURY = 2 Heart disease",
                          
                          "Western Year of Birth, YYYY",
                          "DATE OF DEATH, YYYYMMDD ",
                          "CAUSE OF DEATH, 1991 SURVEY ONLY=2 : Illness",
                          
                          
                          
                          
                          "Gender",
                          "Marital status",
                          "Education level",
                          "Nationality",
                          "Smoking",
                          "Frequency of alcohol consumption",
                          "Intensive Physical Activity",
                          "Moderate Physical Activity ",
                          "Light Physical Activity ",
                          "Cancer",
                          "Diabetes",
                          "Stroke",
                          
                          "Heart Disease/Chest Pain",
                          
                          "Myocardial infarction/heart attack",
                          
                          "Standing Height (cm)",
                          "Weight (kg)",
                          
                          "Waist Circumference (cm)",
                          "Systolic blood pressure (mmHg)",
                          "Diastolic blood pressure  (mmHg)",
                          
                          "TG (mg/dL)",
                          "HDL (mmol/L)",
                          "LDL-C (mmol/L)",
                          "High-Sensitivity-CRP (mg/L)",
                          "Cholesterol (mg/dL)",
                          "Glucose (mg/dL)",
                          "Uric acid (mg/dL)")
saveRDS(whole_data,"whole_data.RDS")
write.csv(whole_data,"whole_data.csv")

whole_data <- readRDS("whole_data.RDS")
time <- whole_data

library(dplyr)
# 假设你的数据框名为time
time <- time %>%
  group_by(IDind) %>%
  mutate(Startyear = min(Startyear), Endyear = max(Endyear)) %>%
  ungroup()
time <- unique(time)
write.xlsx(time,"计算随访时间准备插补.xlsx")

####循环法计算startyaer####
# 加载dplyr包
library(dplyr)
df <- whole_data
# 假设你的数据框名为df，并且有四列，这里我们用mtcars作为示例
df <- whole_data[, 2:5]  # year
# 添加 start_year 和 end_year 列并初始化为 NA
df$start_year <- NA
df$end_year <- NA

# 确定 start_year
for (i in 1:nrow(df)) {
  for (j in 1:4) {
    if (!is.na(df[i,j])) {
      df$start_year[i] <- df[i,j]
      break
    }
  }
}

# 确定 end_year
for (i in nrow(df):1) {
  for (j in 4:1) {
    if (!is.na(df[i,j])) {
      df$end_year[i] <- df[i,j]
      break
    }
  }
}
year <- df[,(5:6)]


df <- whole_data[, 6:9]  # month
# 添加 start_month 和 end_month 列并初始化为 NA
df$start_month <- NA
df$end_month <- NA

# 确定 start_month
for (i in 1:nrow(df)) {
  for (j in 1:4) {
    if (!is.na(df[i,j])) {
      df$start_month[i] <- df[i,j]
      break
    }
  }
}

# 确定 end_month
for (i in nrow(df):1) {
  for (j in 4:1) {
    if (!is.na(df[i,j])) {
      df$end_month[i] <- df[i,j]
      break
    }
  }
}

month <- df[,(5:6)]

data <- cbind(year,month,whole_data[,-c(1:13)])
saveRDS(data,"whole_data1.RDS")
library(openxlsx)
data <- as.data.frame(data)
write.xlsx(data,"whole_data1.xlsx")
