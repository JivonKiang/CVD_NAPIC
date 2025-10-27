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

data2 <- data2[,c("IDind","wave","HbA1c",
                  "TG_MG","HDL_C_MG","LDL_C_MG","HS_CRP","TC_MG",
                  "GLUCOSE_MG","UA_MG")]
colnames(data2)[2] <- c("WAVE5")

data1 <- data1[,c("IDind","wave","M40","M24B_7")]
colnames(data1)[2] <- c("WAVE6")

library(dplyr)
library(plyr)
data_cvd0 <- subset(data1,M40=="2" | M24B_7=="1")
data_cvd0 <- subset(data_cvd0,WAVE6=="2009")
data_cvd1 <- data7[,c(1,2,7,8)]
data_cvd1 <- subset(data_cvd1,U24L=="1" | U24J=="1")
data_cvd1 <- subset(data_cvd1,WAVE2=="2009")
data_cvd <- full_join(data_cvd0,data_cvd1,by="IDind")
data_cvd <- data_cvd[,-c(2,5)]

data <- left_join(data_cvd,data2,by="IDind")
data <- data[!duplicated(data$IDind), ]


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
interview_year <- data[,c("IDind","WAVE1","WAVE2","WAVE3","WAVE4","WAVE5")]
# 提取每个样本的Endyear（wave中的最大值）
# 计算每一行的最大值和最小值
max_values <- apply(interview_year[,-1], 1, max, na.rm = TRUE)
min_values <- apply(interview_year[,-1], 1, min, na.rm = TRUE)
# 将结果合并到一个新的数据框中
sample_interview <- data.frame(IDind = interview_year$IDind,Startyear = min_values,
                     Endyear = max_values)
saveRDS(sample_interview,"original data/sample_interview.RDS")

# 对IDind进行去重，保留Startyear的最小值和Endyear的最大值
result <- sample_interview %>%
  group_by(IDind) %>%
  summarise(
    Startyear = min(Startyear, na.rm = TRUE),
    Endyear = max(Endyear, na.rm = TRUE)
  )
saveRDS(result,"original data/sample_interview_result.RDS")

# 去除可能存在的重复行（根据IDind、Startyear和Endyear判断）
sample_interview <- distinct(sample_interview)

#sample_interview <- na.omit(sample_interview)
saveRDS(sample_interview,"original data/sample_interview.RDS")
sample_interview <- readRDS("original data/sample_interview.RDS")

colnames(data)[4:5] <- c("U24L","U24J")
data_without_interview <- data[,c("IDind",
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
                      "HbA1c",
                      "TG_MG",
                      "HDL_C_MG",
                      "LDL_C_MG",
                      "HS_CRP",
                      "TC_MG",
                      "GLUCOSE_MG",
                      "UA_MG")]
data_without_interview <- distinct(data_without_interview)
# 筛选出IDind列中唯一值的行
data_without_interview <- data_without_interview %>% distinct(IDind, .keep_all = TRUE)

saveRDS(data_without_interview,"original data/data_without_interview.RDS")
data_without_interview <- readRDS("original data/data_without_interview.RDS")
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
                      "HbA1c",
                      "TG_MG",
                      "HDL_C_MG",
                      "LDL_C_MG",
                      "HS_CRP",
                      "TC_MG",
                      "GLUCOSE_MG",
                      "UA_MG")]
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
                          "HbA1c (mmol/L)",
                          "Triglycerides (mg/dL)",
                          "HDL (mmol/L)",
                          "LDL-C (mmol/L)",
                          "High-Sensitivity-CRP (mg/L)",
                          "Cholesterol (mg/dL)",
                          "Glucose (mg/dL)",
                          "Uric acid (mg/dL)")
saveRDS(whole_data,"original data/whole_data.RDS")
write.csv(whole_data,"original data/whole_data.csv")

whole_data <- readRDS("original data/whole_data.RDS")
time <- whole_data

library(dplyr)
# 假设你的数据框名为time
time <- time %>%
  group_by(IDind) %>%
  mutate(Startyear = min(Startyear), Endyear = max(Endyear)) %>%
  ungroup()
time <- unique(time)
write.xlsx(time,"original data/计算随访时间准备插补.xlsx")

####二分类变量替换####
rm(list=ls())
library(openxlsx)
data <- read.xlsx("original data/计算随访时间准备插补.xlsx")
colnames(data)[5:6] <- c("Birthyear","Deathyear")
# 假设你的数据框叫做data
data$Deathyear <- as.character(data$Deathyear) # 确保Deathyear是字符型
data$Deathyear <- substr(data$Deathyear, 1, 4) # 保留前四位
library(dplyr)

# 假设你的数据框叫做data
data <- data %>%
  mutate(
    # 将年份列转换为数值型
    Startyear = as.numeric(Startyear),
    Endyear = as.numeric(Endyear),
    Deathyear = as.numeric(Deathyear),
    Birthyear = as.numeric(Birthyear),
    
    # 计算状态（Status）
    Status = ifelse(is.na(Deathyear), 0, 1),
    
    # 计算随访时间（Time）
    Time = ifelse(
      is.na(Deathyear),
      as.numeric(difftime(as.Date(paste(Endyear, "12", "31"), "%Y %m %d"), as.Date(paste(Startyear, "01", "01"), "%Y %m %d"), units = "days")),
      as.numeric(difftime(as.Date(paste(Deathyear, "12", "31"), "%Y %m %d"), as.Date(paste(Startyear, "01", "01"), "%Y %m %d"), units = "days"))
    ),
    
    # 计算年龄（Age）
    Age = ifelse(
      is.na(Deathyear),
      Endyear - Birthyear,
      Deathyear - Birthyear
    )
  )

# 将最后三列挪到最开始
data <- data %>%
  select(tail(everything(), 3), head(everything(), -3))
data <- data[,-(4:10)]
summary(data)
data <- subset(data, select = -Cancer)
library(dplyr)

# 将data数据框的列名中的"/"替换为"_"
data <- data %>%
  rename_with(~ gsub("/", "_", .x))

data <- data %>%
  mutate(
    Gender = case_when(
      Gender == 1 ~ "1_Male",
      Gender == 2 ~ "2_Female",
      TRUE ~ as.character(Gender) # 保持其他值不变
    ),
    Marital.status = case_when(
      Marital.status == 2 ~ "2_Married",
      Marital.status == 3 ~ "3_Divorced",
      Marital.status == 4 ~ "4_Widowed",
      TRUE ~ as.character(Marital.status) # 标记其他为NA
    ),
    Education.level = case_when(
      Education.level == 0 ~ "0_None",
      Education.level == 1 ~ "1_Grad from primary",
      Education.level == 2 ~ "2_Lower middle school degree",
      Education.level == 3 ~ "3_Upper middle school degree",
      Education.level == 4 ~ "4_Technical or vocational degree",
      Education.level == 5 ~ "5_University or college degree",
      TRUE ~ as.character(Education.level) # 标记其他为NA
    ),
    Nationality = case_when(
      Nationality == 1 ~ "1_Han",
      Nationality == 2 ~ "2_Mongolian",
      Nationality == 3 ~ "3_Hui",
      Nationality == 4 ~ "4_Tibetan",
      Nationality == 5 ~ "5_Vaguer",
      Nationality == 6 ~ "6_Miao",
      Nationality == 7 ~ "7_Yi",
      Nationality == 8 ~ "8_Zhuang",
      Nationality == 9 ~ "9_Buyi",
      Nationality == 10 ~ "10_Korean",
      Nationality == 11 ~ "11_Man",
      Nationality == 12 ~ "12_Dong",
      Nationality == 13 ~ "13_Yao",
      Nationality == 15 ~ "15_Tujia",
      Nationality == 20 ~ "20_Other",
      TRUE ~ as.character(Nationality) # 标记其他为NA
    ),
    Smoking = case_when(
      Smoking == 0 ~ "0_No",
      Smoking == 1 ~ "1_Yes",
      Smoking == 9 ~ "9_Don’t know",
      TRUE ~ as.character(Smoking) # 标记其他为NA
    ),
    Frequency.of.alcohol.consumption = case_when(
      Frequency.of.alcohol.consumption == 1 ~ "1_Almost every day",
      Frequency.of.alcohol.consumption == 2 ~ "2_3-4 times a week",
      Frequency.of.alcohol.consumption == 3 ~ "3_Once or twice a week",
      Frequency.of.alcohol.consumption == 4 ~ "4_Once or twice a month",
      Frequency.of.alcohol.consumption == 5 ~ "5_No more than once a month",
      Frequency.of.alcohol.consumption == 8 ~ "8_Refuse to answer (1997 Only)",
      Frequency.of.alcohol.consumption == 9 ~ "9_Unknown",
      TRUE ~ as.character(Frequency.of.alcohol.consumption) # 标记其他为NA
    ),
    Diabetes = case_when(
      Diabetes == 0 ~ "0_No",
      Diabetes == 1 ~ "1_Yes",
      Diabetes == 9 ~ "9_Don’t know",
      TRUE ~ as.character(Diabetes) # 标记其他为NA
    ),
    Stroke = case_when(
      Stroke == 0 ~ "0_No",
      Stroke == 1 ~ "1_Yes",
      Stroke == 9 ~ "9_Don’t know",
      TRUE ~ as.character(Stroke) # 标记其他为NA
    ),
    Heart.Disease_Chest.Pain = case_when(
      Heart.Disease_Chest.Pain == 0 ~ "0_No",
      Heart.Disease_Chest.Pain == 1 ~ "1_Yes",
      Heart.Disease_Chest.Pain == 9 ~ "9_Don’t know",
      TRUE ~ as.character(Heart.Disease_Chest.Pain) # 标记其他为NA
    ),
    Myocardial.infarction_heart.attack = case_when(
      Myocardial.infarction_heart.attack == 0 ~ "0_No",
      Myocardial.infarction_heart.attack == 1 ~ "1_Yes",
      Myocardial.infarction_heart.attack == 9 ~ "9_Don’t know",
      TRUE ~ as.character(Myocardial.infarction_heart.attack) # 标记其他为NA
    )
  )
# 将data数据框的第4到第16列的所有NA替换为"99_NA"
data <- data %>%
  mutate(across(.cols = 4:16, ~replace(., is.na(.), "99_NA")))
summary(data)
saveRDS(data,"original data/等待插补的数据.RDS")###检查一下99_NA有没有换上去
write.xlsx(data,"original data/等待插补的数据.xlsx")
