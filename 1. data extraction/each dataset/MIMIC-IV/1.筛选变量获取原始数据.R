library(openxlsx)
library(dplyr)
library(stringr)
library(haven)
data1 <- read_dta("data/H_CHARLS_D_Data.dta")
data2 <- read_dta("data/H_CHARLS_EOL_a.dta")
data3 <- read_dta("data/H_CHARLS_LH_a.dta")

colnames(data2)
death_data <- data2[,c("ID","ragcod","raxyear","raxmonth",
                 "raxtiwm","raxtiwy","radtoivwm")]
whole_data <- data1[,c("ID", "r1iwy","r2iwy","r3iwy","r4iwy",
                       "r1iwm", "r2iwm","r3iwm","r4iwm",
                       "r1iwstat","r2iwstat","r3iwstat","r4iwstat",
                       "radyear","radmonth")]
status_and_time <- left_join(whole_data,death_data,by="ID")
saveRDS(status_and_time,"status and time.RDS")

D_colnames <- as.data.frame(colnames(data1))
write.csv(D_colnames,"D_colnames.csv")
information_in_D <- data1[,c("ID", "r1agey","ragender",
                             "r1mstat","s1educ_c","r1hukou", "r1smokev","r1drinkn_c",
                             "r1vgactx_c","r1mdactx_c","r1ltactx_c",
                             "r1cancre","r1diabe","r1stroke","r1hearte",
                             "r1mheight","r1mweight","r1mbmi","r1mwaist",
                             "r1diasto","r1systo"
                             )]
data_without_blood_test <- left_join(status_and_time,information_in_D,by="ID")
saveRDS(data_without_blood_test,"data_without_blood_test.RDS")

library(haven)
blood_2011 <- read_dta("data/blood_2011/blood.dta")
colnames(blood_2011)
blood_in2011 <- blood_2011[,c("ID","newhba1c",
                              "newcho",
                              "newhdl",
                              "newldl",
                              "newcrp",
                              "newtg",
                              "newglu",
                              "newua")]
colnames(blood_in2011) <- c("ID","Glycated haemoglobin/Glyco hemoglobin/Glycohemoglobin (%)",
                            "Total Cholesterol (mg/dL) ",
                            "HDL Cholesterol (mg/dL) ",
                            "LDL Cholesterol (mg/dL) ",
                            "C-Reactive Protein (CRP) (mg/L)",
                            "Triglycerides (mg/dL)",
                            "Glucose (mg/dL) ",
                            "Uric Acid (mg/dL) ")
blood_2015 <- read_dta("data/blood_2015/blood.dta")
blood_in2015 <- blood_2015[,c("ID","bl_hbalc",
                              "bl_cho",
                              "bl_hdl",
                              "bl_ldl",
                              "bl_crp",
                              "bl_tg",
                              "bl_glu",
                              "bl_ua")]
colnames(blood_in2015) <- c("ID","Glycated haemoglobin/Glyco hemoglobin/Glycohemoglobin (%)",
                            "Total Cholesterol (mg/dL) ",
                            "HDL Cholesterol (mg/dL) ",
                            "LDL Cholesterol (mg/dL) ",
                            "C-Reactive Protein (CRP) (mg/L)",
                            "Triglycerides (mg/dL)",
                            "Glucose (mg/dL) ",
                            "Uric Acid (mg/dL) ")
blood <- rbind(blood_in2011,blood_in2015)

whole_data <- left_join(data_without_blood_test,blood,by="ID")
saveRDS(whole_data,"whole_data.RDS")
write.csv(whole_data,"whole_data.csv")

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

data <- readRDS("original data/whole_data1.RDS")
colnames(data)[13:32] <- c("Age",
                           "Sex",
                           "Marital status",
                           "Education level",
                           "Hukou status ",
                           "Smoking",
                           "Drinking",
                           "Intensive Physical Activity days/week",
                           "Moderate Physical Activity days/week",
                           "Light Physical Activity days/week",
                           "Cancer",
                           "Diabetes",
                           "Stroke",
                           "Heart disease",
                           "Standing Height (m)",
                           "Weight (kg)",
                           "BMI (kg/m2) ",
                           "Waist Circumference (cm)",
                           "Systolic blood pressure",
                           "Diastolic blood pressure")
saveRDS(data,"original data/替换好变量名的数据.RDS")
library(openxlsx)
# 将haven_labelled类型的列转换为字符型
data <- data %>%
  mutate(across(where(is.haven_labelled), ~ as.character(.x)))
# 现在可以安全地写入CSV文件
write.xlsx(data, "original data/替换好变量名的数据.xlsx")