# 安装并加载所需包
library(openxlsx)
rm(list = ls())
#data <- read.xlsx("CHARLS.xlsx")
#data <- read.xlsx("CHNS.xlsx")
data <- read.xlsx("Meta.xlsx")
#data <- read.xlsx("NHANES.xlsx")
# 检查Status列的值
table(data$Status)

# 转换Status列，确保值只有0和1
data$Status <- as.numeric(data$Status)

# 再次检查Status列的值
table(data$Status)

# 计算Noval index
data$NI_1 <- log(data$Weight * data$BMI * 2) / data$Waist_Circumference
data$NI_2 <- data$Weight / data$SBP * 1 / data$LDL
data$NI_3 <- log10(data$LDL * data$SBP * 2) / data$Waist_Circumference
data$NI_4 <- data$Uric_acid * data$Standing_Height * 2 * data$Waist_Circumference
data$NI_5 <- data$SBP / data$Waist_Circumference / 2 / data$Waist_Circumference
data$NI_6 <- data$Glycated_haemoglobin / data$Standing_Height * data$WHtR
data$NI_7 <- log10(data$Glycated_haemoglobin / data$Weight) * data$WHtR
data$NI_8 <- log10(data$Cholesterol / data$SBP) / data$WHtR
data$NI_9 <- log10(data$Weight * 2) * data$Waist_Circumference
data$NI_10 <- 1 / data$Weight / 2 * data$Waist_Circumference
data$NI_11 <- data$Age
data$NI_12 <- data$Weight / data$SBP * data$HDL
data$NI_13 <- log(data$BMI / data$Waist_Circumference / 2) * data$Glycated_haemoglobin
data$NI_14 <- log(data$BMI / data$SBP) * data$HDL
data$NI_15 <- log(data$BMI / data$SBP / 2) * data$HDL
data$NI_16 <- log10(data$BMI / data$SBP / 2) * data$HDL
data$NI_17 <- data$BMI / data$SBP / 2 * data$HDL
data$NI_18 <- log(data$BMI / data$SBP) * data$LDL
data$NI_19 <- log10(data$Waist_Circumference / data$BMI) * data$Glycated_haemoglobin
data$NI_20 <- data$Waist_Circumference / data$BMI * data$Glycated_haemoglobin
data$NI_21 <- log(data$Waist_Circumference / data$BMI) / data$Glycated_haemoglobin
data$NI_22 <- log(data$Waist_Circumference * data$SBP) * data$WHtR
data$NI_23 <- log(data$Waist_Circumference * data$SBP * 2) * data$WHtR
data$NI_24 <- log(data$WHtR * data$SBP) * data$WHtR
data$NI_25 <- log10(data$Uric_acid / data$DBP / 2) / data$DBP
data$NI_26 <- log10(data$Uric_acid / data$DBP) / data$Glycated_haemoglobin
data$NI_27 <- data$Uric_acid / data$DBP / data$Glycated_haemoglobin
data$NI_28 <- log(data$SBP / data$BMI / 2) / data$WHtR
data$NI_29 <- log(data$SBP / data$BMI) / data$HDL
data$NI_30 <- log10(data$SBP / data$BMI / 2) * data$HDL
data$NI_31 <- data$SBP / data$BMI / 2 * data$HDL
data$NI_32 <- log(data$SBP * data$Waist_Circumference) * data$WHtR
data$NI_33 <- log(data$SBP * data$Uric_acid * 2) * data$Cholesterol
data$NI_34 <- log10(data$DBP / data$Uric_acid / 2) / data$DBP
data$NI_35 <- data$DBP / data$Uric_acid / 2 / data$DBP
data$NI_36 <- log(data$DBP / data$SBP / 2) * data$Glycated_haemoglobin
data$NI_37 <- data$DBP / data$Glycated_haemoglobin / 2 / data$DBP
data$NI_38 <- data$Glycated_haemoglobin / data$DBP / 2 / data$DBP
data$NI_39 <- data$Cholesterol / data$Uric_acid / data$Glycated_haemoglobin
data$NI_40 <- data$BMI / data$DBP / data$Glycated_haemoglobin
data$NI_41 <- data$BMI * data$Weight / data$Glycated_haemoglobin
data$NI_42 <- log10(data$Standing_Height / data$Weight / 2) / data$HDL

# 计算Traditional index
data$BMI <- data$BMI
data$WHtR <- data$WHtR
data$TyG <- log(data$Triglycerides * data$Glucose / 2)
data$TyG_WC <- data$TyG * data$Waist_Circumference
data$TyG_WHtR <- data$TyG * data$WHtR
data$TyG_BMI <- data$TyG * data$BMI
data$UHR <- data$Uric_acid / data$HDL
data$UHR_WC <- data$UHR * data$Waist_Circumference
data$UHR_WHtR <- data$UHR * data$WHtR
data$UHR_BMI <- data$UHR * data$BMI
data$AIP <- log10(data$Triglycerides / data$HDL)
data$AIP_WC <- data$AIP * data$Waist_Circumference
data$AIP_WHtR <- data$AIP * data$WHtR
data$AIP_BMI <- data$AIP * data$BMI

# 将数据框中的"Inf"或"-Inf"替换为NA
data[] <- lapply(data, function(x) ifelse(x == "Inf" | x == "-Inf", NA, x))
# 删除包含NA值的行
data <- na.omit(data)

library(survcompare) # installed using Bioconductor
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("survcomp")
library(survcomp)
library(survival)

cindex <- function(time = sample.data$os, event =sample.data$death, variable = c("age","bp"), data = sample.data, ties=TRUE,adj = FALSE){
  require(rms)
  surv <- Surv(time,event)
  form <- as.formula(paste("surv~",paste(variable,collapse=" + ")))
  fit.coxph <- coxph(form,data)
  fit.cph <- cph(form, data = data, x = TRUE, y = TRUE, surv = TRUE)
  
  if (ties==FALSE){
    require(survcomp)
    coxPredict <- predict(fit.coxph, data = data, type="risk")
    c_index <-concordance.index(x=coxPredict, surv.time=time, surv.event=event, method="noether")
    res <- paste(c_index$c.index, " (", c_index$lower, " - ", c_index$upper,")", sep = "")
  }
  
  else if (ties==TRUE) {
    sum.surv <- summary(fit.coxph)
    c_index <- sum.surv$concordance
    res <- paste(c_index[1], " (", c_index[1]-1.96*c_index[2], " - ", c_index[1]+1.96*c_index[2],")", sep = "")
  }
  if(adj == FALSE){
    bias_corrected_c_index <- NA
  }
  else if (adj==TRUE) { 
    set.seed(1234)
    v <- rms::validate(fit.cph, dxy = TRUE, B = 1000)
    Dxy  <-  v[rownames(v)=="Dxy", colnames(v)=="index.corrected"]
    bias_corrected_c_index  <- abs(Dxy)/2+0.5
    bias_corrected_c_index
  }
  final <- list()
  final["C-index and 95%CI"] <- res
  final["Bias corrected C-index"] <- bias_corrected_c_index
  final
}

# 计算Noval index和Traditional index的C-index及其95%置信区间
indices <- c(paste0("NI_", 1:42), "BMI", "WHtR", "TyG", "TyG_WC", "TyG_WHtR", 
             "TyG_BMI", "UHR", "UHR_WC", "UHR_WHtR", "UHR_BMI", "AIP", "AIP_WC", "AIP_WHtR", "AIP_BMI")

# 初始化结果数据框
results_df <- data.frame(
  Variable = character(),
  `C-index and 95%CI` = character(),
  `Bias corrected C-index` = numeric(),
  stringsAsFactors = FALSE
)

# 计算每个变量的C-index及其95%置信区间
for (index in indices) {
  result <- cindex(time = data$Time, event = data$Status, variable = index, data = data, ties = TRUE, adj = TRUE)
  results_df <- rbind(results_df, data.frame(
    Variable = index,
    `C-index and 95%CI` = result[[1]],
    `Bias corrected C-index` = result[[2]],
    stringsAsFactors = FALSE
  ))
}

# 查看结果数据框
print(results_df)

# 保存结果到Excel文件
wb <- createWorkbook()
addWorksheet(wb, "Cox_Model_Results")
writeData(wb, "Cox_Model_Results", results_df)
saveWorkbook(wb, "Cox_Model_Results.xlsx", overwrite = TRUE)
