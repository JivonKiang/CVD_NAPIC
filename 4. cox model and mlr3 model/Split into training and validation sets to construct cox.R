####nomogram####
rm(list=ls())
library(survival)
library(rms)
library(openxlsx)
#载入数据
data <- read.xlsx("zstat_data Meta.xlsx",sheet="for model")
library(dplyr)
colnames(data)#生存数据的status设置为1=death，0=alive就行
#二分类变量指定
names<- c("Gender","Marital_status","Race","Education_level","Smoking"                    
          ,"Drinking","Light_Physical_Activity","Moderate_Physical_Activity","Intensive_Physical_Activity"
          ,"Diabetes","Cancer","Stroke","Heart_disease","WSH_quantile","WSH_custom"
)
data[,c(names)] <- lapply(data[,c(names)], factor)#指定变量为二分类变量
data$Status <- as.numeric(data$Status)
summary(data)#检查变量是否符合
sum(is.na(data))#统计缺失值总数
#data <- data[,-1]

###--- 使用Boruta进行特征选择 ---###
library(Boruta)
library(survival)
# 创建Surv对象
surv_obj <- Surv(data$Time, data$Status)
# 准备特征矩阵
features <- data[, -which(names(data) %in% c("Status", "Time"))]
# 运行Boruta算法
boruta_result <- Boruta(x=features, y=surv_obj, doTrace = 2)
saveRDS(boruta_result,"Result/boruta_result.RDS")
# 查看Boruta的结果
print(boruta_result)
# 获取确认的重要特征
confirmed_vars <- getSelectedAttributes(boruta_result, withTentative = T)
print(confirmed_vars)
write.csv(confirmed_vars,"Result/confirmed_vars_index.csv")


####选择合格变量绘制nomogram####
dd <- datadist(data)
options(datadist = "dd")
model <- cph(Surv(Time, Status) ~ 
               Age+
               #Gender+
               #Marital_status+
               Race+
               Education_level+
               Smoking+
               Drinking+
               Light_Physical_Activity+
               Moderate_Physical_Activity+
               #Intensive_Physical_Activity+
               #Diabetes+
               #Cancer+
               #Stroke+
               #Heart_disease+
               #Standing_Height+
               #Weight+
               #BMI+
               #Waist_Circumference+
               #WHtR+
               #HDL+
               #LDL+
               Triglycerides+
               #Glucose+
               #Uric_acid+
               #SBP+
               #DBP+
               Glycated_haemoglobin+
               Cholesterol+
               CRP+
               #WSH+
               #WSH_quantile+
               WSH_custom,
               data = data, x = TRUE, y = TRUE, surv = TRUE, 
               singular.ok = TRUE#设置singular.ok = TRUE来允许程序自动跳过X矩阵中线性相关的列
             )
# 构建生存函数，注意你的最大生存时间
surv <- Survival(model)
surv1 <- function(x) surv(365,x) # 1年OS
surv3 <- function(x) surv(365*3,x) 
surv5 <- function(x) surv(365*5,x) 
surv10 <- function(x) surv(365*10,x) 
surv20 <- function(x) surv(365*20,x)
surv25 <- function(x) surv(365*25,x)

nom <- nomogram(model, fun = list(surv1, surv3,surv5,surv10, surv20,surv25
                                  ),
                lp = T,
                funlabel = c('1 year survival Probability',
                             '3 years survival Probability',
                             '5 years survival Probability',
                             '10 years survival Probability',
                             '20 years survival Probability',
                             '25 years survival Probability'
                             ),
                maxscale = 100,
                fun.at = c(0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
                )
# 保存为PNG格式
# 绘图
if(! dir.exists("诊断效能")){dir.create("诊断效能")}
if(! dir.exists("诊断效能//train")){dir.create("诊断效能//train")}
if(! dir.exists("诊断效能//test")){dir.create("诊断效能//test")}
png("诊断效能//nomogram.png", width = 1900, height = 1500) # 打开PNG设备，指定图片大小
plot(nom, lplabel = "Linear Predictor", xfrac = 0.2)
dev.off()


####在线发布####
library(DynNom)
library(rsconnect)
library(foreign)
library(shinyPredict)
library(shiny)
df <- data
fit2 <- coxph(Surv(Time, Status) ~ 
                Age+
                #Gender+
                #Marital_status+
                Race+
                Education_level+
                Smoking+
                Drinking+
                Light_Physical_Activity+
                Moderate_Physical_Activity+
                #Intensive_Physical_Activity+
                #Diabetes+
                #Cancer+
                #Stroke+
                #Heart_disease+
                #Standing_Height+
                #Weight+
                #BMI+
                #Waist_Circumference+
                #WHtR+
                #HDL+
                #LDL+
                Triglycerides+
                #Glucose+
                #Uric_acid+
                #SBP+
                #DBP+
                Glycated_haemoglobin+
                Cholesterol+
                CRP+
                #WSH+
                #WSH_quantile+
                WSH_custom,
            data = df#, surv = T
            )
rsconnect::setAccountInfo(name='jiangfannbnbnb',
                          token='BFDEAA30940E2C911ACA8CB315E4EF7B',
                          secret='gFKckbIj+CDTAS8BiQmj9t/3OxIH3vg3Vz4AqwEJ')
##https://www.shinyapps.io/admin/#/dashboard
#先进入上面这个网址，输入
#账户是fmmujf@163.com
#密码你懂

#DynNom(fit2,
#       DNxlab = "Survival probability",
#       KMtitle="Kaplan-Meier plot", 
#       KMxlab = "Time (Days)", 
#       KMylab = "Survival probability")
#关闭窗口，打开文件夹中的ui文件，然后点击发布
DNbuilder(fit2)
DynNom(fit2,df)


####训练集构建模型####
library(pROC)
set.seed(1234) # 这里的123可以是任何整数
library(caret)
# 假设data是你的数据集，target是你要预测的目标变量
# p是测试集的比例
p <- 0.3 # 20%的数据作为测试集
trainingIndex <- createDataPartition(data$Status, p = p, list = FALSE, times = 1)
# 使用trainingIndex来拆分数据集
train <- data[-trainingIndex, ]
test <- data[trainingIndex, ]
# 构建模型
library(rms)
library(survival)
model <- coxph(Surv(Time, Status) ~ 
                 Year_of_diagnosis+
                 Age+Sex+Race+
                 Diagnostic_Confirmation+
                 #Histology+
                 Hist_or_behav+
                 #Rare_tumors+
                 Grade_Recode+SEER_Stage+
                 Surg_Prim_Site+
                 Surg_or_Rad+
                 #Reason_no_cancer_directed_surgery+
                 Radiation+
                 Chemotherapy+
                 Systemic_or_Sur+
                 Median_household_income+
                 Rural_Urban_Continuum,
            data = train)
####---计算并绘制ROC曲线---####
#train AUC
pred_probs1 <- predict(model, newdata = train, type = "risk")
#test AUC
pred_probs2 <- predict(model, newdata = test, type = "risk")
# 计算并绘制ROC曲线
library(pROC)
roc1 <- roc(train$Status, pred_probs1)
roc2 <- roc(test$Status, pred_probs2)
train_auc <- paste("AUC in train=",round(auc(roc1), 4))
test_auc <- paste("AUC in test=",round(auc(roc2), 4))
# 绘图
if(! dir.exists("诊断效能")){dir.create("诊断效能")}
if(! dir.exists("诊断效能//train")){dir.create("诊断效能//train")}
if(! dir.exists("诊断效能//test")){dir.create("诊断效能//test")}
# 保存为PNG格式
png("诊断效能//plot.png", width = 300, height = 300) # 打开PNG设备，指定图片大小
plot(smooth(roc1), col="red", legacy.axes=TRUE,
     #print.auc=TRUE, #auc.polygon=TRUE,
     main="ROC Curves", xlab="1 - Specificity",
     ylab="Sensitivity")
plot(smooth(roc2), add=TRUE, col="blue"#, print.auc=TRUE#, auc.polygon=TRUE
)
legend("bottom",
       legend=c(train_auc,test_auc),
       col=c("red", "blue"), lty=2, lwd=4)
dev.off() # 关闭图形设备，保存图片

####计算c index####
library(survival)
library(broom)
# 获取模型的summary
model_summary <- summary(model)
# 将summary转换为data.frame
library(survivalAnalysis)
model_summary_df <- cox_as_data_frame(model_summary)
# 查看转换后的数据框
my_list <- list(model_summary[["waldtest"]],
                model_summary[["logtest"]],
                model_summary[["sctest"]],
                model_summary[["rsq"]],
                model_summary[["concordance"]])
# 使用enframe转换为数据框，然后pivot_wider展开
# 使用vapply和rbind合并数据框
df <- do.call(rbind, lapply(my_list,function(x){c(x, rep(NA, max(sapply(my_list,length)) - length(x)))}))
df <- as.data.frame(df)
rownames(df) <- c("Waldtest","Logtest","Sctest","Rsq","Concordance/C-index")
# 将行名作为第一列
df <- cbind(RowNames = rownames(df), df)
df[4,4] <- c("Maxrsq")
df[5,4] <- c("Se(C index)")
library(openxlsx)
write.xlsx(model_summary_df,"诊断效能//train//model_summary.xlsx")
write.xlsx(df,"诊断效能//train//四大检验和c index.xlsx")

###---test
model <- coxph(Surv(Time, Status) ~ 
                 Year_of_diagnosis+
                 Age+Sex+Race+
                 Diagnostic_Confirmation+
                 #Histology+
                 Hist_or_behav+
                 #Rare_tumors+
                 Grade_Recode+SEER_Stage+
                 Surg_Prim_Site+
                 Surg_or_Rad+
                 #Reason_no_cancer_directed_surgery+
                 Radiation+
                 Chemotherapy+
                 Systemic_or_Sur+
                 Median_household_income+
                 Rural_Urban_Continuum,
               data = test)
# 获取模型的summary
model_summary <- summary(model)
# 将summary转换为data.frame
library(survivalAnalysis)
model_summary_df <- cox_as_data_frame(model_summary)
# 查看转换后的数据框
my_list <- list(model_summary[["waldtest"]],
                model_summary[["logtest"]],
                model_summary[["sctest"]],
                model_summary[["rsq"]],
                model_summary[["concordance"]])
# 使用enframe转换为数据框，然后pivot_wider展开
# 使用vapply和rbind合并数据框
df <- do.call(rbind, lapply(my_list,function(x){c(x, rep(NA, max(sapply(my_list,length)) - length(x)))}))
df <- as.data.frame(df)
rownames(df) <- c("Waldtest","Logtest","Sctest","Rsq","Concordance/C-index")
# 将行名作为第一列
df <- cbind(RowNames = rownames(df), df)
df[4,4] <- c("Maxrsq")
df[5,4] <- c("Se(C index)")
library(openxlsx)
write.xlsx(model_summary_df,"诊断效能//test//model_summary.xlsx")
write.xlsx(df,"诊断效能//test//四大检验和c index.xlsx")

####绘制校准曲线train####
library(rms)
model <- cph(Surv(Time, Status) ~ 
               Year_of_diagnosis+
               Age+Sex+Race+
               Diagnostic_Confirmation+
               #Histology+
               Hist_or_behav+
               #Rare_tumors+
               Grade_Recode+SEER_Stage+
               Surg_Prim_Site+
               Surg_or_Rad+
               #Reason_no_cancer_directed_surgery+
               Radiation+
               Chemotherapy+
               Systemic_or_Sur+
               Median_household_income+
               Rural_Urban_Continuum,
                data = train, surv = T,x=T,y=T)

# 定义你想要观察的时间点
times <- c(365*1, 365*3,365*5,
           365*10,365*15)

# 循环遍历每个时间点，并计算校准曲线
calibrations <- lapply(times, function(t) {
  cal <- calibrate(model, cmethod="KM", method="boot", u = t, m=100,B = 100)
  # 保存图形为PNG文件
  png(paste("诊断效能//train//calibration_curve_at_", t, ".png", sep = ""),
      width = 700, height = 700)
  plot(cal, lwd = 2, lty = 0, errbar.col = "#843", 
       xlim = c(0.1,0.9), ylim = c(0.1,0.9), xlab = "Nomogram-predicted OS (%)", 
       ylab = "Observed OS (%)", col = "#900"
       )
  lines(cal[,c('mean.predicted',"KM")], type = 'b', lwd = 1, col = "#800", pch = 16)
  abline(0,1, lwd = 2, lty = 3, col = "#224444")
  dev.off()
  return(cal)
})


####DCA曲线train####
library(pROC)
# 构建模型
library(rms)
library(survival)
model <- coxph(Surv(Time, Status) ~ 
                 Year_of_diagnosis+
                 Age+Sex+Race+
                 Diagnostic_Confirmation+
                 #Histology+
                 Hist_or_behav+
                 #Rare_tumors+
                 Grade_Recode+SEER_Stage+
                 Surg_Prim_Site+
                 Surg_or_Rad+
                 #Reason_no_cancer_directed_surgery+
                 Radiation+
                 Chemotherapy+
                 Systemic_or_Sur+
                 Median_household_income+
                 Rural_Urban_Continuum,
               data = train)
library(dcurves)
library(ggplot2)
# 假设你想移除第二个环境（索引从1开始）
# 循环遍历每个时间点，并计算校准曲线
survfit_results <- survfit(model, newdata = train)
# 定义你想要观察的时间点
times <- c(365*1, 365*3,365*5,365*10,365*15)
# 循环遍历每个时间点，并绘制DCA曲线
train$predicted_probabilities <- c(1 - summary(survfit_results,times = times[1])$surv)
dca_results <- dca(Surv(Time, Status) ~ train$predicted_probabilities, 
                   data = train, time = times[1])
# 保存图形为PNG文件
png(paste("诊断效能//train//dca_at_", times[1], ".png", sep = ""),
    width = 300, height = 100)
plot(dca_results)
dev.off()

train$predicted_probabilities <- c(1 - summary(survfit_results,times = times[2])$surv)
dca_results <- dca(Surv(Time, Status) ~ train$predicted_probabilities, 
                   data = train, time = times[2])
# 保存图形为PNG文件
png(paste("诊断效能//train//dca_at_", times[2], ".png", sep = ""),
    width = 300, height = 100)
plot(dca_results)
dev.off()

train$predicted_probabilities <- c(1 - summary(survfit_results,times = times[3])$surv)
dca_results <- dca(Surv(Time, Status) ~ train$predicted_probabilities, 
                   data = train, time = times[3])
# 保存图形为PNG文件
png(paste("诊断效能//train//dca_at_", times[3], ".png", sep = ""),
    width = 300, height = 100)
plot(dca_results)
dev.off()

train$predicted_probabilities <- c(1 - summary(survfit_results,times = times[4])$surv)
dca_results <- dca(Surv(Time, Status) ~ train$predicted_probabilities, 
                   data = train, time = times[4])
# 保存图形为PNG文件
png(paste("诊断效能//train//dca_at_", times[4], ".png", sep = ""),
    width = 300, height = 100)
plot(dca_results)
dev.off()

train$predicted_probabilities <- c(1 - summary(survfit_results,times = times[5])$surv)
dca_results <- dca(Surv(Time, Status) ~ train$predicted_probabilities, 
                   data = train, time = times[5])
# 保存图形为PNG文件
png(paste("诊断效能//train//dca_at_", times[5], ".png", sep = ""),
    width = 300, height = 100)
plot(dca_results)
dev.off()