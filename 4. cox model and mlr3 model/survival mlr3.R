# 清除当前环境中的所有对象
rm(list = ls())

# 安装和加载必要的包
if (!requireNamespace("mlr3", quietly = TRUE)) install.packages("mlr3")
if (!requireNamespace("mlr3proba", quietly = TRUE)) install.packages("mlr3proba", repos = "https://mlr-org.r-universe.dev")
if (!requireNamespace("mlr3viz", quietly = TRUE)) install.packages("mlr3viz")
if (!requireNamespace("survival", quietly = TRUE)) install.packages("survival")
if (!requireNamespace("mlr3verse", quietly = TRUE)) install.packages("mlr3verse")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
if (!requireNamespace("mlr3extralearners", quietly = TRUE)) install.packages("mlr3extralearners")
#remotes::install_github("RaphaelS1/survivalmodels")
# 安装其他依赖包
#install.packages("survival")
#install.packages("ranger")
#install.packages("randomForestSRC")
#install.packages("rpart")
#install.packages("survivalsvm")
#install.packages("xgboost")
##install.packages("distr6")
#remotes::install_github("alan-turing-institute/distr6")
#install.packages("partykit")
#install.packages("sandwich")
#install.packages("coin")
#install.packages("pracma")
#install.packages("catboost")
#install.packages("gbm")
#install.packages("glmnet")
#install.packages("keras")
#install.packages("tensorflow")
#install.packages("torch")
# 清除当前环境中的所有对象

library(mlr3)
library(mlr3proba)
library(mlr3viz)
library(survival)
library(mlr3verse)
library(openxlsx)
library(dplyr)
library(remotes)
library(mlr3extralearners)
library(survivalmodels)
library(survcomp)
mlr3::lrn("surv.akritas")


# 读取数据
data <- read.xlsx("zstat_data Meta.xlsx",sheet = "mlr3")

# 确保Status列是整数类型
data$Status <- as.integer(data$Status)
data$Time <- as.numeric(data$Time)
#二分类变量指定
names<- c("Marital_status","Race","Smoking"                    
          ,"Drinking","Moderate_Physical_Activity","Intensive_Physical_Activity"
          ,"Cancer")
data[,c(names)] <- lapply(data[,c(names)], factor)#指定变量为二分类变量
# 检查数据框中的因子类型列
factor_columns <- sapply(data, is.factor)
cat("Factor columns:", names(factor_columns[factor_columns]), "\n")
# 将因子类型列转换为数值类型
data <- data %>%
  mutate(across(where(is.factor), as.numeric))
# 重新检查数据框中的因子类型列
cat("After conversion, factor columns:", names(sapply(data, is.factor)[sapply(data, is.factor)]), "\n")
# 检查数据中是否有NA
sum(is.na(data))
# 如果有NA，进行填补或删除
data <- na.omit(data)

# 将数据转换为mlr3任务
task <- TaskSurv$new(id = "survival_task", backend = data, time = "Time", event = "Status")

# 查看所有Learner类型
learners_df <- as.data.table(mlr_learners)

# 筛选生存分析的Learner
learners_df <- learners_df %>% filter(task_type == "surv")
# 定义要筛选的字符串列表
target_strings <- c("surv.cforest", "surv.aorsf", "surv.rfsrc", "surv.ranger", 
                    #"surv.bart", 
                    "surv.gbm", "surv.penalized", "surv.coxph", 
                    "surv.parametric", "surv.coxboost", "surv.glmnet", "surv.blackboost", 
                    "surv.xgboost.aft", "surv.cv_glmnet","surv.cv_coxboost", "surv.ctree",
                    "surv.rpart","surv.glmboost", "surv.xgboost.cox", "surv.akritas",
                    "surv.kaplan","surv.nelson")
learners_df <- learners_df[grepl(paste(target_strings, collapse = "|"), learners_df$key), ]
learners <- lapply(learners_df$key, lrn)
# 手动列出所有学习器的名称
learner_names <- learners_df$key

# 检查每个学习器是否可用
available_learners <- list()
unavailable_learners <- list()

for (learner_name in learner_names) {
  tryCatch({
    # 尝试实例化学习器
    learner <- lrn(learner_name)
    available_learners[[learner_name]] <- learner
  }, error = function(e) {
    # 如果实例化失败，记录不可用的学习器
    unavailable_learners[[learner_name]] <- e$message
  })
}

# 打印可用的学习器
cat("Available Learners:\n")
print(names(available_learners))

# 打印不可用的学习器及其错误信息
if (length(unavailable_learners) > 0) {
  cat("\nUnavailable Learners:\n")
  for (learner_name in names(unavailable_learners)) {
    cat(learner_name, ": ", unavailable_learners[[learner_name]], "\n")
  }
} else {
  cat("\nAll learners are available.\n")
}

# 查看所有重采样方法
resamplings_df <- as.data.table(mlr_resamplings)

# 选择Holdout重采样方法，训练集:验证集 = 7:3
resampling <- rsmp("holdout", ratio = 0.7)
train_set = sample(task$row_ids, 0.7 * task$nrow)
test_set = setdiff(task$row_ids, train_set)

# 设置随机种子
set.seed(1234)

# 构建基准测试设计
design <- benchmark_grid(tasks = list(task),
                         learners = learners,
                         resamplings = list(resampling))

# 打印设计
print(design)

# 初始化一个列表来存储成功运行的模型
successful_learners <- list()

# 加速
library(future)
plan("multisession",workers=12)

# 减少屏幕输出
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

# 逐个运行每个模型
for (learner in learners) {
  learner_id <- learner$id
  cat("Running benchmark for learner:", learner_id, "\n")
  
  # 构建单个模型的基准测试设计
  single_design <- benchmark_grid(tasks = list(task),
                                  learners = list(learner),
                                  resamplings = list(resampling))
  
  # 尝试运行基准测试，并捕获可能出现的错误
  tryCatch({
    single_bmr <- benchmark(single_design)
    cat("Benchmark successful for learner:", learner_id, "\n")
    # 如果成功，将该模型加入到成功运行的模型列表中
    successful_learners <- c(successful_learners, learner)
  }, error = function(e) {
    cat("Benchmark failed for learner:", learner_id, "with error:", e$message, "\n")
  })
}

# 如果没有模型成功运行，终止程序
if (length(successful_learners) == 0) {
  stop("No learners were successful in the benchmark.")
}

# 使用成功运行的模型构建最终的基准测试设计
final_design <- benchmark_grid(tasks = list(task),
                               learners = successful_learners,
                               resamplings = list(resampling))

# 打印最终的基准测试设计
cat("Final benchmark design with successful learners:\n")
print(final_design)

# 运行最终的基准测试
final_bmr <- benchmark(final_design,store_models = T)

# 保存基准测试结果
saveRDS(final_bmr, "benchmark_result.RDS")

bmr <- final_bmr
#autoplot(bmr,type = "roc")
print(bmr)

# 评估标准
msr_txt = c("surv.rcll", "surv.cindex", "surv.dcalib")
measures = msrs(msr_txt)
results <- bmr$aggregate(measures)[, c("learner_id", ..msr_txt)]

# 导出到 Excel 文件
openxlsx::write.xlsx(results, "surv_c_index_results.xlsx")

