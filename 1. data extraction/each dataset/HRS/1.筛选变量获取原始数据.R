library(openxlsx)
library(dplyr)
library(stringr)
library(haven)
rm(list=ls())
data1 <- read_dta("data/TRK2022/trk2022tr_r.dta")
# 假设你的数据框名为data1，列名为HHID和PN
data1$ID <- paste(data1$HHID, data1$PN, sep = "")

bio2006 <- read_sav("data/biomarker/2006/biomk06bl_r.sav")
bio2008 <- read_sav("data/biomarker/2008/biomk08bl_r.sav")
bio2010 <- read_sav("data/biomarker/2010/biomk10bl_r.sav")
bio2012 <- read_sav("data/biomarker/2012/biomk12bl_r.sav")
bio2014 <- read_sav("data/biomarker/2014/biomk14bl.sav")
bio2016 <- read_sav("data/biomarker/2016/BIOMK16BL_R.sav")
# 循环处理bio06到bio16的数据框，每隔两年增加
for (i in seq(2006, 2016, by = 2)) {
  # 构建数据框名称
  df_name <- paste0("bio", i)
  
  # 检查是否存在名为df_name的数据框
  if (exists(df_name)) {
    # 获取数据框
    df <- get(df_name)
    
    # 合并HHID和PN列，并创建新的ID列
    df$ID <- paste(df$HHID, df$PN, sep = "")
    
    # 覆写原来的数据框
    assign(df_name, df)
  }
}

data <- data1[,c("ID",
                 "KAGE",
                 "LAGE",
                 "MAGE",
                 "NAGE",
                 "OAGE",
                 "PAGE",
                 "KNOWNDECEASEDYR",
                 "KNOWNDECEASEDMO",
                 "GENDER",
                 "HHID",
                 "PN",
                 "BIRTHMO",
                 "BIRTHYR",
                 "EXDEATHMO",
                 "EXDEATHYR",
                 "FIRSTIW",
                 "LASTALIVEMO",
                 "LASTALIVEYR",
                 #"GENDER",
                 "KMARST",
                 "LMARST",
                 "MMARST",
                 "NMARST",
                 "OMARST",
                 "PMARST",
                 "DEGREE",
                 "RACE")]

bio2006 <- bio2006[,c("ID",
                    "KA1CBIOS",
                    "KTCBIOS",
                    "KHDLBIOS",
                    "KCRP_IMP")]
bio2008 <- bio2008[,c("ID",
                      "LA1CBIOS",
                      "LTCBIOS",
                      "LHDLBIOS",
                      "LCRP_IMP")]
bio2010 <- bio2010[,c("ID",
                      "MA1CHER",
                      "MTCHER",
                      "MHDLHER",
                      "MCRPUW")]
bio2012 <- bio2012[,c("ID",
                      "NA1CUW",
                      "NTCUW",
                      "NHDLUW",
                      "NCRPUW")]
bio2014 <- bio2014[,c("ID",
                      "OA1CUW",
                      "OTCUW",
                      "OHDLUW",
                      "OCRPUW")]
bio2016 <- bio2016[,c("ID",
                      "PA1CUW",
                      "PTCUW",
                      "PHDLUW",
                      "PCRPUW")]

library(dplyr)

# 假设bio2006是基础数据框，我们将以此为基础将其他数据框合并进来
biomarker <- bio2006

# 依次将后续年份的数据框合并进来
biomarker <- full_join(biomarker, bio2008, by = "ID")
biomarker <- full_join(biomarker, bio2010, by = "ID")
biomarker <- full_join(biomarker, bio2012, by = "ID")
biomarker <- full_join(biomarker, bio2014, by = "ID")
biomarker <- full_join(biomarker, bio2016, by = "ID")

whole_data <- left_join(data,biomarker,by="ID")
saveRDS(whole_data,"original data/whole_data.RDS")


####读取各年份的一些信息####
####2006的举例####
rm(list=ls())
#使用sps文件，按照存储和保存要求，然后快速点击处理获取sav文件
library(readr)

# 设置文件夹路径
folder_path <- "data/HRS Core/hrs2006"

# 获取文件夹下所有的.sav文件
sav_files <- list.files(path = folder_path, pattern = "\\.sav$", full.names = TRUE)

# 批量读取所有.sav文件
data_list <- lapply(sav_files, read_sav)

df <- data_list

library(readr)

# 假设data_list已经通过之前的步骤获取

# 定义一个函数来检查并合并ID
merge_id <- function(df) {
  if ("HHID" %in% names(df) && "PN" %in% names(df)) {
    df$ID <- paste(df$HHID, df$PN, sep = "")
    return(df)
  } else {
    return(NULL)
  }
}

# 应用函数到data_list的每个元素，并过滤掉NULL值
merged_data_list <- lapply(data_list, merge_id)
merged_data_list <- merged_data_list[!sapply(merged_data_list, is.null)]

# 获取可以合并ID的子集索引
can_merge_ids <- which(sapply(merged_data_list, function(x) !is.null(x)))

# 用户指定的变量名称列表
specified_variables <- c("KZ205", "KC128", "KC223", "KC224", "KC225", 
                         "KZ103", "KZ102", "KZ106", "KZ105", "KC036", 
                         "KC048", "KC040", "KC045", "KC141", "KC139", 
                         "KI907", "KI869", "KI870")

# 提取对应子集的ID和指定变量
extracted_data <- lapply(merged_data_list, function(df) {
  vars_to_extract <- specified_variables[specified_variables %in% names(df)]
  if (length(vars_to_extract) > 0) {
    df[c("ID", vars_to_extract)]
  } else {
    NULL
  }
})

# 过滤掉NULL值
extracted_data <- Filter(Negate(is.null), extracted_data)

# 初始化combined_data为第一个子集（如果有的话）
if (length(extracted_data) > 0) {
  combined_data <- extracted_data[[1]]
} else {
  combined_data <- NULL
  cat("没有数据可以合并。\n")
}

# 遍历剩余的子集，使用left_join按ID合并
for (i in 2:length(extracted_data)) {
  if (!is.null(combined_data)) {
    combined_data <- left_join(combined_data, extracted_data[[i]], by = "ID")
  } else {
    break
  }
}

# 如果combined_data不为空，输出合并后的数据框行数
if (!is.null(combined_data)) {
  cat("合并后的数据框包含", nrow(combined_data), "行数据。\n")
} else {
  cat("没有数据可以合并。\n")
}

# 输出可以合并ID的子集索引
can_merge_ids <- which(!sapply(data_list, is.null))
cat("可以合并ID的子集索引：", can_merge_ids, "\n")
saveRDS(combined_data,"original data/combined_data 2006.RDS")

####批量提取2006~2016的数据####
rm(list=ls())
library(readr)
library(dplyr)

# 定义处理数据的函数
process_year_data <- function(year) {
  # 设置文件夹路径
  folder_path <- paste0("data/HRS Core/hrs", year)
  
  # 获取文件夹下所有的.sav文件
  sav_files <- list.files(path = folder_path, pattern = "\\.sav$", full.names = TRUE)
  
  # 批量读取所有.sav文件
  data_list <- lapply(sav_files, read_sav)
  
  # 定义一个函数来检查并合并ID
  merge_id <- function(df) {
    if ("HHID" %in% names(df) && "PN" %in% names(df)) {
      df$ID <- paste(df$HHID, df$PN, sep = "")
      return(df)
    } else {
      return(NULL)
    }
  }
  
  # 应用函数到data_list的每个元素，并过滤掉NULL值
  merged_data_list <- lapply(data_list, merge_id)
  merged_data_list <- merged_data_list[!sapply(merged_data_list, is.null)]
  
  # 用户指定的变量名称列表，包括不同年份的数据
  variable_prefixes <- c("K", "L", "M", "N", "O", "P")  # 对应2006到2016年的前缀
  base_variables <- c("Z205", "C128", "C223", "C224", "C225", 
                      "Z103", "Z102", "Z106", "Z105", "C036", 
                      "C048", "C040", "C045", "C141", "C139", 
                      "I907", "I869", "I870")
  
  # 创建指定变量名称列表
  specified_variables <- unlist(lapply(variable_prefixes, function(prefix) {
    paste0(prefix, base_variables)
  }))
  
  # 提取对应子集的ID和指定变量
  extracted_data <- lapply(merged_data_list, function(df) {
    vars_to_extract <- specified_variables[specified_variables %in% names(df)]
    if (length(vars_to_extract) > 0) {
      df[c("ID", vars_to_extract)]
    } else {
      NULL
    }
  })
  
  # 过滤掉NULL值
  extracted_data <- Filter(Negate(is.null), extracted_data)
  
  # 初始化combined_data为第一个子集（如果有的话）
  if (length(extracted_data) > 0) {
    combined_data <- extracted_data[[1]]
  } else {
    combined_data <- NULL
    cat("没有数据可以合并。\n")
    return(NULL)
  }
  
  # 遍历剩余的子集，使用left_join按ID合并
  for (i in 2:length(extracted_data)) {
    if (!is.null(combined_data)) {
      combined_data <- left_join(combined_data, extracted_data[[i]], by = "ID")
    } else {
      break
    }
  }
  
  # 如果combined_data不为空，输出合并后的数据框行数
  if (!is.null(combined_data)) {
    cat("合并后的数据框包含", nrow(combined_data), "行数据。\n")
    saveRDS(combined_data, paste0("original data/combined_data_", year, ".RDS"))
  } else {
    cat("没有数据可以合并。\n")
  }
}

# 批量处理2006至2016年的数据
years <- c(2006, 2008, 2010, 2012, 2014, 2016)
lapply(years, process_year_data)

####合并不同年份的数据####
rm(list=ls())
# 加载必要的库
library(dplyr)

# 指定包含.rds文件的文件夹路径
data_dir <- "original data"

# 获取所有符合模式的文件名
files <- list.files(pattern = "combined_data_20[06|08|10|12|14|16]", full.names = TRUE, recursive = TRUE, path = data_dir)

# 读取第一个.rds文件
first_file <- readRDS(files[1])

# 创建一个空的数据框用于存储合并后的数据
combined_data <- first_file

# 遍历剩余的文件并使用full_join按ID合并
for (i in 2:length(files)) {
  next_data <- readRDS(files[i])
  # 假设每个数据框都有一个名为"ID"的列用于合并
  combined_data <- full_join(combined_data, next_data, by = "ID")
}
# 检查结果
str(combined_data)

# 定义一个函数来转换单位和计算BMI
convert_and_calculate_bmi <- function(data, height_col, waist_col, weight_col, bmi_col) {
  # 转换身高从feet转换为厘米
  data[[height_col]] <- data[[height_col]] * 30.48
  # 转换腰围从inches转换为厘米
  data[[waist_col]] <- data[[waist_col]] * 2.54
  # 转换体重从磅转换为千克
  data[[weight_col]] <- data[[weight_col]] * 0.45359237
  # 计算BMI
  data[[bmi_col]] <- data[[weight_col]] / (data[[height_col]] / 100)^2
  return(data)
}

# 对每个周期应用转换和计算BMI
周期 <- c("K", "L", "M", "N", "O", "P")
for (cycle in 周期) {
  height_col <- paste0(cycle, "C141")
  waist_col <- paste0(cycle, "I907")
  weight_col <- paste0(cycle, "C139")
  bmi_col <- paste0(cycle, "BMI")
  combined_data <- convert_and_calculate_bmi(combined_data, height_col, waist_col, weight_col, bmi_col)
}

# 最终的合并数据框存储在combined_data中

data <- combined_data
data1 <- readRDS("original data/whole_data.RDS")
library(dplyr)
whole_data <- left_join(data1,data,by = "ID")
df <- whole_data
# 将第2到第7列中值为999的元素替换为NA
df[, 2:7][df[, 2:7] == 999] <- NA
# 假设df是你的数据框
# 创建新的Status列，如果KNOWNDECEASEDYR或KNOWNDECEASEDMO中至少有一个不是NA，则标记为1，否则标记为0
df$Status <- ifelse(!is.na(df$KNOWNDECEASEDYR) | !is.na(df$KNOWNDECEASEDMO), 1, 0)
# 假设df是你的数据框
colnames(df)
# 将第一次随访的年份和月份转换为日期格式
first_visit_date <- as.Date(paste(df$FIRSTIW, "01", "01", sep = "-"))

# 将已知亡故的年份和月份转换为日期格式
# 如果KNOWNDECEASEDMO是NA，我们假设亡故月份为1月
deceased_month <- ifelse(is.na(df$KNOWNDECEASEDMO), 1, df$KNOWNDECEASEDMO)
deceased_date <- as.Date(paste(df$KNOWNDECEASEDYR, deceased_month, "01", sep = "-"))

# 计算两个日期之间的天数差
df$Time_day <- as.numeric(difftime(deceased_date, first_visit_date, units = "days"))

str(df)
saveRDS(df,"original data/df.RDS")
####数据合并####
rm(list = ls())
df <- readRDS("original data/df.RDS")
library(dplyr)
library(tidyr)
library(stringr)

# 不变变量列表
unchanged_vars <- c("KNOWNDECEASEDYR", "KNOWNDECEASEDMO", "GENDER", "HHID", "PN", "BIRTHMO", "BIRTHYR",
                    "EXDEATHMO", "EXDEATHYR", "FIRSTIW", "LASTALIVEMO", "LASTALIVEYR", "DEGREE", "RACE",
                    "Status", "Time_day")

# 确定ID列的名称
id_var <- "ID"  # 假设ID列的名称是"ID"

# 如果ID列不存在，则需要从df中确定
if (!id_var %in% names(df)) {
  id_var <- names(df)[which(names(df) %in% c("ID", "id", "Id", "iD", "IDNUMBER", "idnumber"))]
}

# 生物标志物变量列表
biomarker_vars <- c(
  "KA1CBIOS", "KTCBIOS", "KHDLBIOS", "KCRP_IMP",
  "LA1CBIOS", "LTCBIOS", "LHDLBIOS", "LCRP_IMP",
  "MA1CHER", "MTCHER", "MHDLHER", "MCRPUW",
  "NA1CUW", "NTCUW", "NHDLUW", "NCRPUW",
  "OA1CUW", "OTCUW", "OHDLUW", "OCRPUW",
  "PA1CUW", "PTCUW", "PHDLUW", "PCRPUW"
)

# 提取不变的变量数据框
unchanged_df <- df %>% 
  select(all_of(c(id_var, unchanged_vars)))

# 提取生物标志物数据框
biomarker_df <- df %>% 
  select(all_of(c(id_var, biomarker_vars)))

# 提取非生物标志物数据框
non_biomarker_df <- df %>% 
  select(-all_of(c(unchanged_vars, biomarker_vars))) %>% 
  mutate(ID = id_var) %>% 
  select(all_of(c(setdiff(names(df), c(unchanged_vars, biomarker_vars)))))
non_biomarker_df <- cbind(df[,1],non_biomarker_df[,-1])

# 现在我们有了三个数据框：
# unchanged_df: 不变变量数据框
# biomarker_df: 生物标志物数据框
# non_biomarker_df: 非生物标志物数据框
# 假设 biomarker_df 是你的原始数据框

# 定义一个函数来转换数据框
convert_biomarkers <- function(df) {
  # 定义周期和对应的列名
  periods <- list(
    K = c("KA1CBIOS", "KTCBIOS", "KHDLBIOS", "KCRP_IMP"),
    L = c("LA1CBIOS", "LTCBIOS", "LHDLBIOS", "LCRP_IMP"),
    M = c("MA1CHER", "MTCHER", "MHDLHER", "MCRPUW"),
    N = c("NA1CUW", "NTCUW", "NHDLUW", "NCRPUW"),
    O = c("OA1CUW", "OTCUW", "OHDLUW", "OCRPUW"),
    P = c("PA1CUW", "PTCUW", "PHDLUW", "PCRPUW")
  )
  
  # 创建一个新的数据框来存储结果
  result_df <- data.frame(ID = integer(), Period = character(), A1C = numeric(), TC = numeric(), HDL = numeric(), CRP = numeric(), stringsAsFactors = FALSE)
  
  # 遍历每个周期，提取数据并添加到结果数据框中
  for (period in names(periods)) {
    period_df <- df[, c("ID", periods[[period]])]
    colnames(period_df) <- c("ID", "A1C", "TC", "HDL", "CRP")
    period_df$Period <- as.character(2005 + match(period, c("K", "L", "M", "N", "O", "P")))
    result_df <- rbind(result_df, period_df)
  }
  
  # 返回结果数据框
  return(result_df)
}

# 调用函数并查看结果
convert_biomarkers_df <- convert_biomarkers(biomarker_df)

# 定义一个函数来转换非生物标志物数据框
convert_non_biomarkers <- function(df) {
  # 定义周期和对应的列名
  periods <- list(
    K = grep("^K", names(df), value = TRUE),
    L = grep("^L", names(df), value = TRUE),
    M = grep("^M", names(df), value = TRUE),
    N = grep("^N", names(df), value = TRUE),
    O = grep("^O", names(df), value = TRUE),
    P = grep("^P", names(df), value = TRUE)
  )
  
  # 创建一个新的数据框来存储结果
  result_df <- data.frame(ID = integer(), Period = integer(), stringsAsFactors = FALSE)
  
  # 添加非生物标志物的列
  for (i in 1:18) {
    result_df[[paste0("NonBiomarker", i)]] <- numeric()
  }
  
  # 遍历每个周期，提取数据并添加到结果数据框中
  for (period in names(periods)) {
    # 提取当前周期的数据
    period_df <- df[, c("ID", periods[[period]])]
    # 重命名列以统一非生物标志物的列名
    colnames(period_df) <- c("ID", paste0("NonBiomarker", 1:length(periods[[period]])))
    # 添加周期标识符列
    period_df$Period <- 2005 + match(period, c("K", "L", "M", "N", "O", "P"))
    result_df <- rbind(result_df, period_df)
  }
  
  # 返回结果数据框
  return(result_df)
}


# 调用函数并查看结果
converted_non_biomarker_df <- convert_non_biomarkers(non_biomarker_df)

# 选择除了第一列和第六列之外的所有列
whole_data <- cbind(converted_non_biomarker_df,convert_biomarkers_df[,-c(1,6)])
# 定义新的变量名
new_variable_names <- c("Age", "Marital status", "Ever drink alcohol",
                        "Intensive Physical Activity", "Moderate Physical Activity",
                        "Light Physical Activity", "Heart condition", "Congestive heart failure",
                        "Myocardial infarction/heart attack", "Angina pectoris",
                        "Standing Height (m)", "Weight (kg)", "Waist Circumference (cm)",
                        "Systolic blood pressure (mmHg)", "Diastolic blood pressure (mmHg)",
                        "Smoking", "Cancer", "Diabetes", "Stroke", "Heart problem", "BMI")

# 更改第1到21个变量的名称
names(whole_data)[2:22] <- new_variable_names

# 定义新的变量名
new_names <- c("Glycohemoglobin (%)", "Total Cholesterol (mg/dL)", "HDL Cholesterol (mmol/L)", "C-Reactive Protein (CRP) (mg/L)")

# 定义旧的变量名
old_names <- c("A1C", "TC", "HDL", "CRP")

# 更改变量名
names(whole_data)[names(whole_data) %in% old_names] <- new_names

# 获取除了 Period 列的所有列的名称
other_columns <- names(whole_data)[!names(whole_data) %in% "Period"]

# 创建新的列顺序，将 Period 列放在第二位
new_order <- c("ID", "Period", other_columns)

# 根据新的列顺序重排数据框
whole_data <- whole_data[, new_order]
whole_data <- whole_data[, -3]

whole_data_final <- left_join(whole_data,unchanged_df,by="ID")
saveRDS(whole_data_final,"original data/whole_data_final.RDS")

####数据清洗####
library(dplyr)

# 清除环境变量并读取数据
rm(list = ls())
whole_data_final <- readRDS("original data/whole_data_final.RDS")

# 筛选出Age不等于NA的数据
whole_data_final <- whole_data_final %>%
  filter(!is.na(Age))

# 确保 Period 列是数值类型
whole_data_final$Period <- as.numeric(as.character(whole_data_final$Period))

# 将9996替换为NA，以便正确处理首次随访时间
whole_data_final$FIRSTIW[whole_data_final$FIRSTIW == 9996] <- NA

# 为每个随访时间点创建一个日期
whole_data_final$visit_date <- as.Date(paste(whole_data_final$Period, "01", "01", sep = "-"))

# 将已知亡故的年份和月份转换为日期格式
# 如果KNOWNDECEASEDMO是NA，我们假设亡故月份为1月
whole_data_final$deceased_month <- ifelse(is.na(whole_data_final$KNOWNDECEASEDMO), 1, whole_data_final$KNOWNDECEASEDMO)
whole_data_final$deceased_date <- as.Date(paste(whole_data_final$KNOWNDECEASEDYR, whole_data_final$deceased_month, "01", sep = "-"))

# 确保whole_data_final数据框中有Status列，并且所有行都初始化为0
whole_data_final$Status <- 0

# 找出每个ID最接近死亡日期的随访记录，并设置Status为1
whole_data_final <- whole_data_final %>%
  group_by(ID) %>%
  mutate(diff_to_death = ifelse(is.na(KNOWNDECEASEDYR), Inf, as.numeric(difftime(deceased_date, visit_date,  units = "days")))) %>%
  filter(is.na(KNOWNDECEASEDYR) | (diff_to_death == min(diff_to_death))) %>%
  mutate(Status = ifelse(is.na(KNOWNDECEASEDYR), 0, 1)) %>%
  ungroup()

# 计算Time_day
whole_data_final <- whole_data_final %>%
  mutate(Time_day = ifelse(
    Status == 1,
    as.numeric(difftime(deceased_date, visit_date, units = "days")),
    as.numeric(difftime(visit_date, as.Date(paste(FIRSTIW, "01", "01", sep = "-")), units = "days"))
  ))

# 处理NA值，确保Time_day列有正确的长度
whole_data_final$Time_day[is.na(whole_data_final$Time_day)] <- 0

# 将日期转换为xxxx年xx月xx日的格式
whole_data_final$visit_date_formatted <- format(whole_data_final$visit_date, "%Y年%m月%d日")
whole_data_final$deceased_date_formatted <- format(whole_data_final$deceased_date, "%Y年%m月%d日")
whole_data_final$first_visit_date_formatted <- format(as.Date(paste(whole_data_final$FIRSTIW, "01", "01", sep = "-")), "%Y年%m月%d日")

# 查看结果
head(whole_data_final)

# 创建CVD列，如果任意一个心血管疾病变量为1，则CVD标记为1，否则为0
# 将指定的心血管疾病变量中的NA替换为"99_NA"
cvd_vars <- c("Stroke", "Heart problem", "Heart condition", 
              "Congestive heart failure", "Myocardial infarction/heart attack", "Angina pectoris")

whole_data_final[cvd_vars] <- lapply(whole_data_final[cvd_vars], function(x) {
  x <- as.character(x)  # 转换为字符型
  x[is.na(x)] <- "99_NA"  # 将NA替换为"99_NA"
  return(x)
})

# 创建CVD列，如果任意一个心血管疾病变量为"1"，则CVD标记为1，否则为0
whole_data_final <- whole_data_final %>%
  mutate(CVD = ifelse(rowSums(sapply(whole_data_final[cvd_vars], function(x) x == "1")) > 0, 1, 0))

# 筛选CVD等于1的行
cvd_filtered_data <- whole_data_final %>%
  filter(CVD == 1)

# 从筛选后的数据框中删除CVD列
cvd_filtered_data <- cvd_filtered_data %>%
  select(-CVD)
saveRDS(cvd_filtered_data,"original data/cvd_filtered_data.RDS")

####二分类变量的替换，最终数据集的构建####
rm(list = ls())
library(openxlsx)
data <- readRDS("original data/cvd_filtered_data.RDS")
str(data)
library(dplyr)

# 假设你的数据框名为data
# 首先，我们将需要替换的列名进行替换
data <- data %>%
  rename(
    `BMI (kg/m2)` = BMI,
    `Race` = RACE,
    `Education level` = DEGREE,
    `Gender` = GENDER
  )

# 使用select()函数提取指定的列
new_data <- data %>%
  select(
    ID, Status, Time_day, Age, 
    `Gender`, `Marital status`, `Education level`, `Race`, Smoking, `Ever drink alcohol`,
    `Intensive Physical Activity`, `Moderate Physical Activity`, `Light Physical Activity`, Cancer, Diabetes, Stroke,
    `Heart problem`, `Heart condition`, `Congestive heart failure`, `Myocardial infarction/heart attack`, `Angina pectoris`,
    `Standing Height (m)`, 'Weight (kg)', `BMI (kg/m2)`, `Waist Circumference (cm)`, 
    `Systolic blood pressure (mmHg)`, `Diastolic blood pressure (mmHg)`, `Glycohemoglobin (%)`, 
    `Total Cholesterol (mg/dL)`, `HDL Cholesterol (mmol/L)`, `C-Reactive Protein (CRP) (mg/L)`
  )

# 现在new_data包含了你指定的所有变量，并且列名中的空格被保留
library(dplyr)

# 替换NA为99_NA
new_data <- new_data %>%
  mutate(
    across(
      .cols = c("Gender", "Marital status", "Education level", "Race", "Smoking", "Ever drink alcohol", 
                "Intensive Physical Activity", "Moderate Physical Activity", "Light Physical Activity", 
                "Cancer", "Diabetes", "Stroke", "Heart problem", "Heart condition", "Congestive heart failure", 
                "Myocardial infarction/heart attack", "Angina pectoris"),
      .fns = ~ifelse(is.na(.), "99_NA", .)
    )
  )

# 替换数值为字符
new_data <- new_data %>%
  mutate(
    Gender = recode(new_data$Gender, `1` = "Male", `2` = "Female"),
    `Marital status` = recode(new_data$`Marital status`, `1` = "Married", `2` = "Separated/Divorced", `3` = "Widowed", `4` = "Never Married", `5` = "Marital Status Unknown"),
    `Education level` = recode(new_data$`Education level`, `0` = "No degree", `1` = "GED", `2` = "High school diploma", `3` = "Two-year college degree", `4` = "Four-year college degree", `5` = "Master's degree", `6` = "Professional degree (Ph.D., M.D., J.D.)", `9` = "Degree unknown/Some College"),
    Race = recode(new_data$Race, `0` = "Not obtained", `1` = "White/Caucasian", `2` = "Black or African American", `7` = "Other"),
    Smoking = recode(new_data$Smoking, `1` = "Yes", `5` = "No"),
    `Ever drink alcohol` = recode(new_data$`Ever drink alcohol`, `1` = "Yes", `3` = "Never Have Used Alcohol", `5` = "No"),
    `Intensive Physical Activity` = recode(new_data$`Intensive Physical Activity`, `1` = "More Than Once A Week", `2` = "Once A Week", `3` = "One To Three Times A Month", `4` = "Hardly Ever Or Never", `7` = "Every Day"),
    `Moderate Physical Activity` = recode(new_data$`Moderate Physical Activity`, `1` = "More Than Once A Week", `2` = "Once A Week", `3` = "One To Three Times A Month", `4` = "Hardly Ever Or Never", `7` = "Every Day"),
    `Light Physical Activity` = recode(new_data$`Light Physical Activity`, `1` = "More Than Once A Week", `2` = "Once A Week", `3` = "One To Three Times A Month", `4` = "Hardly Ever Or Never", `7` = "Every Day"),
    Cancer = recode(new_data$Cancer, `1` = "Yes", `5` = "No"),
    Diabetes = recode(new_data$Diabetes, `1` = "Yes", `5` = "No"),
    Stroke = recode(new_data$Stroke, `1` = "Yes", `5` = "No"),
    `Heart problem` = recode(new_data$`Heart problem`, `1` = "Yes", `5` = "No"),
    `Heart condition` = recode(new_data$`Heart condition`, `1` = "Yes", `3` = "Disputes Previous Wave Record, But Now Has Condition", `4` = "Disputes Previous Wave Record, Does Not Have Condition", `5` = "No"),
    `Congestive heart failure` = recode(new_data$`Congestive heart failure`, `1` = "Yes", `5` = "No"),
    `Myocardial infarction/heart attack` = recode(new_data$`Myocardial infarction/heart attack`, `1` = "Yes", `5` = "No"),
    `Angina pectoris` = recode(new_data$`Angina pectoris`, `1` = "Yes", `5` = "No")
  )
saveRDS(new_data,"original data/new_data.RDS")
