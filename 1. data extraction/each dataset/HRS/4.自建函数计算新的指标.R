####自建函数计算新的指标####
library(purrr)
library(dplyr)
library(tidyr)
library(openxlsx)
rm(list=ls())
# 读取数据
data <- readRDS("result/completedData.RDS")
# 提取所需的列
data <- data[, c(21:32)]
names(data) <- paste0("var_", 1:12)

# 定义运算符号和对数运算相关选项
operators <- c("*", "/")
ln_or_identity <- c("log", "log10", "identity")

# 生成所有可能的运算函数组合
generate_operations <- function() {
  all_operations <- list()
  
  for (var1_index in 1:12) {
    for (var2_index in 1:12) {
      for (op1 in operators) {
        for (var3_index in 1:12) {
          for (number_choose in 1:2) {
            for (op2 in operators) {
              for (ln_op in ln_or_identity) {  # 新增循环遍历对数运算选项
                operation_str <- ""
                
                # 第一部分：var_1~var_11中的任意一个变量相乘或相除var_1~var_11中的任意一个变量
                part1 <- paste0("var_", var1_index, op1, "var_", var2_index)
                
                if (number_choose == 1) {
                  # 第二部分：要么*1
                  part2 <- paste0(part1, " * 1")
                } else {
                  # 第二部分：要么* or / 2
                  part2 <- paste0(part1, op1, " 2")
                }
                
                # 根据选择进行对数运算或保持不变
                if (ln_op == "log") {
                  # 第二部分：进行ln对数运算
                  part3 <- paste0("log(", part2, ")")
                } else if (ln_op == "log10") {
                  # 第二部分：进行log10对数运算
                  part3 <- paste0("log10(", part2, ")")
                } else {
                  # 第二部分：乘1（等同于不进行对数运算）
                  part3 <- paste0("1 * ", part2)
                }
                
                # 第三部分：将前一步的运算结果相乘或相除var_1~var_11中的任意一个变量
                part4 <- paste0(part3, op2, "var_", var3_index)
                
                operation_str <- part4
                
                all_operations <- c(all_operations, operation_str)
              }
            }
          }
        }
      }
    }
  }
  
  return(all_operations)
}

# 假设已经有了名为data的数据框，且列名为var_1到var_11

# 生成所有运算组合
all_operations <- generate_operations()

# 对每个运算组合，添加新列到数据框并计算
for (i in 1:length(all_operations)) {
  operation <- all_operations[[i]]
  data[, paste0("index_", sprintf("%03d", i))] <- with(data, eval(parse(text = operation)))
}

# 保存结果为 RDS 文件
saveRDS(data, "Result/index compute result.RDS")

# 创建一个数据框用于存储对应关系
operation_mapping <- data.frame(
  operation_name = paste0("index_", sprintf("%03d", 1:length(all_operations))),
  operation_expression = unlist(all_operations)
)
# 将对应关系数据框导出为csv文件
write.csv(operation_mapping, "Result/operation_mapping.csv", row.names = FALSE)
