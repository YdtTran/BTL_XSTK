library(dplyr)
library(questionr)
library(ggplot2)

# Kiểm tra thư mục làm việca
setwd('./')
getwd()

# Đọc dữ liệu từ file và hiện 5 hàng đầu
df <- read.csv("./data_sets/All_GPUs.csv")
head(df, 5)


# Tạo ra một dataframe mới gọi là main_df và sẽ làm việc chính trên cái này.
source("clean.R")

# Chỉ lấy các biến được cho là quan trọng trong quá trình kiểm định
ordered_cols <- c(
  "Manufacturer",
  "Memory_Bandwidth",
  "Memory_Speed",
  "L2_Cache",
  "Open_GL",
  "Memory_Type",
  "Direct_X"
)

ordered_cols <- intersect(ordered_cols, colnames(main_df))
main_df <- main_df[, c(ordered_cols, setdiff(colnames(main_df), ordered_cols))]


head(main_df, 5)



write.csv(main_df[,-1], "out.csv")

print(colnames(main_df))


