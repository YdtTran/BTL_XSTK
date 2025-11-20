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

head(main_df, 5)

write.csv(main_df, "out.csv")

ordered_cols <- c(
  "Manufacturer",
  "Release_Date" ,
  "Memory_Bandwidth",
  "Memory_Speed",
  "L2_Cache",
  "Open_GL",
  "Memory_Type",
  "Resolution_WxH",
  "Direct_X"
)

ordered_cols <- intersect(ordered_cols, colnames(main_df))
main_df <- main_df[, c(ordered_cols, setdiff(colnames(main_df), ordered_cols))]

print(colnames(main_df))

# bandwidth <- log(main_df$Memory_Bandwidth)
# manufacturer <- main_df$Manufacturer
# dedicated <- main_df$Dedicated

main_df$Manufacturer <- as.factor(main_df$Manufacturer)
main_df$Dedicated <- as.factor(main_df$Dedicated)
str(main_df$Manufacturer)
str(main_df$Dedicated)

table (main_df$Manufacturer)
