setwd('./')

getwd()

df <- read.csv("./data_sets/Intel_CPUs.csv")

head(df, 5)

# Thay thế các ô thiếu dữ liệu bằng giá trị NA, đồng thời cũng thay thế các chuỗi NA thành giá trị NA.
df[df == ""] <- NA
df[df == "N/A"] <- NA
df[df == "NA"] <- NA

# Vì có rất nhiều cột bị thiếu dữ liệu nhiều hơn so với cột khác, do đó nếu ta khảo sát trên các dữ liệu đó thì sẽ khiến cho việc thống kê trở nên sai lệch.
library(questionr)
missing_counts = freq.na(df)

library(ggplot2)
ggplot(missing_counts, aes(x = rownames(missing_counts), y = missing_counts[,2], )) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_text(aes(label = paste0(missing_counts[,2], "%")), vjust = -0.5, size = 2) +
  labs(title = "Tỷ lệ dữ liệu khuyết của các đặc điểm thống kê", x = "Đặc điểm thống kê", y = "Tỷ lệ dữ liệu khuyết (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(
    size = 10,
    angle = 90,
    hjust = 1
  ))

# Lưu ảnh đồ thị missing_counts
ggsave(
  filename = "missing_plot.png",
  plot = last_plot(),   # hoặc tên đối tượng biểu đồ
  width = 15, height = 8, units = "in", dpi = 300
)

# Ở bài toán này, nhằm tăng độ chính xác của thống kê, ta sẽ chỉ xét các đặc điểm thống kê (feature) có tỉ lệ khuyết <= 10%.
# Loại bỏ các đặc điểm có tỉ lệ khuyết cao.
# cols_to_keep <- names(missing_counts[missing_counts <= 10])

missing_counts_df <- data.frame(
  feature = rownames(missing_counts),
  percent = missing_counts[,2]
)

cols_to_keep <- missing_counts_df$feature[missing_counts_df$percent <= 10]
df_filtered <- df[, cols_to_keep, drop = FALSE]

head(df_filtered, 5)
