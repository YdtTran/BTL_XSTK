# Thay thế các ô thiếu dữ liệu bằng giá trị NA, đồng thời cũng thay thế các chuỗi NA thành giá trị NA.
df <- df %>%
  mutate(across(where(is.character), trimws))

df[df == ""] <- NA
df[df == "N/A"] <- NA
df[df == "NA"] <- NA
df[df == "-"] <- NA
df[df == "Unknown Release Date"] <- NA
# Chỉ lấy năm sản xuất, không lấy ngày cụ thể
df$Release_Date <- as.Date(df$Release_Date, format = "%d-%b-%Y")
df$Release_Date <- format(df$Release_Date, "%Y")


# Vì có rất nhiều cột bị thiếu dữ liệu nhiều hơn so với cột khác, do đó nếu ta khảo sát trên các dữ liệu đó thì sẽ khiến cho việc thống kê trở nên sai lệch.
missing_counts = freq.na(df)

ggplot(missing_counts, aes(x = rownames(missing_counts), y = missing_counts[,2], )) +
  geom_bar(stat = "identity", fill = "cyan") +
  geom_text(aes(label = paste0(missing_counts[,2], "%")), vjust = -0.5, size = 2) +
  labs(title = "Missing rate", x = "Feature", y = "Rate(%)") +
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

cols_to_keep <- missing_counts_df$feature[missing_counts_df$percent <= 15 & missing_counts_df$feature != "Architecture" & missing_counts_df$feature != "Name"]
df_filtered <- df[, cols_to_keep, drop = FALSE]

head(df_filtered, 5)
df_filtered <- na.omit(df_filtered)
print(names(df_filtered))

# Xoá đơn vị
remove_unit_cols <- c("Memory_Bandwidth", "Memory_Speed", "Memory_Bus", "Direct_X", "Memory", "Process")

main_df <- df_filtered

# print(df_filtered[remove_unit_cols])
main_df[remove_unit_cols] <- lapply(df_filtered[remove_unit_cols], function(x) {
  as.numeric(gsub("[^0-9.]", "", x))
})

# Xử lý L2_Cached riêng vì đặc điểm này có nhân hệ số ở sau
clean_cache <- function(x) {
  # Nếu NA hoặc rỗng thì trả về NA
  main <- as.numeric(sub("KB.*", "", x))      # 2304
  mult      <- as.numeric(sub(".*\\(x([0-9]+)\\)", "\\1", x))  # 2
  if (is.na(mult)) mult <- 1
  return(main * mult)
}

main_df$L2_Cache <- sapply(main_df$L2_Cache, clean_cache)

# 
# ### Load file
# # main_df <- read.csv("./out.csv")
# # main_df <- main_df[,-1]
# 
### ------------------ Clean 2

numeric_data <- main_df[, c(
  "Memory_Bandwidth",
  "Process",
  "Memory_Speed",
  "Memory_Bus",
  "L2_Cache"
)]

is_outlier <- function(x) {
  if (all(is.na(x))) return(rep(FALSE, length(x)))
  
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  return(x < lower_bound | x > upper_bound)
}

outlier_matrix <- sapply(numeric_data, is_outlier)
rows_with_outliers <- rowSums(outlier_matrix) > 0
main_df <- main_df[!rows_with_outliers, ]

# 1. Tạo danh sách các giá trị muốn loại bỏ
gia_tri_can_xoa <- c("Intel", "DDR2", "eDRAM", "GDDR2", "GDDR4", "GDDR5X")

# 2. Viết hàm thay thế
thay_the_na <- function(df, danh_sach_xoa) {
  # Duyệt qua từng cột của dataframe
  df[] <- lapply(df, function(x) {
    # Nếu giá trị của x nằm trong danh sách xóa thì thay bằng NA, ngược lại giữ nguyên
    replace(x, x %in% danh_sach_xoa, NA)
  })
  return(df)
}

# 3. Áp dụng hàm vào main_df
main_df <- thay_the_na(main_df, gia_tri_can_xoa)

# 4. Loại bỏ các dòng chứa NA (như ý bạn muốn)
main_df <- na.omit(main_df)
### ----------------------- Clean 2

