# Thống kê mô tả
main_df <- read.csv("./out.csv")
main_df <- main_df[,-1]
numeric_data <- main_df[, c(
  "Memory_Bandwidth",
  "Process",
  "Memory_Speed",
  "Memory_Bus",
  "L2_Cache"
)]
sds <- sapply(numeric_data, sd, na.rm = TRUE)
summary(numeric_data)
sds

par(mfrow = c(2, 2))

# Vẽ lần lượt từng biểu đồ
boxplot(
  main_df$Process,
  main = "Process (nm)",
  col = "lightblue",
  ylab = "Value"
)
boxplot(
  main_df$Memory_Speed,
  main = "Memory Speed (MHz)",
  col = "lightgreen",
  ylab = "Value"
)
boxplot(
  main_df$Memory_Bus,
  main = "Memory Bus (Bit)",
  col = "pink",
  ylab = "Value"
)
boxplot(
  main_df$L2_Cache,
  main = "L2 Cache (KB)",
  col = "wheat",
  ylab = "Value"
)

# Trả lại màn hình về chế độ 1x1 bình thường sau khi vẽ xong
par(mfrow = c(1, 1))

table(main_df$Manufacturer)
table(main_df$Memory_Type)

ggplot(main_df, aes(x = Memory_Bandwidth)) +
  geom_histogram(bins = 20,
                 fill = "skyblue",
                 color = "black")



ve_bieu_do_tuy_chinh <- function(du_lieu_x,
                                 du_lieu_y,
                                 nhan_x,
                                 nhan_y,
                                 namex,
                                 namey) {
  plot(
    x = du_lieu_x,
    y = du_lieu_y,
    # Tự động ghép chuỗi để tạo tiêu đề
    main = paste("Mối quan hệ giữa", namex, "và", namey),
    xlab = nhan_x,
    ylab = nhan_y,
    pch = 19,
    # Kiểu chấm tròn đặc
    col = "darkblue",
    # Màu sắc
    frame = FALSE
  )     # Bỏ khung viền
}

par(mfrow = c(2, 2))
ve_bieu_do_tuy_chinh(
  main_df$Memory_Bus,
  main_df$Memory_Bandwidth,
  "Bit",
  "GB/s",
  "Memory_Bus",
  "Memory_Bandwidth"
)
ve_bieu_do_tuy_chinh(
  main_df$L2_Cache,
  main_df$Memory_Bandwidth,
  "KB",
  "GB/s",
  "L2_Cache",
  "Memory_Bandwidth"
)
ve_bieu_do_tuy_chinh(
  main_df$Memory_Speed,
  main_df$Memory_Bandwidth,
  "MHz",
  "GB/s",
  "Memory_Speed",
  "Memory_Bandwidth"
)
ve_bieu_do_tuy_chinh(
  main_df$Process,
  main_df$Memory_Bandwidth,
  "nm",
  "GB/s",
  "Process",
  "Memory_Bandwidth"
)
par(mfrow = c(1, 1))
