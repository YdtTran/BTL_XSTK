library(caret)
# cố định cách chọn các dòng ngẫu nhiên
set.seed(111)
# chọn dữ liệu cho train với biến mục tiêu là 'Memory_Bandwidth'
train_index <- createDataPartition(main_df$Memory_Bandwidth, p = 0.8, 
                                   list = FALSE)
# tạo training_set bằng các dòng được chọn
training_set <- main_df[train_index, ]
# tạo testing_set bằng các dòng còn lại
testing_set <- main_df[-train_index, ]
# tạo mô hình hồi quy tuyến tính
model<-lm(Memory_Bandwidth ~ Process + Memory + Memory_Speed + 
            Memory_Bus + L2_Cache, training_set)
summary((model))

pred_values <- predict(model, newdata = testing_set)

# Tao df cho de nhin
results_summary_display_original <- data.frame(
  Memory_Bandwidth_Actual = testing_set$Memory_Bandwidth,
  Memory_Bandwidth_Predict = pred_values,
  Error_Original = testing_set$Memory_Bandwidth - pred_values # Tinh sai so
)

head(results_summary_display_original, 10)


ggplot(results_summary_display_original, aes(x = testing_set$Memory_Bandwidth, y = pred_values)) +
  
  # 1. Vẽ đường lý tưởng y = x (Màu đỏ, nét đứt)
  # Bất kỳ điểm nào nằm trên đường này là dự đoán chính xác 100%
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  
  # 2. Vẽ các điểm dữ liệu (Scatter plot)
  # Mỗi điểm đại diện cho một mẫu kiểm tra
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  
  # 3. Trang trí biểu đồ
  labs(title = "Biểu đồ đánh giá mô hình: Thực tế vs Dự đoán",
       subtitle = "Đường nét đứt đỏ là đường lý tưởng (y=x). Điểm càng gần đường đỏ càng tốt.",
       x = "Giá trị Thực tế (Actual)",
       y = "Giá trị Dự đoán (Predicted)") +
  
  # 4. Giữ tỷ lệ khung hình vuông vắn để dễ so sánh đường chéo
  theme_minimal()
