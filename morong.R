training_set_log <- training_set
testing_set_log <- testing_set
training_set_log$Memory <- log(training_set_log$Memory + 1)
training_set_log$Memory_Bus <- log(training_set_log$Memory_Bus + 1)
training_set_log$Memory_Bandwidth <- log(training_set_log$Memory_Bandwidth +
                                           1)
training_set_log$Memory_Speed <- log(training_set_log$Memory_Speed + 1)
training_set_log$L2_Cache <- log(training_set_log$L2_Cache + 1)
training_set_log$Process <- log(training_set_log$Process + 1)
testing_set_log$Memory <- log(testing_set_log$Memory + 1)
testing_set_log$Memory_Bus <- log(testing_set_log$Memory_Bus + 1)
testing_set_log$Memory_Bandwidth <- log(testing_set_log$Memory_Bandwidth +
                                          1)
testing_set_log$Memory_Speed <- log(testing_set_log$Memory_Speed + 1)
testing_set_log$L2_Cache <- log(testing_set_log$L2_Cache + 1)
testing_set_log$Process <- log(testing_set_log$Process + 1)
new_model <- lm(
  Memory_Bandwidth ~ Process + Memory + Memory_Speed
  + Memory_Bus + L2_Cache,
  training_set_log
)
summary(new_model)


pred_log_values <- predict(new_model, newdata = testing_set_log)

# Chuyen tu log ve gia tri binh thuong
pred_original_scale <- exp(pred_log_values) - 1

# Lay gia tri thuc te
actual_original_scale <- testing_set$Memory_Bandwidth

# Tao df cho de nhin
results_summary_display_original <- data.frame(
  Memory_Bandwidth_Actual = actual_original_scale,
  Memory_Bandwidth_Predict = pred_original_scale,
  Error_Original = actual_original_scale - pred_original_scale # Tinh sai so
)

head(results_summary_display_original, 10)

ggplot(results_summary_display_original, aes(x = Memory_Bandwidth_Actual, y = Memory_Bandwidth_Predict)) +
  
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
