library(ggplot2)

# ==============================================================================
# 1. CHUAN BI DU LIEU (TIEN XU LY)
# ==============================================================================

# Tao ban sao du lieu de tranh ghi de len du lieu goc
training_set_log <- training_set
testing_set_log  <- testing_set

# Danh sach cac cot can chuyen doi Log
cols_to_transform <- c("Memory", "Memory_Bus", "Memory_Bandwidth", 
                       "Memory_Speed", "L2_Cache", "Process")

# Thuc hien log(x + 1) cho cac cot da chon
# Cach nay ngan gon hon viec viet tung dong cho tung cot
training_set_log[cols_to_transform] <- log(training_set_log[cols_to_transform] + 1)
testing_set_log[cols_to_transform]  <- log(testing_set_log[cols_to_transform] + 1)

# ==============================================================================
# 2. HUAN LUYEN MO HINH (MODEL TRAINING)
# ==============================================================================

# Xay dung mo hinh hoi quy tuyen tinh
new_model <- lm(
  formula = Memory_Bandwidth ~ Memory + Memory_Speed + Memory_Bus + L2_Cache,
  data    = training_set_log
)

# Hien thi tom tat mo hinh
summary(new_model)

# ==============================================================================
# 3. DU DOAN VA DANH GIA (PREDICTION & EVALUATION)
# ==============================================================================

# Du doan tren tap test (ket qua van dang o dang log)
pred_log_values <- predict(new_model, newdata = testing_set_log)

# Chuyen doi nguoc tu log ve gia tri thuc te: exp(x) - 1
pred_original_scale   <- exp(pred_log_values) - 1
actual_original_scale <- testing_set$Memory_Bandwidth

# Tao dataframe ket qua de de so sanh
results_summary <- data.frame(
  Actual    = actual_original_scale,
  Predicted = pred_original_scale,
  Error     = actual_original_scale - pred_original_scale
)

# Xem 10 dong dau tien
print(head(results_summary, 10))

# --- Tinh toan cac chi so danh gia ---

# 1. Tinh RMSE (Root Mean Squared Error)
mse  <- mean((actual_original_scale - pred_original_scale)^2)
rmse <- sqrt(mse)
print(paste("RMSE:", round(rmse, 4)))

# 2. Tinh R-squared (He so xac dinh)
rss <- sum((actual_original_scale - pred_original_scale)^2)      # Residual Sum of Squares
tss <- sum((actual_original_scale - mean(actual_original_scale))^2) # Total Sum of Squares
r_squared <- 1 - (rss / tss)
print(paste("R-squared:", round(r_squared, 4)))

# ==============================================================================
# 4. TRUC QUAN HOA (VISUALIZATION)
# ==============================================================================

ggplot(results_summary, aes(x = Actual, y = Predicted)) +
  # Duong ly tuong y = x (Mau do, net dut)
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  
  # Cac diem du lieu du doan
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  
  # Trang tri bieu do
  labs(
    title    = "Bieu do danh gia mo hinh: Thuc te vs Du doan",
    subtitle = "Diem cang gan duong mau do thi du doan cang chinh xac",
    x        = "Gia tri Thuc te (Actual)",
    y        = "Gia tri Du doan (Predicted)"
  ) +
  
  # Giu ty le khung hinh vuong van
  theme_minimal() +
  coord_fixed()