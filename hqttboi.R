# ==============================================================================
# 1. NAP THU VIEN VA CHUAN BI DU LIEU
# ==============================================================================
library(caret)
library(car)
library(ggplot2)

# Co dinh hat giong ngau nhien de tai lap ket qua
set.seed(111)

# Chia du lieu: 80% Train, 20% Test
# 'list = FALSE' de tra ve ma tran chi so thay vi list
train_index <- createDataPartition(main_df$Memory_Bandwidth, p = 0.8, list = FALSE)

training_set <- main_df[train_index, ]
testing_set  <- main_df[-train_index, ]

# ==============================================================================
# 2. HUAN LUYEN MO HINH (TRAINING)
# ==============================================================================

# Xay dung mo hinh hoi quy tuyen tinh da bien
model <- lm(
  formula = Memory_Bandwidth ~ Process + Memory + Memory_Speed + Memory_Bus + L2_Cache, 
  data    = training_set
)

# Xem tom tat thong so mo hinh
summary(model)

# ==============================================================================
# 3. DU DOAN VA DANH GIA (PREDICTION & METRICS)
# ==============================================================================

# Du doan tren tap testing
pred_values <- predict(model, newdata = testing_set)

# Tao dataframe so sanh giua Thuc te va Du doan
results_summary <- data.frame(
  Actual    = testing_set$Memory_Bandwidth,
  Predicted = pred_values,
  Error     = testing_set$Memory_Bandwidth - pred_values
)

# Hien thi 10 dong dau tien
print(head(results_summary, 10))

# --- Tinh toan cac chi so danh gia ---

# 1. Tinh RMSE (Can bac hai cua trung binh binh phuong sai so)
# RMSE cang nho cang tot
rmse_val <- sqrt(mean(results_summary$Error^2))
print(paste("RMSE:", round(rmse_val, 4)))

# 2. Tinh R-squared (He so xac dinh tren tap Test)
# R2 cang gan 1 cang tot
rss <- sum(results_summary$Error^2)             # Residual Sum of Squares
tss <- sum((testing_set$Memory_Bandwidth - mean(testing_set$Memory_Bandwidth))^2) # Total Sum of Squares
r2_val <- 1 - (rss / tss)
print(paste("R-squared (Test set):", round(r2_val, 4)))

# ==============================================================================
# 4. TRUC QUAN HOA KET QUA (VISUALIZATION)
# ==============================================================================

ggplot(results_summary, aes(x = Actual, y = Predicted)) +
  # Duong ly tuong y = x (Mau do, net dut)
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +
  
  # Cac diem du lieu (Scatter plot)
  geom_point(color = "blue", alpha = 0.6, size = 2) +
  
  # Trang tri bieu do
  labs(
    title    = "Bieu do danh gia: Thuc te vs Du doan",
    subtitle = "RMSE va R-squared the hien do chinh xac cua mo hinh",
    x        = "Gia tri Thuc te (Actual)",
    y        = "Gia tri Du doan (Predicted)"
  ) +
  
  theme_minimal() +
  coord_fixed() # Giu ty le khung hinh vuong

# ==============================================================================
# 5. KIEM TRA GIA DINH CUA MO HINH (DIAGNOSTICS)
# ==============================================================================

# Thiet lap luoi do thi 2x2 de xem 4 bieu do residuals cung luc
par(mfrow = c(2, 2)) 
plot(model)
par(mfrow = c(1, 1)) # Tra lai thiet lap mac dinh

# Kiem tra phan phoi chuan cua phan du (Residuals)
# Neu p-value > 0.05 => Phan du tuan theo phan phoi chuan (Tot)
print(shapiro.test(model$residuals))

# Kiem tra tu tuong quan (Autocorrelation) - Durbin Watson Test
# Gia tri DW gan 2 la tot (khong co tu tuong quan)
print(durbinWatsonTest(model))

# Kiem tra da cong tuyen (Multicollinearity) - VIF
# Neu VIF > 10 (hoac > 5) => Co dau hieu da cong tuyen
print(vif(model))