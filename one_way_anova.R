library(car)

main_df <- read.csv("out.csv", stringsAsFactors = FALSE)

main_df$Manufacturer <- as.factor(main_df$Manufacturer)

anova_model <- aov(Memory_Bandwidth ~ Manufacturer, data = main_df) 	# thực hiện ANOVA một yếu tố
summary(anova_model)	# in ra bảng phân tích phương sai


res <- residuals(anova_model) 	# lấy phần dư Y-Y
shapiro.test(res) 				# kiểm định Shapiro–Wilk

ggplot(main_df, aes(sample = Memory_Bandwidth)) +
  # Vẽ các điểm dữ liệu
  stat_qq(color = "blue", alpha = 0.6) +
  # Vẽ đường chuẩn lý thuyết (màu đỏ)
  stat_qq_line(color = "red", size = 1) +
  # Tách ra làm 3 biểu đồ dựa trên cột Manufacturer
  facet_wrap(~ Manufacturer, scales = "free") + 
  # Trang trí
  labs(title = "Biểu đồ Q-Q Plot kiểm tra phân phối chuẩn theo từng hãng",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

qqnorm(res)
qqline(res, col = "red")

leveneTest(Memory_Bandwidth~Manufacturer, data = main_df)
summary(anova_model)

tukey_result <- TukeyHSD(anova_model)
tukey_result
