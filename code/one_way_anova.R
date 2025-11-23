library(car)

main_df <- read.csv("out.csv", stringsAsFactors = FALSE)

main_df$Manufacturer <- as.factor(main_df$Manufacturer)

anova_model <- aov(Memory_Bandwidth ~ Manufacturer, data = main_df) 	# thực hiện ANOVA một yếu tố
summary(anova_model)	# in ra bảng phân tích phương sai


res <- residuals(anova_model) 	# lấy phần dư Y-Y
shapiro.test(res) 				# kiểm định Shapiro–Wilk
qqnorm(res)
qqline(res, col = "red")

leveneTest(Memory_Bandwidth~Manufacturer, data = main_df)
summary(anova_model)

tukey_result <- TukeyHSD(anova_model)
tukey_result
