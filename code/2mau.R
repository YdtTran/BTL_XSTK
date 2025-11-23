nvida <- subset(main_df, Manufacturer == "Nvidia")$Memory_Bandwidth
amd   <- subset(main_df, Manufacturer == "AMD")$Memory_Bandwidth

# Lay n, s, trung binh cua du lieu
n1 <- length(nvida)
n2 <- length(amd)

s1<- sd(nvida)
s2<- sd(amd)

tb1<- mean(nvida)
tb2<- mean(amd)

bang_ket_qua <- data.frame(
  "Nhóm" = c(1, 2),
  "n"    = c(n1, n2),
  "tb"  = c(tb1, tb2),
  "s"   = c(s1, s2)
)

print(bang_ket_qua)
# Bước 1: Chia khung hình thành 1 hàng, 2 cột
par(mfrow = c(1, 2))

# Bước 2: Vẽ biểu đồ cho Nvidia
qqnorm(nvida, main = "Q-Q Plot: Nvidia", col = "darkgreen", pch = 19)
qqline(nvida, col = "red", lwd = 2) # lwd là độ dày của đường

# Bước 3: Vẽ biểu đồ cho AMD
qqnorm(amd, main = "Q-Q Plot: AMD", col = "blue", pch = 19)
qqline(amd, col = "red", lwd = 2)

# Bước 4: (Quan trọng) Trả lại khung hình về mặc định (1 hình duy nhất)
# Để các biểu đồ sau này không bị ảnh hưởng
par(mfrow = c(1, 1))

z0 <- (tb1-tb2)/sqrt(s1^2/n1 + s2^2/n2)

alpha = 0.05
RR <- qnorm(p = 1 - alpha)
z0
RR
