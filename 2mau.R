nvida <- subset(main_df, Manufacturer == "Nvidia")$Memory_Bandwidth
amd   <- subset(main_df, Manufacturer == "AMD")$Memory_Bandwidth

# Lay n, s, trung binh cua du lieu
n1 <- length(nvida)
n2 <- length(amd)

sd1<- sd(nvida)
sd2<- sd(amd)

tb1<- mean(nvida)
tb2<- mean(amd)

bang_ket_qua <- data.frame(
  "Nhóm" = c(1, 2),
  "n"    = c(n1, n2),
  "tb"  = c(tb1, tb2),
  "sd"   = c(sd1, sd2)
)

print(bang_ket_qua)
