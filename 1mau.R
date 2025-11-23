Mem <- main_df$Memory
n  <- length(Mem)
tb <- mean(Mem)
s  <- sd(Mem)

mem_df <- data.frame(
  "n" = (n),
  "tb" = (tb),
  "s" = (s)
)

print(mem_df)

qqnorm(Mem)
qqline(Mem)

z0 <- (tb - 2048)/(s/sqrt(n))   
alpha = 0.05
RR <- qnorm(p = 1 - alpha)
z0
RR
