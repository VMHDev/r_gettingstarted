#Khảo sát trọng lượng của một sản phẩm. Cân thử 25 sản phẩm loại đó (ngẫu nhiên) được bảng dữ liệu:
#Trọng lượng (gram)   18    19    20    21
#Số sản phẩm          3     5     15    2
#Tìm khoảng tin cậy 95% cho trọng lượng trung bình của sản phẩm loại đó
n <- 25; anpha <- 1 - 0.95

z <- qnorm(1 - anpha/2); z

sample1 <- c(rep(18, 3), rep(19, 5), rep(20, 15), rep(21, 2)); sample1

# C1
boot_dist <- replicate(10000, mean(sample(sample1, n, replace=TRUE)))
sd(boot_dist); hist(boot_dist, breaks = 40)
quantile(boot_dist, c(anpha/2, 1-anpha/2))

# C2
x_ <- mean(sample1); x_
s <- sd(sample1); s
se <- s/sqrt(n)

ep <- z*se; ep
confint <- x_ + c(-ep, ep); confint