#Giả lập dữ liêu với 40000 giá trị "Nu" và 60000 giá trị "Nam" (Giả lập số lượng)
set.seed(100);
pop <- c(rep('Nu', 40000), rep('Nam', 60000))

#Lấy mẫu với n = 100 phần từ tập pop
n <- 100;
sample1 <- sample(pop, n)

#Tính tỷ lệ trên mẫu
p_hat <- sum(sample1 == 'Nu')/n; p_hat

#####################################Bootstrap###############################################
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, sum((sample(sample1, n, replace = TRUE)) == 'Nu')/n)
sd(boot_dist);

#Xây dựng khoảng tin cậy 95%
anpha <- 1 - 0.95;
quantile(boot_dist, c(anpha/2, 1-anpha/2))

######################################CongThuc##############################################
?qnorm
#Tính Sai số chuẩn
se <- sqrt(p_hat*(1 - p_hat)/n); se

#Tính phân phối chuẩn
z <- qnorm(1 - anpha/2)

#Xác định khoảng tin cậy 95%
p_hat + c(-z*se, z*se)
