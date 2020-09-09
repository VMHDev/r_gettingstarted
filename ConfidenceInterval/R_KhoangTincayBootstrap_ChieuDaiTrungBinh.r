#Giả lập dữ liêu với 100000 phần tử có giá trị từ 0 đến 2 (Giả lập chiều dài)
set.seed(200); 
pop <- runif(100000, min = 0, max = 2)

#Lấy mẫu 100 phần tử
n <- 100; sample1 <- sample(pop, n)

#Tính trung bình mẫu
x_bar <- mean(sample1); x_bar

#Lặp lại việc lấy mẫu 10000 rồi tính trung bình mỗi lần lấy rồi tính độ lệch chuuẩn vẽ biểu đồ hist
samp_dist <- replicate(10000, mean(sample(pop, n)))
sd(samp_dist); hist(samp_dist, breaks = 40)

#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, mean(sample(sample1, n, replace=TRUE)))
sd(boot_dist); hist(boot_dist, breaks = 40)

#Xây dựng khoảng tin cậy 95%
anpha <- 1 - 0.95; quantile(boot_dist, c(anpha/2, 1-anpha/2))