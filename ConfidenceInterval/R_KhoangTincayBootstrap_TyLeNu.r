#Giả lập dữ liêu với 40000 giá trị "Nu" và 60000 giá trị "Nam" (Giả lập số lượng)
set.seed(100); 
pop <- c(rep('Nu', 40000), rep('Nam', 60000))

#Lấy mẫu với n = 100 phần từ tập pop
n <- 100; sample1 <- sample(pop, n)

#Tính tỷ lệ trên mẫu
p_hat <- sum(sample1 == 'Nu')/n; p_hat

#Lặp lại việc lấy mẫu 10000 rồi tính trung bình mỗi lần lấy rồi tính độ lệch chuuẩn vẽ biểu đồ hist
samp_dist <- replicate(10000, sum((sample(pop, n))=='Nu')/n)
sd(samp_dist); hist(samp_dist, breaks = 40)

#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, sum((sample(sample1, n,replace=TRUE))=='Nu')/n)
sd(boot_dist); hist(boot_dist, breaks = 40)

#Xây dựng khoảng tin cậy 95%
anpha <- 1 - 0.95; quantile(boot_dist, c(anpha/2, 1-anpha/2))