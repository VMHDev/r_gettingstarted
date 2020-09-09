#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(100)

#Giả lập dữ liêu với 40000 giá trị "Nu" và 60000 giá trị "Nam" (Giả lập số lượng)
pop <- c(rep('Nu', 40000), rep('Nam', 60000)); pop

#Tính tỷ lệ
p <- sum(pop == 'Nu')/length(pop); p

#Lấy mẫu với n = 100 phần từ tập pop
n <- 100; 
sample1 <- sample(pop, n); sample1

#Tính tỷ lệ trên mẫu
p_hat <- sum(sample1 == 'Nu')/n; p_hat

#Lặp lại việc lấy mẫu 10000 rồi tính trung bình mỗi lần lấy
samp_dist <- replicate(10000, sum((sample(pop, n))=='Nu')/n)

#Tính trung bình của 10000 lần lấy mẫu
mean(samp_dist)

#Tính độ lệch chuẩn (SE)
sd(samp_dist)

#Vẽ biểu đồ hist
hist(samp_dist, breaks = 40)