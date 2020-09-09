#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(200)

#Giả lập dữ liêu với 100000 phần tử có giá trị từ 0 đến 2 (Giả lập chiều dài)
pop <- runif(100000, min = 0, max = 2); pop

#Tính giá trị trung bình của tổng thể
mu <- mean(pop); mu

#Lấy mẫu
n <- 100; sample1 <- sample(pop, n)

#Tính trung bình mẫu
x_bar <- mean(sample1); x_bar

#Lặp lại việc lấy mẫu 10000 rồi tính trung bình mỗi lần lấy
samp_dist <- replicate(10000, mean(sample(pop, n))); samp_dist

#Tính trung bình của 10000 lần lấy mẫu
mean(samp_dist); 

#Tính sai số chuẩn samp_dist
sd(samp_dist); 

#Vẽ biểu đồ hist
hist(samp_dist, breaks = 40)

#Tính sai số chuẩn tổng thể pop
sd(pop); 

#Vẽ biểu đồ hist
hist(pop, breaks = 40)
