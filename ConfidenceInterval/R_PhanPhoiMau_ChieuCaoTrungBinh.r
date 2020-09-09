#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(300)

#Giả lập dữ liêu với 100000 phần tử có giá trị trung bình là 1.6 với độ lệch chuẩn là 0.08 (Giả lập chiều cao)
pop <- rnorm(100000, mean = 1.6, sd = 0.08)

#Tính giá trị trung bình của tổng thể
mu <- mean(pop); mu

#Lấy mẫu 100 phần tử
n <- 100; sample1 <- sample(pop, n)

#Tính trung bình mẫu
x_bar <- mean(sample1); x_bar

#Lặp lại việc lấy mẫu 10000 rồi tính trung bình mỗi lần lấy
samp_dist <- replicate(10000, mean(sample(pop, n)))

#Tính trung bình của 10000 lần lấy mẫu
mean(samp_dist); sd(samp_dist); hist(samp_dist, breaks = 40)

sd(pop); hist(pop, breaks = 40)