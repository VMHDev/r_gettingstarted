#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(100)

#Giả lập dữ liêu với 8 giá trị "1" và 2 giá trị "0" (Giả lập số lượng)
# 8 lần ngửa (1) và 2 lần sấp (0)
sample1 <- c(rep(1, 8), rep(0, 2)); sample1

#Tạo n = 10 phần từ
n <- 10

#Tính tổng sample1
theta_hat <- sum(sample1); theta_hat

#Giả lập dữ liêu với 5 giá trị "1" và 5 giá trị "0" (Giả lập số lượng)
# 5 lần ngửa (1) và 5 lần sấp (0)
sample0 <- c(rep(1, 5), rep(0, 5))

#Tính tổng sample0
theta_0 <- sum(sample0); theta_0

#####################################Randomization###############################################
#Tạo phân phối Randomization rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
rand_dist <- replicate(10000, sum(sample(sample0, n, replace=TRUE))); 
hist(rand_dist, breaks = 40)

pvalue <- sum(abs(rand_dist-theta_0) >= abs(theta_hat - theta_0))/length(rand_dist); pvalue

