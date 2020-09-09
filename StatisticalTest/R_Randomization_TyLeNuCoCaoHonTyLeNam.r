#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(200)

#Tạo n = 100 phần từ
n <- 100; 

#Giả lập dữ liêu với 60 giá trị "1" và 40 giá trị "0" (Giả lập số lượng)
sample1 <- c(rep(1, 60), rep(0, 40))

#Tính tỷ lệ trên sample1
p_hat <- mean(sample1); p_hat

#Giả lập dữ liệu với 50 giá trị "1" và 50 giá trị "0" (Giả lập số lượng)
sample0 <- c(rep(1, 50), rep(0, 50))

#Tính tỷ lệ trên sample0
p_0 <- mean(sample0); p_0

#####################################Randomization###############################################
#Tạo phân phối Randomization rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
rand_dist <- replicate(10000, mean(sample(sample0, n, replace=TRUE)))
hist(rand_dist, breaks = 40)
pvalue <- sum(rand_dist >= p_hat)/length(rand_dist); pvalue