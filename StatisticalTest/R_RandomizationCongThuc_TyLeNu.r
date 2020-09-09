#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(300);

#Mức ý nghĩa anpha
anpha <- 0.05
n <- 100;
p_hat <- 0.6;
p0 <- 0.5

#####################################Randomization###############################################
#Tạo phân phối Randomization rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
rand_dist <- replicate(10000, mean(sample(c(1, 0), n, replace = TRUE, prob = c(p0, 1 - p0))))
pvalue <- sum(rand_dist >= p_hat)/length(rand_dist);
pvalue
pvalue < anpha;
quantile(rand_dist, 1-anpha) < p_hat
se <-sqrt(p0 *(1-p0)/n);
z <- (p_hat-p0)/se;
pvalue <- 1 - pnorm(z); pvalue

pvalue < anpha; qnorm(1-anpha) < z