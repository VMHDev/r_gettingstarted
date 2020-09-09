#Giả lập dữ liêu với 100000 phần tử có giá trị trung bình là 1.6 với độ lệch chuẩn là 0.08 (Giả lập chiều cao)
set.seed(300); 
pop <-rnorm(100000, mean = 1.6, sd = 0.08)

#Lấy mẫu 100 phần tử
n <- 100; sample1 <- sample(pop,n)

#Tính trung bình mẫu
x_bar <- mean(sample1); x_bar

anpha <- 1-0.95;
#####################################Bootstrap###############################################
#Tạo phân phối bootsrap
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, mean(sample(sample1, n, replace = TRUE)))
sd(boot_dist); 

#Xây dựng khoảng tin cậy 95%
quantile(boot_dist, c(anpha/2, 1 - anpha/2))

######################################CongThuc##############################################
#Tính Sai số chuẩn
se <-sd(sample1)/sqrt(n); se

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2);

#Tính phân phối student t
t <- qt(1-anpha/2, df = n-1)

#Xác định khoảng tin cậy 95%
x_bar + c(-z*se, z*se)  #Dựa trên z

x_bar + c(-t*se, t*se)  #Dựa trên t


