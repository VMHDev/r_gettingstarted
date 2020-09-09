#Một công ty sản xuất hạt giống tuyên bố rằng giống mới của họ sẽ có năng suất trung bình ít nhất là 21,5 tạ/ha. 
#Gieo thử nghiệm tại 16 vườn thí nghiệm thu được kết quả sau:
# 19.2, 18.7, 22.4, 20.3, 19.8, 25.1, 17.0, 15.8
# 21.0, 18.6, 23.7, 24.1, 23.4, 19.8, 21.7, 18.9
#Dựa vào kết quả này hãy xác nhận xem quảng cáo của công ty có đúng không. Mức ý nghĩa được chọn là anpha = 5%

###################################################################################################################
#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(100)

# Khởi tạo dữ liệu
sampleLua <- c(19.2, 18.7, 22.4, 20.3, 16.8, 25.1, 17,   15.8, 
       21,   18.6, 23.7, 24.1, 23.4, 19.8, 21.7, 18.9)

# Lấy kích thước mẫu
n <- length(sampleLua); n

# Tính giá trị trung bình
x_bar <- mean(sampleLua); x_bar

# Khai báo mức ý nghĩa anpha 5%
anpha <- 0.05

# Khai báo H0
mu0 <- 21.5

#####################################CongThuc###############################################
# Tính độ lệch chuẩn
se <- sd(sampleLua)/sqrt(n); se

# Tinh t
t <- (x_bar - mu0)/se; t

# Tính p-value
pvalue <- 2*(1 - pt(abs(t), df = n - 1)); pvalue

# vi pvalue > anpha = 0.05 nen ta khong bac bo H0

#####################################Randomization###############################################
#Tạo phân phối Randomization
#replace=TRUE: Lấy có hoàn lại
d_mean <- x_bar - mu0
rand_dist <- replicate(10000, mean(sample(sampleLua - d_mean, n, replace = TRUE)))
pvalue2 <- sum(abs(rand_dist - mu0)>abs(x_bar - mu0)) / length(rand_dist); pvalue2
#pvalue2 <- sum(rand_dist >= x_bar)/length(rand_dist); pvalue2

#####################################SuDungHamR###############################################
t.test(sampleLua, mu=19.5, alternative = "greater", conf.level = 1-anpha)

t.test(sampleLua, mu=19.5, alternative = "less", conf.level = 1-anpha)

t.test(sampleLua, mu=19.5, alternative = "two.sided", conf.level = 1-anpha)

