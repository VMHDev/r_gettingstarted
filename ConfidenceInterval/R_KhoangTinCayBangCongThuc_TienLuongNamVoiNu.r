#Load package mosaicData để sử dụng
?require
require(mosaicData)

#Khảo sát trên dataset CPS85
?CPS85
View(CPS85)

#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(400)

#Mức ý nghĩa anpha = 5% (Khoảng tin cậy 95%)
anpha <- 1 - 0.95

#Vẽ biểu đồ phân bố
boxplot(wage ~ sex, data=CPS85)

#Lọc từ dataset CPS85 lấy wage (lương) của sex = "M" (giới tính Nam)
sample1 <- subset(CPS85, sex=='M', select=c(wage))[[1]]; sample1

#Lọc từ dataset CPS85 lấy wage (lương) của sex = "F" (giới tính Nữ)
sample2 <- subset(CPS85, sex=='F', select=c(wage))[[1]]; sample2

#Lấy kích thước mẫu sample1 và sample2
n1 <- length(sample1); n2 <- length(sample2);  n1; n2

#Tính hiệu trung bình mẫu sample1 và sample2
x_1 <- mean(sample1); x_2 <- mean(sample2); x_1 - x_2

#####################################Bootstrap###############################################
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, mean(sample(sample1, n1, replace=TRUE)) - mean(sample(sample2, n2, replace=TRUE)))
sd(boot_dist); hist(boot_dist, breaks = 40)

#Xây dựng khoảng tin cậy 95%
quantile(boot_dist, c(anpha/2, 1-anpha/2))

######################################CongThuc##############################################
#Tính Sai số chuẩn
se <- sqrt(var(sample1)/n1 + var(sample2)/n2); se

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2); 

#Tính phân phối student t
t <- qt(1-anpha/2, df=min(c(n1-1, n2-1)))

#Xác định khoảng tin cậy 95% theo z
(x_1 - x_2) + c(-z*se, z*se); 

#Xác định khoảng tin cậy 95% theo t
(x_1 - x_2) + c(-t*se, t*se)

