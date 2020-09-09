#Load package mosaicData để sử dụng
require(mosaicData)

#Khảo sát trên dataset SnowGR
?SnowGR
View(SnowGR)

#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(500)

#Mức ý nghĩa anpha = 5% (Khoảng tin cậy 95%)
anpha <- 1 - 0.95

#Lọc từ dataset SnowGR lấy lượng tuyết tháng 2 và tháng 11
SnowGR_Avl <- subset(SnowGR, !is.na(Feb)&!is.na(Nov), select=c(Feb, Nov)); SnowGR_Avl

#Lấy hiệu lượng tuyết tháng 2 và tháng 11
sample1 <- SnowGR_Avl$Feb - SnowGR_Avl$Nov; sample1

#Vẽ biểu đồ hist
hist(sample1, breaks=40)

#Lấy kích thước mẫu
n <- length(sample1); n;

#Lấy trung bình mẫu
xd_bar <- mean(sample1);  xd_bar

#####################################Bootstrap###############################################
#Tạo phân phối bootsrap rồi tính độ lệch chuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, mean(sample(sample1, n, replace=TRUE)))
sd(boot_dist); hist(boot_dist, breaks=40)

#Xây dựng khoảng tin cậy 95%
quantile(boot_dist, c(anpha/2, 1-anpha/2))

######################################CongThuc##############################################
#Tính Sai số chuẩn
se <- sd(sample1)/sqrt(n); se

#Tính phân phối chuẩn
z <- qnorm(1-anpha/2); 

#Tính phân phối chuẩn
t <- qt(1-anpha/2, df=n-1)

#Xác định khoảng tin cậy 95% theo z
xd_bar + c(-z*se, z*se); 

#Xác định khoảng tin cậy 95% theo t
xd_bar + c(-t*se, t*se)

