#Load package mosaicData để sử dụng
require(mosaicData)

#Khảo sát trên dataset SnowGR
?CPS85
View(CPS85)

#Hàm tính tương quan giữa hai biến wage và exper trên dataset data truyền vào
?cor
cal_r <- function(data) cor(data$wage, data$exper);

#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(600); 

#Mức ý nghĩa anpha = 5% (Khoảng tin cậy 95%)
anpha <- 1 - 0.95

#Lọc từ dataset CPS85 lấy wage (lương) và exper (kinh nghiệm)
sample1 <- subset(CPS85, select=c(wage, exper)); sample1

#Vẽ biểu đồ phân bố
plot(sample1$exper, sample1$wage)

#Xem số dòng (kích thước) của dataset 
n <- nrow(sample1); n

#Tính tương quan giữa hai biến
r <- cal_r(sample1); r

#####################################Bootstrap###############################################
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_dist <- replicate(10000, cal_r(sample1[sample(1:n, n, replace=TRUE), ]))
sd(boot_dist); hist(boot_dist, breaks = 40)

#Xây dựng khoảng tin cậy 95%
quantile(boot_dist, c(anpha/2, 1-anpha/2))

######################################CongThuc##############################################
#Tính Sai số chuẩn
se <- sqrt((1-r*r)/(n-2)); se

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2)

#Tính phân phối student t
t <- qt(1-anpha/2, df=n-2)

#Xác định khoảng tin cậy 95% theo z
r + c(-z*se, z*se); 

#Xác định khoảng tin cậy 95% theo t
r + c(-t*se, t*se)

