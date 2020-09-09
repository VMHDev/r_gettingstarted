#Load package Lock5withR để sử dụng
library(Lock5withR)
#Load package mosaic để sử dụng
library(mosaic)
#Load package mosaicData để sử dụng
library(mosaicData)
# Khai báo dataset SpeedDating
data(SpeedDating)
# Xem dataset của SpeedDating
View(SpeedDating)
########################################################################################################
##Khoảng tin cậy
########################################################################################################
#TuongQuan------------------------------------------------------------------------------------------------------
#LikeF - AmbitiousF
#Hàm tính tương quan giữa hai biến LikeF và AmbitiousF trên dataset data truyền vào
cal_r <- function(data) cor(data$LikeF, data$AmbitiousF)

#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(600)

#Mức ý nghĩa anpha = 5% (Khoảng tin cậy 95%)
anpha <- 1 - 0.95

#Lọc từ dataset SpeedDating lấy LikeF và AmbitiousF
sampleAFLF <- subset(SpeedDating, LikeF!="NA" & AmbitiousF!="NA", select=c(LikeF, AmbitiousF)); sampleAFLF

#Vẽ biểu đồ phân bố
plot(sampleAFLF$LikeF, sampleAFLF$AmbitiousF)

#Xem số dòng (kích thước) của dataset 
n <- nrow(sampleAFLF); n

#Tính tương quan giữa hai biến
r <- cal_r(sampleAFLF); r

#####################################Bootstrap###############################################
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_distAFLF <- replicate(10000, cal_r(sampleAFLF[sample(1:n, n, replace=TRUE), ]))
sd(boot_distAFLF); hist(boot_distAFLF, breaks = 40)

#Xây dựng khoảng tin cậy 95%
quantile(boot_distAFLF, c(anpha/2, 1-anpha/2))

######################################CongThuc##############################################
#Tính Sai số chuẩn
seAFLF <- sqrt((1-r*r)/(n-2)); seAFLF

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2)

#Tính phân phối student t
t <- qt(1-anpha/2, df=n-2)

#Xác định khoảng tin cậy 95% theo z
r + c(-z*seAFLF, z*seAFLF); 

#Xác định khoảng tin cậy 95% theo t
r + c(-t*seAFLF, t*seAFLF)
