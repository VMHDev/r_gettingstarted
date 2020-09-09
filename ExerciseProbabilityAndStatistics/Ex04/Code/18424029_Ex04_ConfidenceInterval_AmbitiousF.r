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
#TrungBinh------------------------------------------------------------------------------------------------------
#AmbitiousF
# Lọc từ dataset SpeedDating lấy AmbitiousF
sampleAF <- subset(SpeedDating, AmbitiousF!="NA"  , select=c(AmbitiousF))[[1]]; sampleAF

# Lấy kích thước mẫu
n <- length(sampleAF); n

#Tính trung bình mẫu
x_barAF <- mean(sampleAF); x_barAF

#Khoảng tin cậy 95%
anpha <- 1 - 0.95; anpha

#....................................Bootstrap.............................................
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
#?replicate
boot_distAF <- replicate(10000, mean(sample(sampleAF, n, replace = TRUE)))
sd(boot_distAF); hist(boot_distAF, breaks = 40)

#Xây dựng khoảng tin cậy 95%
#?quantile
quantile(boot_distAF, c(anpha/2, 1-anpha/2), na.rm = FALSE)

#....................................CongThuc...............................................
#Tính Sai số chuẩn
seAF <-sd(sampleAF)/sqrt(n); seAF

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2);

#Tính phân phối student t
t <- qt(1-anpha/2, df = n-1)

#Xác định khoảng tin cậy 95%
x_barAF + c(-z*seAF, z*seAF)  #Dựa trên z

x_barAF + c(-t*seAF, t*seAF)  #Dựa trên t

