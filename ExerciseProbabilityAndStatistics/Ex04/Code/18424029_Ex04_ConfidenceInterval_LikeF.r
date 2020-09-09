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
#LikeF
# Lọc từ dataset SpeedDating lấy LikeF
sampleLF <- subset(SpeedDating, LikeF!="NA"  , select=c(LikeF))[[1]]; sampleLF

# Lấy kích thước mẫu
n <- length(sampleLF); n

#Tính trung bình mẫu
x_barLF <- mean(sampleLF); x_barLF

#Khoảng tin cậy 95%
anpha <- 1 - 0.95; anpha

#....................................Bootstrap.............................................
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
#?replicate
boot_distLF <- replicate(10000, mean(sample(sampleLF, n, replace = TRUE)))
sd(boot_distLF); hist(boot_distLF, breaks = 40)

#Xây dựng khoảng tin cậy 95%
#?quantile
quantile(boot_distLF, c(anpha/2, 1-anpha/2), na.rm = FALSE)

#....................................CongThuc...............................................
#Tính Sai số chuẩn
seLF <-sd(sampleLF)/sqrt(n); seLF

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2);

#Tính phân phối student t
t <- qt(1-anpha/2, df = n-1)

#Xác định khoảng tin cậy 95%
x_barLF + c(-z*seLF, z*seLF)  #Dựa trên z

x_barLF + c(-t*seLF, t*seLF)  #Dựa trên t

