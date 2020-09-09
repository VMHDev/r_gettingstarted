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
#FunF
# Lọc từ dataset SpeedDating lấy FunF
sampleFF <- subset(SpeedDating, FunF!="NA"  , select=c(FunF))[[1]]; sampleFF

# Lấy kích thước mẫu
n <- length(sampleFF); n

#Tính trung bình mẫu
x_barFF <- mean(sampleFF); x_barFF

#Khoảng tin cậy 95%
anpha <- 1 - 0.95; anpha

#....................................Bootstrap.............................................
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
#?replicate
boot_distFF <- replicate(10000, mean(sample(sampleFF, n, replace = TRUE)))
sd(boot_distFF); hist(boot_distFF, breaks = 40)

#Xây dựng khoảng tin cậy 95%
#?quantile
quantile(boot_distFF, c(anpha/2, 1-anpha/2), na.rm = FALSE)

#....................................CongThuc...............................................
#Tính Sai số chuẩn
seFF <-sd(sampleFF)/sqrt(n); seFF

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2);

#Tính phân phối student t
t <- qt(1-anpha/2, df = n-1)

#Xác định khoảng tin cậy 95%
x_barFF + c(-z*seFF, z*seFF)  #Dựa trên z

x_barFF + c(-t*seFF, t*seFF)  #Dựa trên t

