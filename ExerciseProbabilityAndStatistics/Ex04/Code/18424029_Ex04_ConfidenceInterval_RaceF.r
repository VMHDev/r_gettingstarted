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
#TyLe------------------------------------------------------------------------------------------------------
#RaceF 
# Lọc từ dataset SpeedDating lấy RaceF
sampleRF <- subset(SpeedDating, select=c(RaceF)); sampleRF

# Lấy kích thước mẫu
n <- nrow(sampleRF); n

#Tính tỷ lệ trên mẫu
p_hat_Cauc <- sum(sampleRF == 'Caucasian')/n; p_hat_Cauc

#####################################Bootstrap###############################################
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_distRF <- replicate(10000, sum((sample(sampleRF, n, replace = TRUE)) == 'Caucasian')/n)
sd(boot_distRF); hist(boot_distRF, breaks = 40)

#Xây dựng khoảng tin cậy 95%
anpha <- 1 - 0.95;
quantile(boot_distRF, c(anpha/2, 1-anpha/2))

######################################CongThuc##############################################
#Tính Sai số chuẩn
seRF <- sqrt(p_hat_Cauc*(1 - p_hat_Cauc)/n); seRF

#Tính phân phối chuẩn
z <- qnorm(1 - anpha/2)

#Xác định khoảng tin cậy 95%
p_hat_Cauc + c(-z*seRF, z*seRF)
