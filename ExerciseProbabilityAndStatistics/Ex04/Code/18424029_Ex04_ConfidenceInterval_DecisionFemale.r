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
#DecisionFemale
# Lọc từ dataset SpeedDating lấy DecisionFemale
sampleDF <- subset(SpeedDating, select=c(DecisionFemale)); #sampleDF

# Lấy kích thước mẫu
n <- nrow(sampleDF); n

#Tính tỷ lệ trên mẫu
p_hat_No <- sum(sampleDF == 'No')/n; p_hat_No

#....................................Bootstrap.............................................
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_distDF <- replicate(10000, sum((sample(sampleDF, n, replace = TRUE)) == 'No')/n)
sd(boot_distDF); hist(boot_distDF, breaks = 40)

#Xây dựng khoảng tin cậy 95%
anpha <- 1 - 0.95;
quantile(boot_distDF, c(anpha/2, 1-anpha/2))

#....................................CongThuc...............................................
#Tính Sai số chuẩn
se_No <- sqrt(p_hat_No*(1 - p_hat_No)/n); se_No

#Tính phân phối chuẩn
z <- qnorm(1 - anpha/2)

#Xác định khoảng tin cậy 95%
p_hat_No + c(-z*se_No, z*se_No)

