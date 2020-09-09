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
########################################################################################################
##Kiểm định
########################################################################################################
#RaceF
# Lọc từ dataset SpeedDating lấy DecisionFemale
sampleRF <- subset(SpeedDating, select=c(RaceF));  #sampleRF

n <- nrow(sampleRF)
p0 <- 0.5
p_hat_Caucasian <- sum(sampleRF == 'Caucasian')/n; p_hat_Caucasian
anpha <- 0.05
#.....................................Bằng Khỏang Tin Cậy..........................................
boot_distRF <- replicate(10000, sum((sample(sampleRF, n, replace = TRUE)) == 'Caucasian')/n)
confint <- quantile(boot_distRF, c(anpha/2, 1-anpha/2)); confint
!(confint[1] <= p0 && p0 <= confint[2])

#.......................................Bằng công thức ............................................
# Tính sai số chuẩn theo công thức p
(seRF <- sqrt(p0*(1 - p0)/n))

#Tính phân phối mẫu
(z <- (p_hat_Caucasian - p0)/seRF)

#Tính p_value
(p_value <- 1 - pnorm(z))

#Tình giá trị tới hạn z(1-anpha)
(crit_val <- qnorm(1 - anpha))

# Kiểm tra p_value
# p_value < anpha => Bác bỏ H0 chấp nhận H1
p_value < anpha;  #(1)

# Kiểm tra giá trị tới hạn z(1-anpha)
# z(1-anpha) < z => Bác bỏ H0 chấp nhận H1
crit_val < z      #(2)

#..........................................Bằng Hàm .............................................
?prop.test
n <- nrow(sampleRF)
sum_Caucasian <- sum(sampleRF == 'Caucasian')
prop.test(sum_Caucasian, n, p = 0.5, conf.level = 1 - anpha, alternative = "greater")
