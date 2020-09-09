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
########################################################################################################
##Kiểm định
########################################################################################################
#DecisionFemale
# Lọc từ dataset SpeedDating lấy DecisionFemale
sampleDF <- subset(SpeedDating, select=c(DecisionFemale)); #sampleDF

n <- nrow(sampleDF)
p0 <- 0.5
p_hat_No <- sum(sampleDF == 'No')/n; p_hat_No
anpha <- 0.05
#.....................................Bằng Khỏang Tin Cậy..........................................
boot_distDF <- replicate(10000, sum((sample(sampleDF, n, replace = TRUE)) == 'No')/n)
confint <- quantile(boot_distDF, c(anpha/2, 1-anpha/2)); confint
!(confint[1] <= p0 && p0 <= confint[2])

#.......................................Bằng công thức ............................................
# Tính sai số chuẩn theo công thức p
(seDF <- sqrt(p0*(1 - p0)/n))

#Tính phân phối mẫu
(z <- (p_hat_No - p0)/seDF)

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
n <- nrow(sampleDF)
sum_No <- sum(sampleDF == 'No')
prop.test(sum_No, n, p = 0.5, conf.level = 1 - anpha, alternative = "greater")

