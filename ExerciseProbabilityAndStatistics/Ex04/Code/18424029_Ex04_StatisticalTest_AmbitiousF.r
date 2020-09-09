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

########################################################################################################
##Kiểm định
########################################################################################################
n <- length(sampleAF)
mu0 <- 7.429
anpha <- 0.05
#.....................................Bằng Khỏang Tin Cậy..........................................
boot_distAF <- replicate(10000, mean(sample(sampleAF, n, replace = TRUE)))
confint <- quantile(boot_distAF, c(anpha/2, 1-anpha/2), na.rm = FALSE); confint

!(confint[1] <= mu0 && mu0 <= confint[2])

#.......................................Bằng công thức ...........................................
#Tính trung bình mẫu
(x_barAF <- mean(sampleAF))                   

#Tính sai số chuẩn                            
(seAF <- sd(sampleAF)/sqrt(n))                

#Tính phân phối student t
(t <- abs(x_barAF - mu0)/seAF)                

#Tính p_value
(p_value <- 2*(1 - pt(t, df = n - 1)))        

#Tình giá trị tới hạn z(1-anpha)
(crit_val <- qt(1 - anpha/2, df = n - 1))     

# Kiểm tra p_value
# p_value < anpha => Bác bỏ H0 chấp nhận H1
p_value < anpha;                                         

# Kiểm tra giá trị tới hạn z(1-anpha)
# z(1-anpha) < z => Bác bỏ H0 chấp nhận H1
crit_val < t
