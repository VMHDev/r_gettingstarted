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

########################################################################################################
##Kiểm định
########################################################################################################
n <- length(sampleLF)
mu0 <- 6.366
anpha <- 0.05
#.....................................Bằng Khỏang Tin Cậy..........................................
confint <- quantile(boot_distLF, c(anpha/2, 1-anpha/2), na.rm = FALSE); confint

!(confint[1] <= mu0 && mu0 <= confint[2])

#.......................................Bằng công thức ...........................................
#Tính trung bình mẫu
(x_barLF <- mean(sampleLF))                   

#Tính sai số chuẩn                            
(seLF <- sd(sampleLF)/sqrt(n))                

#Tính phân phối student t
(t <- abs(x_barLF - mu0)/seLF)                

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

#(1) ra FALSE => Không bác bỏ H0 => Không chấp nhận H1 => Như vậy, với mức ý nghĩa 5%, chiều cao trung bình của cư dân thành phố là 1.60 mét.

#(2) ra FALSE => Không bác bỏ H0 => Không chấp nhận H1 => Như vậy, với mức ý nghĩa 5%, chiều cao trung bình của cư dân thành phố là 1.60 mét.




