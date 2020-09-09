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
#------------------------------------------------------------------------------------------------------
#DecisionFemale - LikeF
#Giữ giá trị cố định khi giả lập dữ liệu
set.seed(400)

#Mức ý nghĩa anpha = 5% (Khoảng tin cậy 95%)
anpha <- 1 - 0.95

#Vẽ biểu đồ phân bố
boxplot(LikeF ~ DecisionFemale, data=SpeedDating)
bwplot(DecisionFemale ~ LikeF, data = SpeedDating)

#Lọc từ dataset SpeedDating lấy LikeF của DecisionFemale = "Yes"
sampleDFLF_Y <- subset(SpeedDating, DecisionFemale=='Yes'& LikeF!="NA", select=c(LikeF))[[1]]; sampleDFLF_Y

#Lọc từ dataset SpeedDating lấy LikeF của DecisionFemale = "No"
sampleDFLF_N <- subset(SpeedDating, DecisionFemale=='No'& LikeF!="NA", select=c(LikeF))[[1]]; sampleDFLF_N

#Lấy kích thước mẫu sampleDFLF_Y và sampleDFLF_N
n_Y <- length(sampleDFLF_Y); n_N <- length(sampleDFLF_N);  n_Y; n_N

#Tính hiệu trung bình mẫu sampleDFLF_Y và sampleDFLF_N
x_Y <- mean(sampleDFLF_Y); x_N <- mean(sampleDFLF_N); x_Y - x_N

#####################################Bootstrap###############################################
#Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
#replace=TRUE: Lấy có hoàn lại
boot_distDFLF <- replicate(10000, mean(sample(sampleDFLF_Y, n_Y, replace=TRUE)) - mean(sample(sampleDFLF_N, n_N, replace=TRUE)))
sd(boot_distDFLF); hist(boot_distDFLF, breaks = 40)

#Xây dựng khoảng tin cậy 95%
quantile(boot_distDFLF, c(anpha/2, 1-anpha/2))

######################################CongThuc##############################################
#Tính Sai số chuẩn
seDFLF <- sqrt(var(sampleDFLF_Y)/n_Y + var(sampleDFLF_N)/n_N); seDFLF

#Tính phân phối chuẩn z
z <- qnorm(1-anpha/2); 

#Tính phân phối student t
t <- qt(1-anpha/2, df=min(c(n_Y-1, n_N-1)))

#Xác định khoảng tin cậy 95% theo z
(x_Y - x_N) + c(-z*seDFLF, z*seDFLF); 

#Xác định khoảng tin cậy 95% theo t
(x_Y - x_N) + c(-t*seDFLF, t*seDFLF)

