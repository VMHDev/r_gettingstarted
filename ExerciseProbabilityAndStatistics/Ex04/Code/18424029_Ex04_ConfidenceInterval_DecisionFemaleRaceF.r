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
?SpeedDating
########################################################################################################
##Khoảng tin cậy
########################################################################################################
#------------------------------------------------------------------------------------------------------
#DecisionFemale - RaceF
#Lọc từ dataset SpeedDating lấy DecisionFemale = "Yes" của những RaceF=="Caucasian"
sampleDFRF_Y <- subset(SpeedDating, DecisionFemale=='Yes'& RaceF=="Caucasian", select=c(DecisionFemale))[[1]]; sampleDFRF_Y
 
#Lọc từ dataset SpeedDating lấy DecisionFemale = "No" của những RaceF=="Caucasian"
sampleDFRF_N <- subset(SpeedDating, DecisionFemale=='No'& RaceF=="Caucasian", select=c(DecisionFemale))[[1]]; sampleDFRF_N

#Lấy kích thước mẫu sampleDFRF_Y và sampleDFRF_N
n_Y <- length(sampleDFRF_Y); n_N <- length(sampleDFRF_N);  n_Y; n_N

#Lấy số lượng người da trắng tham gia khảo sát
sum_Cauc <- count( ~RaceF, data=SpeedDating, success = 'Caucasian'); sum_Cauc

fracture <- c(7, 20)
total <- c(100, 110)
prop.test(fracture, total)

# #Giữ giá trị cố định khi giả lập dữ liệu
# set.seed(400)
# 
# #Mức ý nghĩa anpha = 5% (Khoảng tin cậy 95%)
# anpha <- 1 - 0.95
# 
# #Lọc từ dataset SpeedDating lấy DecisionFemale = "Yes" của những RaceF=="Caucasian"
# sampleDFRF_Y <- subset(SpeedDating, DecisionFemale=='Yes'& RaceF=="Caucasian", select=c(DecisionFemale))[[1]]; sampleDFRF_Y
# 
# #Lọc từ dataset SpeedDating lấy DecisionFemale = "No" của những RaceF=="Caucasian"
# sampleDFRF_N <- subset(SpeedDating, DecisionFemale=='No'& RaceF=="Caucasian", select=c(DecisionFemale))[[1]]; sampleDFRF_N
# 
# #Lấy kích thước mẫu sampleDFRF_Y và sampleDFRF_N
# n_Y <- length(sampleDFRF_Y); n_N <- length(sampleDFRF_N);  n_Y; n_N
# 
# #Tính hiệu tỷ lệ sampleDFRF_Y và sampleDFRF_N
# p_Y <- n_Y/(n_Y + n_N); p_Y
# 
# p_N <- n_N/(n_Y + n_N); p_N
# 
# abs(p_Y - p_N)
# #####################################Bootstrap###############################################
# #Tạo phân phối bootsrap rồi tính độ lệch chuuẩn vẽ biểu đồ hist
# #replace=TRUE: Lấy có hoàn lại
# boot_distDFRF <- replicate(10000, mean(sample(sampleDFRF_Y, n_Y, replace=TRUE)) - mean(sample(sampleDFRF_N, n_N, replace=TRUE)))
# sd(boot_distDFRF); hist(boot_distDFRF, breaks = 40)
# 
# #Xây dựng khoảng tin cậy 95%
# quantile(boot_distDFRF, c(anpha/2, 1-anpha/2))
# 
# ######################################CongThuc##############################################
# #Tính Sai số chuẩn
# seDFRF <- sqrt(var(sampleDFRF_Y)/n_Y + var(sampleDFRF_N)/n_N); seDFRF
# 
# #Tính phân phối chuẩn z
# z <- qnorm(1-anpha/2); 
# 
# #Tính phân phối student t
# t <- qt(1-anpha/2, df=min(c(n_Y-1, n_N-1)))
# 
# #Xác định khoảng tin cậy 95% theo z
# (x_Y - x_N) + c(-z*seDFRF, z*seDFRF); 
# 
# #Xác định khoảng tin cậy 95% theo t
# (x_Y - x_N) + c(-t*seDFRF, t*seDFRF)


