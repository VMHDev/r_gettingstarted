#Cài đặt package (package "Lock5withR")
install.packages("Lock5withR")

#Cài đặt package (package "Lock5withR")
install.packages("mosaicData")

#Cài đặt package (package "Lock5withR")
install.packages("mosaic")

#Cài đặt package (package "Lock5withR")
install.packages("plotrix")

#Load package để sử dụng
library(Lock5withR)

#Load package mosaic để sử dụng
library(mosaic)

#Load package mosaicData để sử dụng
library(mosaicData)

#Liệt kê các data đã nạp vào
data()

#Liệt kê các data của package Lock5withR
data(package='Lock5withR')

#Xem thông tin mô tả dataset SpeedDating
?SpeedDating

#Xem cấu trúc của một hàm trong R
?View

#Xem dataset của SpeedDating
View(SpeedDating)

#Xem dataset của HELPrct
View(HELPrct)

# Đổi tên một dataset
dataset <- SpeedDating

#Lấy 6 dữ liệu đầu tiên của dataset
head(SpeedDating)

#Xem cấu trúc của dataset
str(SpeedDating)

#Xem số dòng của dataset
nrow(SpeedDating)


####################################################################################
####################### Sử dụng package mosaic và mosaicData #######################
#Xem thông tin pakage mosaic
?mosaic

#Xem thông tin pakage mosaicData
?mosaicData

#Load package mosaic để sử dụng
library(mosaic)

#Load package mosaicData để sử dụng
library(mosaicData)

#Xem thông tin mô tả dataset HELPrct
?HELPrct

#Tính tỷ lệ female (nữ) trong dataset HELPrct
prop( ~sex, data=HELPrct)

#Tính tỷ lệ female (nam) trong dataset HELPrct
prop( ~sex, data=HELPrct, success = "male")

#Đếm số lượng female (nữ) trong dataset HELPrct
count( ~sex, data=HELPrct)

#Đếm số lượng male (nam) trong dataset HELPrct
count( ~sex, data=HELPrct, success = "male")

#Đếm số lượng female (nữ) theo từng giá trị của substance trong dataset HELPrct
count( ~sex | substance, data=HELPrct)

#Đếm số lượng male (nam) theo từng giá trị của substance trong dataset HELPrct
count( ~sex | substance, data=HELPrct, success = "male")

#Tính tỷ lệ female (nữ) theo từng giá trị của substance trong dataset HELPrct
prop( ~sex | substance, data=HELPrct)

#Đếm số lượng male (nam) theo từng giá trị của substance trong dataset HELPrct
prop( ~sex | substance, data=HELPrct, success = "male")

#Tính phần trăm female (nữ) theo từng giá trị của substance trong dataset HELPrct
perc( ~sex | substance, data=HELPrct)

#Tính phần trăm male (nam) theo từng giá trị của substance trong dataset HELPrct
perc( ~sex | substance, data=HELPrct, success = "male")

mean( HELPrct$age )
mean( ~ age, data=HELPrct )
mean( age ~ sex + substance, data=HELPrct )
mean( ~ age | sex + substance, data=HELPrct )
mean( sqrt(age), data=HELPrct )

ICU20 <- subset(HELPrct, age == "37"); ICU20
mean(~age, data = ICU20)

sum( ~ age, data=HELPrct)

count( ~age, data=HELPrct)

x <- c(10, 50)
xm <- mean(x); xm
c(xm, mean(x, trim = 0.10))

#Lập bảng dữ liệu cho thuộc tính sex
tally(~sex, data=HELPrct)

#Lập bảng dữ liệu cho thuộc tính substance
tally(~substance, data=HELPrct)



