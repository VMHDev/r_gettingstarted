#################################### Vẽ biễu đổ ####################################
#Load package mosaicData để sử dụng
library(mosaicData)
?pie
####################################################################################
################################# Sử dụng package R ################################
# Lập bảng dữ liệu cho thuộc tính sex
table(HELPrct$sex)

# Vẽ biểu đồ tròn cho thuộc tính sex
pie(table(HELPrct$sex))

# Vẽ biểu đồ tròn cho thuộc tính sex với tên biểu đồ là Pie Chart of Sex
pie(table(HELPrct$sex), main = 'Pie Chart of Sex') 

# Vẽ biểu đồ cho thuộc tính sex với tên biểu đồ là Pie Chart of Sex và dữ liệu (data frame)
mytable <- table(HELPrct$sex); mytable
lbls <- paste(names(mytable), "\n", mytable, sep="")    #paste: Nối chuỗi | names(mytable): Lấy tên của dữ liệu
pie(mytable, labels = lbls, main = 'Pie Chart of Sex') 

# Vẽ biểu đồ cho thuộc tính sex với tên biểu đồ là Pie Chart of Sex và phần trăm (Percentages)
mytable <- table(HELPrct$sex); mytable
pct <- round(mytable/sum(mytable)*100, digits = 1); pct   # Tính tỷ lệ % với làm tròn 1 chữ số
#lblsp <- paste(names(mytable),"\n", pct, sep="")         
lblsp <- paste(c("Nam", "Nu"),"\n", pct, sep="")          # Đổi tên
lblsp <- paste(lblsp,"%",sep="")                          # Thêm kí tự % vào nhãn
pie(mytable, labels = lblsp, main="Pie Chart of Sex")

#Vẽ biểu đồ phân bố
?dotPlot
bwplot(DecisionFemale~LikeF, data = SpeedDating)
#
dotPlot(~LikeF | DecisionFemale, layout = c(1, 2), width = 1, cex = 1, data = SpeedDating)
#
bargraph(~LikeF | DecisionFemale, data = SpeedDating)

#Hai biến định lượng
?xyplot
xyplot(LikeF~AmbitiousF, data = SpeedDating)

##########################################################################################
################################# Sử dụng package plotrix ################################
#Load package plotrix để sử dụng
library(plotrix)

# Lập bảng dữ liệu cho thuộc tính sex
table(HELPrct$sex)

# Vẽ biểu đồ 3D cho thuộc tính sex
pie3D(table(HELPrct$sex))

##########################################################################################
################################# Sử dụng package mosaic ################################
#Load package mosaic để sử dụng
library(mosaic)

#----------------------------------------------------------------------------------------
#Vẽ biểu đồ cột cho thuộc tính sex (theo chiều dọc)
bargraph(~sex, data=HELPrct, ylab = "Số lượng", xlab = "Giới tính", type = "percent")

#Vẽ biểu đồ cột cho thuộc tính substance (theo chiều ngang)
bargraph( ~ substance, data = HELPrct, horizontal = TRUE)

#Vẽ biểu đồ cột cho thuộc tính substance theo sex
bargraph(~substance, group=sex, data=HELPrct)

?bargraph
#----------------------------------------------------------------------------------------
#Vẽ biểu đồ hist
?histogram

histogram(~age, data=HELPrct, breaks = 10)

#Vẽ theo count
histogram(~LikeF, data=SpeedDating, label = TRUE, type = "count", breaks = 10)

#Vẽ theo percent
histogram(~LikeF, data=SpeedDating, label = TRUE, type = "percent", breaks = 10)


