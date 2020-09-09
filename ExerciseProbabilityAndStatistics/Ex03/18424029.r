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
#Xem thông tin mô tả dataset SpeedDating
?SpeedDating
########################################################################################################
# DecisionFemale
SLMuon <- count( ~DecisionFemale, data=SpeedDating); SLMuon

TyLeMuon <- prop( ~DecisionFemale, data=SpeedDating); TyLeMuon

PhanTramMuon <- perc( ~DecisionFemale, data=SpeedDating); PhanTramMuon

#..................................................................................
SLKoMuon <- count( ~DecisionFemale, data=SpeedDating, success = "Yes"); SLKoMuon

TyLeKoMuon <- prop( ~DecisionFemale, data=SpeedDating, success = "Yes"); TyLeKoMuon

PhanTramKoMuon <- perc( ~DecisionFemale, data=SpeedDating, success = "Yes"); PhanTramKoMuon

#..................................................................................
tblDecision <- table(SpeedDating$DecisionFemale)
pct <- round(tblDecision/sum(tblDecision)*100, digits = 1)
lblsp <- paste(names(tblDecision),"\n", pct, sep="")
lblsp <- paste(lblsp,"%",sep="")
pie(tblDecision, labels = lblsp, main="Pie Chart Of Decision Female")
#--------------------------------------------------------------------------------------------------------
# RaceF
SLChauA <- count( ~RaceF, data=SpeedDating, success = 'Asian'); SLChauA

TyLeChauA <- prop( ~RaceF , data=SpeedDating, success = 'Asian'); TyLeChauA

PhanTramChauA <- perc( ~RaceF, data=SpeedDating, success = 'Asian'); PhanTramChauA

#..................................................................................
SLDaDen <- count( ~RaceF, data=SpeedDating, success = 'Black'); SLDaDen

TyLeDaDen <- prop( ~RaceF , data=SpeedDating, success = 'Black'); TyLeDaDen

PhanTramDaDen <- perc( ~RaceF, data=SpeedDating, success = 'Black'); PhanTramDaDen

#..................................................................................
SLDaTrang <- count( ~RaceF, data=SpeedDating, success = 'Caucasian'); SLDaTrang

TyLeDaTrang <- prop( ~RaceF , data=SpeedDating, success = 'Caucasian'); TyLeDaTrang

PhanTramDaTrang <- perc( ~RaceF, data=SpeedDating, success = 'Caucasian'); PhanTramDaTrang

#..................................................................................
SLLaTinh <- count( ~RaceF, data=SpeedDating, success = 'Latino'); SLLaTinh

TyLeLaTinh <- prop( ~RaceF , data=SpeedDating, success = 'Latino'); TyLeLaTinh

PhanTramLaTinh <- perc( ~RaceF, data=SpeedDating, success = 'Latino'); PhanTramLaTinh

#..................................................................................
SLKhac <- count( ~RaceF, data=SpeedDating, success = 'Other'); SLKhac

TyLeKhac <- prop( ~RaceF , data=SpeedDating, success = 'Other'); TyLeKhac

PhanTramKhac <- perc( ~RaceF, data=SpeedDating, success = 'Other'); PhanTramKhac

#..................................................................................
SLKhongXacDinh <- count( ~RaceF, data=SpeedDating, success = ''); SLKhongXacDinh

TyLeKhongXacDinh <- prop( ~RaceF , data=SpeedDating, success = ''); TyLeKhongXacDinh

PhanTramKhongXacDinh <- perc( ~RaceF, data=SpeedDating, success = ''); PhanTramKhongXacDinh

#..................................................................................
bargraph( ~RaceF, data=SpeedDating, horizontal = TRUE)

#--------------------------------------------------------------------------------------------------------
# LikeF
tally(~LikeF, data=SpeedDating)

sum(~LikeF, data=SpeedDating, na.rm=TRUE)

mean(SpeedDating$LikeF, na.rm=TRUE)
mean( ~LikeF, data=SpeedDating, na.rm=TRUE)

#..................................................................................
histogram(~LikeF, data=SpeedDating, label = TRUE, type = "count", breaks = 10)

histogram(~LikeF, data=SpeedDating, label = TRUE, type = "percent", breaks = 10)

#--------------------------------------------------------------------------------------------------------
# FunF
tally(~FunF, data=SpeedDating)

sum(~FunF, data=SpeedDating, na.rm=TRUE)

mean(SpeedDating$FunF, na.rm=TRUE)
mean( ~FunF, data=SpeedDating, na.rm=TRUE)

#..................................................................................
histogram(~FunF, data=SpeedDating, label = TRUE, type = "count", breaks = 10)

histogram(~FunF, data=SpeedDating, label = TRUE, type = "percent", breaks = 10)

#--------------------------------------------------------------------------------------------------------
# AmbitiousF
tally(~AmbitiousF, data=SpeedDating)

sum(~AmbitiousF, data=SpeedDating, na.rm=TRUE)

mean(SpeedDating$AmbitiousF, na.rm=TRUE)
mean( ~AmbitiousF, data=SpeedDating, na.rm=TRUE)

#..................................................................................
histogram(~AmbitiousF, data=SpeedDating, label = TRUE, type = "count", breaks = 10)

histogram(~AmbitiousF, data=SpeedDating, label = TRUE, type = "percent", breaks = 10)

########################################################################################################
# Hai biến định tính
tally(~DecisionFemale + RaceF, margins = TRUE, data = SpeedDating)

#--------------------------------------------------------------------------------------------------------
count( ~DecisionFemale | RaceF, data=SpeedDating)

prop( ~DecisionFemale | RaceF, data=SpeedDating)

perc( ~DecisionFemale| RaceF, data=SpeedDating)
#.......................................................................................................
count( ~DecisionFemale | RaceF, data=SpeedDating, success = "Yes")

prop( ~DecisionFemale | RaceF, data=SpeedDating, success = "Yes")

perc( ~DecisionFemale| RaceF, data=SpeedDating, success = "Yes")

#--------------------------------------------------------------------------------------------------------
bargraph(~DecisionFemale, groups = RaceF, stack = TRUE, auto.key = TRUE, data = SpeedDating)

bargraph(~RaceF, groups = DecisionFemale, stack = TRUE, auto.key = TRUE, data = SpeedDating)
#--------------------------------------------------------------------------------------------------------
########################################################################################################
# Một biến định tính DecisionFemale, một biến định lượng LikeF
favstats(~LikeF | DecisionFemale, data = SpeedDating)
mean(~LikeF | DecisionFemale, data = SpeedDating, na.rm = TRUE)
diff(mean(~LikeF | DecisionFemale, data = SpeedDating, na.rm = TRUE))

#--------------------------------------------------------------------------------------------------------
bwplot(DecisionFemale~LikeF, data = SpeedDating)
dotPlot(~LikeF | DecisionFemale, layout = c(1, 2), width = 1, cex = 1, data = SpeedDating)
bargraph(~LikeF | DecisionFemale, data = SpeedDating)
########################################################################################################
# Hai biến định lượng LikeF và AmbitiousF
cor(LikeF~AmbitiousF, data = SpeedDating, use="complete.obs")

cor(AmbitiousF~LikeF, data = SpeedDating, use="complete.obs")

#--------------------------------------------------------------------------------------------------------
xyplot(LikeF~AmbitiousF, data = SpeedDating)

