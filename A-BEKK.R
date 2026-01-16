setwd("C:/important/Projects/FRAM Project")
mydata=read.csv("Energy_data.csv", header=TRUE)
attach(mydata)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)
library(tseries)
library(rmgarch)
library(pastecs)
library(tidyr)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(data.table)
library(MTS)
library(mgarchBEKK)
library(mvtnorm)
library(BEKKs)
date = dmy(?..Date)
X1 =data.frame(OVXR,GVZR,VXSLVR,ECOR,S.PGCER,SolarR,S.P.TSX.RECTR,FTSE4GOOD.UK.50R,FTSE4GOOD.US.100R,FTSE4GOOD.Europe.50R,FTSE4GOOD.US.Global.100R)
x1 = as.matrix(X1)
obj_spec = bekk_spec(model = list(type = "bekk", asymmetric = TRUE))
x2 = bekk_fit(obj_spec, x1)
summary(x2)
x2[["sigma_t"]][["Conditional correlation of OVXR  and  ECOR"]]
View(x2[["sigma_t"]])
x = data.frame((x2[["sigma_t"]]))
dimnames(x)
View(obj_spec)
View(x)
colnames(x)[4] <- "ECOR/OVXR"
colnames(x)[5] <- "SPGCER/OVXR"
colnames(x)[6] <- "SolarR/OVXR"
colnames(x)[7] <- "SPTSXR/OVXR"
colnames(x)[8] <- "FTSE4GOOD-UK50/OVXR"
colnames(x)[9] <- "FTSE4GOOD-US100/OVXR"
colnames(x)[10] <- "FTSE4GOOD-EUROPE50/OVXR"
colnames(x)[11] <- "FTSE4GOOD-USGLOBAL100/OVXR"
colnames(x)[14] <- "ECOR/GVZR"
colnames(x)[15] <- "SPGCER/GVZR"
colnames(x)[16] <- "SolarR/GVZR"
colnames(x)[17] <- "SPTSXR/GVZR"
colnames(x)[18] <- "FTSE4GOOD-UK50/GVZR"
colnames(x)[19] <- "FTSE4GOOD-US100/GVZR"
colnames(x)[20] <- "FTSE4GOOD-EUROPE50/GVZR"
colnames(x)[21] <- "FTSE4GOOD-USGLOBAL100/GVZR"
colnames(x)[23] <- "ECOR/VXSLVR"
colnames(x)[24] <- "SPGCER/VXSLVR"
colnames(x)[25] <- "SolarR/VXSLVR"
colnames(x)[26] <- "SPTSXR/VXSLVR"
colnames(x)[27] <- "FTSE4GOOD-UK50/VXSLVR"
colnames(x)[28] <- "FTSE4GOOD-US100/VXSLVR"
colnames(x)[29] <- "FTSE4GOOD-EUROPE50/VXSLVR"
colnames(x)[30] <- "FTSE4GOOD-USGLOBAL100/VXSLVR"
dim(x)
dimnames(x)
correlation = data.frame(x[,c(4,5,6,7,8,9,10,11,14,15,16,17,18,19,20,21,23,24,25,26,27,28,29,30)])
summary(correlation[1])
plot.ts(correlation[1])
data = data.frame(date,(correlation[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24)]))
head(data)
dimnames(data)
date = data$date
c1 = data$ECOR.OVXR
c2 = data$SPGCER.OVXR
c3 = data$SolarR.OVXR 
c4 = data$SPTSXR.OVXR
c5 = data$FTSE4GOOD.UK50.OVXR
c6 = data$FTSE4GOOD.US100.OVXR
c7 = data$FTSE4GOOD.EUROPE50.OVXR
c8 = data$FTSE4GOOD.USGLOBAL100.OVXR
c9 = data$ECOR.GVZR
c10 = data$SPGCER.GVZR
c11 = data$SolarR.GVZR
c12 = data$SPTSXR.GVZR
c13 = data$FTSE4GOOD.UK50.GVZR
c14 = data$FTSE4GOOD.US100.GVZR
c15 = data$FTSE4GOOD.EUROPE50.GVZR
c16 = data$FTSE4GOOD.USGLOBAL100.GVZR
c17 = data$ECOR.VXSLVR
c18 = data$SPGCER.VXSLVR
c19 = data$SolarR.GVZR
c20 = data$SPTSXR.VXSLVR
c21 = data$FTSE4GOOD.UK50.VXSLVR
c22 = data$FTSE4GOOD.US100.VXSLVR
c23 = data$FTSE4GOOD.EUROPE50.VXSLVR
c24 = data$FTSE4GOOD.USGLOBAL100.VXSLVR
summary(data[2])
StdDev(data[2])
StdDev(data$ECOR.OVXR)
plot1 = ggplot2::ggplot(data = data.frame(date, data$ECOR.OVXR), aes(x = date , y = data$ECOR.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "ECO/OVX")
print(plot1)
summary(data[3])
StdDev(data$SPGCER.OVXR)
plot2 = ggplot2::ggplot(data = data.frame(date, data$SPGCER.OVXR), aes(x = date , y = data$SPGCER.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPGCE/OVX")
print(plot2)
summary(data[4])
StdDev(data$SolarR.OVXR)
plot3 = ggplot2::ggplot(data = data.frame(date, data$SolarR.OVXR), aes(x = date , y = data$SolarR.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "Solar/OVX")
print(plot3)
summary(data[5])
StdDev(data$SPTSXR.OVXR)
plot4 = ggplot2::ggplot(data = data.frame(date, data$SPTSXR.OVXR), aes(x = date , y = data$SPTSXR.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPTSX/OVX")
print(plot4)
summary(data[6])
StdDev(data$FTSE4GOOD.UK50.OVXR)
plot5 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.UK50.OVXR), aes(x = date , y = data$FTSE4GOOD.UK50.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.UK50/OVX")
print(plot5)
summary(data[7])
StdDev(data$FTSE4GOOD.US100.OVXR)
plot6 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.US100.OVXR), aes(x = date , y = data$FTSE4GOOD.US100.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.US100/OVX")
print(plot6)
summary(data[8])
StdDev(data$FTSE4GOOD.EUROPE50.OVXR)
plot7 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.EUROPE50.OVXR), aes(x = date , y = data$FTSE4GOOD.EUROPE50.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.EUROPE50/OVX")
print(plot7)
summary(data[9])
StdDev(data$FTSE4GOOD.USGLOBAL100.OVXR)
plot8 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.USGLOBAL100.OVXR), aes(x = date , y = data$FTSE4GOOD.USGLOBAL100.OVXR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.USGLOBAL100/OVX")
print(plot8)
summary(data[10])
StdDev(data$ECOR.GVZR)
plot9 = ggplot2::ggplot(data = data.frame(date, data$ECOR.GVZR), aes(x = date , y = data$ECOR.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "ECO.GVZ")
print(plot9)
summary(data[11])
StdDev(data$SPGCER.GVZR)
plot10 = ggplot2::ggplot(data = data.frame(date, data$SPGCER.GVZR), aes(x = date , y = data$SPGCER.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPGCE.GVZ")
print(plot10)
summary(data[12])
StdDev(data$SolarR.GVZR)
plot11 = ggplot2::ggplot(data = data.frame(date, data$SolarR.GVZR), aes(x = date , y = data$SolarR.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "Solar.GVZ")
print(plot11)
summary(data[13])
StdDev(data$SPTSXR.GVZR)
plot12 = ggplot2::ggplot(data = data.frame(date, data$SPTSXR.GVZR), aes(x = date , y = data$SPTSXR.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPTSX.GVZ")
print(plot12)
summary(data[14])
StdDev(data$FTSE4GOOD.UK50.GVZR)
plot13 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.UK50.GVZR), aes(x = date , y = data$FTSE4GOOD.UK50.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.UK50.GVZ")
print(plot13)
summary(data[15])
StdDev(data$FTSE4GOOD.US100.GVZR)
plot14 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.US100.GVZR), aes(x = date , y = data$FTSE4GOOD.US100.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.US100.GVZ")
print(plot14)
summary(data[16])
StdDev(data$FTSE4GOOD.EUROPE50.GVZR)
plot15 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.EUROPE50.GVZR), aes(x = date , y = data$FTSE4GOOD.EUROPE50.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.EUROPE50.GVZ")
print(plot15)
summary(data[17])
StdDev(data$FTSE4GOOD.USGLOBAL100.GVZR)
plot16 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.USGLOBAL100.GVZR), aes(x = date , y = data$FTSE4GOOD.USGLOBAL100.GVZR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.USGLOBAL100.GVZ")
print(plot16)
summary(data[18])
StdDev(data$ECOR.VXSLVR)
plot17 = ggplot2::ggplot(data = data.frame(date, data$ECOR.VXSLVR), aes(x = date , y = data$ECOR.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "ECO.VXSLV")
print(plot17)
summary(data[19])
StdDev(data$SPGCER.VXSLVR)
plot18 = ggplot2::ggplot(data = data.frame(date, data$SPGCER.VXSLVR), aes(x = date , y = data$SPGCER.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPGCE.VXSLV")
print(plot18)
summary(data[20])
StdDev(data$SolarR.VXSLVR)
plot19 = ggplot2::ggplot(data = data.frame(date, data$SolarR.VXSLVR), aes(x = date , y = data$SolarR.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "Solar.VXSLV")
print(plot19)
summary(data[21])
StdDev(data$SPTSXR.VXSLVR)
plot20 = ggplot2::ggplot(data = data.frame(date, data$SPTSXR.VXSLVR), aes(x = date , y = data$SPTSXR.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPTSX.VXSLV")
print(plot20)
summary(data[22])
StdDev(data$FTSE4GOOD.UK50.VXSLVR)
plot21 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.UK50.VXSLVR), aes(x = date , y = data$FTSE4GOOD.UK50.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.UK50.VXSLV")
print(plot21)
summary(data[23])
StdDev(data$FTSE4GOOD.US100.VXSLVR)
plot22 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.US100.VXSLVR), aes(x = date , y = data$FTSE4GOOD.US100.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.US100.VXSLV")
print(plot22)
summary(data[24])
StdDev(data$FTSE4GOOD.EUROPE50.VXSLVR)
plot23 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.EUROPE50.VXSLVR), aes(x = date , y = data$FTSE4GOOD.EUROPE50.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.EUROPE50.VXSLV")
print(plot23)
summary(data[25])
StdDev(data$FTSE4GOOD.USGLOBAL100.VXSLVR)
plot24 = ggplot2::ggplot(data = data.frame(date, data$FTSE4GOOD.USGLOBAL100.VXSLVR), aes(x = date , y = data$FTSE4GOOD.USGLOBAL100.VXSLVR)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE4GOOD.USGLOBAL100.VXSLV")
print(plot24)
