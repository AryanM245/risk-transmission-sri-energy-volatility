
install.packages("quantmod")
install.packages("xts")
install.packages("PerformanceAnalytics")
install.packages("rugarch")
install.packages("tseries")
install.packages("rmgarch")
install.packages("pastecs")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("scales")
install.packages("lubridate")
install.packages("data.table")
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
date = dmy(?..Date)
date
s = ugarchspec(mean.model = list(armaOrder = c(0,0)),
               variance.model = list(model = "sGARCH"),
               distribution.model = 'norm')
dcc.garch11.spec = dccspec(uspec = multispec( replicate(11, s) ),
                           dccOrder = c(1,1),
                           distribution = "mvnorm")
dcc.garch11.spec
dimnames(mydata)
dim.data.frame(mydata)
modelfit = dccfit(dcc.garch11.spec,data = data.frame(OVXR,GVZR,VXSLVR,ECOR,S.PGCER,SolarR,S.P.TSX.RECTR,FTSE4GOOD.UK.50R,FTSE4GOOD.US.100R,FTSE4GOOD.Europe.50R,FTSE4GOOD.US.Global.100R))
modelfit
plot(modelfit)
correlation = rcor(modelfit)
correlation
dim(correlation)
summary(correlation)
ECOROVXR = correlation[4,1,]
ECOROVXR
ECOOVX= as.numeric(ECOROVXR)
summary(ECOOVX)
StdDev(ECOOVX)
plot.ts(ECOOVX)
plot1 = ggplot2::ggplot(data = data.frame(date,ECOOVX), aes(x = date , y = ECOOVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "ECO/OVX")
print(plot1)
ECORGVZR = correlation[4,2,]
ECORGVZR
ECOGVZ = as.numeric(ECORGVZR)
ECOGVZ
summary(ECOGVZ)
StdDev(ECOGVZ)
plot.ts(ECOGVZ)
plot2 = ggplot2::ggplot(data = data.frame(date,ECOGVZ), aes(x = date , y = ECOGVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "ECO/GVZ")
print(plot2)
ECORVXSLVR = correlation[4,3,]
ECOVXSLV=as.numeric(ECORVXSLVR)
summary(ECOVXSLV)
StdDev(ECOVXSLV)
plot.ts(ECOVXSLV)
plot3 = ggplot2::ggplot(data = data.frame(date,ECOVXSLV), aes(x = date , y = ECOVXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "ECO/VXSLV")
print(plot3)
SPGCEROVXR = correlation[5,1,]
SPGCEOVX = as.numeric(SPGCEROVXR)
summary(SPGCEOVX)
StdDev(SPGCEOVX)
plot.ts(SPGCEOVX)
plot4 = ggplot2::ggplot(data = data.frame(date,SPGCEOVX), aes(x = date , y = SPGCEOVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPGCE/OVX")
print(plot4)
SPGCERGVZR = correlation[5,2,]
SPGCEGVZ = as.numeric(SPGCERGVZR)
summary(SPGCEGVZ)
StdDev(SPGCEGVZ)
plot.ts(SPGCEGVZ)
plot5 = ggplot2::ggplot(data = data.frame(date,SPGCEGVZ), aes(x = date , y = SPGCEGVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPGCE/GVZ")
print(plot5)
SPGCERVXSLVR = correlation[5,3,]
SPGCEVXSLV = as.numeric(SPGCERVXSLVR)
summary(SPGCEVXSLV)
StdDev(SPGCEVXSLV)
plot.ts(SPGCEVXSLV)
plot6 = ggplot2::ggplot(data = data.frame(date,SPGCEVXSLV), aes(x = date , y = SPGCEVXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPGCE/VXSLV")
print(plot6)
SOLRXROVXR = correlation[6,1,]
SOLRXOVX = as.numeric(SOLRXROVXR)
summary(SOLRXOVX)
StdDev(SOLRXOVX)
plot.ts(SOLRXOVX)
plot7 = ggplot2::ggplot(data = data.frame(date,SOLRXOVX), aes(x = date , y = SOLRXOVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "Solar/OVX")
print(plot7)
SOLRXRGVZR = correlation[6,2,]
SOLRXGVZ = as.numeric(SOLRXRGVZR)
summary(SOLRXGVZ)
StdDev(SOLRXGVZ)
plot.ts(SOLRXGVZ)
plot8 = ggplot2::ggplot(data = data.frame(date,SOLRXGVZ), aes(x = date , y = SOLRXGVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "Solar/GVZ")
print(plot8)
SOLRXRVXSLVR = correlation[6,3,]
SOLRXVXSLV = as.numeric(SOLRXRVXSLVR)
summary(SOLRXVXSLV)
StdDev(SOLRXVXSLV)
plot.ts(SOLRXVXSLV)
plot9 = ggplot2::ggplot(data = data.frame(date,SOLRXVXSLV), aes(x = date , y = SOLRXVXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "Solar/VXSLV")
print(plot9)
SPTSXROVXR = correlation[7,1,]
SPTSXOVX = as.numeric(SPTSXROVXR)
summary(SPTSXOVX)
StdDev(SPTSXOVX)
plot.ts(SPTSXOVX)
plot10 = ggplot2::ggplot(data = data.frame(date,SPTSXOVX), aes(x = date , y = SPTSXOVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPTSX/OVX")
print(plot10)
SPTSXRGVZR = correlation[7,2,]
SPTSXGVZ = as.numeric(SPTSXRGVZR)
summary(SPTSXGVZ)
StdDev(SPTSXGVZ)
plot.ts(SPTSXGVZ)
plot11 = ggplot2::ggplot(data = data.frame(date,SPTSXGVZ), aes(x = date , y = SPTSXGVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPTSX/GVZ")
print(plot11)
SPTSXRVXSLVR = correlation[7,3,]
SPTSXVXSLV = as.numeric(SPTSXRVXSLVR)
summary(SPTSXVXSLV)
StdDev(SPTSXVXSLV)
plot.ts(SPTSXVXSLV)
plot12 = ggplot2::ggplot(data = data.frame(date,SPTSXVXSLV), aes(x = date , y = SPTSXVXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "SPTSX/VXSLV")
print(plot12)
FTSE4GOODUK50ROVXR = correlation[8,1,]
FTSE4GOODUK50OVX = as.numeric(FTSE4GOODUK50ROVXR)
summary(FTSE4GOODUK50OVX)
StdDev(FTSE4GOODUK50OVX)
plot.ts(FTSE4GOODUK50OVX)
plot13 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUK50OVX), aes(x = date , y = FTSE4GOODUK50OVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSEUK50/OVX")
print(plot13)
FTSE4GOODUK50RGVZR = correlation[8,2,]
FTSE4GOODUK50GVZ = as.numeric(FTSE4GOODUK50RGVZR)
summary(FTSE4GOODUK50GVZ)
StdDev(FTSE4GOODUK50GVZ)
plot.ts(FTSE4GOODUK50RGVZR)
plot14 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUK50GVZ), aes(x = date , y = FTSE4GOODUK50GVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSEUK50/GVZ")
print(plot14)
FTSE4GOODUK50RVXSLVR = correlation[8,3,]
FTSE4GOODUK50VXSLV = as.numeric(FTSE4GOODUK50RVXSLVR)
summary(FTSE4GOODUK50VXSLV)
StdDev(FTSE4GOODUK50VXSLV)
plot.ts(FTSE4GOODUK50VXSLV)
plot15 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUK50VXSLV), aes(x = date , y = FTSE4GOODUK50VXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSEUK50/VXSLV")
print(plot15)
FTSE4GOODUS100ROVXR = correlation[9,1,]
FTSE4GOODUS100OVX = as.numeric(FTSE4GOODUS100ROVXR)
summary(FTSE4GOODUS100OVX)
StdDev(FTSE4GOODUS100OVX)
plot.ts(FTSE4GOODUS100OVX)
plot16 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUS100OVX), aes(x = date , y = FTSE4GOODUS100OVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE US 100/OVX")
print(plot16)
FTSE4GOODUS100RGVZR = correlation[9,2,]
FTSE4GOODUS100GVZ = as.numeric(FTSE4GOODUS100RGVZR)
summary(FTSE4GOODUS100GVZ)
StdDev(FTSE4GOODUS100GVZ)
plot.ts(FTSE4GOODUS100GVZ)
plot17 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUS100GVZ), aes(x = date , y = FTSE4GOODUS100GVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE US 100/GVZ")
print(plot17)
FTSE4GOODUS100RVXSLVR = correlation[9,3,]
FTSE4GOODUS100VXSLV = as.numeric(FTSE4GOODUS100RVXSLVR)
summary(FTSE4GOODUS100VXSLV)
StdDev(FTSE4GOODUS100VXSLV)
plot.ts(FTSE4GOODUS100VXSLV)
plot18 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUS100VXSLV), aes(x = date , y = FTSE4GOODUS100VXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE US 100/VXSLV")
print(plot18)
FTSE4GOODEUROPE50ROVXR = correlation[10,1,]
FTSE4GOODEUROPR50OVX = as.numeric(FTSE4GOODEUROPE50ROVXR)
summary(FTSE4GOODEUROPR50OVX)
StdDev(FTSE4GOODEUROPR50OVX)
plot.ts(FTSE4GOODEUROPR50OVX)
plot19 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODEUROPR50OVX), aes(x = date , y = FTSE4GOODEUROPR50OVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE EUROPE 50/OVX")
print(plot19)
FTSE4GOODEUROPE50RGVZR = correlation[10,2,]
FTSE4GOODEUROPE50GVZ = as.numeric(FTSE4GOODEUROPE50RGVZR)
summary(FTSE4GOODEUROPE50GVZ)
StdDev(FTSE4GOODEUROPE50GVZ)
plot.ts(FTSE4GOODEUROPE50GVZ)
plot20 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODEUROPE50GVZ), aes(x = date , y = FTSE4GOODEUROPE50GVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE EUROPE 50/GVZ")
print(plot20)
FTSE4GOODEUROPE50RVXSLVR = correlation[10,3,]
FTSE4GOODEUROPE50VXSLV = as.numeric(FTSE4GOODEUROPE50RVXSLVR)
summary(FTSE4GOODEUROPE50VXSLV)
StdDev(FTSE4GOODEUROPE50VXSLV)
plot.ts(FTSE4GOODEUROPE50VXSLV)
plot21 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODEUROPE50VXSLV), aes(x = date , y = FTSE4GOODEUROPE50VXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE EUROPE 50/VXSLV")
print(plot21)
FTSE4GOODUSGLOBALROVXR = correlation[11,1,]
FTSE4GOODUSGLOBALOVX = as.numeric(FTSE4GOODUSGLOBALROVXR)
summary(FTSE4GOODUSGLOBALOVX)
StdDev(FTSE4GOODUSGLOBALOVX)
plot.ts(FTSE4GOODUSGLOBALOVX)
plot22 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUSGLOBALOVX), aes(x = date , y = FTSE4GOODUSGLOBALOVX)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE US GLOBAL/OVX")
print(plot22)
FTSE4GOODUSGLOBALRGVZR = correlation[11,2,]
FTSE4GOODUSGLOBALGVZ = as.numeric(FTSE4GOODUSGLOBALRGVZR)
summary(FTSE4GOODUSGLOBALGVZ)
StdDev(FTSE4GOODUSGLOBALGVZ)
plot.ts(FTSE4GOODUSGLOBALGVZ)
plot23 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUSGLOBALGVZ), aes(x = date , y = FTSE4GOODUSGLOBALGVZ)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE US GLOBAL/GVZ")
print(plot23)
FTSE4GOODUSGLOBALRVXSLVR = correlation[11,3,]
FTSE4GOODUSGLOBALVXSLV = as.numeric(FTSE4GOODUSGLOBALRVXSLVR)
summary(FTSE4GOODUSGLOBALVXSLV)
StdDev(FTSE4GOODUSGLOBALVXSLV)
plot.ts(FTSE4GOODUSGLOBALVXSLV)
plot24 = ggplot2::ggplot(data = data.frame(date,FTSE4GOODUSGLOBALVXSLV), aes(x = date , y = FTSE4GOODUSGLOBALVXSLV)) +
  geom_line() +
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y")) +
  labs(x = "Year", y = "FTSE US GLOBAL/VXSLV")
print(plot24)
