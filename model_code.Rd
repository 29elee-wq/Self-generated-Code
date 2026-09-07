Num<-5000
AL<-read.csv("Alabama.csv",header=T)
AZ<-read.csv("Arizona.csv",header=T)
AR<-read.csv("Arkansas.csv",header=T)
CA<-read.csv("CA.csv",header=T)
CO<-read.csv("CO.csv",header=T)
CT<-read.csv("CT.csv",header=T)
FL<-read.csv("Florida.csv",header=T)
GA<-read.csv("Georgia.csv",header=T)
ID<-read.csv("Idaho.csv",header=T)
IL<-read.csv("Illinois.csv",header=T)
IN<-read.csv("Indiana.csv",header=T)
IA<-read.csv("Iowa.csv",header=T)
KS<-read.csv("Kansas.csv",header=T)
KY<-read.csv("Kentucky.csv",header=T)
LA<-read.csv("Louisiana.csv",header=T)
ME<-read.csv("Maine.csv",header=T)
MD<-read.csv("Maryland.csv",header=T)
MI<-read.csv("Michigan.csv",header=T)
MN<-read.csv("Minnesota.csv",header=T)
MS<-read.csv("Mississippi.csv",header=T)
MO<-read.csv("Missouri.csv",header=T)
MT<-read.csv("Montana.csv",header=T)
NE<-read.csv("Nebraska.csv",header=T)
NJ<-read.csv("NJ.csv",header=T)
NY<-read.csv("NY.csv",header=T)
NC<-read.csv("North Carolina.csv",header=T)
ND<-read.csv("North Dakota.csv",header=T)
OH<-read.csv("Ohio.csv",header=T)
OR<-read.csv("OregonNew.csv",header=T)
PA<-read.csv("PA.csv",header=T)
SD<-read.csv("South Dakota.csv",header=T)
TN<-read.csv("Tennessee.csv",header=T)
TX<-read.csv("Texas.csv",header=T)
UT<-read.csv("Utah.csv",header=T)
VT<-read.csv("Vermont.csv",header=T)
VA<-read.csv("Virginia.csv",header=T)
WA<-read.csv("WashingtonNew.csv",header=T)
WV<-read.csv("West Virginia.csv",header=T)
WI<-read.csv("Wisconsin.csv",header=T)
WY<-read.csv("Wyoming.csv",header=T)

#DE<-read.csv("Delaware.csv",header=T) 
#MA<-read.csv("Massachusetts.csv",header=T)
#NV<-read.csv("Nevada.csv",header=T)
#NH<-read.csv("New Hampshire.csv",header=T)
#NM<-read.csv("New Mexico.csv",header=T)
#OK<-read.csv("Oklahoma.csv",header=T)
#RI<-read.csv("RI.csv",header=T)
#SC<-read.csv("South Carolina.csv",header=T)
###### Finish all state
 

 
 
### linear model fit based on WA and the Year
All<-cbind(WA[,2],WA[,9],OR[,9])
Testing<-All[1:10,]
Training<-All[11:39,]
M1<-lm(Training[,3]~Training[,2]+Training[,1])
summary(M1)
Forecast2016<-M1$coef[[1]]+M1$coef[[2]]*WA[10,9]+M1$coef[[3]]*2016
Forecast2017<-M1$coef[[1]]+M1$coef[[2]]*WA[9,9]+M1$coef[[3]]*2017
Forecast2018<-M1$coef[[1]]+M1$coef[[2]]*WA[8,9]+M1$coef[[3]]*2018
Forecast2019<-M1$coef[[1]]+M1$coef[[2]]*WA[7,9]+M1$coef[[3]]*2019
Forecast2020<-M1$coef[[1]]+M1$coef[[2]]*WA[6,9]+M1$coef[[3]]*2020
Forecast2021<-M1$coef[[1]]+M1$coef[[2]]*WA[5,9]+M1$coef[[3]]*2021
Forecast2022<-M1$coef[[1]]+M1$coef[[2]]*WA[4,9]+M1$coef[[3]]*2022
Forecast2023<-M1$coef[[1]]+M1$coef[[2]]*WA[3,9]+M1$coef[[3]]*2023
Forecast2024<-M1$coef[[1]]+M1$coef[[2]]*WA[2,9]+M1$coef[[3]]*2024
Forecast2025<-M1$coef[[1]]+M1$coef[[2]]*WA[1,9]+M1$coef[[3]]*2025

plot(OR[,2],OR[,9])
lines(OR[,2],OR[,9])
points(2016,Forecast2016,col=2,pch=8) 
points(2017,Forecast2017,col=2,pch=8)
points(2018,Forecast2018,col=2,pch=8) 
points(2019,Forecast2019,col=2,pch=8) 
points(2020,Forecast2020,col=2,pch=8) 
points(2021,Forecast2021,col=2,pch=8)  
points(2022,Forecast2022,col=2,pch=8) 
points(2023,Forecast2023,col=2,pch=8) 
points(2024,Forecast2024,col=2,pch=8) 
points(2025,Forecast2025,col=2,pch=8) 

#### TS analysis for M1 residuals
library(tseries)
library(dplyr) 
acf(M1$res[29:1])
POG<-arima(M1$res[29:1],c(1,0,0)) 
plot(OR[,2],OR[,9],main="Oregon Time Series Analysis", xlab="Years", ylab="Colonies")
lines(OR[,2],OR[,9])
PredOG<-predict(POG,10)

PredOG$pred[1:9] 
NoBanPrediction2016<-PredOG$pred[1]+Forecast2016 
NoBanPrediction2017<-PredOG$pred[2]+Forecast2017
NoBanPrediction2018<-PredOG$pred[3]+Forecast2018
NoBanPrediction2019<-PredOG$pred[4]+Forecast2019
NoBanPrediction2020<-PredOG$pred[5]+Forecast2020
NoBanPrediction2021<-PredOG$pred[6]+Forecast2021
NoBanPrediction2022<-PredOG$pred[7]+Forecast2022
NoBanPrediction2023<-PredOG$pred[8]+Forecast2023
NoBanPrediction2024<-PredOG$pred[9]+Forecast2024
NoBanPrediction2025<-PredOG$pred[10]+Forecast2025


points(2016,NoBanPrediction2016,col=2,pch=8) 
points(2017,NoBanPrediction2017,col=2,pch=8) 
points(2018,NoBanPrediction2018,col=2,pch=8) 
points(2019,NoBanPrediction2019,col=2,pch=8) 
points(2020,NoBanPrediction2020,col=2,pch=8) 
points(2021,NoBanPrediction2021,col=2,pch=8) 
points(2022,NoBanPrediction2022,col=2,pch=8) 
points(2023,NoBanPrediction2023,col=2,pch=8)  
points(2024,NoBanPrediction2024,col=2,pch=8)
points(2025,NoBanPrediction2025,col=2,pch=8)
###############

 

 
 