#Average 20th century January temperature by states
##Uncomment when use for the first time
##install.packages("maps");
##install.packages("maptools");
##install.packages("sp");
library(maps)
library(maptools)
library(sp)

AL<-read.csv("Alabama.csv",header=T)
NAL<-AL
NAL[,9]<-scale(AL[,9])

AZ<-read.csv("Arizona.csv",header=T)
NAZ<-AZ
NAZ[,9]<-scale(AZ[,9])

AR<-read.csv("Arkansas.csv",header=T)
NAR<-AR
NAR[,9]<-scale(AR[,9])

CA<-read.csv("CA.csv",header=T)
NCA<-CA
NCA[,9]<-scale(CA[,9])

CO<-read.csv("CO.csv",header=T)
NCO<-CO
NCO[,9]<-scale(CO[,9])

CT<-read.csv("CT.csv",header=T)
NCT<-CT
NCT[,9]<-scale(CT[,9])

FL<-read.csv("Florida.csv",header=T)
NFL<-FL
NFL[,9]<-scale(FL[,9])

GA<-read.csv("Georgia.csv",header=T)
NGA<-GA
NGA[,9]<-scale(GA[,9])

ID<-read.csv("Idaho.csv",header=T)
NID<-ID
NID[,9]<-scale(ID[,9])

IL<-read.csv("Illinois.csv",header=T)
NIL<-CO
NIL[,9]<-scale(IL[,9])

IN<-read.csv("Indiana.csv",header=T)
NIN<-IN
NIN[,9]<-scale(IN[,9])

IA<-read.csv("Iowa.csv",header=T)
NIA<-IA
NIA[,9]<-scale(IA[,9])

KS<-read.csv("Kansas.csv",header=T)
NKS<-KS
NKS[,9]<-scale(KS[,9])

KY<-read.csv("Kentucky.csv",header=T)
NKY<-KY
NKY[,9]<-scale(KY[,9])

LA<-read.csv("Louisiana.csv",header=T)
NLA<-LA
NLA[,9]<-scale(LA[,9])

ME<-read.csv("Maine.csv",header=T)
NME<-ME
NME[,9]<-scale(ME[,9])

MD<-read.csv("Maryland.csv",header=T)
NMD<-MD
NMD[,9]<-scale(MD[,9])

MI<-read.csv("Michigan.csv",header=T)
NMI<-MI
NMI[,9]<-scale(MI[,9])

MN<-read.csv("Minnesota.csv",header=T)
NMN<-MN
NMN[,9]<-scale(MN[,9])

MS<-read.csv("Mississippi.csv",header=T)
NMS<-MS
NMS[,9]<-scale(MS[,9])

MO<-read.csv("Missouri.csv",header=T)
NMO<-MO
NMO[,9]<-scale(MO[,9])

MT<-read.csv("Montana.csv",header=T)
NMT<-MT
NMT[,9]<-scale(MT[,9])

NE<-read.csv("Nebraska.csv",header=T)
NNE<-NE
NNE[,9]<-scale(NE[,9])

NJ<-read.csv("NJ.csv",header=T)
NNJ<-NJ
NNJ[,9]<-scale(NJ[,9])

NY<-read.csv("NY.csv",header=T)
NNY<-NY
NNY[,9]<-scale(NY[,9])

NC<-read.csv("North Carolina.csv",header=T)
NNC<-NC
NNC[,9]<-scale(NC[,9])

ND<-read.csv("North Dakota.csv",header=T)
NND<-ND
NND[,9]<-scale(ND[,9])

OH<-read.csv("Ohio.csv",header=T)
NOH<-OH
NOH[,9]<-scale(OH[,9])

OR<-read.csv("Oregon.csv",header=T)
NOR<-OR
NOR[,9]<-scale(OR[,9])

PA<-read.csv("PA.csv",header=T)
NPA<-PA
NPA[,9]<-scale(PA[,9])

SD<-read.csv("South Dakota.csv",header=T)
NSD<-SD
NSD[,9]<-scale(SD[,9])

TN<-read.csv("Tennessee.csv",header=T)
NTN<-TN
NTN[,9]<-scale(TN[,9])

TX<-read.csv("Texas.csv",header=T)
NTX<-TX
NTX[,9]<-scale(TX[,9])

UT<-read.csv("Utah.csv",header=T)
NUT<-UT
NUT[,9]<-scale(UT[,9])

VT<-read.csv("Vermont.csv",header=T)
NVT<-VT
NVT[,9]<-scale(VT[,9])

VA<-read.csv("Virginia.csv",header=T)
NVA<-VA
NVA[,9]<-scale(VA[,9])

WA<-read.csv("Washington.csv",header=T)
NWA<-WA
NWA[,9]<-scale(WA[,9])

WV<-read.csv("West Virginia.csv",header=T)
NWV<-WV
NWV[,9]<-scale(WV[,9])

WI<-read.csv("Wisconsin.csv",header=T)
NWI<-WI
NWI[,9]<-scale(MI[,9])

WY<-read.csv("Wyoming.csv",header=T)
NWY<-WY
NWY[,9]<-scale(WY[,9])

###




#DE<-read.csv("Delaware.csv",header=T) 
#MA<-read.csv("Massachusetts.csv",header=T)
#NV<-read.csv("Nevada.csv",header=T)
#NH<-read.csv("New Hampshire.csv",header=T)
#NM<-read.csv("New Mexico.csv",header=T)
#OK<-read.csv("Oklahoma.csv",header=T)
#RI<-read.csv("RI.csv",header=T)
#SC<-read.csv("South Carolina.csv",header=T)
###### Finish all state
 
Temp<-0
stb<-function(Year){
	standardizedbee<-c(NAL[NAL[,2]==Year,9][1],NAZ[NAZ[,2]==Year,9][1],NAR[NAR[,2]==Year,9][1],    NCA[CA[,2]==Year,9][1],NCO[NCO[,2]==Year,9][1],NCT[NCT[,2]==Year,9][1],Temp,  NFL[NFL[,2]==Year,9][1],NGA[NGA[,2]==Year,9][1],NID[NID[,2]==Year,9][1],  NIL[NIL[,2]==Year,9][1],NIN[NIN[,2]==Year,9][1],NIA[NIA[,2]==Year,9][1], NKS[NKS[,2]==Year,9][1],NKY[NKY[,2]==Year,9][1],NLA[NLA[,2]==Year,9][1],  NME[NME[,2]==Year,9][1],NMD[NMD[,2]==Year,9][1],Temp,NMI[NMI[,2]==Year,9][1],  NMN[NMN[,2]==Year,9][1], NMS[NMS[,2]==Year,9][1],NMO[NMO[,2]==Year,9][1], NMT[NMT[,2]==Year,9][1],NNE[NNE[,2]==Year,9][1],Temp,  
Temp,NNJ[NNJ[,2]==Year,9][1],Temp, 
NNY[NNY[,2]==Year,9][1],NNC[NNC[,2]==Year,9][1], 
NND[NND[,2]==Year,9][1],NOH[NOH[,2]==Year,9][1], Temp, NOR[NOR[,2]==Year,9][1], NPA[NPA[,2]==Year,9][1],Temp,Temp, NSD[NSD[,2]==Year,9][1], NTN[NTN[,2]==Year,9][1],NTX[NTX[,2]==Year,9][1], NUT[NUT[,2]==Year,9][1], NVT[NVT[,2]==Year,9][1],NVA[NVA[,2]==Year,9][1], NWA[NWA[,2]==Year,9][1], NWV[NWV[,2]==Year,9][1][1],NWI[NWI[,2]==Year,9][1], NWY[NWY[,2]==Year,9][1])
return(standardizedbee)
}

Year<-2023



 
change<-stb(2024)-stb(1990)  

txt <- "AL  AZ AR    CA  CO  CT  DE  FL  GA  ID  IL  IN  IA KS KY LA ME MD MA MI MN MS MO MT NE NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WI WY
45.1 40.7 39.3 42.3 23.7 25.4 33.3 57.8 46.1 22.0 25.3 26.5 17.7 28.6 33.6 49.4 13.2 32.5 24.1 18.2 6.5 45.2 29.4 16.7 22.1 30.0 17.1 29.6 33.7 19.9 40.0 6.4 26.9 36.5 30.2 25.3 27.2 44.6 15.0 37.2 45.4 25.3 15.3 34.8 28.4 30.6 12.4 18.4"
dat <- stack(read.table(text = txt,  header = TRUE))
names(dat)[2] <-'state.abb'
dat$states <- tolower(state.name[match(dat$state.abb,  state.abb)])
##Then load the map and convert to a SpatialPolygons
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
##And now you add the information from your data: 
idx <- match(unique(nms),  dat$states)
dat$values<-change
dat2 <- data.frame(value = dat$value[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
##Finally you plot it:
spplot(USAsp['value'],col.regions = heat.colors(20))



 