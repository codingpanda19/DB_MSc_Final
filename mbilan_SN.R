library(rgdal) 
library(rgeos)
library(data.table)
library(raster)
library(bilan)
library(hydroGOF)
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(airGR)
library(DEoptim)

#----------------------------Catchments----------------------------------------------------------

#My Working directory
setwd('/Users/Doudou/Downloads/MB_data')

# set strings as factors to false
options(stringsAsFactors = F)

curP<-getwd()

# nacti data z myPath
fls <- list.files(paste0(curP,"/RDS_SN"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/RDS_SN"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}


#-------------------------------BasinObs------------------------------------------------------


curP<-getwd()

# nacti data z myPath
fls <- list.files(paste0(curP,"/BasinObs"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/BasinObs"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}

WAV1<-WAV[1:36]
WAV2<-WAV[41:147]

#min
#a<-apply(GMB, margin, ...)
#---------------------GIS_CATCHEMENTS_RIVERS_RAINGUAGES----------------------------------------------------------

GR<-readOGR(paste('GMB_wat/GMB_water_areas_dcw.shp'))
GR<-spTransform(GR, CRSobj="+init=EPSG:3857")

DB<-readOGR(paste('CATCH_GAMBIA_RIVER/6CATCH_GAMBIA_RIVER.shp'))
DB<- spTransform(DB, CRSobj ="+init=EPSG:3857")

CN<-readOGR(paste('CATCH_GAMBIA_RIVER/6CHANNELS_GAMBIA_RIVER.shp'))
CN<- spTransform(CN, CRSobj ="+init=EPSG:3857")

OUTLETS<-readOGR(paste('CATCH_GAMBIA_RIVER/6OUTLETS_GAMBIA_RIVER.shp'))
OUTLETS<- spTransform(OUTLETS, CRSobj ="+init=EPSG:3857")

RGST<-readOGR(paste('CATCH_GAMBIA_RIVER/7PT_RGST_GAMBIA_RIVER.shp'))
RGST<- spTransform(RGST, CRSobj ="+init=EPSG:3857")

#---------------------------------------OSM VIEW-------------------------------------
library(leaflet)
library(magrittr) 

SNmap<-leaflet() %>%
  addTiles() %>%
  setView(-13,14, zoom=5)%>%
  addMarkers(-13.73, 13.47, popup="SF by Area")
SNmap

#----------------------------------------------------------------------------------

plot(DB, lwd=.7)
plot(GR, lwd=.7, col='blue', add=TRUE)
plot(CN, col='grey',lwd=0.2, add=TRUE)
plot(OUTLETS, col='blue', pch=19,lwd=0.8, add=TRUE)
plot(RGST, col='darkorchid4', pch=19, lwd=0.8, add=TRUE)

#------------------------MY QUANTILES -----------------------------------------------
library(caTools)
# estimate lenth of times Series
# influence of calibration  period 
#9 x 3 years period 28 years
#9 values sum of precipitations
#highest and lowest picks 
#is there any impact of the content of my data of description of HM which im working ?
#Why calibrate? Describing HM = describe Hydrology
#Whats the HM capable to represent my data?
# Simulations are very closed to reality?
#aggregate on annual values (Hydrological Years)
#average flows and temperatures
#then the quantiles

#-----------------------------Hydrological Year----------------------------------------

library(lubridate)
library(data.table)

#------------------
install.packages(Hmisc)
library(Hmisc)
BObs<-GMB
str(BObs)
dtf<-describe(BObs)
inf<-info(BObs)
#-------------------------------------------------
runoff<-BObs$R
runoff

prec<-BObs$P
prec

temp<-BObs$T
temp

#------------------------

dt=seq(from=as.Date("1971/01/16"),to=as.Date("1999/12/16"),by="month")
dt

year(dt[1])
ndt=length(dt)                   
yearHM=c()

for(i in 1:ndt){
  if(month(dt[i])<4) yearHM[i] =year(dt[i])-1  
  else yearHM[i] =year(dt[i])
}


year(dt)
#-----------------------------------------

sum<-data.table(sum(runoff),sum(prec),sum(temp))
sum
Dt=data.table(DTM=yearHM,R=runoff)

a<-Dt[,.(sum(R)),by=.(DTM)]
a<-a[2:30,]
a<-data.table(DTM=a$DTM, R=a$V1)
a
#-----------------------------------


library(caTools)
r=c(a$R)
r
mn<-data.table(r)
mn



#-0-----------------------test loop run mean---
k=25; 
k2 = k
k1 = k-k2-1
a = runmean(r, k)
b = array(0,n)

for(j in 1:n) {
  lo = max(1, j-k1)
  hi = min(n, j+k2)
  b[j] = mean(r[lo:hi], na.rm = TRUE)
}

a

b

k2
#----

dta<-runmean(r, k=2)
dta<-data.table(dta)
c<-data.table(DTM=a$DTM, R=dta$dta)
#c<-c[1:13]
c
q20<-quantile(x=c$R, probs=0.2)
q20

Dryyear<-c[c$R<q20]

Dryyear<-data.table(Dryyear)
Dryyear
save<-saveRDS(Dryyear,'M_AV/GMB/R/CAL/bil_save/K2/saveDY.rds')

#k2 75-89 [49:228]

#[37:252]
#View(BObs)
SDY<-data.table(DTM=BObs$DTM[61:300], P=BObs$P[61:300],
                T=BObs$T[61:300], R=BObs$R[61:300])
SDY
save<-saveRDS(SDY,'M_AV/GMB/R/CAL/bil_save/K2/SDY.rds')

#View(SDY)

DY1<-SDY[1:36]  # 0.75
DY2<-SDY[13:48] # 0.85
DY3<-SDY[73:108] # 0.82
DY4<-SDY[85:120] # 0.68
DY5<-SDY[97:132]
DY6<-SDY[157:192] # 0.62
DY7<-SDY[169:204] # 0.55
DY8<-SDY[181:216] # 0.55
DY9<-SDY[193:228]
DY10<-SDY[205:240]



q80<-quantile(x=c$R, probs=0.8)
q80
WetYear<-c[c$R>q80]
WetYear
save<-saveRDS(WetYear,'M_AV/GMB/R/CAL/bil_save/K2/saveWY.rds')

SWY<-data.table(DTM=BObs$DTM[13:348], P=BObs$P[13:348],
                T=BObs$T[13:348], R=BObs$R[13:348])
SWY
save<-saveRDS(SWY,'M_AV/GMB/R/CAL/bil_save/K2/saveSWY.rds')

#K2 [253:348]

WY1<-SWY[1:36] # 0.75
WY2<-SWY[13:48] # 0.85
WY3<-SWY[25:60] # 0.82
WY4<-SWY[253:288] # 0.82
WY5<-SWY[265:300]
WY6<-SWY[278:312]
WY7<-SWY[290:324]
WY8<-SWY[302:336]

#plotting position formula to estimate the quatiles

#------------------------MODEL CALIBATION on Dry year---------------------------------------------------------

calibrage = bil.new("m",modif='critvars')
begDTM=range(DY1$DTM)[1]
bil.set.values(calibrage, DY1, init_date = begDTM)
bil.pet(calibrage, "latit", 13.48)
table<-bil.get.values(calibrage)
dt<-as.data.table(table)
BasinObs<-cbind(DY1,PET=dt$vars.PET)
BasinObs<-as.data.table(BasinObs)
Obs<-plot(table$vars$R,type='b', lwd=1, col='limegreen')

library(expss)
begDTM
begDTM<-substring(begDTM,1,4)
endDTM<-last(DY1$DTM)
endDTM
endDTM<-substring(endDTM,1,4)
endDTM<-substring(endDTM,3)
endDTM

label<-paste(begDTM,endDTM, sep="-")
label
#-----------------------------------------------------------------------------

#-----------------------------------WARM UP----------------------------------------------

warmupCal<-1:12
KGEwarmUpCal<- function(sim){
  obst<-BasinObs$R[-warmupCal]
  simt<-sim[-warmupCal]
  -1*hydroGOF::KGE(sim = simt, obs = obst)
}

#---------------------PARAMETER SET-------------------------------------------------------------

bil.set.params.lower(calibrage, list(Spa =100,Grd= 0.001, Alf=0.00001))
bil.set.params.upper(calibrage, list(Spa = 2000, Grd= 1, Alf=0.003))
bil.set.critvars(model = calibrage,
                 weights = c(1),
                 obs_vars = c('R'),
                 mod_vars=c("RM"),
                 obs_values=c(-1),
                 crits=c('custom'),
                 funs=c(KGEwarmUpCal))

bil.set.optim(calibrage, method = "DE", crit = "NS", DE_type = "best_one_bin", n_comp = 4,
              comp_size = 10, cross = 0.95, mutat_f = 0.95, mutat_k = 0.85, maxn_shuffles = 30,
              n_gen_comp = 15, ens_count = 30, seed = 446, weight_BF = 0, init_GS = 5)


#--------------------------OPTIMIZATION----------------------------------------------------------

model<-bil.optimize(calibrage)
res = bil.get.values(calibrage)
resEns<-bil.get.ens.resul(calibrage)
resEns
res<-as.data.table(res)
res
unique(res$crit)
params<-bil.get.values(calibrage)$params
params
bil.get.values(calibrage)$params

#NSE(obs=model$R,sim=model$RM)
NSE<-NSE(obs=model$R[13:36],sim=model$RM[13:36])
KGE<-KGE(obs=model$R[13:36],sim=model$RM[13:36])
MAE<-mae(obs=model$R[13:36],sim=model$RM[13:36])
MSE<-mse(obs=model$R[13:36],sim=model$RM[13:36])
RMSE<-rmse(obs=model$R[13:36],sim=model$RM[13:36])

OBJ<-data.table(label,NSE,KGE,MAE,MSE,RMSE)
OBJ
OBJ<-saveRDS(OBJ,'M_AV/GMB/R/CAL/Obj_Fun/K2/D1.rds')
bil.write.file(calibrage, 'M_AV/GMB/R/CAL/bil_save/K2/D1.txt')

curP<-getwd()
# nacti data z myPath
fls <- list.files(paste0(curP,"/M_AV/GMB/CAL/Obj_Fun/K2"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/M_AV/GMB/CAL/Obj_Fun/K2"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}

boxplot(OBJ$label,NSE)


#{
#With KGE as objective fun we obtain better results
#0.737655
#0.8696783
#2.960617}


#----------------------------Graphs-------------------------------------------------------

ggplot(data = model[13:36]) +
  geom_line(aes(x = DTM, y = R, colour = factor(1))) +
  geom_line(aes(x = DTM, y = RM, colour = factor(2))) +
  geom_line(aes(x = DTM, y = BF, colour = factor(3))) +
  scale_color_manual(values = c('darkblue', 'red', 'yellow'),
                     labels = c('R', 'RM', 'BF'),
                     name = '') +
  theme_bw()


#plot
plot(x=model$DTM, y=res$vars.R,type='l', lwd=1.5, col='darkblue')
lines(x=model$DTM, y=res$vars.RM,type='l',lwd=1.5, col='brown')
lines(x=model$DTM, y=res$vars.BF, type='l', lwd=1.5, col='limegreen')

#----------------------Validation on Wet Year---------------------------------------

Val = bil.new("m", modif='critvars')
begDTM=range(DY4$DTM)[1]
bil.set.values(Val,DY4, init_date = begDTM)
bil.pet(Val, "latit", 13.48)
table<-bil.get.values(Val)
dt<-as.data.table(table)
BasinObs<-cbind(DY4,PET=dt$vars.PET)
BasinObs<-as.data.table(BasinObs)
Obs<-plot(table$vars$R,type='l', lwd=1, col='darkblue')

#-----------------------------------WARM UP VAL----------------------------------------------

warmupVal<-1:12
KGEwarmUpVal<- function(sim){
  obst<-BasinObs$R[-warmupVal]
  simt<-sim[-warmupVal]
  -1*hydroGOF::KGE(sim = simt, obs = obst)
}
#---------------------------------------------------------------------------------------

bil.set.critvars(model = Val,
                 weights = c(1),
                 obs_vars = c('R'),
                 mod_vars=c("RM"),
                 obs_values=c(-1),
                 crits=c('custom'),
                 funs=c(KGEwarmUpVal))

#----------------------------Validation-----------------------------------------
#K3 only validated DY to WY
#bil.read.params.file(Val, "Bilan_Model/Calibration/Gouloumbou/G_DY2Cal.txt")

res_Val<-bil.run(Val,file="M_AV/GMB/CAL/bil_save/K6/WY2.txt")
resval = bil.get.values(Val)
resval<-as.data.table(resval)
resval
#becareful not NSE prob default
unique(resval$crit)

#NSE(obs=model$R,sim=model$RM)
NSE<-NSE(obs=model$R[13:84],sim=model$RM[13:84])
KGE<-KGE(obs=model$R[13:84],sim=model$RM[13:84])
MAE<-mae(obs=model$R[13:84],sim=model$RM[13:84])
MSE<-mse(obs=model$R[13:84],sim=model$RM[13:84])
RMSE<-rmse(obs=model$R[13:84],sim=model$RM[13:84])

OBJ<-data.table(NSE,KGE,MAE,MSE,RMSE)
OBJ<-saveRDS(OBJ,'M_AV/GMB/VAL/Obj_Fun/K6/D4W2.rds')
bil.write.file(Val, "M_AV/GMB/VAL/bil_save/K6/D4W2.txt")

plot(resval$vars.R,type='l', lwd=2, col='darkblue')
lines(resval$vars.RM,lwd=2, col='red')
bil.get.values(Val)$params

boxplot(D8W5)
