
#=================FIRST STEP FILTER BY BASIN ==========================
B1<-allMyPeriods%>% group_by(ENS) %>% 
  filter(ENS=='GMB')
#======================================================================

#===================SECOND STEP FILTER BY K============================

pk7<-gmb_params%>% group_by(K) %>% 
                    filter(K==7)

K7<-B1%>% group_by(K) %>% 
  filter(K==7)

df<-rbindlist(paramTH6)
bilTH6param<-data.table(drought,df)

bil_wav_params<-rbind.data.frame(bilPD6param,bilTH6param,bilRW6param)

saveRDS(bil_wav_params,"INPUTS/BILAN/VAL/wav_params.rds")


paramsk2<-rbindlist(paramsk2)


morello<-TH6res

paramTH6=list()
for (i in 1:length(morello))
{
  paramTH6[[i]]<-pluck(morello,i,3)
  
}




K2=list()
for(i in 1:nrow(drought)){
  K2[[i]]<-WAM[DTM >= drought$begDTM[[i]] & 
  DTM <= drought$endDTM[[i]],]
}


paramsk2<-sim_params%>%group_by(K)%>%
  filter(K==7)%>%ungroup()




GMBa<-data.table(GMBa)

inputa<-sim_params%>% group_by(K) %>% 
  filter(K==7)


inputa=c()
for(i in 1:nrow(PD)){
  inputa[[i]]<-GMBa[DTM >= PD$begDTM[[i]]
                    & DTM <= PD$endDTM[[i]],]
  
}


print(K7)

saveRDS(K7,"INPUTS/VAL/SIM/K7.rds")

}

#===================THIRD STEP FILTER BY DROUGHT TYPE=================

PD=list()
for (i in 1:length(B1K)){
  PD[[i]]<-filter(B1K[[i]],Num_Drought==1)
  
  print(PD)
}
#=====================================================================
#====================STEP FOR SELECT THE PERIODS============


p=list()
for(j in 1:length(df)){
  p[[j]]<-flatten(df[[j]])
  print(p)
}


#=================FIRST STEP FILTER BY BASIN ==========================
B1<-allMyPeriods%>% group_by(ENS) %>% 
  filter(ENS=='WAM')

B1K<-B1%>% group_by(ENS) %>% 
  filter(Num_Drought==1)


morello<-TL5res

paramTL1=list()
for (i in 1:length(morello))
{
  paramTL1[[i]]<-pluck(morello,i,3)
  
}

df<-paramPD1
df<-data.table(paramPD1)
ff<-(df)
PDm<-data.table(KGE)


wam_params<-rbind.data.frame(PD5params,PW5params,TL5params,
                             TH5params,RD5params,RW5params)
saveRDS(wam_params,"INPUTS/GR2M/VAL/wam_params.rds")


X1=list()
for (i in 1:length(paramPD1))
{
  X1[[i]]<-paramPD1[[i]][[1]]
  
  print(X1)
}


X2=list()
for (i in 1:length(paramPD1))
{
  X2[[i]]<-paramPD1[[i]][[2]]
  
  print(X2)
}


X1<-data.table(X1)
X2<-data.table(X2)

X1X2<-cbind(X1,X2)


TH6params<-data.table(B1K,X1X2)


