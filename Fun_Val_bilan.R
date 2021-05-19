ValFun<-function(x,y,z,m,k,o){
b=bil.new('m', modif = 'critvars')
bil.set.values(b, x,
init_date = startDTM[[j]])
bil.pet(b, "latit", 13.03)
y<-bil.get.values(b)

y<-data.frame(P=y$vars$P,
T=y$vars$T,
R=y$vars$R,
PET=y$vars$PET)

  saveRes=list()
  for(i in 1:nrow(a)){
  bil.set.params.curr(b, list(Spa=a$Spa[i],
                              Dgw=a$Dgw[i],
                              Alf=a$Alf[i],
                              Dgm=a$Dgm[i],
                              Soc=a$Soc[i],
                              Wic=a$Wic[i],
                              Mec=a$Mec[i],
                              Grd=a$Grd[i] ))
   
     #run model
    m<-bil.run(b)
    #Res get val
    res=bil.get.values(b)
   
  
z<-res
#Plotting 
plot( res$vars$DTM,res$vars$R,type='b',
      lwd=2, col='blue')
plot(res$vars$DTM,res$vars$RM,type='b', 
     lwd=1, col='magenta')

bilget<-bil.get.ens.resul(b)
o<-bilget

KGE<-KGE(m$R[13:length(m$R)],
m$RM[13:length(m$RM)])
NSE<-NSE(m$R[13:length(m$R)],
m$RM[13:length(m$RM)])
MAE<-mae(m$R[13:length(m$R)],
m$RM[13:length(m$RM)])
MSE<-mse(m$R[13:length(m$R)],
m$RM[13:length(m$RM)])
RMSE<-rmse(m$R[13:length(m$R)],
m$RM[13:length(m$RM)])

# print(res)

k<-list(KGE,NSE,MAE,MSE,RMSE)

result<-list(x,y,z,m,k,o)

# print(res)
saveRes[[i]]=res

saveRes = rbind(saveRes)
saveRes
    
  }
return(list(saveRes,k))
    
}


#-----------------------------||----------------------

a<-pk7
input<-K7

startDTM=list()
for(j in 1:length(input)){
startDTM[[j]]<-input[[j]][["DTM"]][1]
print(startDTM)
}


saveVal<-input%>%
  lapply(ValFun)  


saveRDS(saveVal,'OUTPUTS/BILAN/VAL/WAM/valK7.rds')
#----------------------------------||-----------------





