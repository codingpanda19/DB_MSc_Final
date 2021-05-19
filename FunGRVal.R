FunGR1<-function(x,y,w){

# set strings as factors to false
options(stringsAsFactors = F)

b=bil.new('m')
bil.set.values(b, x, init_date = startDTM[[j]])
bil.pet(b, "latit", 13.35)
y<-bil.get.values(b)

y<-data.frame(DTM=x$DTM,
              P=y$vars$P,
              T=y$vars$T,
              R=y$vars$R,
              PET=y$vars$PET)


return(y)
}


FunGR2<-function(x,l,k,z){

BasinObs<-x

#Run model ------------------------------------
#Preparation of the InputsModel object

w<-ValInputs<- CreateInputsModel(FUN_MOD = RunModel_GR2M,
                           DatesR = x$DTM, 
                           Precip = x$P,
                           PotEvap = x$PET)


##run period selection

begWarm1<-substring(x$DTM[1],1,7)

endWarm1<-substring(x$DTM[12],1,7)

begRun1<-substring(x$DTM[13],1,7)

endRun1<-substring(x$DTM[nrow(x)],1,7)

list(begWarm1,endWarm1,begRun1,endRun1)



ValWarmUp<- seq(which(format(x$DTM, format="%Y-%m")==begWarm1),
             which(format(x$DTM, format="%Y-%m")==endWarm1))


ValRun <- seq(which(format(x$DTM, format="%Y-%m")==begRun1),
           which(format(x$DTM, format="%Y-%m")==endRun1))



## Preparation of RunOptions object

# IniStates<-CreateIniStates(FUN_MOD = RunModel_GR2M, InputsModel = InputsModel, ProdStore = 350,
#                                 RoutStore = 100, ExpStore = NULL)
# str(IniStates)


ValRunOpt <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                           InputsModel = ValInputs, IndPeriod_Run = ValRun, 
                           IndPeriod_WarmUp = ValWarmUp)

#plots

plot(BasinObs$DTM, BasinObs$R,type='l', col='darkblue')



## Simulation

saveRes=list()
for(i in 1:nrow(a)){
ValParam<- c(X1=a$X1[[i]], X2=a$X2[[i]])    

k<-ValOutput<- RunModel_GR2M(InputsModel = ValInputs, RunOptions = ValRunOpt,
                          Param=ValParam)

## results preview
Rplot<-plot(ValOutput, Qobs = BasinObs$R[ValRun])

## efficiency criterion: Nash-Sutcliffe Efficiency
ValInputsCrit <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = ValInputs, 
                                  RunOptions = ValRunOpt, Obs = BasinObs$R[ValRun])
NSE<- ErrorCrit_NSE(InputsCrit = ValInputsCrit, OutputsModel = ValOutput)

NSE

## efficiency criterion: Kling-Gupta Efficiency
ValInputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = ValInputs, 
                                RunOptions = ValRunOpt, Obs = BasinObs$R[ValRun])
KGE<- ErrorCrit_KGE(InputsCrit = ValInputsCrit, OutputsModel = ValOutput)

KGE<-KGE$CritValue
str(KGE)


KGE2<- ErrorCrit_KGE2(InputsCrit = ValInputsCrit, OutputsModel = ValOutput)
KGE2<-KGE2$CritValue


ValInputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_RMSE, InputsModel = ValInputs, 
                                RunOptions = ValRunOpt, Obs = BasinObs$R[ValRun])
RMSE<- ErrorCrit_RMSE(InputsCrit = ValInputsCrit, OutputsModel = ValOutput)

RMSE<-RMSE$CritValue


z<-list(KGE,KGE2,NSE,RMSE)

result<-list(w,k,z)

# print(res)
saveRes[[i]]=result

saveRes = rbind(saveRes)
saveRes

}
return(list(saveRes,result))

}



a<-pk7
input<-K7

startDTM=list()
for(j in 1:length(input)){
startDTM[[j]]<-input[[j]][["DTM"]][1]
print(startDTM)
}

PETestm<-input%>%
lapply(FunGR1)  

input2<-PETestm

resValGR<-input2%>%
lapply(FunGR2)  

saveRDS(resValGR,"OUTPUTS/GR2M/VAL/WAM/valK7GRres.rds")
