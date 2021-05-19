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

Inputs<- CreateInputsModel(FUN_MOD = RunModel_GR2M,
                           DatesR = x$DTM, 
                           Precip = x$P,
                           PotEvap = x$PET)


##run period selection

begWarm1<-substring(x$DTM[1],1,7)

endWarm1<-substring(x$DTM[12],1,7)

begRun1<-substring(x$DTM[13],1,7)

endRun1<-substring(x$DTM[nrow(x)],1,7)

list(begWarm1,endWarm1,begRun1,endRun1)

WarmUp<- seq(which(format(x$DTM, format="%Y-%m")==begWarm1),
             which(format(x$DTM, format="%Y-%m")==endWarm1))

Run <- seq(which(format(x$DTM, format="%Y-%m")==begRun1),
           which(format(x$DTM, format="%Y-%m")==endRun1))




## Preparation of RunOptions object

# IniStates<-CreateIniStates(FUN_MOD = RunModel_GR2M, InputsModel = InputsModel, ProdStore = 350,
#                                 RoutStore = 100, ExpStore = NULL)
# str(IniStates)

RunOpt <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                           InputsModel = Inputs, IndPeriod_Run = Run, 
                           IndPeriod_WarmUp = WarmUp)

#plots

plot(BasinObs$DTM, BasinObs$R,type='l', col='darkblue')



## Simulation

Param<- c(X1=4064.929, X2=.7)    

Outputs_M<- RunModel_GR2M(InputsModel = Inputs, RunOptions = RunOpt,
                          Param=Param)

##result view
plot(Outputs_M,Qobs=BasinObs$R[Run])


##efficiency criterion : Nash-Sutcliffe Efficiency

InputsCrit <-CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = Inputs, 
                              RunOptions = RunOpt, Obs=BasinObs$R[Run])





#Calibrage----------------------------

w<-CalInputs<- CreateInputsModel(FUN_MOD = RunModel_GR2M,
                                 DatesR = x$DTM, 
                                 Precip = x$P,
                                 PotEvap = x$PET)



CalWarmUp<- seq(which(format(x$DTM, format="%Y-%m")==begWarm1),
             which(format(x$DTM, format="%Y-%m")==endWarm1))

CalRun <- seq(which(format(x$DTM, format="%Y-%m")==begRun1),
           which(format(x$DTM, format="%Y-%m")==endRun1))

CalRunOpt <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                              InputsModel = CalInputs, IndPeriod_Run = CalRun, 
                              IndPeriod_WarmUp = CalWarmUp)


## preparation of CalibOptions object
CalOpt <- CreateCalibOptions(FUN_MOD = RunModel_GR2M, FUN_CALIB = Calibration_Michel)

## calibration
CalOutput <- Calibration_Michel(InputsModel = CalInputs, RunOptions = CalRunOpt, 
                                InputsCrit = InputsCrit, CalibOptions = CalOpt, 
                                FUN_MOD = RunModel_GR2M)
## simulation
p<-CalParam <- CalOutput$ParamFinalR

k<-CalOutput <-  RunModel_GR2M(InputsModel = CalInputs,
                            RunOptions = CalRunOpt, Param = CalParam)
CalOutput

## results preview
Rplot<-plot(CalOutput, Qobs = BasinObs$R[CalRun])

## efficiency criterion: Nash-Sutcliffe Efficiency
InputsCritCal <- CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = Inputs, 
                                  RunOptions = CalRunOpt, Obs = BasinObs$R[CalRun])
NSE<- ErrorCrit_NSE(InputsCrit = InputsCrit, OutputsModel = CalOutput)

NSE<-NSE$CritValue
NSE

## efficiency criterion: Kling-Gupta Efficiency
InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_KGE, InputsModel = Inputs, 
                                RunOptions = CalRunOpt, Obs = BasinObs$R[CalRun])
KGE<- ErrorCrit_KGE(InputsCrit = InputsCrit, OutputsModel = CalOutput)

KGE<-KGE$CritValue
str(KGE)


KGE2<- ErrorCrit_KGE2(InputsCrit = InputsCrit, OutputsModel = CalOutput)
KGE2<-KGE2$CritValue


InputsCrit  <- CreateInputsCrit(FUN_CRIT = ErrorCrit_RMSE, InputsModel = Inputs, 
                                RunOptions = CalRunOpt, Obs = BasinObs$R[CalRun])
RMSE<- ErrorCrit_RMSE(InputsCrit = InputsCrit, OutputsModel = CalOutput)

RMSE<-RMSE$CritValue

z<-list(KGE,KGE2,NSE,RMSE)

result<-list(w,k,z,p)
return(result)
}

input<-RW5
  
startDTM=list()
for(j in 1:length(input)){
startDTM[[j]]<-input[[j]][["DTM"]][1]
print(startDTM)
}

PETestm<-input%>%
  lapply(FunGR1)  

input2<-PETestm

resCalGR<-input2%>%
lapply(FunGR2)  

saveRDS(resCalGR,"OUTPUTS/GR2M/CAL/WAM/RW5GRres.rds")
