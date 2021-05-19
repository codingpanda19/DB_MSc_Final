#**********Other Calibration Methods******************************************************

# Definition of the necessary function
# Parameter estimation can be performed by defining a function that takes a parameter set
#as input and returns the value of the performance criterion. There are two important steps:
#the transformation of parameters to real space and the computation of the value of the 
#performance criterion. Here we choose to minimize the root mean square error.

# The change of the repository from the “real” parameter space to a “transformed” space 
#ensures homogeneity of displacement in the different dimensions of the parameter space 
#during the step-by-step procedure of the calibration algorithm of the model.


Ind_Run <- seq(which(format(BasinObs$DTM, format="%Y-%m")=="1970-05"),
               which(format(BasinObs$DTM, format="%Y-%m")=="1975-03"))


## Preparation of RunOptions object

RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
)




OptimGR2M <- function(ParamDoudou) {
  ## Transformation of the parameter set to real space
  RawParamDoudou<- airGR::TransfoParam_GR2M(ParamIn = ParamDoudou,
                                            Direction = "TR")
  ## Simulation given a parameter set
  OutputsModelDoudou <- airGR::RunModel_GR2M(InputsModel = InputsModel,
                                       RunOptions = RunOptions,
                                       Param = RawParamDoudou)
  ## Computation of the value of the performance criteria
  OutputsCrit <- airGR::ErrorCrit_NSE(InputsCrit = InputsCrit,
                                      OutputsModel = OutputsModel,
                                      verbose = FALSE)
  return(OutputsCrit$CritValue)
}


# In addition, we need to define the lower and upper bounds of the four GR2M parameters 
# in the transformed parameter space:

lowerGR2M <- rep(-9.99, times = 2)
upperGR2M <- rep(+9.99, times = 2)

#lowerGR2M <- c(X1=50, X2=0.8)
#upperGR2M <- c(X1=60, X2=1.0)

# Local optimization
# We start with a local optimization strategy by using the PORT routines (using the nlminb() 
# of the stats package) 
# and by setting a starting point in the transformed parameter space:


startGR2M <- c(7, 0.4)
optPORT <- stats::nlminb(start = startGR2M, 
                         objective = OptimGR2M,
                         lower = lowerGR2M, upper = upperGR2M,
                         control = list(trace = 1))


# The RMSE value reaches a local minimum value after xx iterations.
# We can also try a multi-start approach to test the consistency of the local optimization.
# Here we use the same grid used for the filtering step of the Michel’s calibration strategy 
# (Calibration_Michel() function).
# For each starting point, a local optimization is performed.


startGR2M <- expand.grid(data.frame(CalibOptions$StartParamDistrib))
optPORT_ <- function(x) {
  opt <- stats::nlminb(start = x, 
                       objective = OptimGR2M,
                       lower = lowerGR2M, upper = upperGR2M,
                       control = list(trace = 1))
}
listOptPORT <- apply(startGR2M, MARGIN = 1, FUN = optPORT_)

#We can then extract the best parameter sets and the value of the performance criteria:

parPORT <- t(sapply(listOptPORT, function(x) x$par))
objPORT <- sapply(listOptPORT, function(x) x$objective)
resPORT <- data.frame(parPORT, NSE = objPORT)

#As can be seen below, the optimum performance criterion values (column objective) can 
#differ from the global optimum value in many cases, resulting in various parameter sets.

summary(resPORT)


# Global optimization
# Global optimization is most often used when facing a complex response surface, 
# with multiple local mimina. Here we use the following R implementation of some popular 
# strategies:
#   
# DEoptim: differential evolution
# hydroPSO: particle swarm
# Rmalschains: memetic algorithms


#Differential Evolution
optDE <- DEoptim::DEoptim(fn = OptimGR2M,
                          lower = lowerGR2M, upper = upperGR2M,
                          control = DEoptim::DEoptim.control(NP = 40, trace = 10))


#Particle Swarm
optPSO <- hydroPSO::hydroPSO(fn = OptimGR2M,
                             lower = lowerGR2M, upper = upperGR2M,
                             control = list(write2disk = FALSE, verbose = FALSE))


#MA-LS-Chains
optMALS <- Rmalschains::malschains(fn = OptimGR2M,
                                   lower = lowerGR2M, upper = upperGR2M, 
                                   maxEvals = 2000)



#**********Fin Other Calibration Methods******************************************************





#**********************************Validation**********************************************

##run period selection

Ind_Run <- seq(which(format(BasinObs$DTM, format="%Y-%m")=="1990-05"),
               which(format(BasinObs$DTM, format="%Y-%m")=="2000-03"))

## Preparation of RunOptions object

RunOptions <- CreateRunOptions(FUN_MOD = RunModel_GR2M,
                               InputsModel = InputsModel, IndPeriod_Run = Ind_Run,
)

## Simulation

# 46.618,    0.838

Param<- c(X1=84.775, X2=0.985)    
OutputsModel<- RunModel_GR2M(InputsModel = InputsModel, RunOptions = RunOptions, Param=Param
)

##result view
plot(OutputsModel,Qobs=BasinObs$R[Ind_Run])


##efficiency criterion : Nash-Sutcliffe Efficiency

InputsCrit <-CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE, InputsModel = InputsModel, 
                              RunOptions = RunOptions, Obs=BasinObs$R[Ind_Run])

OutputsModelCrit<- ErrorCrit_NSE(InputsCrit=InputsCrit, OutputsModel=OutputsModel)

#*****************************************************************************************

#criteria..why...goal
#models
#budyko curve
#if model overestimating/...understk
#decribe catchements//
#gaps....
#
