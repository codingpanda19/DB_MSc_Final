CalFun<-function(x,y,z,m,k) {
b=bil.new('m', modif = 'critvars')
bil.set.values(b, x,
init_date = startDTM[[j]])
bil.pet(b, "latit", 13.35)
y<-bil.get.values(b)
y<-data.frame(P=y$vars$P,
              T=y$vars$T,
              R=y$vars$R,
              PET=y$vars$PET)


#Setting warump period
warmup<-1:12
KGEwarmUp<- function(sim){
obst<-y$R[-warmup]
simt<-sim[-warmup]
-1*hydroGOF::KGE(sim = simt, obs = obst)}

#Setting model parameters

bil.set.params.lower(b, list(Spa =100,Grd= 0.001, 
                             Alf=0.00001))
bil.set.params.upper(b, list(Spa = 2000, Grd= 1,
                             Alf=0.003))

bil.set.optim(b, method = "DE", crit = "NS", DE_type = "best_one_bin",
              n_comp = 4,comp_size = 10, cross = 0.95, mutat_f = 0.95, 
              mutat_k = 0.85, maxn_shuffles = 30,
              n_gen_comp = 15, ens_count = 1, seed = 446, 
              weight_BF = 0, init_GS = 5)

#Setting modif critvars
bil.set.critvars(model = b,
                 weights = c(1),
                 obs_vars = c('R'),
                 mod_vars=c("RM"),
                 obs_values=c(-1),
                 crits=c('custom'),
                 funs=c(KGEwarmUp))

#Optimizing the model
m<-model<-bil.optimize(b)  

#Res get val
res=bil.get.values(b)

#Plotting 
plot( res$vars$DTM,res$vars$R,type='b', lwd=2, col='blue')
plot(res$vars$DTM,res$vars$RM,type='b', lwd=1, col='magenta')

bilget<-bil.get.ens.resul(b)
z<-bilget


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

k<-list(KGE,NSE,MAE,MSE,RMSE)

result<-list(x,y,z,m,k)

return(result)
}


input<-RW5

startDTM=list()
for(j in 1:length(input)){
  startDTM[[j]]<-input[[j]][["DTM"]][1]
  print(startDTM)
}


resCal<-input%>%
lapply(CalFun)  

saveRDS(resCal,'OUTPUTS/BILAN/CAL/WAM/RW5res.rds')

m<-TH1res[[1]][[4]]
m

for(i in 13:36)
{
a<-((m$R[i] - m$RM[i])^2) 
}

for(i in 13:36)
{
b<- ((m$R[i]) - (mean(m$R[i]))^2)
}

NS<-1-(sum1/sum2)
NS


1-(sum((m$R[13:36] - m$RM[13:36])^2)/sum((m$R[13:36]) - (mean(m$R[13:36]))^2))

NSEQ<-(1-(s1/s2))
NSEQ

NSE<-NSE(m$R[13:length(m$R)],
         m$RM[13:length(m$RM)])
NSE
