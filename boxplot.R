#----------Loading Packages---------------------------------------

load_pkg <- rlang::quos(rgdal,tidyverse,rgeos,data.table,raster,bilan,hydroGOF,
                        readxl,ggplot2,dplyr,airGR,DEoptim,lubridate, readxl,Hmisc,
                        sqldf,caTools,tidyverse,hrbrthemes,viridis)
                        

invisible(lapply(lapply(load_pkg, rlang::quo_name),
                 library,
                 character.only = TRUE ))

#----------------------Importing Catchments data----------------------------------------------------------

#Setting Working directory
setwd('/Users/Doudou/Downloads/MB_data')

# set strings as factors to false
options(stringsAsFactors = F)


curP<-getwd()
fls <- list.files(paste0(curP,"/OUTPUTS/FACET_WRAP/"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/OUTPUTS/FACET_WRAP/"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')
for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}


allcalbil<-rbind(gmbcalbil,kdgcalbil,mkocalbil,simcalbil,wamcalbil,wavcalbil)

GRparams<-rbind(allGRparams)


WriteXLS::WriteXLS(allvalGr,"OUTPUTS/FACET_WRAP/grcrits/allvalGr.xlsx")


saveRDS(allcalbil,"OUTPUTS/FACET_WRAP/grcrits/allcalbil.rds")

allvalGr<-allvalGr%>%
                  summarise(Period=Period, K=K, Drought=Drought,KGE1=KGE,KGE2=NSE, NSE=MAE, RMSE=MSE)

rb<-rbindlist(allGRparams)
df<-rbindlist(rb)
saveRDS(bilcritscal,"OUTPUTS/BOXPLOT/CRITS/bilcritscal.rds")

bilcritscal<-list(gmbcalbp,kdgcalbp,mkocalbp,simcalbp,wamcalbp,wavcalbp)

allGRparams<-list(gmb_params,kdg_params,mko_params,sim_params)


#----------Val crits------------------------- 
neworder<-c("PD","PW","RD","RW","TL","TH")

MRCgr<-arrange(transform(MRCgr,
                         Drought=factor(Drought, levels = neworder)),Drought)

hays<-allcalbil%>%group_by(K)%>%
                  summarise(Period=Period, Drought=Drought, KGE=KGE)
hays2<-allcalbil%>%group_by(K)%>%
                summarise(Period=Period, Drought=Drought,KGE=KGE)


grparams<-rbind(gmb_params,kdg_params,mko_params,sim_params)
GRparams<-rbind(allGRparams)

grparams<-grparams[,-c(4,5,6)]

bilparms<-rbind(bilparams,bilparams1)
          

a<-data.frame(Drought=rep("Dry", each=246))
a1<-data.frame(Drought=rep("Wet", each=178))
aa<-rbind(a,a1)
b<-data.frame(Drought=aa,Period=allcalGr$Period, Drought=allcalGr$Drought,K=allcalGr$K, KGE=allcalGr$KGE1)
g<-data.frame(Drought=aa,Period=allvalGr$Period, Drought=allvalGr$Drought, K=allvalGr$K, KGE=allvalGr$KGE1)

data<-rbind(b,g)

ggplot(data=b, aes(factor(K), y=KGE, fill=Drought))+
geom_boxplot(size=1, alpha=.6) +
  scale_fill_manual(values=c("blue", "red"),
                       labels = c("Dry","Wet"),
                       guide = "legend")+  
  geom_jitter(size=1, alpha=.6)+
  theme_ipsum(base_size = 35) +
  theme(axis.title.x=element_text(size=35, face = "bold"),
  axis.title.y =element_text(size=35, face="bold"))+
  theme(
    legend.position="top",
    plot.title = element_text(size=35)
  ) +
  ylim(.4,1)+
  xlab("K")+ ylab("KGE")


P<-PD1res[[1]][[4]]$P

library(ggplot2)

tab<-data.frame(DTM=DTM, R=R, RM=RM, Qsim=Qsim)
ggplot(tab,aes(x=DTM)) + 
  geom_point(aes(y=R), colour="darkgrey")+
  geom_point(aes(y=RM), colour="darkblue")+
  geom_point(aes(y=Qsim), colour="brown")+
  geom_line(aes(y=R), col="darkgrey", size=.8)+
  geom_line(aes(y=RM),col="darkblue",linetype="longdash", size=.8)+
  geom_line(aes(y=Qsim), col="brown", linetype="twodash", size=.8)+
  scale_linetype_identiy(name="Hydrograph",
                       breaks = c("darkgrey","darkblue","brown"),
                       labels = c("Robs",
                                  "RBil",
                                  "RGr"),
                       guide = "legend")+  
  theme_ipsum(base_size = 30) +
  theme(axis.title.x=element_text(size=30, face = "bold"),
        axis.title.y =element_text(size=30, face="bold"))+
  theme(
    legend.position="top",
    plot.title = element_text(size=30)
  ) + 
  ylab("runoff (mm)")+xlab("year")

DTM<-PD1res[[24]][[4]][["DTM"]]
R<-PD1res[[24]][[4]][["R"]]
RM<-PD1res[[24]][[4]][["RM"]]
Qsim<-PD1GRres[[28]][[2]][["Qsim"]]

PD1GRres[[28]][[2]][["DatesR"]]

ggplot(tab, aes(x=DTM, y=P))+
  geom_bar(stat="identity")
    


library(xtable)
xt<-xtable(c)
print(xt)

ggplot(data=grp,aes(factor(K), y=X2, fill=K,Drought)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_gradient(low="purple")+
  geom_jitter(color="black", size=2., alpha=.7) +
  theme_ipsum(base_size = 25) +
  theme(strip.text.x=element_text(size=30, face = "bold"),
    legend.position="none",
    plot.title = element_text(size=30)
  ) +
  theme(axis.title.x=element_text(size=30, face = "bold"),
        axis.title.y =element_text(size=30, face="bold"))+
  xlab("K")+ylab("X2")+
  facet_wrap(~ENS ,scales = "free")

  
library(ggplot2)
theme_set(theme_bw())  



order<-c("P","T","R")
df<-data.frame(Type=rep(c("P","T","R"),6))

df<-arrange(transform(df,
     Type=factor(Type, levels = order)),Type)


as.data.table(grcritabval)
Drybil<-bilcritabcal[1:18,]
Wetbil<-bilcritacal[19:36,]

Calcrits<-data.table(Period=Drybil$Period, K=Drybil$K, 
                     Drought=df$Type,
                     KGE=Drybil$KGE, KGEgr=Wetbil$KGE)

grmeanparams<-GRparams%>%group_by(K,Drought)%>%
        summarise(X1=mean(as.numeric(X1)),X2=mean(as.numeric(X2)))
 
RM<-PD1res[[22]][[4]]$RM

Qm<-PD1GRres[[27]][[2]][["Qsim"]]



tab<-data.frame(DTM=DTM, Robs=R, Rbil=RM, Rgr=Qm)
tab<-tab[-c(1:12),]

ggplot(tab,aes(x=DTM)) + 
  geom_jitter(aes(y=Robs,colour="black"), size=2)+
  geom_line(aes(y=Rbil), colour="darkblue", size=1)+
  geom_line(aes(y=Rgr), colour="brown", size=1)+
  scale_color_identity(name = "simulation X observation",
                       breaks = c("blue", "red"),
                       labels = c("Observed runoff (R) mm",
                                  "Simulated runoff RM mm"),
                       guide = "legend")+ 
  theme_ipsum(base_size = 15) +
  theme(strip.text.x=element_text(size=30, face = "bold"),
    legend.position="right",
    plot.title = element_text(size=30)
  ) +
theme(axis.title.x=element_text(size=30, face = "bold"),
      axis.title.y =element_text(size=30, face="bold"))+
  xlab("Year")+ylab("runoff (mm)")

MRCbil%>%
  ggplot(aes(x=K, y=mrc, fill=K)) +
  geom_boxplot()+
  scale_color_gradient(low= "pink", high = "darkred")+
  geom_jitter(color="black", size=2., alpha=.7) +
  theme_ipsum(base_size = 15) +
  theme(strip.text.x=element_text(size=30, face = "bold"),
        legend.position="right",
        plot.title = element_text(size=30)
  ) +
  theme(axis.title.x=element_text(size=30, face = "bold"),
        axis.title.y =element_text(size=30, face="bold"))+
  xlab("sub-period")+ylab("MRC criteria")+
  ylim(-1,1)


ggplot(data=df,aes(x=year, y=T, group=BASIN, colour=BASIN)) +
  geom_line() + 
  geom_jitter(color="black", size=2., alpha=.7) +
  theme_ipsum(base_size = 15) +
  theme(strip.text.x=element_text(size=30, face = "bold"),
        legend.position="right",
        plot.title = element_text(size=30)
  ) +
  theme(axis.title.x=element_text(size=30, face = "bold"),
        axis.title.y =element_text(size=30, face="bold"))+
  xlab("year")+ylab("temperature (°C)")+
  facet_wrap(~BASIN ,scales = "free")


brung<-allcalGr%>%group_by(Period,Drought,K)%>%
                      summarise(KGE=mean(KGE1))

fusionxcal<-cbind(ziiz,brung)
fusioncal<-fusionxcal[,-c(1,2,5,6,7)]

library(xtable)
xtable(ziiz)     

WriteXLS::WriteXLS(fusioncal,"/Users/Doudou/Downloads/m2.xls")
moo<-read_excel("/Users/Doudou/Downloads/X1.xlsx")

m<-ENS_BASIN[1:500,][,-2]

            
print(xtable(m,digits=c(0,0,0,0,0,0),math.style.exponents = TRUE), include.rownames=FALSE)



