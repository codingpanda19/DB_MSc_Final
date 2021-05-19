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
fls <- list.files(paste0(curP,"/BasinObs/"), pattern = '.rds', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/BasinObs/"), pattern = '.rds', full.names = FALSE),
           pattern = '.rds',
           replacement = '')
for (n in seq_along(nm)) {
  assign(nm[n], readRDS(fls[n]))
}


ENS_BASIN<-ENS_BASIN%>%group_by(ENS)
                                    
# set strings as factors to false
options(stringsAsFactors = FALSE)

# if you send the month function a particular DTM and specify the format, it returns the month
month(as.POSIXlt("01-01-2003", format = "%m-%d-%y"))
## [1] 
month((as.POSIXlt('19-03-1993', format='%d-%m-%y')))


# extract just the DTM from the DTM field in your data.frame

BASIN<-ENS_BASIN[1:1740,]

BASIN<-BASIN%>%mutate(DTM=as.POSIXct(DTM))


sbasin<-GMB%>%mutate(DTM=as.POSIXct(DTM))

summary(sbasin)                        
head(month(sbasin$DTM))

## [1] 1 1 2 2 2 2


df<-as.data.frame(ENS_BASIN)

colnames(df) <- c("Basin", "Ens", "DTM", "Pr", "Tp", "Rf")

# add a month column to your myUP1dat data.frame
myBasin <- BASIN%>%group_by(DTM,ENS)%>%
  mutate(month = month(DTM), year=year(DTM))

#myUP1dat$test <- as.DTM(paste0(myUP1dat$month_year,"-01"),"%Y-%m-%d")

# calculate the sum precipitation for each month
df<- myBasin %>%group_by(BASIN,ENS,year)%>%
                            summarise(P=mean(P),T=mean(T),R=mean(R))

byMonth<- myBasin %>%group_by(BASIN,year,month, ENS)%>%
  summarise(P=mean(P),T=mean(T),R=mean(R))

#********************************************************************************
#Discharge
library(dygraphs)
library(xts)
library(plotly)

ggplotly(annual_precip)

ggplot(byYear,aes(x=year,y=P)) + 
  geom_bar(stat = 'identity')+
  scale_y_reverse()+
  geom_line(data =byYear,aes(x=year,y=R))


x=byYear$year
y1=byYear$P
y2=byYear$R


(y1 ~ x)
lines(y2 ~ x)
library(lubridate)
library(EcoHydRology)
dataforhydrograph <- as.data.frame(cbind(date = GMB$DTM, precip = GMB$P, discharge = GMB$R))
dataforhydrograph$date <- as.Date(dataforhydrograph$date, format="%y%m%d")
hydrograph(input=byYear, S1.col = "darkgreen", P.units = "mm", S.units = "mm")

hydrograph(streamflow = GMB$R,
           timeSeries = GMB$DTM, precip =GMB$P,
           P.units = "mm",
           S1.col = "brown", stream.label = "Streamflow (mm)")

plot(GMB$DTM,GMB$P, type="l",col='brown')
lines(KDG$DTM,KDG$P, type="l",col='blue')
lines(MKO$DTM,MKO$P, type="l",col='darkgreen')
lines(SIM$DTM,SIM$P, type="l",col='orange')
lines(WAM$DTM,WAM$P, type="l",col='brewer')
lines(WAV$DTM,WAV$P, type="l")

Pplot<-data.frame(DTM=GMB$DTM, P=GMB$P, P1=KDG$P, 
                  P2=MKO$P, P3=SIM$P, P4=WAM$P)

ggplot(Pplot,aes(x=DTM)) + 
  geom_point(aes(y=P,colour="blue"))+
  geom_point(aes(y=P1,colour="red"))+
  geom_point(aes(y=P2,colour="yellow"))+
  geom_point(aes(y=P3,colour="orange"))+
  geom_point(aes(y=P4,colour="black"))+
  geom_line(aes(y=P, colour="blue"))+
  geom_line(aes(y=P1,colour="red"))+
  geom_line(aes(y=P2,colour="yellow"))+
  geom_line(aes(y=P3,colour="orange"))+
  geom_line(aes(y=P4,colour="black"))+
  scale_color_identity(name = "Catchments",
                       breaks = c("blue", "red", "yellow", 
                                  "orange", "black"
                                  ),
                       labels = c("Gouloumbou","Kedougou","Mako",
                                  "Simenti",
                                  "Wassadou-Amont"),
                       guide = "legend")+  
  theme_ipsum() +
  theme(
    legend.position="right",
    plot.title = element_text(size=11)
  ) + 
  ggtitle("Monthly average rainfall time series 
over a set of 5 catchments of the Gambia river basin
for a period of 30 years from 1970 to 2000") +
  xlab("P= precipitation in mm")


library(lubridate)



byMonthYear%>%
  ggplot(aes(x =year, y = R)) +
  geom_line() +
  geom_point() +
  geom_smooth() +
  labs(y = "R (mm)") +
  theme_minimal()

require(ggplot2)

x  <- GMB$DTM
y1 <- GMB$T
y2 <- KDG$T
y3 <- MKO$T
y4 <- SIM$T
y5 <- WAM$T
y6 <- WAV$T
df <- data.frame(x,y1,y2,y3,y4,y5)
df2 <- melt(data = df, id.vars = "x")


ggplot(df, aes(x)) +                    # basic graphical object
  geom_line(aes(y=y1), colour="brown") +  # first layer
  geom_line(aes(y=y2), colour="darkgreen")+  # second layer
  geom_line(aes(y=y3), colour="darkblue")+ 
  geom_line(aes(y=y4), colour="black")+ 
  geom_line(aes(y=y5), colour="yellow")+
  labs(title = "Mean Annual Temperature of 5 catchments of the Gambia River, Senegal",
       subtitle = "Gouloumbou, Kedougou, Mako, Simenti, Wassadou-Amont",
       y = "Temperature (°C)",
       x = "Year") + theme_bw(base_size =15)

#--------------------------


library(ggplot2)
library(dplyr)
library(lubridate)
# set strings as factors to false
options(stringsAsFactors = FALSE)

df<-data.frame(ENS_BASIN)

df<-ENS_BASIN%>%group_by(year)%>%
      summarise_(P=mean(P))
df<-byMonth
fd<-df%>%filter(ENS==1)
