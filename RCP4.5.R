require(ncdf4)
require(fields)



#My Working directory
setwd('/Users/Doudou/Downloads/MB_data')

# set strings as factors to false
options(stringsAsFactors = F)

curP<-getwd()

# nacti data z myPath
fls <- list.files(paste0(curP,"/RCP 4.5/Precipitation2060"), pattern = '.nc', full.names = TRUE)
nm <- gsub(list.files(paste0(curP,"/RCP 4.5/Precipitation2060"), pattern = '.nc', full.names = FALSE),
           pattern = '.nc',
           replacement = '')

for (n in seq_along(nm)) {
  assign(nm[n], nc_open(fls[n]))
}



pr<-'/Volumes/CCCOMA_X64F/RCP 4.5/Precipitation2060/pr_AFR-22_CCCma-CanESM2_rcp45_r1i1p1_CCCma-CanRCM4_r2_mon_200601-201012.nc'

nc_open(pr)

lat<-ncvar_get(pr,'time')



