# load the ncdf4 package
library(ncdf4)

# set path and filename
ncpath <- "/Users/bartlein/Projects/ESSD/data/nc_files/"
ncname <- "cru10min30_tmp"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)

# open a netCDF file
ncin <- nc_open(ncfname)
print(ncin)

# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

tunits

# get temperature
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

ls()

# load some packages
library(chron)
library(lattice)
library(RColorBrewer)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))

# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA

length(na.omit(as.vector(tmp_array[,,1])))

# get a single slice or layer (January)
m <- 1
tmp_slice <- tmp_array[,,m]

# quick map
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)


# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

# create dataframe and add names
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)


# set path and filename
csvpath <- "/Users/bartlein/Projects/ESSD/data/csv_files"
csvname <- "cru_tmp_1.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(tmp_df01),csvfile, row.names=FALSE, sep=",")

# reshape the array into vector
tmp_vec_long <- as.vector(tmp_array)
length(tmp_vec_long)

# reshape the vector into a matrix
tmp_mat <- matrix(tmp_vec_long, nrow=nlon*nlat, ncol=nt)
dim(tmp_mat)

head(na.omit(tmp_mat))

# create a dataframe
lonlat <- as.matrix(expand.grid(lon,lat))
tmp_df02 <- data.frame(cbind(lonlat,tmp_mat))
names(tmp_df02) <- c("lon","lat","tmpJan","tmpFeb","tmpMar","tmpApr","tmpMay","tmpJun",
                     "tmpJul","tmpAug","tmpSep","tmpOct","tmpNov","tmpDec")
# options(width=96)
head(na.omit(tmp_df02, 20))

# get the annual mean and MTWA and MTCO
tmp_df02$mtwa <- apply(tmp_df02[3:14],1,max) # mtwa
tmp_df02$mtco <- apply(tmp_df02[3:14],1,min) # mtco
tmp_df02$mat <- apply(tmp_df02[3:14],1,mean) # annual (i.e. row) means
head(na.omit(tmp_df02))


dim(na.omit(tmp_df02))

# write out the dataframe as a .csv file
csvpath <- "/Users/bartlein/Projects/ESSD/data/csv_files/"
csvname <- "cru_tmp_2.csv"
csvfile <- paste(csvpath, csvname, sep="")
write.table(na.omit(tmp_df02),csvfile, row.names=FALSE, sep=",")


# create a dataframe without missing values
tmp_df03 <- na.omit(tmp_df02)
head(tmp_df03)

ls()

# time an R process
ptm <- proc.time() # start the timer
# ... some code ...
proc.time() - ptm # how long?

# copy lon, lat and time from the initial netCDF data set
lon2 <- lon
lat2 <- lat
time2 <- time
tunits2 <- tunits
nlon2 <- nlon; nlat2 <- nlat; nt2 <- nt

# generate lons, lats and set time
lon2 <- as.array(seq(-179.75,179.75,0.5))
nlon2 <- 720
lat2 <- as.array(seq(-89.75,89.75,0.5))
nlat2 <- 360
time2 <-as.array(c(27773.5, 27803.5, 27833.5, 27864.0, 27894.5, 27925.0,
                   27955.5, 27986.5, 28017.0, 28047.5, 28078.0, 28108.5))
nt2 <- 12
tunits2 <- "days since 1900-01-01 00:00:00.0 -0:00"


ptm <- proc.time() # start the timer
# convert tmp_df02 back into an array
tmp_mat2 <- as.matrix(tmp_df02[3:(3+nt-1)])
dim(tmp_mat2)

# then reshape the array
tmp_array2 <- array(tmp_mat2, dim=c(nlon2,nlat2,nt))
dim(tmp_array2)


# convert mtwa, mtco and mat to arrays
mtwa_array2 <- array(tmp_df02$mtwa, dim=c(nlon2,nlat2))
dim(mtwa_array2)
## [1] 720 360
mtco_array2 <- array(tmp_df02$mtco, dim=c(nlon2,nlat2))
dim(mtco_array2)



# generate lons, lats and set time
lon3 <- as.array(seq(-179.750,179.750,0.50))
nlon3 <- 720
lat3 <- as.array(seq(-89.750,89.750,0.50))
nlat3 <- 360
time3 <- as.array(c(27773.5, 27803.5, 27833.5, 27864.0, 27894.5, 27925.0,
                    27955.5, 27986.5, 28017.0, 28047.5, 28078.0, 28108.5))
nt3 <- 12
tunits3 <- "days since 1900-01-01 00:00:00.0 -0:00"
# copy lon, lat and time from initial netCDF data set
lon4 <- lon
lat4 <- lat
time4 <- time
tunits4 <- tunits
nlon4 <- nlon; nlat4 <- nlat; nt4 <- nt
Next, an nlon by nlat by nt array is created, and filled with the original fill value (or an alternative). Also, three nlon by nlat arrays for MTWA, MTCO, and MAT are created and filled. The generated lontitudes and latitudes are used here (as opposed to copies from the original netCDF file–this is more general)

# create arrays
# nlon * nlat * nt array
fillvalue <- 1e32
tmp_array3 <- array(fillvalue, dim=c(nlon3,nlat3,nt3))
# nlon * nlat arrays for mtwa, mtco and mat
mtwa_array3 <- array(fillvalue, dim=c(nlon3,nlat3))
mtco_array3 <- array(fillvalue, dim=c(nlon3,nlat3))
mat_array3 <- array(fillvalue, dim=c(nlon3,nlat3))
4.2.2 Explicit copying from a data frame to array
In the first, most explict, approach, we loop over the rows in the data frame, find the j-th and k-th column and row that each observation falls in (using the which.min() function), and then copy the values for each row into the arrays. This takes a relatively long time for data sets with hundreds of rows and columns.

# loop over the rows in the data frame 
# most explicit, but takes a VERY LONG TIME
ptm <- proc.time() # time the loop
nobs <- dim(tmp_df03)[1]
for(i in 1:nobs) {
  
  # figure out location in the target array of the values in each row of the data frame
  j <- which.min(abs(lon3-tmp_df03$lon[i]))
  k <- which.min(abs(lat3-tmp_df03$lat[i]))
  
  # copy data from the data frame to array
  tmp_array3[j,k,1:nt] <- as.matrix(tmp_df03[i,3:(nt+2)])
  mtwa_array3[j,k] <- tmp_df03$mtwa[i]
  mtco_array3[j,k] <- tmp_df03$mtco[i]
  mat_array3[j,k] <- tmp_df03$mat[i]
}
proc.time() - ptm # how long?
##    user  system elapsed 
##   81.98   59.23  141.24
4.2.3 Partial loop avoidance
In the second approach, the sapply() function is used to repeatedly apply a function to create two vectors of indices (j2 and k2) that describe which column and row of the array each row of the data frame is assigned to. For example, the function function(x) which.min(abs(lon3-x)) finds the closest longitude of the full array (lon3) to the longitude of each row of the data frame (tmp_df03$lon, the x argument of the function).

# loop-avoidance approaches 
# get vectors of the grid-cell indices for each row in the data frame
ptm <- proc.time() 
j2 <- sapply(tmp_df03$lon, function(x) which.min(abs(lon3-x)))
k2 <- sapply(tmp_df03$lat, function(x) which.min(abs(lat3-x)))
Then, the values are copied (one time at a time) by first reshaping the appropriate column in the data frame (using the as.matrix() function) into a temporary array (temp_array), which is then copied into tmp_array3 (with temp meaning “temporary” and tmp denoting temperature here). Note how the square-bracket selection on the left side of the assignment ([cbind(j2,k2)]) puts each row of the data frame into the proper location in the array.

fillvalue <- 1e32
# partial loop avoidance for tmp_array3
temp_array <- array(fillvalue, dim=c(nlon3,nlat3))
nobs <- dim(tmp_df03)[1]
for (l in 1:nt) {
  temp_array[cbind(j2,k2)] <- as.matrix(tmp_df03[1:nobs,l+2]) 
  tmp_array3[,,l] <- temp_array
}
The 2-d arrays can be copied directly:
  
  # copy 2-d arrays directly
  mtwa_array3[cbind(j2,k2)] <- as.matrix(tmp_df03$mtwa)
mtco_array3[cbind(j2,k2)] <- as.matrix(tmp_df03$mtco) 
mat_array3[cbind(j2,k2)] <- as.matrix(tmp_df03$mat) 
proc.time() - ptm
##    user  system elapsed 
##   1.067   0.245   1.330
4.2.4 Complete loop-avoidance approach
Loops can be totally avoided as follows, extending the [...] selection to all three dimensions of the full array (tmp_array3). Note that the code fragment 3:(nt3+2) implies that the data are in columns 3 through 14 in the data frame (i.e. lon and lat are in the first two columns):
  
  # loop avoidance for all of the variables
  ptm <- proc.time() 
nobs <- dim(tmp_df03)[1]
l <- rep(1:nt3,each=nobs)
tmp_array3[cbind(j2,k2,l)] <- as.matrix(tmp_df03[1:nobs,3:(nt3+2)])
mtwa_array3[cbind(j2,k2)] <- as.matrix(tmp_df03$mtwa) 
mtco_array3[cbind(j2,k2)] <- array(tmp_df03$mtco) 
mat_array3[cbind(j2,k2)] <- array(tmp_df03$mat) 
proc.time() - ptm
##    user  system elapsed 
##   0.089   0.013   0.110
# some plots to check creation of arrays
library(lattice)
library(RColorBrewer)
m <- 1
levelplot(tmp_array3[,,m] ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="Mean July Temperature (C)")
levelplot(mtwa_array3 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="MTWA (C)")
levelplot(mtco_array3 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="MTCO (C)")
levelplot(mat_array3 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="MAT (C)")





mat_array2 <- array(tmp_df02$mat, dim=c(nlon2,nlat2))
dim(mat_array2)
## [1] 720 360
proc.time() - ptm # how long?
##    user  system elapsed 
##   0.956   0.116   1.194
4.1.3 Check the conversion
It’s generally a good idea to plot (map) the resulting arrays to check for anomalies or misapprehensions about the layout of the data. First plot the January values, then MTWA, MTCO and MAT.

# some plots to check creation of arrays
library(lattice)
library(RColorBrewer)

levelplot(tmp_array2[,,1] ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="Mean July Temperature (C)")
levelplot(mtwa_array2 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="MTWA (C)")
levelplot(mtco_array2 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="MTCO (C)")
levelplot(mat_array2 ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="MAT (C)")












