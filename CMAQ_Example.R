rm (list=ls())

# Without fixed effect
path <- ("C:/Users/Jeffrey/Research/")

setwd(paste0(path,"/SmokeTransport/"))

dataFormat <- 1 # 0 for CMAG/NAM, 1 for Marcela


if(dataFormat == 0) {
  Mon_loc   <- read.csv("data/LCC_6370997.csv",header=TRUE)
  
  
  PM25_Y13  <- read.csv("data/CMAQ_OBS_WestUS_Daily_2013.csv",header=TRUE)
  PM25_Y14  <- read.csv("data/CMAQ_OBS_WestUS_Daily_2014.csv",header=TRUE)
  NAM_Y13   <- read.csv("data/NAM_2013.csv",header=TRUE)
  NAM_Y14   <- read.csv("data/NAM_2014.csv",header=TRUE)
  
  
  
  PM25      <- rbind(PM25_Y13,PM25_Y14)
  NAMO      <- rbind(NAM_Y13,NAM_Y14)
  
  
  strDates  <- paste0(PM25$SMM,"/",PM25$SDD,"/",PM25$SYYYY)
  PM25$date <- as.Date(strDates, "%m/%d/%Y")
  strDates  <- paste0(NAMO$MM,"/",NAMO$DD,"/",NAMO$YYYY)
  NAMO$date <- as.Date(strDates, "%m/%d/%Y")
  
  head(PM25$date)
  head(NAMO$date)
} else if(dataFormat == 1) {
  Mon_loc   <- read.csv("data/mon_loc_unique_col61_all_11_20_2019.csv")
  load("data/data_Grid_fixed_Nov_21_2019.RData")
  PM25      <- read.csv("data/pm25_unique_col61_all_11_20_2019.csv")
  NAMO      <- read.csv("data/LUR.csv")
  
  
  
  
  datesplit   <- strsplit(as.character(PM25$date), "[- ]")
  dates       <- paste0(sapply(datesplit, "[[", 2),"/",sapply(datesplit, "[[", 3),"/",sapply(datesplit, "[[", 1))
  PM25$date   <- as.Date(dates, "%m/%d/%Y")
  NAMO$date   <- as.Date(NAMO$date, "%m/%d/%Y")
  
  colnames(PM25)[colnames(PM25) == "monID"] <- "SiteID"
  colnames(PM25)[colnames(PM25) == "pm25"]  <- "PM_FRM_ob"
  
  
  Mon_loc     <- merge(Mon_loc, grid.loc_Fixed, by="Grid_Cell")
  colnames(Mon_loc)[colnames(Mon_loc) == "ID"]        <- "SiteID"
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_x"]      <- "SiteX"
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_y"]      <- "SiteY"
  colnames(Mon_loc)[colnames(Mon_loc) == "X"]         <- "GridRow"
  Mon_loc$GridRow                                     <- 1
  Mon_loc$GridCol                                     <- Mon_loc$SiteID
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_lon"]    <- "SiteLon"
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_lat"]    <- "SiteLat"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_lon"]   <- "GridLon"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_lat"]   <- "GridLat"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_x"]     <- "Grid_Centroid_X"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_y"]     <- "Grid_Centroid_Y"
  
  # monitors 9 and 10 occupy the same grid; remove site 10
  Mon_loc <- subset(Mon_loc, Mon_loc$SiteID != 10)
  PM25    <- subset(PM25, PM25$SiteID != 10)
  
  NAMO <- merge(NAMO, Mon_loc[,c("Grid_Cell", "SiteID")], by.x="Grid_Cell", by.y="Grid_Cell")
  save(PM25, file="pm25.RData")
  save(Mon_loc, file="monloc.RData")
  save(NAMO, file="namo.RData")
}





PM25$mergeID <- paste0(PM25$date,"-",PM25$SiteID)
NAMO$mergeID <- paste0(NAMO$date,"-",NAMO$SiteID)

NAMO$date    <- NULL
NAMO$SiteID  <- NULL
NAMO$Latitude  <- NULL
NAMO$Longitude <- NULL
head(PM25$mergeID)
head(NAMO$mergeID)

# Sort both dataset by mergeID and then merge
ID1 <- order(PM25$mergeID)
ID2 <- order(NAMO$mergeID)

PM25 <- PM25[ID1,]
NAMO <- NAMO[ID2,]

ALLPM  <- merge(PM25,NAMO,by.x ="mergeID", by.y = "mergeID", all.x = TRUE, all.y=FALSE)
dim(ALLPM)

# sort both dataset by SiteId
ID0       <- order(Mon_loc$SiteID)
Mon_loc   <- Mon_loc[ID0,]

#Mon_loc$MonID <- 1:nrow(Mon_loc)


ID00      <- order(ALLPM$SiteID)
ALLPM      <- ALLPM[ID00,]

#Mon_loc$pm_x <- Mon_loc$Row*12000
#Mon_loc$pm_y <- Mon_loc$Column*12000

str(Mon_loc)
str(ALLPM)

length(unique(Mon_loc$SiteID))

length(unique(ALLPM$SiteID))

## Merge CMAQ/LU/Met data to PM2.5 data by date and grid
##   NOTE: only grids/days with a PM2.5 measurements are kept
dat_orii = merge(ALLPM, Mon_loc, by.x ="SiteID", by.y = "SiteID", all.x = TRUE, all.y=FALSE)

#name_cov  <- c("T_2m_Avg","Dwt_2m_Avg","Ws_10m_Avg","Wdir_10m_Avg","Pblh_Avg","Pmsl_Avg","Hgt_Avg","Veg_Avg")
name_cov  <- c("T_2m_Avg","Dwt_2m_Avg","Ws_10m_Avg","Wdir_10m_Avg","Pmsl_Avg","Veg_Avg")
name_loc  <- c("SiteX","SiteY","GridRow","GridCol","SiteLon","SiteLat","GridLon","GridLat","Grid_Centroid_X","Grid_Centroid_Y")
name_keep <- c("SiteID","date","PM_FRM_ob","PM_FRM_mod",name_cov,name_loc)

dat_ori = dat_orii[,name_keep]
dim(dat_ori)
str(dat_ori)

##drop rows with missing observed pm2.5
## PM2.5 < 2 treated as missing
dat_ori$PM_FRM_ob[ dat_ori$PM_FRM_ob < 2] <- NA
dat = subset (dat_ori, !is.na (dat_ori$PM_FRM_ob+dat_ori$PM_FRM_mod))


sum((!is.na(dat_ori$PM_FRM_ob))&is.na(dat_ori$PM_FRM_mod)) # should be 0

which((!is.na(dat_ori$PM_FRM_ob))&is.na(dat_ori$PM_FRM_mod)) # should be integer(0)

#subset(dat_ori,(!is.na(dat_ori$PM_FRM_ob))&is.na(dat_ori$PM_FRM_mod))[1:10,]

dim(dat_ori)
dim(dat)

head(dat)

# Create ID to label duplicated monitor within the same grid the same day
dat$AggID  <- paste0(dat$date,"-",dat$SiteX,"-",dat$SiteY)
sum(duplicated(dat$AggID)) # should be 0

dat_f <- dat
dat_f$AggID  <- NULL

if( sum(duplicated(dat$AggID))!=0 ){
  dat$GridID <- paste0(dat$SiteX,"-",dat$SiteY)
  dat$GridID[1:20]
  dat_f <- aggregate(.~date+GridID,data=dat_f,FUN="mean")
}

# get back to data format


##resort by date, SiteID
dat_f = dat_f[order (dat_f$date, dat_f$SiteID),]

ID.vec <- unique(dat$SiteID)
ID.mat <- data.frame(SiteID=ID.vec,MonID=1:length(ID.vec))

dat_f <- merge(dat_f,ID.mat,by="SiteID")

dat_f = dat_f[order (dat_f$date, dat_f$MonID),]
head(dat_f)

str(dat_f)


#########################################
#### Create Inputs for the Downcaler ####
#########################################

#X = proxy
#Y = PM2.5
#Z = matrix of LU/Met predictors
X = dat_f$PM_FRM_mod
Y = dat_f$PM_FRM_ob
Z = as.matrix (dat_f[, name_cov])


# there is no Met predictors
#Z = as.matrix (dat[, c("elevation", "forestcover", "lim.hwy.length", "point.emi.any", "tmp", "wind")])

#Space ID = monitor ID 1, 2, ...
#Time ID = consecutive days with label 1, 2, ...
Space.ID = dat_f$MonID # 192

if(dataFormat == 0) {
  dat_f$timeorder = as.numeric(as.Date(dat_f$date) - as.Date ("2013-01-01"))+1
} else if(dataFormat == 1){
  dat_f$timeorder = as.numeric(as.Date(dat_f$date) - as.Date ("2013-01-01"))+1
}
Time.ID = dat_f$timeorder


### Create spatial distance matrix (km)
### Extract unique rows of MonID
#mon_sit <- unique(dat_f[,c("MonID","pm_x","pm_y","Row","Column")])

mon_sit <- unique(dat_f[,c("MonID","SiteX","SiteY")])
mon_sit <- mon_sit[order(mon_sit$MonID),]

N.loc = nrow(mon_sit)
Dist.mat = matrix(NA, ncol=N.loc, nrow=N.loc)
for (i in 1:N.loc){
    Dist.mat[,i] = round(sqrt( (mon_sit$SiteX[i]-mon_sit$SiteX)^2 + (mon_sit$SiteY[i] - mon_sit$SiteY)^2)/1000,2 )
}

Mon.coord = mon_sit[c("SiteX", "SiteY")]/1000

###### Run Downscaler Code ######
### MCMC iterations to run
###  Note: this might take a while (~30 min on my computer)

source ("Downscaler_Final.R")


n.iter = 30000
burn = 5000
thin = 5
fit = DownScaler (Y, X, Z, Dist.mat, Space.ID, Time.ID, Mon.coord, n.iter = n.iter, burn = burn, thin = thin,taper=TRUE, save.beta = TRUE)

##save the model results
save (fit, file = "DSrun.RData")

###Here are some example to look at the fitted results
#Temporal additive bias
plot (fit$beta[,1], type = "l", main = "Daily Additive Bias"); abline(h=0, col = 2)

#Temporal multiplicative bias
plot (fit$beta[,2], type = "l", main = "Daily Multiplicative Bias"); abline(h=0, col = 2)

#Fixed effects
junk = data.frame(fit$gamma)
names(junk) = c("intercept", "CMAQ", colnames(Z))
apply (junk, 2, mean, na.rm = T) ## Estimates
apply (junk, 2, sd, na.rm = T) ## Standard deviation



########################################################
###### Make predictions at CMAQ grid centroids #########
########################################################

rm (list=ls())

path <- ("C:/Users/Jeffrey/Research/")

setwd(paste0(path,"/SmokeTransport/"))

#load ("data.RData")
load ("DSrun.RData")

dataFormat <- 1 # 0 for CMAG/NAM, 1 for Marcela


if(dataFormat == 0) {
  Mon_loc   <- read.csv("data/LCC_6370997.csv",header=TRUE)
  
  
  PM25_Y13  <- read.csv("data/CMAQ_OBS_WestUS_Daily_2013.csv",header=TRUE)
  PM25_Y14  <- read.csv("data/CMAQ_OBS_WestUS_Daily_2014.csv",header=TRUE)
  NAM_Y13   <- read.csv("data/NAM_2013.csv",header=TRUE)
  NAM_Y14   <- read.csv("data/NAM_2014.csv",header=TRUE)
  
  
  
  PM25      <- rbind(PM25_Y13,PM25_Y14)
  NAMO      <- rbind(NAM_Y13,NAM_Y14)
  
  
  strDates  <- paste0(PM25$SMM,"/",PM25$SDD,"/",PM25$SYYYY)
  PM25$date <- as.Date(strDates, "%m/%d/%Y")
  strDates  <- paste0(NAMO$MM,"/",NAMO$DD,"/",NAMO$YYYY)
  NAMO$date <- as.Date(strDates, "%m/%d/%Y")
  
  head(PM25$date)
  head(NAMO$date)
} else if(dataFormat == 1) {
  Mon_loc   <- read.csv("data/mon_loc_unique_col61_all_11_20_2019.csv")
  load("data/data_Grid_fixed_Nov_21_2019.RData")
  PM25      <- read.csv("data/pm25_unique_col61_all_11_20_2019.csv")
  NAMO      <- read.csv("data/LUR.csv")
  
  
  
  
  datesplit   <- strsplit(as.character(PM25$date), "[- ]")
  dates       <- paste0(sapply(datesplit, "[[", 2),"/",sapply(datesplit, "[[", 3),"/",sapply(datesplit, "[[", 1))
  PM25$date   <- as.Date(dates, "%m/%d/%Y")
  NAMO$date   <- as.Date(NAMO$date, "%m/%d/%Y")
  
  colnames(PM25)[colnames(PM25) == "monID"] <- "SiteID"
  colnames(PM25)[colnames(PM25) == "pm25"]  <- "PM_FRM_ob"
  
  
  Mon_loc     <- merge(Mon_loc, grid.loc_Fixed, by="Grid_Cell")
  colnames(Mon_loc)[colnames(Mon_loc) == "ID"]        <- "SiteID"
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_x"]      <- "SiteX"
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_y"]      <- "SiteY"
  colnames(Mon_loc)[colnames(Mon_loc) == "X"]         <- "GridRow"
  Mon_loc$GridRow                                     <- 1
  Mon_loc$GridCol                                     <- Mon_loc$SiteID
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_lon"]    <- "SiteLon"
  colnames(Mon_loc)[colnames(Mon_loc) == "pm_lat"]    <- "SiteLat"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_lon"]   <- "GridLon"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_lat"]   <- "GridLat"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_x"]     <- "Grid_Centroid_X"
  colnames(Mon_loc)[colnames(Mon_loc) == "AOD_y"]     <- "Grid_Centroid_Y"
  
  # monitors 9 and 10 occupy the same grid; remove site 10
  Mon_loc <- subset(Mon_loc, Mon_loc$SiteID != 10)
  PM25    <- subset(PM25, PM25$SiteID != 10)
  
  NAMO <- merge(NAMO, Mon_loc[,c("Grid_Cell", "SiteID")], by.x="Grid_Cell", by.y="Grid_Cell")
}





PM25$mergeID <- paste0(PM25$date,"-",PM25$SiteID)
NAMO$mergeID <- paste0(NAMO$date,"-",NAMO$SiteID)


NAMO$date    <- NULL
NAMO$SiteID  <- NULL
NAMO$Latitude  <- NULL
NAMO$Longitude <- NULL
head(PM25$mergeID)
head(NAMO$mergeID)

# Sort both dataset by mergeID and then merge
ID1 <- order(PM25$mergeID)
ID2 <- order(NAMO$mergeID)

PM25 <- PM25[ID1,]
NAMO <- NAMO[ID2,]

ALLPM  <- merge(PM25,NAMO,by.x ="mergeID", by.y = "mergeID", all.x = TRUE, all.y=FALSE)
dim(ALLPM)

# sort both dataset by SiteId
ID0       <- order(Mon_loc$SiteID)
Mon_loc   <- Mon_loc[ID0,]

#Mon_loc$MonID <- 1:nrow(Mon_loc)


ID00      <- order(ALLPM$SiteID)
ALLPM      <- ALLPM[ID00,]

#Mon_loc$pm_x <- Mon_loc$Row*12000
#Mon_loc$pm_y <- Mon_loc$Column*12000

str(Mon_loc)
str(ALLPM)

length(unique(Mon_loc$SiteID))

length(unique(ALLPM$SiteID))

## Merge CMAQ/LU/Met data to PM2.5 data by date and grid
##   NOTE: only grids/days with a PM2.5 measurements are kept
dat_orii = merge(ALLPM, Mon_loc, by.x ="SiteID", by.y = "SiteID", all.x = TRUE, all.y=FALSE)

#name_cov  <- c("T_2m_Avg","Dwt_2m_Avg","Ws_10m_Avg","Wdir_10m_Avg","Pblh_Avg","Pmsl_Avg","Hgt_Avg","Veg_Avg")
name_cov  <- c("T_2m_Avg","Dwt_2m_Avg","Ws_10m_Avg","Wdir_10m_Avg","Pmsl_Avg","Veg_Avg")
name_loc  <- c("SiteX","SiteY","GridRow","GridCol","SiteLon","SiteLat","GridLon","GridLat","Grid_Centroid_X","Grid_Centroid_Y")
name_keep <- c("SiteID","date","PM_FRM_ob","PM_FRM_mod",name_cov,name_loc)

dat_ori = dat_orii[,name_keep]
dim(dat_ori)
str(dat_ori)

##drop rows with missing observed pm2.5
## PM2.5 < 2 treated as missing

dat = subset (dat_ori, !is.na (dat_ori$PM_FRM_mod))


dat$AggID  <- paste0(dat$date,"-",dat$GridCol,"-",dat$GridRow)
sum(duplicated(dat$AggID)) # should be 0

if( sum(duplicated(dat$AggID))!=0 ){
  #dat$GD <- paste0(dat$GridCol,"-",dat$GridRow)
  #dat$GD[1:20]
  #dat_f <- aggregate(.~date+GD,data=dat_f,FUN="mean")
	dat <- dat[!duplicated(dat$AggID),]
	dat$AggID <- NULL
}


#subset(dat_ori,(!is.na(dat_ori$PM_FRM_ob))&is.na(dat_ori$PM_FRM_mod))[1:10,]

dim(dat_ori)
dim(dat)

head(dat)





# get back to data format

dat$GD  <- paste0(dat$GridRow,"-",dat$GridCol)
GD.vec  <- unique(dat$GD)
GD.mat  <- data.frame(GD=(GD.vec),GridID=1:length(GD.vec))
dat     <- merge(dat,GD.mat,by="GD")

# Create ID to label duplicated monitor within the same grid the same day

dat_pred <- dat

str(dat_pred)

##resort by date, GridID
dat_pred = dat_pred[order (dat_pred$date, dat_pred$SiteID),]

ID.vec <- unique(dat_pred$SiteID)
ID.mat <- data.frame(SiteID=ID.vec,MonID=1:length(ID.vec))

dat_pred <- merge(dat_pred,ID.mat,by="SiteID")

dat_pred = dat_pred[order (dat_pred$MonID),]
head(dat_pred)


str(dat_pred)

# ==========================================================================================
# Use the complete CMAQ datasets here

# Need the X and Z values of a GRID, NOT a Site
X.pred <- dat_pred$PM_FRM_mod
Z.pred <- as.matrix (dat_pred[, name_cov])
#Space ID = monitor ID 1, 2, ...
#Time ID = consecutive days with label 1, 2, ...
Space.ID.pred <- dat_pred$GridID

if(dataFormat == 0) {
  Time.ID.pred  <- as.numeric(as.Date(dat_pred$date) - as.Date ("2013-01-01"))+1
} else if(dataFormat == 1) {
  Time.ID.pred  <- as.numeric(as.Date(dat_pred$date) - as.Date ("2013-01-01"))+1
}




Coord.pred = unique(Mon_loc[, c("Grid_Centroid_X", "Grid_Centroid_Y")]/1000)
#Coord.pred    <- unique(dat_pred[c("pm_x", "pm_y")]/1000)


# ==========================================================================================


### Computation time will increase considerably by the number of prediction locations (n^2).
### This takes about 20 min on my computer
source ("Downscaler_Final.R")
pred = pred.downscaler (fit, X.pred, Z.pred, Space.ID.pred, Time.ID.pred, Coord.pred, n.iter = 100)

### Merge the predictions and standard errors with the time-stamp and grid cell
dat.pred = data.frame (date=as.Date(dat_pred$date), GridID = dat_pred$GridID, pmob=dat_pred$PM_FRM_ob, pred = pred$Est, se = pred$SD)

save (dat.pred, file = "Pred.RData")

# ===========================================================================================

load(file="Pred.RData")
Coord.plot <- unique(Mon_loc[, c("GridLon", "GridLat")])
#Spatial plot on July 1, 2004
if(dataFormat == 0) {
  july1 = subset (dat.pred, date == "2014-02-01")
} else if(dataFormat == 1) {
  july1 = subset (dat.pred, date == "2013-08-20")
}

#july2 = subset (dat.pred, date == "2014-02-02")

library (classInt)
library (RColorBrewer)
library (maps)

nclr <- 10
plotclr <- brewer.pal(nclr,"YlOrRd")
plotclr <- plotclr[1:nclr] # reorder colors
class <- classIntervals(round(july1$pred,1), nclr, style = "pretty")
colcode <- findColours(class, plotclr)
windows()
plot (Coord.plot[,1], Coord.plot[,2], col = colcode, pch = 15, cex = 1, main = "Example: Predictions on Feb 1, 2014")
map ("county", add = TRUE, col = "grey")
map ("state", add = T)
legend("bottomleft", legend=names(attr(colcode, "table")),  fill=attr(colcode, "palette"), cex=1,bg = "white")

nclr <- 10
plotclr <- brewer.pal(nclr,"YlOrRd")
plotclr <- plotclr[1:nclr] # reorder colors
class <- classIntervals(round(july1$se,1), nclr, style = "pretty")
colcode <- findColours(class, plotclr)

windows()
plot (Coord.plot[,1], Coord.plot[,2], col = colcode, pch = 15, cex = 1, main = "Example: Predictions on July 1, 2004")
map ("county", add = T, col = "grey")
map ("state", add = T)
legend("bottomleft", legend=names(attr(colcode, "table")),  fill=attr(colcode, "palette"), cex=1, bg = "white")

# ==========================================================================================================================
# Check the MSE for monitors having observed PM2.5

pmid <- sort(unique(subset(dat.pred,!is.na(dat.pred$pmob))$GridID))

#plot((july1$pmob-july1$pred)[idd])

#abline(h=0,col="red")

rmse <- c()
rsgn <- c()
for(ij in pmid){
	monsite <- subset (dat.pred, GridID==ij)
	rmse <- c(rmse,print(sqrt( mean((monsite$pmob - monsite$pred)^2,na.rm=TRUE) )) )
}

which(rmse>14)

dat_rmse <- data.frame(GridID=pmid,rmse=rmse)

plot_rmse <- merge(dat_rmse,unique(dat_pred[,c("GridID","GridLon","GridLat")]),by="GridID",all.x=TRUE)
str(plot_rmse)
library (classInt)
library (RColorBrewer)
library (maps)

nclr <- 4
plotclr <- brewer.pal(nclr,"YlOrRd")
plotclr <- plotclr[1:nclr] # reorder colors
class <- classIntervals(round(plot_rmse$rmse,1), nclr, style = "pretty")
colcode <- findColours(class, plotclr)

windows()
plot (plot_rmse$GridLon, plot_rmse$GridLat, col = colcode, pch = 15, cex = 1, main = "Example: Predictions on Feb 1, 2014")
map ("county", add = TRUE, col = "grey")
map ("state", add = T)
legend("bottomleft", legend=names(attr(colcode, "table")),  fill=attr(colcode, "palette"), cex=1,bg = "white")



cbind(pmid,rmse)

monsite <- subset (dat.pred, GridID==98)
plot(monsite$pmob,cex=2)
points(monsite$pred,col="red")


plot(rmse,cex=2)

idd  <- !is.na(july1$pmob)
idd <- !is.na(monsite$pmob)
plot(abs((monsite$pmob-monsite$pred)[idd])/(monsite$pred[idd]))




