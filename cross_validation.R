rm (list=ls())

seed <- 1
set.seed(seed)

# Without fixed effect
path <- ("C:/Users/Jeffrey/Research/")

setwd(paste0(path,"/SmokeTransport/"))

model.name <- "M04"

# load data
load("namo.RData")
load("pm25.RData")
load("monloc.RData")






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
if(model.name == "M01") {
  name_cov  <- c()
} else if(model.name == "M02") {
  name_cov  <- c("Albedo_560")
} else if(model.name == "M03") {
  name_cov  <- c("HGT", "RH", "PBLH","Ws_10m_Avg","Wdir_10m_Avg")
} else if(model.name == "M04") {
  name_cov  <- c("HGT", "RH", "PBLH","Ws_10m_Avg","Wdir_10m_Avg","Veg_Avg","Dist","Pop_den")
} else if(model.name == "M05") {
  name_cov  <- c("HGT", "RH", "PBLH","Ws_10m_Avg","Wdir_10m_Avg")
} else if(model.name == "M06") {
  name_cov  <- c("HGT", "RH", "PBLH","Ws_10m_Avg","Wdir_10m_Avg")
} else if(model.name == "M07") {
  name_cov  <- c("HGT", "RH", "PBLH","Ws_10m_Avg","Wdir_10m_Avg")
} else {
  name_cov  <- c("T_2m_Avg","Dwt_2m_Avg","Ws_10m_Avg","Wdir_10m_Avg","Pmsl_Avg","Veg_Avg")
}
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

# prepare for cross-validation
rownames(dat_f) <- NULL
k <- 10
k_size <- round(nrow(dat_f) / 10, 0)
dat_s <- dat_f[sample(1:nrow(dat_f)),]
rownames(dat_s) <- NULL

best.fit <- NULL
lowest.rmse <- .Machine$integer.max
rmses <- c()


for(iter in 0:(k-1)) {
  print(paste('Fold',iter+1))
  val.ind <- c((iter*k_size+1):((iter+1)*k_size))
  # split data
  dat_val <- dat_s[val.ind,]
  dat_train <- dat_s[-val.ind,]
  ##resort by date, SiteID
  dat_train = dat_train[order (dat_train$date, dat_train$SiteID),]
  
  ID.vec <- unique(dat_train$SiteID)
  ID.mat <- data.frame(SiteID=ID.vec,MonID=1:length(ID.vec))
  
  dat_train <- merge(dat_train,ID.mat,by="SiteID")
  
  dat_train = dat_train[order (dat_train$date, dat_train$MonID),]
  
  
  #########################################
  #### Create Inputs for the Downcaler ####
  #########################################
  
  #X = proxy
  #Y = PM2.5
  #Z = matrix of LU/Met predictors
  X = dat_train$PM_FRM_mod
  Y = dat_train$PM_FRM_ob
  if(!is.null(name_cov)) {
    Z = as.matrix (dat_train[, name_cov])
  } else {
    Z = NULL
  }
  
  # there is no Met predictors
  #Z = as.matrix (dat[, c("elevation", "forestcover", "lim.hwy.length", "point.emi.any", "tmp", "wind")])
  
  #Space ID = monitor ID 1, 2, ...
  #Time ID = consecutive days with label 1, 2, ...
  Space.ID = dat_train$MonID # 192
  
  dat_train$timeorder = as.numeric(as.Date(dat_train$date) - as.Date ("2013-01-01"))+1
  Time.ID = dat_train$timeorder
  
  
  ### Create spatial distance matrix (km)
  ### Extract unique rows of MonID
  #mon_sit <- unique(dat_f[,c("MonID","pm_x","pm_y","Row","Column")])
  
  mon_sit <- unique(dat_train[,c("MonID","SiteX","SiteY")])
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
  fit = DownScaler (Y, X, Z, Dist.mat, Space.ID, Time.ID, Mon.coord, n.iter = n.iter, burn = burn, thin = thin,taper=TRUE, save.beta = TRUE, verbose=FALSE)

  
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
  
  
  
  
  
  
  # validation step
  dat_pred = dat_val[order (dat_val$date, dat_val$SiteID),]
  
  ID.vec <- unique(dat_pred$SiteID)
  ID.mat <- data.frame(SiteID=ID.vec,MonID=1:length(ID.vec))
  
  dat_pred <- merge(dat_pred,ID.mat,by="SiteID")
  
  dat_pred = dat_pred[order (dat_pred$MonID),]
  
  
  dat_pred$GD  <- paste0(dat_pred$GridRow,"-",dat_pred$GridCol)
  GD.vec  <- unique(dat_pred$GD)
  GD.mat  <- data.frame(GD=(GD.vec),GridID=1:length(GD.vec))
  dat_pred     <- merge(dat_pred,GD.mat,by="GD")
  
  # ==========================================================================================
  # Use the complete CMAQ datasets here
  
  # Need the X and Z values of a GRID, NOT a Site
  X.pred <- dat_pred$PM_FRM_mod
  if(!is.null(name_cov)) {
    Z.pred = as.matrix (dat_pred[, name_cov])
  } else {
    Z.pred = NULL
  }
  #Space ID = monitor ID 1, 2, ...
  #Time ID = consecutive days with label 1, 2, ...
  Space.ID.pred <- dat_pred$GridID
  
  Time.ID.pred  <- as.numeric(as.Date(dat_pred$date) - as.Date ("2013-01-01"))+1
  
  
  
  
  
  Coord.pred = unique((Mon_loc[Mon_loc$SiteID %in% unique(dat_pred$SiteID),])[, c("Grid_Centroid_X", "Grid_Centroid_Y")]/1000)
  #Coord.pred    <- unique(dat_pred[c("pm_x", "pm_y")]/1000)
  
  
  # ==========================================================================================
  
  
  ### Computation time will increase considerably by the number of prediction locations (n^2).
  ### This takes about 20 min on my computer
  pred = pred.downscaler (fit, X.pred, Z.pred, Space.ID.pred, Time.ID.pred, Coord.pred, n.iter = 100, verbose=FALSE)
  
  ### Merge the predictions and standard errors with the time-stamp and grid cell
  dat.pred = data.frame (date=as.Date(dat_pred$date), GridID = dat_pred$GridID, pmob=dat_pred$PM_FRM_ob, pred = pred$Est, se = pred$SD)
  
  save(fit, dat.pred, file=paste0('./', model.name,'/',iter,'_',seed,'.RData'))
  
  rmse <- mean((dat.pred$pmob - dat.pred$pred)^2)^0.5
  print(rmse)
  if(rmse < lowest.rmse) {
    best.fit <- iter
    lowest.rmse = rmse
  }
  rmses = append(rmses, rmse)
}

print(paste('Best model is', best.fit))


##save the model results
save (rmses, file = paste0('./', model.name, '/rmse.RData'))
