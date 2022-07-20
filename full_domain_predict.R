rm (list=ls())

path <- ("C:/Users/Jeffrey/Research/")

setwd(paste0(path,"/SmokeTransport/"))


load ("DSrun_OU_MOD_AOD.RData")
load("data/data_Grid_fixed_Nov_21_2019.RData")
#load("namofull.RData")
load("MOD.RData")

name_cov  <- c("T_2m_Avg","Dwt_2m_Avg","Ws_10m_Avg","Wdir_10m_Avg","Pmsl_Avg","Veg_Avg")

AOD <- merge(AOD, grid.loc_Fixed, by.x="GRID_ID", by.y="Grid_Cell")

# begin full-domain prediction

source ("Downscaler_Final.R")
dat.pred <- data.frame(matrix(ncol=5,nrow=0))
colnames(dat.pred) <- c('date','GridID','pmob','pred','se')
chunk_size <- 3000
# predict for all cell centroids for Aug 20 2013
ncentroids <- length(unique(AOD[['GRID_ID']]))
niter <- ceiling(ncentroids/chunk_size)
for (i in 0:(niter-1)) {
  print(chunk_size*i+1)
  print(min(c(ncentroids,(chunk_size*(i+1)))))
  #chunk <- subset(NAMO_FULL, NAMO_FULL['Grid_Cell'] > chunk_size*i & NAMO_FULL['Grid_Cell'] <= chunk_size*(i+1))
  chunk <- subset(AOD, AOD['GRID_ID'] > chunk_size*i & AOD['GRID_ID'] <= chunk_size*(i+1))
  
  #X.pred <- chunk$PM_FRM_mod
  X.pred <- chunk$aod_550
  #Z.pred <- as.matrix (chunk[, name_cov])
  Z.pred <- NULL
  #Space ID = monitor ID 1, 2, ...
  #Time ID = consecutive days with label 1, 2, ...
  Space.ID.pred <- 1:min(ncentroids-i*chunk_size, chunk_size)#grid.loc_Fixed$Grid_Cell[(chunk_size*i+1):min(c(ncentroids,(chunk_size*(i+1))))]
  Time.ID.pred <- as.numeric(as.Date(chunk$date) - as.Date ("2013-01-01"))+1
  
  
  
  
  Coord.pred = grid.loc_Fixed[(chunk_size*i+1):min(c(ncentroids,(chunk_size*(i+1)))), c("AOD_x", "AOD_y")]/1000
  
  pred = pred.downscaler (fit, X.pred, Z.pred, Space.ID.pred, Time.ID.pred, Coord.pred, n.iter = 100)
  
  ### Merge the predictions and standard errors with the time-stamp and grid cell
  #dat.predchunk = data.frame (date=as.Date(chunk$date), GridID = chunk$Grid_Cell, pmob = chunk$PM_FRM_mod, pred = pred$Est, se = pred$SD)
  dat.predchunk = data.frame (date=as.Date(chunk$date), GridID = chunk$GRID_ID, pmob = chunk$aod_550, pred = pred$Est, se = pred$SD)
  dat.pred <- rbind(dat.pred, dat.predchunk)
}

save (dat.pred, file = "Pred_OU_FULL_MOD_AOD.RData")


# Begin plotting
load('Pred_OU_FULL_MOD_AOD.RData')

library (classInt)
library (RColorBrewer)
library (maps)
library (plotrix)

Coord.plot <- grid.loc_Fixed[,c('AOD_lon', 'AOD_lat')]

plot_day <- function(dat.pred) {
  date <- dat.pred$date[1]
  print(date)
  pm_breaks<-seq(from =0, to=50, by = 10)
  png(filename = paste0('./images/MOD_AOD_Predictions/','pm25_',date,'.png'))
  title_str=expression(bold(paste("Predicted PM"["2.5 "], "(", mu,"g m"^"-3",")")))
  
  
  nclr <- 6
  par(mar=(c(5,5,5,5)))
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  plotclr <- plotclr[1:nclr] # reorder colors
  class <- classIntervals(round(dat.pred$pred,1), nclr, style = "pretty")
  class$brks<-pm_breaks
  colcode <- findColours(class, plotclr)
  
  plot (Coord.plot[,1], Coord.plot[,2], col = colcode, 
        pch = 19, 
        cex = 0.7, 
        main = title_str,
        xlab="Longitude",
        ylab="Latitude",
        font.lab=2,
        cex.lab=1.5,
        xaxt='n',
        yaxt='n',
        cex.main=1.5,
        xlim=c(-124, -103),
        bg=2,
        ylim=c(32,48))
  axis(side=1,tck=0.01,font.axis=2,cex.axis=1.5)
  axis(side=2,tck=0.01,font.axis=2,cex.axis=1.5)
  map ("county", add = TRUE, col = "grey")
  map ("state", add = T)
  mtext(date, line=0.2,font=2,cex=1.3)
  col.labels<-pm_breaks
  color.legend(-101.7,33,-100.7,46,col.labels,plotclr,cex=1.5,align='rb',gradient="y", font=2) 
  dev.off()
  
  # Plot SE
  pm_breaks<-seq(from =8, to=20, by = 2)
  png(filename = paste0('./images/MOD_AOD_Predictions/','se_',date,'.png'))
  title_str=expression(bold(paste("Predicted SE", "(", mu,"g m"^"-3",")")))
  
  nclr <- 7
  par(mar=(c(5,5,5,5)))
  plotclr <- rev(brewer.pal(nclr,"RdYlBu"))
  plotclr <- plotclr[1:nclr] # reorder colors
  class <- classIntervals(round(dat.pred$se,1), nclr, style = "pretty")
  class$brks<-pm_breaks
  colcode <- findColours(class, plotclr)
  
  
  plot (Coord.plot[,1], Coord.plot[,2], col = colcode, 
        pch = 19, 
        cex = 0.7, 
        main = title_str,
        xlab="Longitude",
        ylab="Latitude",
        font.lab=2,
        cex.lab=1.5,
        xaxt='n',
        yaxt='n',
        cex.main=1.5,
        xlim=c(-124, -103),
        bg=2,
        ylim=c(32,48))
  axis(side=1,tck=0.01,font.axis=2,cex.axis=1.5)
  axis(side=2,tck=0.01,font.axis=2,cex.axis=1.5)
  map ("county", add = TRUE, col = "grey")
  map ("state", add = T)
  mtext(date, line=0.2,font=2,cex=1.3)
  col.labels<-pm_breaks
  color.legend(-101.7,33,-100.7,46,col.labels,plotclr,cex=1.5,align='rb',gradient="y", font=2) 
  dev.off()
}


dat.pred_split <- split(dat.pred, f=dat.pred$date)
lapply(dat.pred_split, plot_day)




