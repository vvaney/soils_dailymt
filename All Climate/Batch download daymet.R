# # batch download daymet data  #  #

library(daymetr)
library(dplyr)

# get locations from FIA data
tr <- readRDS("./Data/FIA_data_Damages_back_to_1998.RDS")

#as.data.frame(colnames(tr))
# extract plots with pines
tr <- subset(tr, tr$SPCD == 121 | 
               tr$SPCD == 111 |
               tr$SPCD == 131)
tr <- subset(tr, tr$MEASYEAR>=2012)

#  need plot locations in this form
# first column plot ID 
# something like this: 
tr$id<-paste(tr$STATECD,tr$COUNTYCD,tr$PLOT, sep=".")
#
# then latitude
# then third column longitude
locs <- tr[,c(187,87,88, 79,80,81)]
locs$meas_date <- ymd(paste0(locs$MEASYEAR,"-",locs$MEASMON,"-",locs$MEASDAY))

locs$yday <- as.numeric(strftime(locs$meas_date, format = "%j"))
# remove duplicates
locs2 <- distinct(locs)

# test first few locations
#locs2 <- locs2[1:4,]

# save it to csv

# ALREADY COMPLETE took 24 hours

# write.table(locs2, "./Data/Plot_locations_for_daymet.csv", sep = ",",
#             col.names = TRUE,
#             row.names = FALSE,
#             quote = FALSE)
# 
# # set working directory to folder to hold daymet data
# 
# download_daymet_batch(file_location = "./Data/Plot_locations_for_daymet.csv",
#                       start=2010, internal = FALSE, path = "F:/FIA/needlecast daymet/")

##################################################
library(lubridate)
#process the daymet data to extract 30, 60, and 90 day temp and precip

# top level wrapper to cycle through plots
# set directory to needlecast daymet folder
plot.id <- locs2$id
meas.yr <- locs2$MEASYEAR
yday <- locs2$yday
yday <- ifelse(yday>365, 365, yday)

needlecast.clim <- function(plot.id, meas.yr, yday){
  # load daymet data for this plot
  plt.table <- paste0(plot.id,"_2010_2019.csv")
  fls <- list.files()
 if(plt.table %in% fls & file.info(plt.table)$size>0) do.clim(plt.table, meas.yr=meas.yr, yday=yday)
   else return(c(NA))
}
# testing
# meas.yr <- unique(tr[tr$id=='1.1.1',]$MEASYEAR)
# plt.table <- "1.85.15_2010_2019.csv"
# yday <- unique(tr[tr$id=='1.1.1',]$yday)

setwd("F:/FIA/needlecast daymet/")
# plot level calculations
do.clim <- function(plt.table, meas.yr, yday){

dm <- read.table(file=plt.table,
                 sep=",", skip=7, header=T)

# add counter to rows for date range selection
dm$cnt <- 1:nrow(dm)

#find the cnt for the day of measure
end_day <- dm[dm$year==meas.yr & dm$yday==yday,]$cnt

# subset for time period of interest
dm <- dm[dm$cnt >= end_day-120 & dm$cnt < end_day,]
# sum of precipitation
p30 <- sum(dm[(nrow(dm)-30):nrow(dm),]$prcp..mm.day., na.rm = TRUE) 
p60 <- sum(dm[(nrow(dm)-60):nrow(dm),]$prcp..mm.day., na.rm = TRUE)
p90 <- sum(dm[(nrow(dm)-90):nrow(dm),]$prcp..mm.day., na.rm = TRUE)
p120 <- sum(dm$prcp..mm.day., na.rm = TRUE)

# temperatures
#mean
t30 <- mean(((dm[(nrow(dm)-30):nrow(dm),]$tmax..deg.c.) + (dm[(nrow(dm)-30):nrow(dm),]$tmin..deg.c.))/2, na.rm = TRUE)
t60 <- mean(((dm[(nrow(dm)-60):nrow(dm),]$tmax..deg.c.) + (dm[(nrow(dm)-60):nrow(dm),]$tmin..deg.c.))/2, na.rm = TRUE)
t90 <- mean(((dm[(nrow(dm)-90):nrow(dm),]$tmax..deg.c.) + (dm[(nrow(dm)-90):nrow(dm),]$tmin..deg.c.))/2, na.rm = TRUE)
t120 <- mean(((dm$tmax..deg.c.) + (dm$tmin..deg.c.))/2, na.rm = TRUE)

# mean min
tmin30 <- mean((dm[(nrow(dm)-30):nrow(dm),]$tmin..deg.c.), na.rm = TRUE)
tmin60 <- mean((dm[(nrow(dm)-60):nrow(dm),]$tmin..deg.c.), na.rm = TRUE)
tmin90 <- mean((dm[(nrow(dm)-90):nrow(dm),]$tmin..deg.c.), na.rm = TRUE)
tmin120 <- mean(dm$tmin..deg.c., na.rm = TRUE)

# mean max
tmax30 <- mean((dm[(nrow(dm)-30):nrow(dm),]$tmax..deg.c.), na.rm = TRUE)
tmax60 <- mean((dm[(nrow(dm)-60):nrow(dm),]$tmax..deg.c.), na.rm = TRUE)
tmax90 <- mean((dm[(nrow(dm)-90):nrow(dm),]$tmax..deg.c.), na.rm = TRUE)
tmax120 <- mean(dm$tmax..deg.c., na.rm = TRUE)

res <- c(p30, p60, p90,p120,
          t30, t60, t90, t120,
          tmin30, tmin60, tmin90, tmin120,
          tmax30, tmax60, tmax90, tmax120)

return(res)
}


clim.res <- data.frame(plot.id = plot.id, p30=NA, p60=NA, p90=NA,p120=NA,
                       t30=NA, t60=NA, t90=NA, t120=NA,
                       tmin30=NA, tmin60=NA, tmin90=NA, tmin120=NA,
                       tmax30=NA, tmax60=NA, tmax90=NA, tmax120=NA)

for(i in 1:length(plot.id)){
  clim.res[i,2:17] <- needlecast.clim(plot.id= plot.id[i], meas.yr=meas.yr[i], yday=yday[i])
  print(i)
}


saveRDS(clim.res, "Climate results for needlecast plots.RDS")

# get missing
tna <- subset(clim.res, is.na(clim.res$tmax120))

plt.list <- tna$plot.id

