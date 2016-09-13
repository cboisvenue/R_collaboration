# Data exploration: trying to figure out the data Juha gave me for 
# the Stand Reconstruction (SR) sites.
#
# June 28, 2016
# CBoisvenue
#--------------------------------------------------------------------

library(data.table)

jdir = "C:/Celine/Syndocs/RES_Work/Work/StandReconstruction/Juha/data/"

list.files(paste(jdir,"site&biomass",sep=""))
# [1] "DataDocumentation.xlsx"     "DataProcessingScript.r"     "MeasData.csv"              
# [4] "PlotData.csv"               "StandGrowth_BiomassDBH.csv" "StudyData.csv"             
# [7] "TreeData.csv"               "TreeGrowth.csv"      

tree <- fread(paste(jdir,"site&biomass/TreeGrowth.csv",sep=""))

plt <-fread(paste(jdir,"site&biomass/StandGrowth_BiomassDBHHT.csv",sep=""))

# To do:
# sum the tree-level increment to make sure they match the plot-level increments
# tree-level biomass is in kg 
#
# figure out the years covered by each plot - how many go before 1945?
# FetchClimate goes back to 1943 
#(H:\Celine_other\Brenton Chookolingo\Complete Package\Climate Data\Fetch Climate\Fetch Climate Canadian Dataset.csv)
# The db1 goes from 2000 to 2014 because it is linked to MODIS NPP


#------------------------------------------------------------------------------
# sum ind tree increments to see how it compares to plot level...
# summing the biomass calculated using dbh and ht equations from Lambert et al.
sum.tree <- tree[,.(all.tinc = sum(BIOIdh)), by=.(StudyName,PlotID,Year)]
setkey(sum.tree,StudyName,PlotID)
# apparently we need the study name to get the same no of lines of the plt data...
unique(tree$StudyName)
# [1] "BCDF49"                 "BermsOA"                "BermsOBS"              
# [4] "BermsOJP"               "CandleLakeOJP"          "DFFert"                
# [7] "FtSmith"                "Inuvik"                 "OBSThompson"           
# [10] "PhDThesis"              "ThunderBaySpacingTrial"

# need the plot size
study <- fread(paste(jdir,"site&biomass/StudyData.csv",sep=""))
size <- study[,.(StudyName,PlotID,PlotSize)]
setkey(size,StudyName,PlotID)
plot1 <- merge(sum.tree,size)
plot1 <- plot1[,biomha := all.tinc*10000/PlotSize]
plot2 <- plot1[,c("all.tinc","PlotSize") := NULL]
setkey(plot2,StudyName,PlotID,Year)
setkey(plt,StudyName,PlotID,Year)
# the lines should exactly match here, both dt have 5232 lines
unitcheck <- merge(plot2,plt)
check1 <- unitcheck$biomha - unitcheck$AnnualIncrement
range(check1)
# [1] -5.000402e-09  4.998356e-09
# check finished: they are the same---------------------------------------------

# year range per plot-----------------------------------
pltyr <- plt[,.(first.yr = min(Year), last.yr = max(Year)),by=.(StudyName,PlotID)]
dim(pltyr)
#[1] 64  4
pre1945 <- pltyr[first.yr<1945]
dim(pre1945)
#[1] 36  4
# finished year range ---------------------------------

# how many plots?--------------------------------------
no.plt <- unique(plt[,.(StudyName,PlotID)]) # 64
plt.tree <- unique(tree[,.(StudyName,PlotID)]) #64
no.study <- unique(study[,.(StudyName,PlotID)]) #981
setkey(no.study,StudyName,PlotID)
setkey(no.plt,StudyName,PlotID)
setkey(plt.tree,StudyName,PlotID)
no.plt %in% plt.tree
which(no.plt != plt.tree)
#integer(0)
which(no.plt == plt.tree)
# works these are equal

# which of the 981 is not in those two?
#no.match <- subset(no.study, !(no.study %in% plt.tree))
matches <- which(no.study %in% plt.tree)
## ??? I have no answer question for Juha--------------

# get UTMs to JWhite--------------------------------------------
# Juha has 981 plots

## July 15  JW identified a problem with the CIPHA plots: the northings are all negative (I see
## why that is in the code below: the code I copied said to add a negative but my values are 
## already negative)
## Other problem JW identified is that the other UTMs (not CIPHA plot) all have low precision 
## on the UTMs. I am now calculating the UTMs from the Lat Lon of Brenton's db instead of taking
## the UTMs values from the study information to see if there is a difference in precision

# this is where I took the UTMs from the study info
dim(study)
#[1] 981  19
utm <- study[,.(StudyName,PlotID,PlotSize,UTMZone,Easting,Northing)]
# this db seems to have 1056 plots
##


db1 <- fread("M:/Brenton/MODIS Climate Database v11.csv")
# there are a bunch of missing years
# get rid of them
db2 <- db1[!is.na(Year)]
# check if previous versions of the db had NAs
#db3 <- read.table(file="C:/Celine/Syndocs/RES_Work/Work/BrentonChookolingo/MODIS_Climate Database v4.csv",sep=",")
# can't read that in...not sure why
db3 <- unique(db2[,.(Study.Name,Plot.ID,Latitude,Longitude)])
dim(db3)
#[1] 1056    4

# how many of these are CIPHA
db4 <-  db3[Study.Name=="CIPHA"]
dim(db4)
#[1] 75  4
981+75
# it adds up
# need to get UTMs for CIPHA plots
head(db4)
# added this line below to check on the UTM low precision issue
db4.1 <- unique(db2[Study.Name!="CIPHA",.(Study.Name,Plot.ID,Latitude,Longitude)])
# 981 plots

# to get the zone (function off http://stackoverflow.com/)
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

db5 <- db4[,zone:= long2UTM(Longitude)]
unique(db5$zone)
#[1] 14 13 12 11 10

## added the lones below to check on the low precision UTM issue
db5.1 <- db4.1[,zone:= long2UTM(Longitude)]
zones <- unique(db5.1$zone)
#[1] 16 10 13 14 12  8

library(sp)
library(rgdal)

sp1 <- SpatialPoints(cbind(db4$Longitude, db4$Latitude), proj4string=CRS("+proj=longlat"))
which(db4$zone==14)
which(db4$zone==13)
which(db4$zone==12)
which(db4$zone==11)
which(db4$zone==10)
cipha14 <- as.data.frame(spTransform(sp1[which(db4$zone==14)],CRS("+proj=utm +zone=14")))
cipha13 <- as.data.frame(spTransform(sp1[which(db4$zone==13)],CRS("+proj=utm +zone=13")))
cipha12 <- as.data.frame(spTransform(sp1[which(db4$zone==12)],CRS("+proj=utm +zone=12")))
cipha11 <- as.data.frame(spTransform(sp1[which(db4$zone==11)],CRS("+proj=utm +zone=11")))
cipha10 <- as.data.frame(spTransform(sp1[which(db4$zone==10)],CRS("+proj=utm +zone=10")))

# for all the other plots - checking is the UTMs in the study file is the same as that in the 
# Brenton db
sp2 <- SpatialPoints(cbind(db4.1$Longitude, db4.1$Latitude), proj4string=CRS("+proj=longlat"))
standRecOnlyUTMs <- as.data.frame(spTransform(sp2[which(db4.1$zone==zones[1])],
                                              CRS(paste("+proj=utm +zone=",zones[1],sep=""))))
for(i in 2:length(zones)){
  newUTMs <- as.data.frame(spTransform(sp2[which(db4.1$zone==zones[i])],
                                       CRS(paste("+proj=utm +zone=",zones[2],sep=""))))
  standRecOnlyUTMs <- as.data.table(rbind(standRecOnlyUTMs,newUTMs))
}
setnames(standRecOnlyUTMs,names(standRecOnlyUTMs),c("Easting","Northing"))


cipha <- as.data.table(rbind(cipha14,cipha13,cipha12,cipha11,cipha10))
setnames(cipha,names(cipha),c("Easting","Northing"))

db6 <- db5[,.(Study.Name,Plot.ID,zone)]
PlotSize <- rep(400,75)
ciphaAll <- cbind(db6,PlotSize,cipha)
setnames(ciphaAll,names(ciphaAll),c("StudyName","PlotID","UTMZone","PlotSize","Easting","Northing"))
setcolorder(ciphaAll,c("StudyName","PlotID","PlotSize","UTMZone","Easting","Northing"))
# fixed the low-precision utm so new file below
#StandRecUTM <- rbind(utm,ciphaAll)
#write.table(StandRecUTM,file="C:/Celine/Syndocs/RES_Work/Work/StandReconstruction/work/data/standRecUTMs.csv",sep=",",row.names = FALSE)

# stand Rec only
db6.1 <- db5.1[,.(Study.Name,Plot.ID,zone)]
study2 <- fread(paste(jdir,"site&biomass/StudyDataPlotNamesChgdToMatch.csv",sep=""))
recPlotSize <- study2[,.(StudyName,PlotID,PlotSize)]
setkey(recPlotSize,StudyName,PlotID)
recPlotsAll <- cbind(db6.1,standRecOnlyUTMs)
setnames(recPlotsAll,names(recPlotsAll),c("StudyName","PlotID","UTMZone","Easting","Northing"))
setkey(recPlotsAll,StudyName,PlotID)

#Fixed: the names did not match between the "StudyData.csv", where we get plot sizes, 
# and the climate db. 
### I changed the names in the StudyData.csv and saved is as StudyDataPlotNamesChgdToMatch.csv
recPlotsUTMs <- merge(recPlotsAll,recPlotSize)
#FIXED### LOOSING 37 plots in the merge - WHY???
# recPlotsUTMs2 <- merge(recPlotsAll,recPlotSize, all=TRUE)
# missingplots <-recPlotsUTMs2[is.na(PlotSize),]
# BermsOA1 <- recPlotsAll[StudyName=="BermsOA",]
# BermsOA2 <- recPlotSize[StudyName=="BermsOA",]
# found the problem


unique(recPlotsUTMs$StudyName)
unique(recPlotSize$StudyName)

setcolorder(recPlotsUTMs,c("StudyName","PlotID","PlotSize","UTMZone","Easting","Northing"))
allPlotsUTMS <- rbind(recPlotsUTMs,ciphaAll)
write.table(allPlotsUTMS,file="C:/Celine/Syndocs/RES_Work/Work/StandReconstruction/work/data/standRecUTMs.csv",sep=",",row.names = FALSE)
## something wrong with the precision of the plots, so need to check on process of 
##converting lat lon to UTMs
# for now send db3 to Joanne White
write.table(db3,file="C:/Celine/Syndocs/RES_Work/Work/StandReconstruction/work/data/standRecLatLong.csv",sep=",",row.names = FALSE)


# UTMs and plot size to JWhite completed----------------------------------------


# tree diameter diameter range? (in PlotData.csv) ------------------------------------------------
plotData <- fread(paste(jdir,"site&biomass/PlotData.csv",sep=""))
no.data <- unique(plotData[,.(StudyName,PlotID)]) # 985 

# diameter range by species
sps.dbh <- plotData[,.(lowdbh = min(DBH), highdbh = max(DBH)), by = .(StudyName,PlotID,Species,MeasNo)]
# There is a range of dbh by species by plots by measurement
plt.dbh <- plotData[,.(lowdbh = min(DBH), highdbh = max(DBH)), by = .(StudyName,PlotID,MeasNo)]
table(plt.dbh$lowdbh)
# there are small trees measured
# dbh finished------------------------------------------------------------------------------------


# tree species? (in PlotData.csv)--------------------------------------------------
plt.sps <- plotData[,.(no.sps = .N),by=.(StudyName,PlotID,Species,MeasNo)]
sps.chg <- dcast(plt.sps, StudyName+PlotID+Species ~ MeasNo, value.var = "no.sps")
setnames(sps.chg,names(sps.chg),c("StudyName","PlotID","Species","meas0","meas3"))
setkey(sps.chg,StudyName,PlotID)

sps.count <- sps.chg[,.(tot.count = sum(meas0),dom = max(meas0)),by=.(StudyName,PlotID)]
setkey(sps.count,StudyName,PlotID)

dom1 <- merge(sps.chg[,.(StudyName,PlotID,Species,meas0)],sps.count)
dom <- dom1[meas0==dom,.(domSps = Species,prop=dom/tot.count),by=.(StudyName,PlotID)]
# species finished---------------------------------------------------------------


# plot-level biomass-------------------------------------------------------------
# these estimates are from the sum of the individual tree-level biomass calculated using
# Lambert et al equations with dbh and ht as predictor variables.
plt.biom <- merge(plt[,.(StudyName,PlotID,Year, AnnualIncrement, Density)],dom)

write.table(plt.biom,file="C:/Celine/Syndocs/RES_Work/Work/StandReconstruction/work/data/plotBiomHa.csv",sep=",",row.names = FALSE)



