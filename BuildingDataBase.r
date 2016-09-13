#-----------------------------------------------------------
# Building the databases for the stats analysis on Juha's
# Stand reconstruction plots 
#
# Read-in Brenton's MODIS info
# Read-in the FetchClimate data
# see how much linkages I can do
#
# CBoisvenue
# ---------------------------------------------------------

library(data.table)

modis <- fread("M:/Brenton/MODIS Climate Database v11.csv")
fetchClim <- fread("M:/Brenton/Fetch Climate Canadian Dataset.csv")
biomHa <- fread("C:/Celine/Syndocs/RES_Work/Work/StandReconstruction/work/data/plotBiomHa.csv")

# list to think/do
# select right fetchClim pixels that the plots are in
## August 3, 2016
# the fetchClim data seems to have climate data back to 1943
# there are a few instances of plots that have biomass estimates prior to 1943 (36 instances)
# I decided that I would only go as far back as 1943 given the time it would take me to get 
# further back climate data
# 
# Now, I need to match the locations of the plots to the 100kmX100km fetchClim tiles
# Brenton has alread done that with to matche the plots and corresponding the MODIS pixels
# to fetchClim data, but only for the years 2000 to 2015 (the MODIS years)
#
## What I have learned about the data up to now
# there are a bunch of blanks in the MODIS Climate Database v11.csv
# I remove them using this
modis <- modis[modis$Study.Name!="",] 
##but before doing this, check if all columns are 
# # blank when the Study.Name is blank
# blanks <- modis[which(modis$Study.Name==""),] 
# str(blanks)
# length(which(!is.na(blanks)))
# # there are some non-blank columns...which? the Study.Name and Plot.ID are "" which are not
# # counted as blanks (2*93702 = 187404).
# dim(blanks)
# #[1] 93702    66
# # 93702*66
# #[1] 6184332
# length(which(!is.na(blanks)))
# #[1] 187404
# # 187404+5996928
# #[1] 6184332


# get fetch climate data for each plot ----------------------------------------
# plot in biomHa
dim(unique(biomHa[,.(StudyName,PlotID)]))
#[1] 64  2
# there are fewer plots here then in the MODIS. JM explained this: 64 plots are intensively 
# measured, but 981 plots are inventory plots for 

# plot cooddinates in the modis db
dim(unique(modis[,.(Study.Name,Plot.ID)]))
#[1] 1056    2
dim(unique(modis[,.(Latitude,Longitude)]))
#[1] 1055    2
# why one less? ## two plots with the same coordinate (?)
plotLoc <- unique(modis[,.(Study.Name,Plot.ID,Latitude,Longitude)])

# trying one plot at a time ######## WORKING IN HERE ###############
 v1 <- bondsFetchClim[latmin < plotLoc$Latitude[1] & 
                     latmax > plotLoc$Latitude[1] &
                     lonmin < plotLoc$Longitude[1] &
                     lonmax > plotLoc$Longitude[1],.(latmin,latmax,lonmin,lonmax)]


# get the fetchClim lines with these coordinate bounds
v2 <- fetchClim[latmin == v1$latmin & latmax == v1$latmax &
        lonmin == v1$lonmin & lonmax == v1$lonmax,.(Date, latmin,latmax,lonmin,lonmax,
                                                    `FetchClimate Temp`,`FetchClimate Precip`,`FetchClimate VPD`)]
dim(v2)
#[1] 1104   16

# playing with dates

d1 <- strptime(v2$Date,format="%m/%d/%Y %H:%M:%S")
year <- as.numeric(format(d1,"%Y"))
month <- as.numeric(format(d1,"%m"))

# add the StudyName and PlotID to match the biomHa 
StudyName <- rep(plotLoc$Study.Name[1],length(d1))
PlotId <- rep(plotLoc$Plot.ID[1],length(d1))

v3 <- as.data.table(cbind(StudyName,PlotId,year,month,v2))
# add average GS temp and sd GS temp
v4 <- v3[,.(StudyName,PlotId,year,GSavgTemp = mean(`FetchClimate Temp`,na.rm=TRUE),
            GSavgPrecip = mean(`FetchClimate Precip`,na.rm=TRUE)GSavgVDP = mean(`FetchClimate VDP`,na.rm=TRUE),
            ),by=.(year)]



which(is.na(v3$`FetchClimate Precip`))
v3[876,]
which(is.na(v3$`FetchClimate VPD`))
v3[229,]













