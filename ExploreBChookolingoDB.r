#----------------------------------------------------
# Looking at the db that Brenton Chookolingo
# has created during his time at PFC
# Also looking at the data Juha provided for his stand
# reconstruction sites.
#
# This ultimate goal is to use both climate and 
# remotely sensing info to analyse the productivity
# measurements and estimates on Juha's and Ted's plots
# CBoisvenue
# April 14, 2016
#----------------------------------------------------


library(data.table)

list.files("C:/Celine/data/Juha/")
# [1] "DataDocumentation.xlsx"         "MeasData.csv"                  
# [3] "MODIS Climate Database v10.csv" "PlotData.csv"                  
# [5] "StudyData.csv"                  "TreeData.csv"  

# database from Brenton  April 2016
# climate info mostly
db1 <- fread("M:/Brenton/MODIS Climate Database v11.csv")
# Why are there a bunch of NAs?
dim(db1)
# [1] 236262     66
which(is.na(db1$Year))[1:20]
# [1] 142561 142562 142563 142564 142565 142566 142567 142568 142569 142570 142571 142572
# [13] 142573 142574 142575 142576 142577 142578 142579 142580
db1[which(is.na(db1$Year))[1:20]]



# Files from Juha April 2016
# description of their content here "DataDocumentation.xlsx" 
# unique identifier is PLOTID
# general info study sites/plots
study <- fread("C:/Celine/data/Juha/StudyData.csv")
# measurement inof like number and date, has plot age
meas <- fread("C:/Celine/data/Juha/MeasData.csv") # has age

plot1 <- fread("C:/Celine/data/Juha/PlotData.csv")
tree <- fread("C:/Celine/data/Juha/TreeData.csv") 

# Juha's already calculated fluxes -  only done for some sites
ddir <- "C:/Celine/data/Juha/"




# These lines below are from my SK PSPs to calculate biomass per tree...
### I HAVE NOT LOOKED AT THIS YET
# Biomass calculations -------------------------------------------------------------------------
# Calculate biomass for all trees in data frame-------------------------------------------------
# calculate biomass for all trees with dbh by year
params1 <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/SK_SPSBiomassParametersChecked.csv",sep=",", header=TRUE)
params2 <- read.table("C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/GrowthRaster/BiomassEstimation/SK_SPSBiomassParametersCheckedDBH_only.csv",sep=",", header=TRUE)
params2 <- params2[1:9,1:9]

# biomass
biom.tree1 <- left_join(biom9,params2) 

biom.dbh <- mutate(biom.tree1,wood.dbh=b1w*(DBH^b2w),bark.dbh =b1b*(DBH^b2b),
                   branches.dbh = b1br*(DBH^b2br),foliage.dbh = b1f*(DBH^b2f),
                   biom.dbh = wood.dbh+bark.dbh+foliage.dbh+branches.dbh) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,HEIGHT,age,DBH,wood.dbh,bark.dbh,branches.dbh,foliage.dbh,biom.dbh)

biom.tree2 <- left_join(biom9,params1) 

biom.tree3 <- mutate(biom.tree2,wood=b1w*(DBH^b2w)*(HEIGHT^b3w),bark =b1b*(DBH^b2b)*(HEIGHT^b3b),
                     branches = b1br*(DBH^b2br)*(HEIGHT^b3br),foliage = b1f*(DBH^b2f)*(HEIGHT^b3f),
                     biomass = wood+bark+foliage+branches) %>%
  select(PLOT_ID,TREE_NO,YEAR,SPECIES,dom, HEIGHT,age,DBH,wood,bark,branches,foliage,biomass)

biom.tree4 <- inner_join(biom.tree3,biom.dbh)

write.table(biom.tree4,file="C:/Celine/CelineSync/RES_Work/Work/JoanneWhite/SK_work/data/CleanedUpForUsing/SK_2000TreeBiomass.csv",sep=",",row.names = FALSE)

# biomass from these equations are in kg
ggplot(data=biom.tree4) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# there might only be one tree over 1000
biomcheck1 <- filter(biom.tree4,biomass>=1000) 
ggplot(data=biomcheck1) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
dim(biomcheck1)#[1] 460  12
biomcheck2 <- filter(biom.tree4, biomass<1000)
ggplot(data=biomcheck2) + geom_bar(aes(biomass, fill=SPECIES),colour="black")
# distribution looks ok
# Results ok
# End of calculating biomass per tree--------------------------------------------------------------
