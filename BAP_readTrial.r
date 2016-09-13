# trial read Joanne's data
#
#Cboisvenue July20th, 2016
#--------------------------------------

library(data.table)

links <- fread("G:/RES_Work/Work/StandReconstruction/work/data/whitejoannenrcanrncanrebapproxyvaluesforintens/Boisvenue_plots_link.csv")
setkey(links,UNIQUE)
library(foreign)
#data <- read.dbf("<Path to your file>")
zone14 <- read.dbf("G:/RES_Work/Work/StandReconstruction/work/data/whitejoannenrcanrncanrebapproxyvaluesforintensTrial/zone14_BAP_extract.dbf")
zone14 <- as.data.table(zone14)
setkey(zone14,UNIQUE)

trial14 <- merge(links,zone14)
dim(trial14)


# complete set
bap <- fread("G:/RES_Work/Work/StandReconstruction/work/data/whitejoannenrcanrncanrebapproxyvaluesforintens/CB_plots_BAP_July292016.csv")
