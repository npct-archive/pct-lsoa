#This code subsets flow data based on criteria

#############
#THis code will be incorparated into prepFlow
###########


#librarys
library(stplanr)

#Inputs
flow <- readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Code
flow_oneway <- onewayid(flow, attrib = 3:54, id1 ="lsoa1", id2 = "lsoa2")    #Pair up bidirectional flows
flow$id <- paste(pmin(flow$lsoa1, flow$lsoa2), pmax(flow$lsoa1, flow$lsoa2)) #Create ID


#########
# remove inter zone flows 

####

saveRDS(flow, "../pct-lsoa/Data/02_INput/LSOA_flow_oneway.Rds")
