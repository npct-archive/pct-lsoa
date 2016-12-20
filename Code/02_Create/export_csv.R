#Joins Data ane expoets to csv

#Inputs
routes = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_less3p_fix.Rds")
flow = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Libs
library(dplyr)


#Code
routes_data = routes@data
remove(routes)
flow <- flow[flow$id %in% routes_data$ID,]

nrow(routes_data) == nrow(flow) #TRUE

joined = left_join(flow,routes_data, by = c("id" = "ID"))

write.csv(joined,"../pct-lsoa/Data/04_Output/flow_results_nat_less3p_fix.csv")
