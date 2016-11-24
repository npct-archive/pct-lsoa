#This code takes straight liens and produces routes

#Inputs
lines <- readRDS("../pct-lsoa/Data/03_Intermediate/l_Camb.Rds")
lines <- lines[,1:2]
#lines <- lines[1:500,] #test subset
lines <- lines[which(lines$lsoa1 != lines$lsoa2), ]

rq <- line2route(l = lines, route_fun = route_cyclestreet, plan = "quietest", base_url = "http://pct.cyclestreets.net/api/")
saveRDS(rq,file = "../pct-lsoa/Data/03_Intermediate/rq_Camb.Rds")

rf <- line2route(l = lines, route_fun = route_cyclestreet, plan = "fastest", base_url = "http://pct.cyclestreets.net/api/")
saveRDS(rf,file = "../pct-lsoa/Data/03_Intermediate/rf_Camb.Rds")


