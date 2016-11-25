#This code takes straight liens and produces routes

#Inputs
lines <- readRDS("../pct-lsoa/Data/03_Intermediate/l_Camb.Rds")

#Parameters
route_type <- "fastest"
linessub <- lines[1:100,] # test subset



routes <- line2route(l = lines, route_fun = route_cyclestreet, plan = route_type, base_url = "http://pct.cyclestreets.net/api/")
saveRDS(routes,file = paste0("../pct-lsoa/Data/03_Intermediate/r_",route_type,".Rds"))




