#This code takes straight liens and produces routes

#Inputs
lines <- readRDS("../pct-lsoa/Data/03_Intermediate/l_100.Rds")
lines[1:10,]

#Parameters
cores_spare <- 4 #Number of cores to NOT use, to prevent computer grinding to a halt

#Libraries
library(doParallel)
library(raster)

#Code
cores_run <- detectCores() - cores_spare
batch_size <- ceiling(nrow(lines)/cores_run)
cl <- makeCluster(cores_run)
registerDoParallel(cl)

routes_list <- foreach(i = 1:cores_run) %dopar% {
  l_start <- as.integer(1 + (i-1)*batch_size)
  if(i*batch_size < nrow(lines)){
    l_fin <- as.integer(i*batch_size)
    }else{
    l_fin <- as.integer(nrow(lines))
  }
  lines_sub <- lines[c(l_start:l_fin),]
  rf_temp <- line2route(l = lines_sub, route_fun = route_cyclestreet, plan = "fastest", base_url = "http://pct.cyclestreets.net/api/")
  

}


Sys.time()

#saveRDS(rq,file = paste0("../pct-lsoa/Data/03_Intermediate/temp/rq_batch_",i,".Rds"))