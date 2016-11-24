#This code takes straight liens and produces routes

#Inputs
lines <- readRDS("../pct-lsoa/Data/03_Intermediate/l_Camb.Rds")
lines <- lines[,1:3]
lines <- lines[1:500,] #test subset
lines <- lines[which(lines$lsoa1 != lines$lsoa2), ]

#Simple
#print("single")
#Sys.time()
#single <- line2route(l = lines, route_fun = route_cyclestreet, plan = "quietest", base_url = "http://pct.cyclestreets.net/api/")
#Sys.time()

print("paralell")
Sys.time()
#Parameters
cores_spare <- 4 #Number of cores to NOT use, to prevent computer grinding to a halt

#Libraries
library(doParallel)

#Code
cores_run <- detectCores() - cores_spare
batch_size <- ceiling(nrow(lines)/cores_run)
cl <- makeCluster(cores_run)
registerDoParallel(cl)

foreach(i = 1:cores_run) %dopar% {
  l_start <- as.integer(1 + (i-1)*batch_size)
  if(i*batch_size < nrow(lines)){
    l_fin <- as.integer(i*batch_size)
    }else{
    l_fin <- as.integer(nrow(lines))
  }
  lines_sub <- lines[c(l_start:l_fin),]
  rq <- line2route(l = lines_sub, route_fun = route_cyclestreet, plan = "quietest", base_url = "http://pct.cyclestreets.net/api/")
  saveRDS(rq,file = paste0("../pct-lsoa/Data/03_Intermediate/temp/rq_batch_",i,".Rds"))
  remove(rq)
  return(i)
}
  
Sys.time()

#lines_sub <- lines[(1 + (i-1)*batch_size):(i*batch_size),]

# generate the fastest routes

#saveRDS(rq,file ="../pct-bigdata/msoa_rerun/oldway/rq_EW_20-30.Rds")


#rf = line2route(l = lines, route_fun = route_cyclestreet, plan = "fastest", base_url = "http://pct.cyclestreets.net/api/")
#saveRDS(rf,file ="../pct-bigdata/msoa_rerun/oldway/rf_EW_20-30.Rds")

#saveRDS(lines, file ="../pct-bigdata/msoa_rerun/oldway/lines_missing_2.Rds")