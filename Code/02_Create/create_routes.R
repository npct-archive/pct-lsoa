#This code takes straight liens and produces routes

#Inputs
lines <- readRDS("../pct-lsoa/Data/03_Intermediate/lines/l_nat_4plus.Rds")

#Parameters
route_type <- "fastest" #fastest or quietest
#lines <- lines[1:100,] # test subset
size_limit <- 5000 #maximum size of a batch
file_name <- "nat_4plus" #Any additonal naming you wish to give the output file, e.g. region name or nat
delete_temp <- FALSE #If temp files are to  be deleted
#libaries
library(maptools)
library(stplanr)

#Code
nbatch <- ceiling(nrow(lines)/size_limit)

for(i in 1:nbatch){
  l_start <- as.integer(1 + (i-1)*size_limit)
  if(i*size_limit < nrow(lines)){
    l_fin <- as.integer(i*size_limit)
  }else{
    l_fin <- as.integer(nrow(lines))
  }
  lines_sub <- lines[c(l_start:l_fin),]
  
  routes <- line2route(l = lines_sub, route_fun = route_cyclestreet, plan = route_type, base_url = "http://pct.cyclestreets.net/api/")
  routes@data <- routes@data[,!names(routes@data) %in% c("plan","start","finish")]
  routes@data$ID <- lines_sub@data$id
  saveRDS(routes,file = paste0("../pct-lsoa/Data/03_Intermediate/temp/r",substr(route_type, 1, 1),"_",file_name,"_",i,"of",nbatch,".Rds"))
  print(paste0("Batch ",i," of ",nbatch," finished at ",Sys.time()))
  
}
#Rejoin the files
stack <- readRDS(paste0("../pct-lsoa/Data/03_Intermediate/temp/r",substr(route_type, 1, 1),"_",file_name,"_",1,"of",nbatch,".Rds"))
if(nbatch == 1){
  #No nothing
} else {
  
  for(i in 2:nbatch){
    file_next <- readRDS(paste0("../pct-lsoa/Data/03_Intermediate/temp/r",substr(route_type, 1, 1),"_",file_name,"_",i,"of",nbatch,".Rds"))
    stack <- spRbind(stack, file_next)
  }

}
nrow(stack) == nrow(lines)
summary(stack@data$ID == lines@data$id)
saveRDS(stack,file = paste0("../pct-lsoa/Data/03_Intermediate/routes/r",substr(route_type, 1, 1),"_",file_name,".Rds"))

#Clean up temp files
if(delete_temp == TRUE){
  for(i in 1:nbatch){
    file.remove(paste0("../pct-lsoa/Data/03_Intermediate/temp/r",substr(route_type, 1, 1),"_",file_name,"_",i,"of",nbatch,".Rds"))
  }
}





