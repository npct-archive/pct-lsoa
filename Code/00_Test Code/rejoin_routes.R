#Varient of the rejoing code from create routes fro when batches are different

route_type <- "fastest" #fastest or quietest
file_name <- "nat_less3p" #Any additonal naming you wish to give the output file, e.g. region name or nat
nbatch <- 595

library(maptools)

#Rejoin the files
stack <- readRDS(paste0("../pct-lsoa/Data/03_Intermediate/temp/r",substr(route_type, 1, 1),"_",file_name,"_",1,"of",nbatch,".Rds"))

for(i in 2:nbatch){
    file_next <- readRDS(paste0("../pct-lsoa/Data/03_Intermediate/temp/r",substr(route_type, 1, 1),"_",file_name,"_",i,"of",nbatch,".Rds"))
    file_next$error <- NULL
    if(ncol(file_next) == 14){
      if(identical(file_next$id,file_next$ID)){
        file_next$id <- NULL
      } else {
        print(paste0("ID to id check is ",identical(file_next$id,file_next$ID)," for file ",i))
        break()
      }
    
    }
    #names(file_next)
    stack <- spRbind(stack, file_next)
    print(paste0("Done file ",i," at ", Sys.time()))
}
  
#
nrow(stack) == nrow(lines)
#summary(stack@data$ID == lines@data$id)
saveRDS(stack,file = paste0("../pct-lsoa/Data/03_Intermediate/routes/r",substr(route_type, 1, 1),"_",file_name,".Rds"))