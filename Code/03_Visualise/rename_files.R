#Rename Files
#Changes the name of output rasters from abc-12 to abc-0012 because ArcGIS is stupid and cant sort them numerically otherwise

#############
# Warning - make Sure there is nothing else (files or folders) in the folder before runing this script
#############


folder <- "E:/Users/Malcolm/Git/pct-lsoa/Data/test" #Where are the files to rename?
common_start <- "abc-" #Any test that appears at the start of every file
common_end   <- ".txt" #Any test that appears at the start of every file e.g. file extension

files <- list.files(folder, full.names = T) #,pattern=”searchPattern”)


sapply(files,FUN=function(eachPath){
  
  #Take of the common start and end
  crop <- sub(paste0(folder,"/",common_start),"",eachPath)
  crop <- sub(common_end,"",crop)
  #Remove any -number
  split <- unlist(strsplit(crop, "-"))
  numb <- split[1]
  #Add in the zeros
  if(nchar(numb)==1){
    out <- paste0("000",numb)
  } else if (nchar(numb)==2){
    out <- paste0("00",numb)
  } else if (nchar(numb)==3){
    out <- paste0("0",numb)
  } else {
    out <- numb
  }
  #Rebuild the file name
  if(length(split)==2){
    fin <- paste0(folder,"/",common_start,out,"-",split[2],common_end)
  } else {
    fin <- paste0(folder,"/",common_start,out,common_end)
  }
  
  #Rename the file  
  file.rename(from=eachPath,to=fin)
})
