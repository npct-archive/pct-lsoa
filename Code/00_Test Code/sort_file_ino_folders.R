my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}
j = 1
batch = 1000

for(i in  1:254401){
  if(i %% batch == 0){j = j + 1}
  my.file.rename(from = paste0("D:/Git/pct-lsoa/Data/03_Intermediate/temp/rasters/indiv/rf-nat-l3-",i,".tif"),
                 to = paste0("D:/Git/pct-lsoa/Data/03_Intermediate/temp/rasters/indiv-batch-l3/",j,"/rf-nat-l3-",i,".tif"))
}


