# Sort lines into rasterisation regions

library(rgdal)
library(sp)

#Inputs
lines = readRDS("../pct-lsoa/Data/03_Intermediate/lines/l_nat.Rds")
regions = readOGR(dsn =  "../pct-lsoa/Data/01_Raw/regions", layer = "rasterzones")

#Code
regions <- spTransform(regions, CRS(proj4string(lines)))

for(i in regions$Id){
  regions_sub = regions[regions$Id == i,]
  lines_sub = lines[regions_sub,]
  saveRDS(lines_sub,paste0("../pct-lsoa/Data/03_Intermediate/lines/l_nat_region",i,".Rds"))
  lines = lines[!(lines$id %in% lines_sub$id), ]
  print(paste0("region ",i," saved"))
}
saveRDS(lines,paste0("../pct-lsoa/Data/03_Intermediate/lines/l_nat_region",i+1,".Rds"))
