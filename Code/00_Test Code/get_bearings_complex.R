#splits lines into groups based on bearing


library(stplanr)
library(dplyr)
# Get bearing of line

lines = readRDS("../pct-lsoa/Data/03_Intermediate/lines/l_nat_4plus.Rds")
size_limit = 100000
#Check if too many lines to do at once
if(nrow(lines) < size_limit){
  goes = 1
} else {
  goes = ceiling(nrow(lines)/size_limit)
}
#Set up data
data = data.frame(id = as.character(),dist=as.numeric(),bearing=as.integer())

#Do bearings in chunks becuase fails over 100,000
for(v in 1:goes){
  if((v*size_limit)>=nrow(lines)){l_max = nrow(lines)}else{l_max = (v*size_limit)}
  lines_part = lines[(1 + (v-1)*size_limit):l_max,]
  bearing <- round(line_bearing(lines_part),0)
  for(h in 1:length(bearing)){
    if(bearing[h]<=0){bearing[h]=bearing[h]+180}
    if(bearing[h]==0){bearing[h]=180} # first if intorduces some 0 so remove them
  }
  data_part = lines_part@data
  data_part$bearing <- bearing
  data = rbind(data,data_part)
  print(paste0("Done  bearing loop ",v," of ",goes))
}

#data_backup = data
data$dist <- NULL

ngroups =  18 #must be a whole fraction of 180
nperg = ceiling(180/ngroups)

groups = data.frame(bearing = c(1:180), group = rep(1:ngroups,each=nperg))
data = left_join(data,groups, by = "bearing")
data$group2 <- as.character("")
summary(is.na(data$group)) # Check for NA

for(j in 1:ngroups){
  data_sub = data[data$group == j,]
  for(k in 1:4){
    data_sub2 <- data_sub[seq.int(from = k, to = nrow(data_sub), 6),] # Subset evenly across the country
    data_sub2$group2 <- as.character(paste0(j,"-",k))
    #saveRDS(data_sub2, paste0("../pct-lsoa/Data/03_Intermediate/groups/l_nat_4plus_Bgrp_",j,"-",k,".Rds"))
    for(l in 1:nrow(data_sub2)){
      data$group2[data$id == data_sub2$id[l]] <- data_sub2$group2[l]
    }
  }
  print(paste0("Done grouping loop ",j," of ",ngroups))
}
summary(is.na(data$group2))
saveRDS(data, paste0("../pct-lsoa/Data/03_Intermediate/groups/l_nat_4plus_Bgrp.Rds"))
