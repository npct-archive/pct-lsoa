#Route routes using the groupsin functions

#Parameters
name = "rf_nat_4plus_a4" #Output file name


#Inputs
routes = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_4plus.Rds")
flow_data = readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")

#Prep and Join data
flow_data = flow_data[,c("id","bicycle_16p")]
flow_data = flow_data[flow_data$bicycle_16p > 0,]
routes = routes[routes$ID %in% flow_data$id,]
routes@data = left_join(routes@data, flow_data, by = c("ID" = "id"))
routes@data = routes@data[,c("ID","bicycle_16p")]
names(routes@data) = c("id","bike")
remove(flow_data)

#Start Grouping
source("../pct-lsoa-test/R/grouping_functions.R")

#Use Grid/Bearing Approach with a 1k grid
stage = 1
groups_1 = grouplines_GrdBear(routes, group_no = 1, grid_size = 1000, attempts = 2)
unfin = groups_1$id_old[is.na(groups_1$group)]
routes_unfin = routes[routes$id %in% unfin,]
nxtgrp = max(groups_1$group[!is.na(groups_1$group)]) + 1
groups_1 = groups_1[!is.na(groups_1$group),]
groups_1 = groups_1[,c("id_old","id_new","group")]
#saveRDS(groups_1,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1
#remove(groups_1)

#Use Grid/Bearing Approach with a 10k grid
groups_2 = grouplines_GrdBear(routes_unfin, group_no = nxtgrp, grid_size = 10000, attempts = 20)
unfin = groups_2$id_old[is.na(groups_2$group)]
routes_unfin = routes[routes$id %in% unfin,]
nxtgrp = max(groups_2$group[!is.na(groups_2$group)]) + 1
groups_2 = groups_2[!is.na(groups_2$group),]
groups_2 = groups_2[,c("id_old","id_new","group")]
#saveRDS(groups_2,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Use Grid Appraoch with a 20k grid
groups_3 = grouplines_grid(routes_unfin,nxtgrp,20000)
unfin = groups_3$id_old[is.na(groups_3$group)]
routes_unfin = routes[routes$id %in% unfin,]
nxtgrp = max(groups_3$group[!is.na(groups_3$group)]) + 1
groups_3 = groups_3[!is.na(groups_3$group),]
groups_3 = groups_3[,c("id_old","id_new","group")]
#saveRDS(groups_3,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#Use complete approach with small batch sizes
groups_4 <- grouplines_complete(routes_unfin, nxtgrp, 2000)
summary(is.na(groups_4$nrow)) #Should be no ungrouped lines
nxtgrp = max(groups_4$nrow[!is.na(groups_4$nrow)]) + 1
groups_4 = groups_4[!is.na(groups_4$nrow),]
names(groups_4) = c("id_new","group","id_old")
groups_4 = groups_4[,c(3,1,2)]
#saveRDS(groups_4,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
stage = stage + 1

#clean up
groups_all = rbind(groups_1,groups_2,groups_3,groups_4)
saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
remove(groups_1,groups_2,groups_3,groups_4)

summary(is.na(groups_all$group))

for(l in 1:10){
  print(paste0("Attempting to remove small groups, attempt ", l," at ",Sys.time()))
  #We have now grouped all the lines but many lines will have been grouped poorly
  groups_summary = as.data.frame(table(groups_all$group))
  groups_summary$Var1 = as.integer(levels(groups_summary$Var1))[groups_summary$Var1] #as.integer(groups_summary$Var1)
  groups_all$group = as.integer(groups_all$group)
  #Work out how many lines are poorly grouped
  freq_summary = data.frame(freq = unique(groups_summary$Freq), sum = as.integer(0))
  for(j in 1:nrow(freq_summary)){
    freq_summary$sum[j] = sum(groups_summary$Freq[groups_summary$Freq == freq_summary$freq[j]])
  }
  #We don't want to rerun loads of lines so work out threshold for 5000 lines
  threshold = 1
  for(k in 1:400){
    sum = sum(freq_summary$sum[freq_summary$freq <= threshold])
    if(sum > 5000){
      break()
    } else {
      threshold = threshold + 1
    }
  }
  smallgroups = groups_summary[groups_summary$Freq <= threshold,]
  regroup = groups_all[groups_all$group %in% smallgroups$Var1,]
  routes_regroup = routes[routes$id %in% regroup$id_old,]
  print(paste0("There currently ",length(unique(regroup$group))," groups"))

  #Use complete approach with small batch sizes
  groups <- grouplines_complete(routes_regroup, nxtgrp, 6000)
  summary(is.na(groups$nrow)) #Should be no ungrouped lines
  nxtgrp = max(groups$nrow[!is.na(groups$nrow)]) + 1
  groups = groups[!is.na(groups$nrow),]
  names(groups) = c("id_new","group","id_old")
  groups = groups[,c(3,1,2)]
  saveRDS(groups,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
  stage = stage + 1
  print(paste0("Replaced with ",length(unique(groups$group))," new groups"))

  #Update Groups_all
  groups_all$id_old = as.character(groups_all$id_old )
  groups$id_old = as.character(groups$id_old )

  for(i in 1:nrow(groups)){
    groups_all$group[groups_all$id_old == groups$id_old[i]] <- groups$group[i]
  }

  saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_p",stage,".Rds"))
  stage = stage + 1
  groups_summary = as.data.frame(table(groups_all$group))
  hist(groups_summary$Freq)
  print(paste0("groups ",length(unique(groups_all$group))))

}

saveRDS(groups_all,paste0("../pct-lsoa/Data/03_Intermediate/groups/",name,"_finished.Rds"))
# stage 69 1226 groups
#stage 90 1253 groups



