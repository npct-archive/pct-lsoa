library(maptools)

# Replace mssing route data
routes_main = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_less3p.Rds")
routes_miss = readRDS("../pct-lsoa/Data/03_Intermediate/routes/rf_nat_redo.Rds")

#Updated routes_miss names
routes_miss$error <- NULL
names(routes_miss)[names(routes_miss)=="id"] <- "ID"

missing <- routes_main[is.na(routes_main$length),]
routes_miss <- routes_miss[routes_miss$ID %in% missing$ID,]
nrow(routes_miss)
nrow(missing)
summary(duplicated(routes_miss$ID))

nrow(routes_main)
routes_main <- routes_main[!(routes_main$ID %in% routes_miss$ID),]
nrow(routes_main)
#test_routes = routes_miss
main <- as.integer(rownames(routes_main@data))
nmain <- max(main)

#nmain <- length(routes_main@lines)
nmiss <- length(routes_miss@lines)
for (i in 1:nmiss) routes_miss@lines[[i]]@ID=as.character(nmain+i)
rownames(routes_miss@data) <- 1:nmiss + nmain
routes_main <- spRbind(routes_main, routes_miss)
test <- routes_main[is.na(routes_main$length),]
saveRDS(routes_main,"../pct-lsoa/Data/03_Intermediate/routes/rf_nat_less3p_fix.Rds")
plot(test)


##### End of Code





#names <- as.integer(rownames(routes_main@data))
#max(names)
#for (i in 1:n2) routes_miss@lines[[i]]@ID=as.character(nAL+i-1)
#rownames(routes_miss@data) <- as.character(1:nrow(routes_miss) + 1056370 + 1000)

#a <- rownames(routes_main@data)
#b <- rownames(routes_miss@data)
#c <- c(a,b)
#summary(duplicated(c))


#test <- rownames(routes_main) %in% rownames(routes_miss)
#test <- test + nrow(routes_main)
#row.names(routes_miss) <- as.character(test)

#foo <- routes_miss$ID[routes_miss$ID %in% routes_main$ID]


#library(maptools)
#routes_main2 <- spRbind(routes_main, routes_miss)
