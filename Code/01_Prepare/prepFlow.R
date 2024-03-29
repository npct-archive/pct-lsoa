#THis code prepares the LSOA-LSOA flow data from zipped csv to Rds

#Inputs
unzip("../pct-lsoa/Data/01_Raw/Flow/WM12EW[CT0489]_lsoa.zip", exdir = "../pct-lsoa/Data/03_Intermediate/temp")
cents <- readRDS("../pct-lsoa/Data/02_Input/LSOA_cents.Rds")

#Parameters
max_length <- 20    #Maximum lenght of lines in km
min_people <- 1

# Libraries
library(sp)
library(stplanr)

#Code
flow <- readr::read_csv("../pct-lsoa/Data/03_Intermediate/temp/WM12EW[CT0489]_lsoa.csv")
do.call(file.remove, list(list.files("../pct-lsoa/Data/03_Intermediate/temp/", full.names = TRUE)))

names <- c("Area of usual residence", "Area of Workplace", "AllMethods_AllSexes_Age16Plus", "AllMethods_AllSexes_Age16to24", 
          "AllMethods_AllSexes_Age25to34", "AllMethods_AllSexes_Age35to49", "AllMethods_AllSexes_Age50to64", "AllMethods_AllSexes_Age65to74", 
           "AllMethods_AllSexes_Age75Plus","AllMethods_Male_Age16Plus",   "AllMethods_Male_Age16to24",  "AllMethods_Male_Age25to34",     
           "AllMethods_Male_Age35to49", "AllMethods_Male_Age50to64",  "AllMethods_Male_Age65to74", "AllMethods_Male_Age75Plus",
           "AllMethods_Female_Age16Plus", "AllMethods_Female_Age16to24", "AllMethods_Female_Age25to34", "AllMethods_Female_Age35to49",
           "AllMethods_Female_Age50to64", "AllMethods_Female_Age65to74", "AllMethods_Female_Age75Plus", "WorkAtHome_AllSexes_Age16Plus",
           "Underground_AllSexes_Age16Plus", "Train_AllSexes_Age16Plus", "Bus_AllSexes_Age16Plus",  "Taxi_AllSexes_Age16Plus",
           "Motorcycle_AllSexes_Age16Plus", "CarOrVan_AllSexes_Age16Plus", "Passenger_AllSexes_Age16Plus", "Bicycle_AllSexes_Age16Plus",
           "Bicycle_AllSexes_Age16to24",   "Bicycle_AllSexes_Age25to34", "Bicycle_AllSexes_Age35to49", "Bicycle_AllSexes_Age50to64",    
           "Bicycle_AllSexes_Age65to74", "Bicycle_AllSexes_Age75Plus", "Bicycle_Male_Age16Plus",   "Bicycle_Male_Age16to24",  "Bicycle_Male_Age25to34",  
           "Bicycle_Male_Age35to49",  "Bicycle_Male_Age50to64",  "Bicycle_Male_Age65to74",  "Bicycle_Male_Age75Plus",        
           "Bicycle_Female_Age16Plus",  "Bicycle_Female_Age16to24",  "Bicycle_Female_Age25to34",  "Bicycle_Female_Age35to49",   
           "Bicycle_Female_Age50to64",  "Bicycle_Female_Age65to74", "Bicycle_Female_Age75Plus", "OnFoot_AllSexes_Age16Plus", "OtherMethod_AllSexes_Age16Plus")


flow <- flow[,names]

flow <- dplyr::rename(flow,
                      lsoa1 = `Area of usual residence`, lsoa2 = `Area of Workplace`, all_16p = `AllMethods_AllSexes_Age16Plus`,  
                      all_16_24 = `AllMethods_AllSexes_Age16to24`, all_25_34 =`AllMethods_AllSexes_Age25to34`,  all_35_49 =`AllMethods_AllSexes_Age35to49`, 
                      all_50_64 =`AllMethods_AllSexes_Age50to64`,all_65_74 =`AllMethods_AllSexes_Age65to74`, all_75p =`AllMethods_AllSexes_Age75Plus`,
                      male_16p = `AllMethods_Male_Age16Plus`, male_16_24 = `AllMethods_Male_Age16to24`, male_25_35 = `AllMethods_Male_Age25to34`,     
                      male_35_49 = `AllMethods_Male_Age35to49`, male_50_64 = `AllMethods_Male_Age50to64`, male_65_74 = `AllMethods_Male_Age65to74`,
                      male_75p = `AllMethods_Male_Age75Plus`, female_16p = `AllMethods_Female_Age16Plus`, female_16_24 = `AllMethods_Female_Age16to24`,
                      female_25_34 = `AllMethods_Female_Age25to34`, female_35_49 = `AllMethods_Female_Age35to49`, female_50_64 = `AllMethods_Female_Age50to64`,
                      female_65_74 = `AllMethods_Female_Age65to74`, female_75p = `AllMethods_Female_Age75Plus`, workathome = `WorkAtHome_AllSexes_Age16Plus`,
                      underground = `Underground_AllSexes_Age16Plus`, train = `Train_AllSexes_Age16Plus`, bus = `Bus_AllSexes_Age16Plus`,
                      taxi = `Taxi_AllSexes_Age16Plus`, motorcycle = `Motorcycle_AllSexes_Age16Plus`, carorvan = `CarOrVan_AllSexes_Age16Plus`,
                      passenger = `Passenger_AllSexes_Age16Plus`, bicycle_16p = `Bicycle_AllSexes_Age16Plus`, bicycle_16_24 = `Bicycle_AllSexes_Age16to24`,   
                      bicycle_25_34 = `Bicycle_AllSexes_Age25to34`, bicycle_35_49 = `Bicycle_AllSexes_Age35to49`, bicycle_50_64 = `Bicycle_AllSexes_Age50to64`,    
                      bicycle_65_74 = `Bicycle_AllSexes_Age65to74`, bicycle_75p = `Bicycle_AllSexes_Age75Plus`, bicycle_male_16p = `Bicycle_Male_Age16Plus`,        
                      bicycle_male_16_24 = `Bicycle_Male_Age16to24`,  bicycle_male_25_34 = `Bicycle_Male_Age25to34`, bicycle_male_35_49 = `Bicycle_Male_Age35to49`,        
                      bicycle_male_50_64 = `Bicycle_Male_Age50to64`, bicycle_male_65_74 = `Bicycle_Male_Age65to74`, bicycle_male_75p = `Bicycle_Male_Age75Plus`,        
                      bicycle_female_16p = `Bicycle_Female_Age16Plus`, bicycle_female_16_24 =`Bicycle_Female_Age16to24`,  bicycle_female_25_34 =`Bicycle_Female_Age25to34`,      
                      bicycle_female_35_49 =`Bicycle_Female_Age35to49`, bicycle_female_50_64 =`Bicycle_Female_Age50to64`,  bicycle_female_65_74 =`Bicycle_Female_Age65to74`,      
                      bicycle_female_75p =`Bicycle_Female_Age75Plus`,onfoot = `OnFoot_AllSexes_Age16Plus`, other = `OtherMethod_AllSexes_Age16Plus`)

nrow(flow)
flow <- flow[which(flow$lsoa1 != flow$lsoa2),]
flow <- flow[flow$all_16p >= min_people,]
nrow(flow)

# Generate the Lines
o <- flow$lsoa1 %in% cents$code
d <- flow$lsoa2 %in% cents$code
flow <- flow[o & d, ] # subset OD pairs with o and d in study area

omatch = match(flow$lsoa1, cents$code)
dmatch = match(flow$lsoa2, cents$code)

cents_o = cents@coords[omatch,]
cents_d = cents@coords[dmatch,]
summary(is.na(cents_o)) # check how many origins don't match
summary(is.na(cents_d))
geodist = geosphere::distHaversine(p1 = cents_o, p2 = cents_d) / 1000 # assign euclidean distanct to lines (could be a function in stplanr)
summary(is.na(geodist))

hist(geodist)
flow$dist = geodist
flow = flow[!is.na(flow$dist),] # destinations with no matching cents - remove
flow = flow[flow$dist < max_length,]
hist(flow$dist)

#Dist AtoB == BtoA so subsetting reduces no of lines that have to be checked
flow_dup <- flow[duplicated(flow$dist) | duplicated(flow$dist, fromLast=TRUE),]
flow_nodup <- flow[!duplicated(flow$dist) & !duplicated(flow$dist, fromLast=TRUE),]
flow_nodup$is_two_way <- FALSE
flow_nodup$dist <- NULL 
nrow(flow) == nrow(flow_dup) + nrow(flow_nodup)
flow_dup <- onewayid(flow_dup, attrib = 3:54, id1 ="lsoa1", id2 = "lsoa2")
flow_all <- rbind(flow_dup, flow_nodup)
flow_all$id <- paste(pmin(flow_all$lsoa1, flow_all$lsoa2), pmax(flow_all$lsoa1, flow_all$lsoa2)) #Create ID
class(flow_all)
flow_all <- as.data.frame(flow_all)
row.names(flow_all) <- c(1:nrow(flow_all))
saveRDS(flow_all,"../pct-lsoa/data/02_Input/LSOA_flow.Rds")