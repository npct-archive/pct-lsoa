#This code creates LSOA desire lines based on subsetting rules

# Libraries
library(sp)
library(stplanr)

#Parameters
max_length <- 20    #Maximum lenght of lines in km
min_length <- 0     #Minimum lenght of lines in km
min_people <- 10    #Minimum number of communters in a line
region <- "Camb"    #Subset to an area for testing set to NULL for national build, uses text search on LSOA names

#Inputs
flow <- readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")
#flow <- flow[,c("lsoa1","lsoa2","id","all_16p")] # Remove unneeded data from the lines 

cents <- readRDS("../pct-lsoa/Data/02_Input/LSOA_cents.Rds")

#Subset
flow <- flow[flow$all_16p >= min_people ,]
if(!is.null(region)){cents = cents[grep(pattern = region, x = cents$name),]}

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

hist(geodist, breaks = 0:800)
flow$dist = geodist
flow = flow[!is.na(flow$dist),] # destinations with no matching cents - remove
flow = flow[flow$dist >= min_length,] # subset based on euclidean distance
flow = flow[flow$dist < max_length,]

lines = od2line2(flow = flow, zones = cents)
lines = SpatialLinesDataFrame(sl = lines, data = flow)
proj4string(lines) <- CRS("+proj=longlat +init=epsg:3857")


#Save the lines file name based on if national build or not
if(is.null(region)) {
  saveRDS(lines,"../pct-lsoa/Data/03_Intermediate/l_nat.Rds")
} else {
  saveRDS(lines,paste0("../pct-lsoa/Data/03_Intermediate/l_",region,".Rds"))
}

 


