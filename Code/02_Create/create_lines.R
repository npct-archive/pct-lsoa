#This code creates LSOA desire lines based on subsetting rules

# Libraries
library(sp)



#Parameters
max_length <- 20  #Maximum lenght of lines
min_people <- 0   #Minimum number of communters in a line 



#Inputs
flow <- readRDS("../pct-lsoa/Data/02_Input/LSOA_flow.Rds")
cents <- readRDS("../pct-lsoa/Data/02_Input/LSOA_cents.Rds")

#Subset flow to min_people
flow <- flow[flow$all_16p >= min_people ,]



