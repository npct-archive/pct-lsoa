routes <- readRDS(choose.files())
names(routes)
routes <- routes[is.na(routes$length),]
nrow(routes)
plot(routes)

library(leaflet)

leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addPolylines(data = routes, color ="green", weight = 1) 
