#test union method
library(raster)
library(sp)


poly_master <- gBuffer(routes_master[1:100,], byid = T, width = 11)
poly <- gSimplify(poly_master, tol = 0.1, topologyPreserve=T)
poly2 <- gSimplify(poly_master, tol = 0.5, topologyPreserve=T)
poly <- gBuffer(poly, byid=TRUE, width=0)
sum(gIsValid(poly2, byid=TRUE)==FALSE) #should equal 0


# any bad polys?
poly5 <- gSimplify(routes_poly, tol = 0.1)
plot(poly[3,])


gIsValid(poly)
gIsSimple(poly, byid = T)

y <- rgeos::union(poly[1:5,],poly[1:5,])


x <- raster::union(poly2[1:5,])
plot(poly2[1:5,])
x = rgeos::gIntersection(poly2[1:5,], poly2[1:5,], byid = T)
plot(x, add = T, col = 1:25)
