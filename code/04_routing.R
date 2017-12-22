###
#routing
###

pacman::p_load(sp, sf, rgdal, tidyverse, leaflet,TSP, fields, osmdata, Rcpp,
               stplanr, ggmap, raster, dodgr, osmar)

#data
setwd("C:/Users/Eric/Desktop/404/01_rewe/")
dir_main <- "."
dir_ima = file.path(dir_main, "images")
load(file.path(dir_ima, "tsp.Rdata"))

#store data_frame
route <- coords.df[path,]
route_table <- data.frame(stores_home$plz, stores_home$city,
                          stores_home$street)
#do the routing, stplanr vers. --
temp_list <- list()
for (i in 1:nrow(route)){
  if (i == nrow(route)){
    print("done")
    break
  }
  temp_list[i] <- route_osrm(from = route[i,], to = route[i+1,])
}
all_routes <- do.call(rbind.SpatialLines, temp_list)

###
#dodgr routing---
###


str_net <- dodgr_streetnet(pts = route[1:nrow(route)-1,], expand = 0.5, quiet = F) #hmm uneffiecent
save.image("./images/route.Rdata") # save str_net
 # dir_ima = file.path(dir_main, "images")
 # load(file.path(dir_ima, "route.Rdata")) #load if needed
graph <- weight_streetnet (str_net, wt_profile = "goods")

#calculate all paths
dp <- dodgr_paths(from =route, to = route,
                  wt_profile = "goods", vertices = T)
verts <- dodgr_vertices(graph)

i <- 1
temp_list <- list()
for (i in 1:length(dp)){
  if(i == length(dp)-1){ #last segment
    route1 <- verts[match (dp[[i]][[i+1]], verts$id),]
    coordinates(route1) <- c("x","y")
    temp_list[[i]]<- SpatialLines(list(Lines(list(Line(coordinates(route1))),"X")))
    print(i)
    print("done")
    break
  }
  route1 <- verts[match (dp[[i]][[i+1]], verts$id),]
  coordinates(route1) <- c("x","y")
  temp_list[[i]]<- SpatialLines(list(Lines(list(Line(coordinates(route1))),"X")))
  print(i)
  print(i+1)
}
all_routes2 <- do.call(rbind.SpatialLines, temp_list)


#plot the routes
routing <- leaflet() %>% addTiles() %>%
  addPolylines(data = all_routes2,color = "green", group = "dodgr") %>%
  addPolylines(data = path_line, group = "direct") %>%
  addPolylines(data = all_routes, color = "red", group = "stplanr") %>%
  addCircles(lat = route$Y, lng = route$X, color = "black") %>%
  addProviderTiles(provider = providers$Hydda) %>%
  addLayersControl(overlayGroups = c("direct","stplanr","dodgr"),
                   options = layersControlOptions(collapsed = FALSE))

routing
#

save.image("./images/all.Rdata")
gc()
