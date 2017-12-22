###
#03_TSP
###
pacman::p_load(sp, sf, rgdal, tidyverse, leaflet,TSP, fields)

#load data
setwd("C:/Users/Eric/Desktop/404/01_rewe/")
dir_main <- "."
dir_ima = file.path(dir_main, "images")
load(file.path(dir_ima, "stores.Rdata")) #from 02_Data.R

# stores the lon and lat data in a matrix
coords.df <- data.frame(st_coordinates(stores_home))
coords.mx <- data.matrix(coords.df)

# calculates the distance in kilometer
dist.mx <- rdist.earth(coords.df, miles=FALSE)

# creates a TSP object
TSP <- as.TSP(dist.mx)
tsp <- insert_dummy(TSP, label = "cut")

# the used methods
methods <- c("identity", "random", "nearest_insertion", "farthest_insertion", "cheapest_insertion",
             "arbitrary_insertion", "nn", "repetitive_nn", "two_opt")

set.seed(12345)
# greates the different tours
tours <- sapply(methods, FUN = function(m) solve_TSP(tsp,
                                                     method = m,
                                                     control = "repetitions"),
                simplify = FALSE)

#tour ranking
tour_ranking <- as.data.frame(x=sort(c(sapply(tours, tour_length))))
tour_ranking$method <- row.names(tour_ranking)

setwd("C:/Users/Eric/Desktop/404/01_rewe/figures")
png("tour_rank.png")
tour_ranking %>%
  ggplot(aes(reorder(method,`sort(c(sapply(tours, tour_length)))`),
             `sort(c(sapply(tours, tour_length)))`)) + geom_jitter(col = "blue")+
  coord_flip() +xlab("") + ylab("Tour lenght")
dev.off()

fast <- solve_TSP(tsp, method = tour_ranking$method[1] )# best tour with the best method
path <- cut_tour(fast, "cut")
path <- c(path, path[1])# start to end

slow <- solve_TSP(tsp, method = tour_ranking$method[nrow(tour_ranking)] )# best tour with the best method
path2 <- cut_tour(slow, "cut")
path2 <- c(path2, path2[1])# start to end

#best tour
path_line <- SpatialLines(list(Lines(list(Line(coords.df[path,])),ID = "1")))
#worst tour
path_line2 <- SpatialLines(list(Lines(list(Line(coords.df[path2,])),ID = "1")))

#plot tours

tours <- leaflet() %>% addTiles() %>%
  addPolylines(data = path_line, group = "best") %>%
  addCircles(data = stores_home, color = "black") %>%
  addPolylines(data = path_line2, color = "orange",group = "worst") %>%
  addProviderTiles(provider = providers$Hydda) %>%
  addLayersControl(baseGroups = c("best","worst"),
                   options = layersControlOptions(collapsed = FALSE))

tours
#save
rm(list=setdiff(ls(), c("stores_all","stores_home","coords.df",
                        "path","path_line", "tours","home_plot")))
setwd("C:/Users/Eric/Desktop/404/01_rewe/")
save.image("./images/tsp.Rdata")

gc()
