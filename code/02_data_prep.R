# Filename: 02_data_prep.R (2017-12-15)
#
#
# Author: Eric Krueger
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. USE WEB-SCRAPE DATA AND SUBSET HOME PLZ
# 3. STORE FILTERING AND PLOTTING
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
pacman::p_load(sp, sf, rgdal, tidyverse, leaflet,TSP, fields, dismo, mapview)

##################
#download data----
##################

setwd("C:/Users/Eric/Desktop/404/01_rewe/data")
download.file( {"http://www.suche-postleitzahl.org/downloads?download_file=plz-gebiete.shp.zip"
  }, destfile = "ger_zip.zip" )
ger_plz <- unzip("ger_zip.zip") #unzip shp
plz <- readOGR(ger_plz[1], layer ="plz-gebiete") #read plz areas
plz <- st_as_sf(plz) #to sf for data handling

############################
#web scraping data---------
###########################
setwd("C:/Users/Eric/Desktop/404/01_rewe/")
dir_main <- "."
dir_ima = file.path(dir_main, "images")
load(file.path(dir_ima, "web_data.Rdata")) #img from web scraping script (01)

#make it spatial
coordinates(stores_all) = c("lon", "lat")
stores_all<-st_as_sf(stores_all)
# filter plz to home plz
home <- "60306" #80331"#"10119"#10117"#"07743"#"04103"#"
home_plz <- subset(plz,plz == home )
ngh1 <- st_intersection(plz,home_plz)
#1st neighbors - initial ngh
ngh1 <- subset(plz, plz %in% ngh1$plz)

#filter stores
prj <- st_crs(ngh1) #get the crs from ngh1 object
stores_all <- st_set_crs(stores_all,prj) #set crs

#plot all stores - 3057
setwd("C:/Users/Eric/Desktop/404/01_rewe/figures/")
png("home.png")
g <- gmap(st_coordinates(stores_all))
p <- Mercator(st_coordinates(stores_all))
plot(g,inter = TRUE)
points(p, pch = 19, cex = 0.45, col = "indianred")
dev.off()
#####################
#Store filtering----
####################

#Find neighbor plz and requiered amount of stores within plz
find_ngh <- function(n){  # n - neighbor generation
  temp <- st_union(n, by_feature = T)
  temp2 <- st_intersection(temp,plz)
  temp3 <-subset(plz, plz %in% temp2$plz.1)
  return(temp3)
}
#filter stores in home plz - autom. builts up required ngh objects and requiered
#number of stores  --> makes up scaling possible
i = 1
j = TRUE
tresh <- 15
ngh_list <- list()
while(j == TRUE){
  ngh_list[[1]] <- ngh1
  ngh_list[[i+1]] <- find_ngh(ngh_list[[i]])
  temp <- ngh_list[[i+1]]
  stores_home <- st_intersection(temp,stores_all)
  if(nrow(stores_home) >= tresh){
    print("done")
    j = FALSE
  }
  i = i + 1
}

ngh_fun <- function(){  #dynamically plots nghs
  base <- mapview(stores_home) %>%
    addFeatures(ngh1, color = "red", group = "ngh1") %>%
    addLayersControl(options = layersControlOptions(collapsed = FALSE),
                     overlayGroups = c(paste0("ngh", 1:length(ngh_list))))
  map_list <- list()
  col.list<-palette(rainbow(length(ngh_list)))
  for (i in 2:length(ngh_list)){
    map_list[[i]] <- base %>% addFeatures(ngh_list[[i]],group = paste0("ngh",i)
                                          ,color = col.list[i])
    if(length(map_list) > 2){
      map_list[[i]] <- map_list[[i-1]] %>% addFeatures(ngh_list[[i]],
                                                       group = paste0("ngh",i)
                                            ,color = col.list[i])
    }
  }

  map <- map_list[length(ngh_list)]
  return(map)
}
home_plot <- ngh_fun()
home_plot[[1]]
###
#save rdata for next step----
###

rm(list=setdiff(ls(), c("stores_all","stores_home","home_plot","plz")))
setwd("C:/Users/Eric/Desktop/404/01_rewe/")
save.image("./images/stores.Rdata")
gc()
