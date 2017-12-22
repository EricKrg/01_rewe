# Filename: rewe_scraping.R (2017-11-13)
#
# TO DO: Read REWE locator page
#
# Author(s): Jannes Muenchow
#
#**********************************************************
# CONTENTS-------------------------------------------------
#**********************************************************
#
# 1. ATTACH PACKAGES AND DATA
# 2. SCRAPE REWE LOCATIONS
#
#**********************************************************
# 1 ATTACH PACKAGES AND DATA-------------------------------
#**********************************************************

# attach packages
library("RCurl")
library("XML")
library("stringr")

#**********************************************************
# 2 SCRAPE REWE LOCATIONS----------------------------------
#**********************************************************

# pcs <- c("90403", "90417", "85221")
url = "https://marktsuche.rewe.de/"
#browseURL(url)

# Load the webpage into R
doc = getURL(url, ssl.verifypeer = FALSE)
# parse it
# find the root node
root = xmlRoot(htmlParse(doc))
names(root)
root["comment"]
root["head"]
# extract REWE markets
adds = xpathSApply(root[["body"]],
                   "//div[@class = 'gmap']/following-sibling::script",
                   saveXML)
# saveXML saves the result as a character string
class(adds)

#NEW
readable<- read_lines(adds)
#
city <- grep("city:",readable,value = T)
street <- grep("street:", readable, value = T)
lat <- grep("lat:",readable,value = T)
lon <- grep("lon:",readable, value = T)

#
clean <- function(input){
  temp <- gsub(" |'|,","",input)
  return(gsub(".*[a-z]:","",temp))
}
city <-clean(city)
street <- clean(street)
lat <- clean(lat)
lon <- clean(lon)

stores_all <- data.frame(city,street,lat = as.numeric(lat),lon = as.numeric(lon))

#remove all but stores_all
rm(list=setdiff(ls(), "stores_all"))
setwd("C:/Users/Eric/Desktop/404/01_rewe/")
save.image("./images/web_data.Rdata")



