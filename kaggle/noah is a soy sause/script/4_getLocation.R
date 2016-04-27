require(XML)
setwd("../Desktop")
load("dt_g_id.RData")
load("dt_zip.RData")
load("dt_location_id.RData")
require(data.table)
dt.location_id
dt.g_id
dt.zip


## refine the dt.zip
dt.zip
ls.xml.zip <- list()
z <- 1
for(loc in dt.zip$loc_name){
  path.zip <- paste0("http://api.geonames.org/search?name=", loc, "&country=AU&maxRows=1&username=noahhhhhh")
  xml.zip <- xmlParse(path.zip)
  ls.xml.zip[[loc]] <- xmlToList(xml.zip)
  print(paste(z, "in", nrow(dt.zip)))
}


## refine the dt.g_id
dt.g_id
ls.xml.g_id <- list()
g <- 1
for(loc in substr(dt.g_id$location_id, 6, nchar(dt.g_id$location_id))){
  path.g_id <- paste0("http://api.geonames.org/get?geonameId=", loc, "&username=noahhhhhh")
  xml.g_id<- xmlParse(path.g_id)
  ls.xml.g_id[[loc]] <- xmlToList(xml.g_id)
  print(paste(g, "in", nrow(dt.g_id)))
  g <- g + 1
}

