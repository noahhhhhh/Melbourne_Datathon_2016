# setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(XML)
load("dt_jobs.RData")
#######################################################################################
## clean raw location #################################################################
#######################################################################################
## location with  g_id
loc.id.proper <- dt.jobs$location_id[grepl("g_id:[[:digit:]]", dt.jobs$location_id)]
loc.id.proper.substr <- unique(substr(loc.id.proper, 6, nchar(loc.id.proper)))
ls.xml <- list()
for(loc in loc.id.proper.substr){
    path <- paste0("http://api.geonames.org/get?geonameId=", loc, "$maxRows=1&username=noahhhhhh")
    xml <- xmlParse(path)
    ls.xml[[loc]] <- xmlToList(xml)
}

## location with g_zip
loc.zip.proper <- dt.jobs$location_id[grepl("g_zip:", dt.jobs$location_id)]
loc.zip.proper.substr <- unique(as.numeric(gsub("[^\\d]+", "", loc.zip.proper, perl = T)))

ls.xml.zip <- list()
for(loc in loc.zip.proper.substr){
    path.zip <- paste0("http://api.geonames.org/postalCodeSearch?postalcode=", loc, "&maxRows=1&username=noahhhhhh")
    xml.zip <- xmlParse(path.zip)
    ls.xml.zip[[loc]] <- xmlToList(xml.zip)
}