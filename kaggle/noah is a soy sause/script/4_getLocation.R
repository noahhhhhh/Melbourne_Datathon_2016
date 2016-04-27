# setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(XML)
require(caret)
load("dt_jobs.RData")
#######################################################################################
## clean raw location #################################################################
#######################################################################################
## location with g_zip
loc.zip.proper <- dt.jobs$location_id[grepl("g_zip:", dt.jobs$location_id)]
loc.zip.proper.substr <- unique(gsub("[^\\d]+", "", loc.zip.proper, perl = T))

ls.xml.zip <- list()
i <- 1
for(loc in loc.zip.proper.substr){
    path.zip <- paste0("http://api.geonames.org/postalCodeSearch?postalcode=", loc, "&country=AU&maxRows=1&username=noahhhhhh")
    xml.zip <- xmlParse(path.zip)
    ls.xml.zip[[loc]] <- xmlToList(xml.zip)
    print(i)
    i <- i + 1
}

dt.zip <- data.table(g_zip = 0, loc_name = "NA", lat = 0, lng = 0, loc_state = "NA")
for(loc in loc.zip.proper.substr){
    dt.zip <- rbind(dt.zip, data.table(g_zip = loc
                                       , loc_name = ls.xml.zip[[loc]]$code$name
                                       , lat = ls.xml.zip[[loc]]$code$lat
                                       , lng = ls.xml.zip[[loc]]$code$lng
                                       , loc_state = ls.xml.zip[[loc]]$code$adminCode1)
    )
}
dt.zip <- dt.zip[-1]
save(dt.zip, file = "dt_zip.RData")

## location with g_id
loc.id.proper <- dt.jobs$location_id[grepl("g_id:[[:digit:]]", dt.jobs$location_id)]
loc.id.proper.substr <- unique(substr(loc.id.proper, 6, nchar(loc.id.proper)))
ls.xml <- list()
j <- 1
for(loc in loc.id.proper.substr){
    path <- paste0("http://api.geonames.org/get?geonameId=", loc, "&maxRows=1&username=noahhhhhh")
    xml <- xmlParse(path)
    ls.xml[[loc]] <- xmlToList(xml)
    print(j)
    j <- j + 1
}

ls.xml[["6943572"]]

i <- 1
dt.g_id <- data.table(g_id = 0, loc_name = "NA", lat = 0, lng = 0, loc_state = "NA", loc_fclName = "NA", loc_fclfcodeName = "NA")
for(loc in loc.id.proper.substr){
    dt.g_id <- rbind(dt.g_id, data.table(g_id = loc
                                         , loc_name = ifelse(is.null(ls.xml[[loc]]$name), "NA", ls.xml[[loc]]$name)
                                         , lat = ifelse(is.null(ls.xml[[loc]]$lat), "NA", ls.xml[[loc]]$lat)
                                         , lng = ifelse(is.null(ls.xml[[loc]]$lng), "NA", ls.xml[[loc]]$lng)
                                         , loc_state = ifelse(is.null(as.character(ls.xml[[loc]]$adminCode1$.attrs)), "NA", as.character(ls.xml[[loc]]$adminCode1$.attrs))
                                         , loc_fclName = ifelse(is.null(ls.xml[[loc]]$fclName), "NA", ls.xml[[loc]]$fclName)
                                         , loc_fclfcodeName = ifelse(is.null(ls.xml[[loc]]$fcodeName), "NA", ls.xml[[loc]]$fcodeName)
    )
    )
    print(i)
    i <- i + 1
}
dt.g_id <- dt.g_id[-1]
dt.g_id
save(dt.g_id, file = "dt_g_id.RData")
## location with a_id
loc.a_id  <- dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]
loc.a_id.substr <- unique(substr(loc.id.proper, 6, 10))
loc_a_id_state <- ifelse(grepl("AU-01", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "ACT"
                         , ifelse(grepl("AU-02", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "NSW"
                                  , ifelse(grepl("AU-03", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "NT"
                                           , ifelse(grepl("AU-04", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "QLD"
                                                    , ifelse(grepl("AU-05", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "SA"
                                                             , ifelse(grepl("AU-06", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "TAS"
                                                                      , ifelse(grepl("AU-07", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "VIC"
                                                                               , ifelse(grepl("AU-08", dt.jobs$location_job_id[grepl("a_id:", dt.jobs$location_id)]), "WA"
                                                                                        , "AU"))))))))

#######################################################################################
## clean up the dt.zip and dt.g_id ####################################################
#######################################################################################
load("dt_zip.RData")
load("dt_g_id.RData")
load("dt_location_id.RData")

## refine the dt.zip
dt.zip
ls.xml.zip <- list()
z <- 1
for(loc in dt.zip$loc_name){
    path.zip <- paste0("http://api.geonames.org/search?name=", loc, "&country=AU&maxRows=1&username=noahhhhhh")
    xml.zip <- xmlParse(path.zip)
    ls.xml.zip[[loc]] <- xmlToList(xml.zip)
    print(paste(z, "in", nrow(dt.zip)))
    z <- z + 1
}
ls.xml.zip[["Victoria Park"]]$geoname$fcode

i <- 1
dt.zip2 <- data.table(location_id = 0, loc_name = "NA", lat = 0, lng = 0, loc_state = "NA", loc_fcl = "NA", loc_fcode = "NA")
for(zip in 1:nrow(dt.zip)){
    dt.zip2 <- rbind(dt.zip2, data.table(location_id = dt.zip$location_id[zip]
                                         , loc_name = ifelse(is.null(ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$name), "NA", ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$name)
                                         , lat = ifelse(is.null(ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$lat), "NA", ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$lat)
                                         , lng = ifelse(is.null(ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$lng), "NA", ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$lng)
                                         , loc_state = "NA"
                                         , loc_fcl = ifelse(is.null(ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$fcl), "NA", ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$fcl)
                                         , loc_fcode = ifelse(is.null(ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$fcode), "NA", ls.xml.zip[[dt.zip$loc_name[zip]]]$geoname$fcode)
    )
    )
    print(paste(i, "in", nrow(dt.g_id)))
    i <- i + 1
}
dt.zip2 <- dt.zip2[-1]

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
ls.xml.g_id[["2057215"]]$name

i <- 1
dt.g_id2 <- data.table(location_id = 0, loc_name = "NA", lat = 0, lng = 0, loc_state = "NA", loc_fcl = "NA", loc_fcode = "NA")
for(loc in substr(dt.g_id$location_id, 6, nchar(dt.g_id$location_id))){
    dt.g_id2 <- rbind(dt.g_id2, data.table(location_id = loc
                                           , loc_name = ifelse(is.null(ls.xml.g_id[[loc]]$name), "NA", ls.xml.g_id[[loc]]$name)
                                           , lat = ifelse(is.null(ls.xml.g_id[[loc]]$lat), "NA", ls.xml.g_id[[loc]]$lat)
                                           , lng = ifelse(is.null(ls.xml.g_id[[loc]]$lng), "NA", ls.xml.g_id[[loc]]$lng)
                                           , loc_state = ifelse(is.null(as.character(ls.xml.g_id[[loc]]$adminCode1$.attrs)), "NA", as.character(ls.xml.g_id[[loc]]$adminCode1$.attrs))
                                           , loc_fcl = ifelse(is.null(ls.xml.g_id[[loc]]$fcl), "NA", ls.xml.g_id[[loc]]$fcl)
                                           , loc_fcode = ifelse(is.null(ls.xml.g_id[[loc]]$fcode), "NA", ls.xml.g_id[[loc]]$fcode)
    )
    )
    print(paste(i, "in", nrow(dt.g_id)))
    i <- i + 1
}
dt.g_id2 <- dt.g_id2[-1]

## state of dt.zip2
# State/Territory	Abbreviation	Postcode range
# New South Wales	NSW	1000—1999 (LVRs and PO Boxes only)
# 2000—2599
# 2619—2899
# 2921—2999
# Australian Capital Territory	ACT	0200—0299 (LVRs and PO Boxes only)
# 2600—2618
# 2900—2920
# Victoria	VIC	3000—3999
# 8000—8999 (LVRs and PO Boxes only)
# Queensland	QLD	4000—4999
# 9000—9999 (LVRs and PO Boxes only)
# South Australia	SA	5000—5799
# 5800—5999 (LVRs and PO Boxes only)
# Western Australia	WA	6000—6797
# 6800—6999 (LVRs and PO Boxes only)
# Tasmania	TAS	7000—7799
# 7800—7999 (LVRs and PO Boxes only)
# Northern Territory	NT	0800—0899
# 0900—0999 (LVRs and PO Boxes only)
dt.zip2[, loc_state := ifelse(
    (dt.zip2$location_id >= 1000 & dt.zip2$location_id <= 1999) |
        (dt.zip2$location_id >= 2000 & dt.zip2$location_id <= 2599) |
        (dt.zip2$location_id >= 2619 & dt.zip2$location_id <= 2899) |
        (dt.zip2$location_id >= 2921 & dt.zip2$location_id <= 2999)
    , "NSW"
    , ifelse(
        (dt.zip2$location_id >= 200 & dt.zip2$location_id <= 299) |
            (dt.zip2$location_id >= 2600 & dt.zip2$location_id <= 2618) |
            (dt.zip2$location_id >= 2900 & dt.zip2$location_id <= 2920)
        , "ACT"
        , ifelse(
            (dt.zip2$location_id >= 3000 & dt.zip2$location_id <= 3999) |
                (dt.zip2$location_id >= 8000 & dt.zip2$location_id <= 8999)
            , "VIC"
            , ifelse(
                (dt.zip2$location_id >= 4000 & dt.zip2$location_id <= 4999) |
                    (dt.zip2$location_id >= 9000 & dt.zip2$location_id <= 9999)
                , "QLD"
                , ifelse(
                    (dt.zip2$location_id >= 5000 & dt.zip2$location_id <= 5999)
                    , "SA"
                    , ifelse(
                        (dt.zip2$location_id >= 6000 & dt.zip2$location_id <= 6797) |
                            (dt.zip2$location_id >= 6800 & dt.zip2$location_id <= 6999)
                        , "WA"
                        , ifelse(
                            (dt.zip2$location_id >= 7000 & dt.zip2$location_id <= 7999)
                            , "TAS"
                            , ifelse(
                                (dt.zip2$location_id >= 800 & dt.zip2$location_id <= 999)
                                , "NT"
                                , "NA"
                            )
                        )
                    )
                )
            )
        )
    )
)]
dt.zip2
dt.g_id2

## refine the a_id
loc_a_id_state <- ifelse(grepl("AU-01", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "ACT"
                         , ifelse(grepl("AU-02", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "NSW"
                                  , ifelse(grepl("AU-03", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "NT"
                                           , ifelse(grepl("AU-04", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "QLD"
                                                    , ifelse(grepl("AU-05", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "SA"
                                                             , ifelse(grepl("AU-06", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "TAS"
                                                                      , ifelse(grepl("AU-07", dt.jobs$location_id[grepl("a_id:", dt.jobs$location_id)]), "VIC"
                                                                               , ifelse(grepl("AU-08", dt.jobs$location_job_id[grepl("a_id:", dt.jobs$location_id)]), "WA"
                                                                                        , "AU"))))))))

dt.a_id2 <- data.table(location_id = c("AU-01", "AU-02", "AU-03", "AU-04", "AU-05", "AU-06", "AU-07", "AU-08", "AU")
                       , loc_name = c("Canberra", "Sydney", "Darwin", "Brisbane", "Adelaide", "Hobart", "Melbourne", "Perth", "AU")
                       , lat = c(-35.28346, -33.86785, -12.46113, -27.46794, -34.92866, -42.89387, -37.814, -31.95224, -35.28346)
                       , lng = c(149.12807, 151.20732, 130.84185, 153.02809, 138.59863, 147.27886, 144.96332, 115.8614, 149.12807)
                       , loc_state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA", "AU")
                       , loc_fcl = c("P", "P", "P", "P", "P", "A", "P", "P", "NA")
                       , loc_fcode = c("PPLC", "PPLA", "PPLA", "PPLA", "PPLA", "AMD2", "PPLA", "PPLA", "NA"))
dt.a_id2

## merge and make the dt.loc
dt.loc <- rbind(dt.zip2, dt.g_id2, dt.a_id2)

## convert the location_id in dt.job
dt.location_id
# remove g_id:
dt.location_id[, location_id := ifelse(grepl("g_id:", dt.location_id$location_id)
                                       , substr(dt.location_id$location_id, 6, nchar(dt.location_id$location_id))
                                       , ifelse(grepl("g_zip:", dt.location_id$location_id)
                                                , gsub("[^\\d]+", "", dt.location_id$location_id, perl = T)
                                                , substr(dt.location_id$location_id, 6, 10)))]
dt.location_id

## merge dt.location_id with dt.loc
dt.location <- merge(dt.location_id
                     , dt.loc
                     , by = "location_id"
                     , sort = F)
dt.location[, location_id := NULL]
dt.location[, id := NULL]
dt.location[, lat := as.numeric(ifelse(dt.location$lat == "NA", 0, dt.location$lat))]
dt.location[, lng := as.numeric(ifelse(dt.location$lng == "NA", 0, dt.location$lng))]
dt.location[, lat := range01(dt.location$lat)]
dt.location[, lng := range01(dt.location$lng)]
dt.location[, rot45_x := range01(.707 * dt.location$lng + .707 * dt.location$lat)]
dt.location[, rot45_y := range01(.707 * dt.location$lng - .707 * dt.location$lat)]
dt.location[, rot30_x := range01((1.732/2) * dt.location$lng + .5 * dt.location$lat)]
dt.location[, rot30_y := range01((1.732/2) * dt.location$lng - .5 * dt.location$lat)]
dt.location[, rot60_x := range01(.5 * dt.location$lng + (1.732/2)  * dt.location$lat)]
dt.location[, rot60_y := range01(.5 * dt.location$lng - (1.732/2)  * dt.location$lat)]
dt.location[, radial_r := range01(sqrt(dt.location$lng ^ 2 + dt.location$lat ^ 2))]
dt.location[, loc_name := ifelse(is.na(dt.location$loc_name), "NA", dt.location$loc_name)]
dt.location[, loc_state := ifelse(is.na(dt.location$loc_state), "NA", dt.location$loc_state)]
dt.location[, loc_fcl := ifelse(is.na(dt.location$loc_fcl), "NA", dt.location$loc_fcl)]
dt.location[, loc_fcode := ifelse(is.na(dt.location$loc_fcode), "NA", dt.location$loc_fcode)]

dt.location.dummy <- dt.location[, c("loc_name", "loc_state", "loc_fcl", "loc_fcode"), with = F]
fList <- lapply(names(dt.location.dummy),reformulate,intercept=FALSE)
smm.location <- lapply(fList,sparse.model.matrix,data=dt.location.dummy)
do.call(cBind,smm.location)
save(dt.location, file = "dt_location.RData")
save(smm.location, file = "smmlocation.RData")

#######################################################################################
## make features ######################################################################
#######################################################################################
# creat a new location_id2: for g_id:, remove "g_id"; for g_zip: only include zip code; for a_id: only include AU-0X
# add id to it
# rbind the dt.g_id, dt.zip, d.a_id
# merge the two and order by id


# location_id <- rep("NA", nrow(dt.jobs))
# loc_name <- rep("NA", nrow(dt.jobs))
# lat <- rep("NA", nrow(dt.jobs))
# lng <- rep("NA", nrow(dt.jobs))
# loc_state <- rep("NA", nrow(dt.jobs))
# loc_fclName <- rep("NA", nrow(dt.jobs))
# loc_fclfcodeName <- rep("NA", nrow(dt.jobs))
# ## g_id
# dt.g_id[, g_id := paste0("g_id:", dt.g_id$g_id)]
# dt.g_id[, location_id := dt.g_id$g_id]
# dt.g_id[, g_id := NULL]
# dt.g_id <- data.table(dt.g_id)
# dt.location_id <- data.table(location_id = dt.jobs$location_id, id = 1:nrow(dt.jobs))
# 
# dt.location_id[grepl("g_id:[[:digit:]]", dt.location_id$location_id)]
# 
# dt.loc <- as.data.table(merge(dt.g_id
#                               , dt.location_id
#                               , by = "location_id"
#                               , all.y = T
#                               , sort = F))
# dt.loc <- dt.loc[order(dt.loc$id)]
# ## g_zip
# dt.zip
# dt.zip[, location_id := dt.zip$g_zip]
# dt.zip[, g_zip := NULL]
# dt.location.zip <- dt.location_id[grepl("g_zip:", dt.jobs$location_id)]
# dt.location.zip[, location_id := gsub("[^\\d]+", "", loc.zip.proper, perl = T)]
# 
# as.data.table(merge(dt.zip
#                     , dt.location.zip
#                     , by = "location_id"
#                     , all.y = T
#                     , sort = F))



