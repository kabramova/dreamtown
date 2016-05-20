library(RCurl)
library(RJSONIO)
library(dplyr)

setwd('/Users/katja/rwd/dataproducts/')
sun <- read.csv('sundata.csv')
#http://data.un.org/Data.aspx?d=CLINO&f=ElementCode%3a15
#http://weather.noaa.gov/tg/site.shtml
#http://www.wmo.int/pages/prog/www/ois/volume-a/vola-home.htm
#http://www.who.int/gho/phe/outdoor_air_pollution/exposure/en/

sun.mean <- filter(sun, Statistic.Description == "Mean Number of Hours")
sun.small <- select(sun.mean, Country.or.Territory, Station.Name, WMO.Station.Number,
                    Annual, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
sun.clean <- filter(sun.small, Annual > 0)
hist(sun.clean$Annual)

stations <- read.csv('stations.flatfile', sep='\t')
stations.small <- select(stations, IndexNbr, Latitude, Longitude)

dms2dec <- function(coord) {
    dms <- unlist(strsplit(as.character(coord), " "))
    dir <- substr(dms[3],3,3)
    s <- substr(dms[3],1,2)
    dec <- as.numeric(dms[1]) + 
        as.numeric(dms[2])/60 + 
        as.numeric(s)/3600
    #reverse for south or west coordinates
    if (dir == 'S' || dir == 'W') {
        dec = dec * (-1)
    }
    dec
}


m <- stations.small[,2:3]
n <- apply(m, c(1,2), dms2dec)
stations.small$Lat <- n[,1]
stations.small$Lng <- n[,2]


sun.locations <- merge(sun.clean, stations.small, by.x="WMO.Station.Number",
                       by.y="IndexNbr")

# "Mixed case" capitalizing - toupper(every first letter of a word)
simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), tolower(substring(s, 2)),
          sep = "", collapse = " ")
}

sun.locations$Station.Name <- lapply(as.character(sun.locations$Station.Name), simpleCap)

sun.locations <- subset(sun.locations, !duplicated(sun.locations$WMO.Station.Number))

saveRDS(sun.locations, file="sunlocations.Rds")


library(XLConnect)
pollution <- readWorksheetFromFile("who.xlsx", sheet="database", startRow=3)
colnames(pollution)[6:9] <- c("PM10.Annual.Mean", "PM10.Year", "PM10.Station.Types",
                              "PM10.Notes")
colnames(pollution)[10:13] <- c("PM2.5.Annual.Mean", "PM2.5.Year", "PM2.5.Station.Types",
                              "PM2.5.Notes")
colnames(pollution)[15] <- "Reference"

pollution.small <- select(pollution, Country, City=City.Town, PM10.Annual=PM10.Annual.Mean,
                          PM25.Annual=PM2.5.Annual.Mean)



url <- function(address, return.call = "json", sensor = "false") {
    root <- "http://maps.google.com/maps/api/geocode/"
    u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
    return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
    if(verbose) cat(address,"\n")
    u <- url(address)
    doc <- getURL(u)
    x <- fromJSON(doc,simplify = FALSE)
    if(x$status=="OK") {
        lat <- x$results[[1]]$geometry$location$lat
        lng <- x$results[[1]]$geometry$location$lng
        return(c(lat, lng))
    } else {
        return(c(NA,NA,NA, NA))
    }
}

mapsKey <- "AIzaSyAsV8qybrz6UFW1Z9syQekYQz8Ow8yEMJE"

locations <- paste(pollution.small[,1], pollution.small[,2], sep=", ")
locations2 <- locations[!complete.cases(coord.matrix)]
locations3 <- locations2[!complete.cases(coord.matrix2)]
#coordinates <- lapply(locations, geoCode)
backup <- coordinates
saveRDS(backup, file="city_coordinates.Rds")

for (idx in seq_along(coordinates)){
    if (length(coordinates[[idx]]) > 2){
        coordinates[[idx]] <- coordinates[[idx]][1:2]
    }}
coord.matrix <- matrix(unlist(coordinates), ncol = 2, byrow = TRUE)


pollution.small$Lat <- coord.matrix[,1]
pollution.small$Lng <- coord.matrix[,2]

pollution.small$City <- lapply(as.character(pollution.small$City), simpleCap)
pollution.small$City <- as.character(pollution.small$City)
sun.locations$Station.Name <- as.character(sun.locations$Station.Name)


sl <- select(sun.locations, Lng, Lat)
ps <- select(pollution.small, Lng, Lat)

closest.idx <- numeric(length=dim(ps)[1])

for (el in 1:dim(ps)[1]){
    d <- distGeo(ps[el,], sl)
    min_dist <- which.min(d)
    if (length(min_dist)==0){
        closest.idx[el] <- NA    
    }
    else {closest.idx[el] <- min_dist}
}

# sun.locations[closest.idx[2973],]
# pollution.small[2973,]

alldata <- pollution.small

for (el in 1:dim(ps)[1]){
    alldata$Sunshine[el] <- sun.locations$Annual[closest.idx[el]]
}

alldata.final <- alldata[complete.cases(alldata),]

sun.year <- select(sun.locations, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec)
yeardata <- data.frame(matrix(nrow=dim(ps)[1], ncol=13))
yeardata[,1] <- rownames(pollution.small)
for (el in 1:dim(ps)[1]){
    yeardata[el,2:13] <- sun.year[closest.idx[el],]
}
colnames(yeardata) <- c("ids", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                        "Aug", "Sep", "Oct", "Nov", "Dec")

yeardata.final <- yeardata[complete.cases(alldata),]
yeardata.final[yeardata.final<0] <- 0

saveRDS(alldata.final, file="alldata.Rds")
saveRDS(yeardata.final, file="yeardata.Rds")


# sunshine
# pollution
# distance to sea
# public transport

# library(twitteR)
# library(curl)
# 
# consumer_key <- "zCLZirpc7n4veimbphotLcEgw"
# consumer_secret <- "944jRdApcJFxq3X5JlJtCHkyqa3qgGrFJH33xopAgARvDpD0a1"
# access_token <- "3151322422-ZrUDjg8FV1EAK13i9wRrstidfZxU855jt7bv9E0"
# access_secret <- "CnOTXdOQXVXS97HmA8LwlcSJ8nmwVcrxvABtWlk0Sa4hL"
# options(httr_oauth_cache=T) #This will enable the use of a local file to cache OAuth access credentials between R sessions.
# setup_twitter_oauth(consumer_key,
#                     consumer_secret,
#                     access_token,
#                     access_secret)
# 
# 
# kod=searchTwitter('@KOD', n=10)
# textKod <- lapply(kod, function(t) t$getText())

