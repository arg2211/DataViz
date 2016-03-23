setwd("~/GitHub/DataViz/Assignment3")

# parse tweets
library(streamR)
tweets.02.09.df <- parseTweets("./primaries-data/tweets.02.09.2016.summary.json")
tweets.02.20.df <- parseTweets("./primaries-data/tweets.02.20.2016.summary.json")
tweets.02.23.df <- parseTweets("./primaries-data/tweets.02.23.2016.summary.json")
tweets.03.01.df <- parseTweets("./primaries-data/tweets.03.01.2016.summary.json")
tweets.03.05.df <- parseTweets("./primaries-data/tweets.03.05.2016.summary.json")
tweets.03.06.df <- parseTweets("./primaries-data/tweets.03.06.2016.summary.json")
tweets.03.08.df <- parseTweets("./primaries-data/tweets.03.08.2016.summary.json")
tweets.03.15.df <- parseTweets("./primaries-data/tweets.03.15.2016.summary.json")

# save tweets as .rda
tweets.primaries <- rbind(tweets.02.09.df, tweets.02.20.df, tweets.02.23.df, tweets.03.01.df, tweets.03.05.df, tweets.03.06.df, tweets.03.08.df, tweets.03.15.df)
tweets.primaries <- tweets.primaries[, c(1, 15, 16, 37:40)]
save(tweets.primaries, file = "tweets_all.rda")
load("./primaries-data/tweet_all.rda")

# save tweets as .csv (for easy Excel viewing)
write.csv(tweet_all, file = "primariestweets.csv") # create .csv of parsed tweets
primariestweets <- read.csv("./primaries-data/primariestweets.csv", header = TRUE)

# change encoding of $text of tweets
primariestweets$text <- sapply(primariestweets$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
#tweet_all$text <- iconv(tweet_all$text, from = "UTF-8", to = "latin1", sub = "")

# find out how many tweets have these key words
length(grep("hillaryclinton", tweet_all$text, ignore.case = TRUE))

# filter tweets by candidate
hc <- subset(primariestweets, subset = grepl("hillary|clinton|hillaryclinton", text, ignore.case=TRUE))
tc <- subset(primariestweets, subset = grepl("ted|cruz|tedcruz", text, ignore.case=TRUE))
mr <- subset(primariestweets, subset = grepl("marco|rubio|marcorubio", text, ignore.case=TRUE))
bs <- subset(primariestweets, subset = grepl("bernie|sanders|berniesanders", text, ignore.case=TRUE))
dt <- subset(primariestweets, subset = grepl("donald|trump|donaldtrump|drumpf|donalddrumpf", text, ignore.case=TRUE))

# save separated-by-candidate tweets as .csv files (for easy Excel viewing)
write.csv(hc, file = "./primaries-data/hc.csv") # create .csv of parsed tweets
write.csv(tc, file = "./primaries-data/tc.csv") # create .csv of parsed tweets
write.csv(mr, file = "./primaries-data/mr.csv") # create .csv of parsed tweets
write.csv(bs, file = "./primaries-data/bs.csv") # create .csv of parsed tweets
write.csv(dt, file = "./primaries-data/dt.csv") # create .csv of parsed tweets

# read .csv files into R
#hc <- read.csv("./primaries-data/hc.csv", header = TRUE) # read .csv into R
#tc <- read.csv("./primaries-data/tc.csv", header = TRUE) # read .csv into R
#mr <- read.csv("./primaries-data/mr.csv", header = TRUE) # read .csv into R
#bs <- read.csv("./primaries-data/bs.csv", header = TRUE) # read .csv into R
#dt <- read.csv("./primaries-data/dt.csv", header = TRUE) # read .csv into R

library(sp)
library(maps)
library(rgdal)
library(rgeos)
library(GISTools)
library(maptools)

# extract long & lat from data into a spatialpoint object
# for hillary clinton
geo.hc <- cbind(hc$lon, hc$lat)
geo.hc2 <- na.omit(geo.hc)  # get rid of all NAs
sp.hc <- SpatialPoints(geo.hc2)  # need to make matrix into spatialpoints (sp package)
plot(sp.hc)  # plot spatial points that we created from long & lat
class(sp.hc)  # check that the value is sp

# for ted cruz
geo.tc <- cbind(tc$lon, tc$lat)
geo.tc2 <- na.omit(geo.tc)  # get rid of all NAs
sp.tc <- SpatialPoints(geo.tc2)  # need to make matrix into spatialpoints (sp package)
plot(sp.tc)  # plot spatial points that we created from long & lat
class(sp.tc)  # check that the value is sp

# for marco rubio
geo.mr <- cbind(mr$lon, mr$lat)
geo.mr2 <- na.omit(geo.mr)  # get rid of all NAs
sp.mr <- SpatialPoints(geo.mr2)  # need to make matrix into spatialpoints (sp package)
plot(sp.mr)  # plot spatial points that we created from long & lat
class(sp.mr)  # check that the value is sp

# for bernie sanders
geo.bs <- cbind(bs$lon, bs$lat)
geo.bs2 <- na.omit(geo.bs)  # get rid of all NAs
sp.bs <- SpatialPoints(geo.bs2)  # need to make matrix into spatialpoints (sp package)
plot(sp.bs)  # plot spatial points that we created from long & lat
class(sp.bs)  # check that the value is sp

# for donald trump
geo.dt <- cbind(dt$lon, dt$lat)
geo.dt2 <- na.omit(geo.dt)  # get rid of all NAs
sp.dt <- SpatialPoints(geo.dt2)  # need to make matrix into spatialpoints (sp package)
plot(sp.dt)  # plot spatial points that we created from long & lat
class(sp.dt)  # check that the value is sp


# what is this?? 
#crs.geo <- CRS("+init=EPSG:32633")    
#proj4string(tw_points_B) <- crs.geo
# what is this?? ^ 

# now we have our data points - we need the map (polygons)
library(ggplot2) # need for map_data function

# for states in usa
all_states <- map_data("state")
plot(all_states)

# for countries in world
worldmap <- map_data("world")
plot(worldmap)

library(rgeos)
library(GISTools)

require(maps)
usa <- map("state", fill = TRUE)  # produces map of US by state
worldmap <- map("world", fill = TRUE) #produces map of world

require(sp)
require(maptools)

# use USA map that we made to produce a spatial polygon
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])  
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

# use world map that we made to produce a spatial polygon
worldIDs <- sapply(strsplit(worldmap$names, ":"), function(x) x[1])
worldmap <- map2SpatialPolygons(worldmap, IDs=worldIDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

# count tweets by state for each candidate
poly.counts(sp.hc, usa)
poly.counts(sp.tc, usa)
poly.counts(sp.mr, usa)
poly.counts(sp.bs, usa)
poly.counts(sp.dt, usa)

# count tweets by country for each candidate
poly.counts(sp.hc, worldmap)
poly.counts(sp.tc, worldmap)
poly.counts(sp.mr, worldmap)
poly.counts(sp.bs, worldmap)
poly.counts(sp.dt, worldmap)

# sum of all tweets for each candidate
sum(poly.counts(sp.hc, usa)) # = 173
sum(poly.counts(sp.tc, usa)) # = 119
sum(poly.counts(sp.mr, usa)) # = 59
sum(poly.counts(sp.bs, usa)) # = 203
sum(poly.counts(sp.dt, usa)) # = 221

# sum of all tweets for each candidate
sum(poly.counts(sp.hc, worldmap)) # = 190
sum(poly.counts(sp.tc, worldmap)) # = 131
sum(poly.counts(sp.mr, worldmap)) # = 65
sum(poly.counts(sp.bs, worldmap)) # = 225
sum(poly.counts(sp.dt, worldmap)) # = 248

# create a variable that includes the # of tweets in each state/usa
usa$statecountHC <- poly.counts(sp.hc, usa)
usa$statecountTC <- poly.counts(sp.tc, usa)
usa$statecountMR <- poly.counts(sp.mr, usa)
usa$statecountBS <- poly.counts(sp.bs, usa)
usa$statecountDT <- poly.counts(sp.dt, usa)

# create a variable that includes the # of tweets in each country/world
worldmap$countrycountHC <- poly.counts(sp.hc, worldmap)
worldmap$countrycountTC <- poly.counts(sp.tc, worldmap)
worldmap$countrycountMR <- poly.counts(sp.mr, worldmap)
worldmap$countrycountBS <- poly.counts(sp.bs, worldmap)
worldmap$countrycountDT <- poly.counts(sp.dt, worldmap)

worldmap$countrycountHC # look at country counts

# create a variable that includes the proportion (%) of tweets in each state compared to total # of tweets for candidate
usa$statepropHC <- poly.counts(sp.hc, usa)/sum(poly.counts(sp.hc, usa))*100
usa$statepropTC <- poly.counts(sp.tc, usa)/sum(poly.counts(sp.tc, usa))*100
usa$statepropMR <- poly.counts(sp.mr, usa)/sum(poly.counts(sp.mr, usa))*100
usa$statepropBS <- poly.counts(sp.bs, usa)/sum(poly.counts(sp.bs, usa))*100
usa$statepropDT <- poly.counts(sp.dt, usa)/sum(poly.counts(sp.dt, usa))*100

library(tmap)
library(RColorBrewer)

# create choropleth maps for each candidate, mapping # of tweets in each state
qtm(usa, "statecountHC")
qtm(usa, "statecountTC")
qtm(usa, "statecountMR")
qtm(usa, "statecountBS")
qtm(usa, "statecountDT")

library(tm)

# create choropleth maps for each candidate, mapping # of tweets in each state
qtm(worldmap, fill = "countrycountHC", fill.title = "Tweet Count", 
    fill.style="fixed", fill.breaks=c(0,1,5,80,200))
worldmap$countrycountHC

qtm(worldmap, fill = "countrycountTC", fill.title = "Tweet Count",
    fill.style = "fixed", fill.breaks = c(0, 1, 2, 3, 5, 100, 110))
worldmap$countrycountTC

qtm(worldmap, fill = "countrycountMR", fill.title = "Tweet Count",
    fill.style = "fixed", fill.breaks = c(0, 1, 4, 100, 200))
worldmap$countrycountMR

qtm(worldmap, fill = "countrycountBS", fill.title = "Tweet Count",
    fill.style = "fixed", fill.breaks = c(0, 1, 4, 100, 200))
worldmap$countrycountBS

qtm(worldmap, fill = "countrycountDT", fill.title = "Tweet Count",
    fill.style = "fixed", fill.breaks = c(0, 1, 3, 10, 100, 205),
    tmap.style = natural)
worldmap$countrycountDT

style_catalogue()


summary(usa)

# create choropleth maps for each candidate, mapping proportion (%) of total tweets in each state
qtm(usa, "statepropHC")
qtm(usa, "statepropTC")
qtm(usa, "statepropMR")
qtm(usa, "statepropBS")
qtm(usa, "statepropDT")







-----------------------------------------------------------
# Jenn's code for choropleth maps
# see: http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r for latlong2state function

#install.packages("choroplethr")
#install.packages("choroplethrMaps")
library(choroplethr)

state_vals <- data.frame(latlong2state(bs_coordinates_B))
names(state_vals) <- c("region")
state_cnts <- count(state_vals, region)
names(state_cnts) <- c("region", "value")

state_choropleth(na.omit(state_cnts))

# Ling's code for choropleth maps

library(choroplethr)
latlong2state <- function(pointsDF) {
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

state_valsDT <- data.frame(latlong2state(sp.dt))
names(state_valsDT) <- c("region")
state_countsDT <- count(state_valsDT, region)
names(state_countsDT) <- c("region", "value")

Sander_usamap <- state_choropleth(na.omit(state_cnts))

sp.dt.df <- data.frame(sp.dt)
BS.usa.map <- state_choropleth(sp.dt.df)
-----------------------------------------





# ------------------------------- # Erica's code # ------------------------------------ #

setwd("/Users/erica_kilbride/Downloads")
load("tweet_all.rda", envir = parent.frame(), verbose = FALSE)

candidates <- c("Bernie", "Hillary", "Marco", "Ted", "Donald")
regexes <- list(c("(Bernie|Sanders|BernieSanders|Feelthebern)","Bernie"),
                c("(Hillary|Clinton|Hillaryclinton|HRC|Hills)","Hillary"),
                c("(Marco|Rubio|Marcorubio)","Marco"),
                c("(Ted|Cruz|Tedcruz)","Ted"),
                c("(Donald|Trump|Donaldtrump)","Donald"))
#Create a vector, the same length as the df
output_vector <- character(nrow(tweet_all))

#For each regex..
for(i in seq_along(regexes)){
  
  #Grep through d$name, and when you find matches, insert the relevant 'tag' into
  #The output vector
  output_vector[grepl(x = tweet_all$text,ignore.case = TRUE, pattern = regexes[[i]][1])] <- regexes[[i]][2]
  
} 
#Insert that now-filled output vector into the dataframe
tweet_all$candidate <- output_vector
tweet_all <- subset(tweet_all, tweet_all$candidate != "")

Bernie <- subset(tweet_all, tweet_all$candidate == "Bernie")
Hillary <- subset(tweet_all, tweet_all$candidate == "Hillary")
Ted <- subset(tweet_all, tweet_all$candidate == "Ted")
Marco <- subset(tweet_all, tweet_all$candidate == "Marco")
Donald <- subset(tweet_all, tweet_all$candidate == "Donald")

require(maps)
world_map <-map("world", fill = FALSE)
geo_tweets = tweet_all
tw_coordinates_B<- cbind(geo_tweets$lon,geo_tweets$lat)
tw_coordinates_B2 <- na.omit(tw_coordinates_B)
geo <- data.frame(tw_coordinates_B2)

#seperate by candidate
Bernie <- subset(tweet_all, tweet_all$candidate == "Bernie")
tw_coordinates_B<- cbind(Bernie$lon,Bernie$lat)
tw_coordinates_B2 <- na.omit(tw_coordinates_B)
B_geo <- data.frame(tw_coordinates_B2)

Hillary <- subset(tweet_all, tweet_all$candidate == "Hillary")
tw_coordinates_B<- cbind(Hillary$lon,Hillary$lat)
tw_coordinates_B2 <- na.omit(tw_coordinates_B)
H_geo <- data.frame(tw_coordinates_B2)

Ted <- subset(tweet_all, tweet_all$candidate == "Ted")
tw_coordinates_B<- cbind(Ted$lon,Ted$lat)
tw_coordinates_B2 <- na.omit(tw_coordinates_B)
T_geo <- data.frame(tw_coordinates_B2)

Marco <- subset(tweet_all, tweet_all$candidate == "Marco")
tw_coordinates_B<- cbind(Marco$lon,Marco$lat)
tw_coordinates_B2 <- na.omit(tw_coordinates_B)
M_geo <- data.frame(tw_coordinates_B2)

Donald <- subset(tweet_all, tweet_all$candidate == "Donald")
tw_coordinates_B<- cbind(Donald$lon,Donald$lat)
tw_coordinates_B2 <- na.omit(tw_coordinates_B)
D_geo <- data.frame(tw_coordinates_B2)


tw_points_B <- SpatialPoints(tw_coordinates_B2)
plot(tw_points_B)
class(tw_points_B)


world <- map_data("world")
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = geo, aes(x = geo$X1, y = geo$X2), size = 1, alpha = 1/5, color = "blue")

#seperate by candidate
#Bernie
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = B_geo, aes(x = B_geo$X1, y = B_geo$X2), size = 1, alpha = 1/5, color = "dark blue")

#Hillary
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "white", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = H_geo, aes(x = H_geo$X1, y = H_geo$X2), size = 1, alpha = 1/5, color = "pink")

#Ted
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "white", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = T_geo, aes(x = T_geo$X1, y = T_geo$X2), size = 1, alpha = 1/5, color = "red")

#Marco
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "white", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = M_geo, aes(x = M_geo$X1, y = M_geo$X2), size = 1, alpha = 1/5, color = "orange")

#Donald
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "white", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = D_geo, aes(x = D_geo$X1, y = D_geo$X2), size = 1, alpha = 1/5, color = "black")

# --------------------------- # end of Erica's code # -----------------------------------------#

# --------------------------- # Jenn's code # -------------------------------------------------#
tweets_ST.df <- parseTweets("tweets.03.01.2016.summary.json.geo")

tweets_ST.df$text <- iconv(tweets_ST.df$text, from = "UTF-8", to = "latin1", sub = "")

bs <- filter(tweets_ST.df, grepl("bernie|sanders", text, ignore.case=TRUE))

# install.packages("sp")
# install.packages("maps")
# install.packages("rgdal")
# install.packages("rgeos")
# install.packages("GISTools")
# install.packages("maptools")
library(sp)
library(maps)
library(rgdal)
library(rgeos)
library(GISTools)
library(maptools)

bs_coordinates_B <- cbind(tweets_ST.df$lon, tweets_ST.df$lat)
bs_coordinates_b2 <- na.omit(bs_coordinates_B)
bs_points_B <- SpatialPoints(bs_coordinates_b2)
plot(bs_points_B)
class(bs_points_B)

library(maps)
library(ggplot2)
all_states <- map_data("state")
plot(all_states)

world <- map_data("world")
plot(world)

# crs.geo <- CRS("+init=EPSG:32633")
# proj4string(bs_points_B) <- crs.geo

require(maps)
usa <- map("state", fill=FALSE)
usa$names  # prints all names of states in usa map

worldmap <- map("world", fill=TRUE)
worldmap$names # prints all country names in world map

require(sp)
require(maptools)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

worldIDs <- sapply(strsplit(worldmap$names, ":"), function(x) x[1])
worldmap <- map2SpatialPolygons(worldmap, IDs=worldIDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

install.packages("choroplethr")
install.packages("choroplethrMaps")
library(choroplethr)

# see: http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r for latlong2state function

library(sp)
library(maps)
library(maptools)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

state_vals <- data.frame(latlong2state(bs_coordinates_B))
names(state_vals) <- c("region")
state_cnts <- count(state_vals, region)
names(state_cnts) <- c("region", "value")

state_choropleth(na.omit(state_cnts))
# -------------------------------------# end of Jenn's code # --------------------------------------#

# -------------------------------# Ling's code for maps # ---------------------------#
## world map for Sanders
world_map <- map("world", fill=F)
Sanders_coordinates<- cbind(Sanders$place_lon,Sanders$place_lat)
Sanders_coordinates<- na.omit(Sanders_coordinates)
Sanders_geo <- data.frame(Sanders_coordinates)

Sanders_points <- SpatialPoints(Sanders_coordinates)
plot(Sanders_points)
class(Sanders_points)

world <- map_data("world")
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = Sanders_geo, aes(x = Sanders_geo$X1, y = Sanders_geo$X2), size = 1, alpha = 1/5, color = "blue")

## usa map for Sanders
require(sp)
require(maptools)

all_states <- map_data("state")
plot(all_states)
usa <- map("state", fill=T)

IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

poly.counts(Sanders_points, usa)

##install.packages("choroplethr")
##install.packages("choroplethrMaps")
library(choroplethr)

latlong2state <- function(pointsDF) {
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  indices <- over(pointsSP, states_sp)
  
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

state_vals <- data.frame(latlong2state(Sanders_coordinates))
names(state_vals) <- c("region")
state_cnts <- count(state_vals, region)
names(state_cnts) <- c("region", "value")

Sander_usamap <- state_choropleth(na.omit(state_cnts))
# ------------------------- # end of Ling's code # -----------------------------------#
