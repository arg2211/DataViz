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
