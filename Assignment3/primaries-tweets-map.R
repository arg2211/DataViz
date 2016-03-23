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
all_states <- map_data("state")

plot(all_states)

library(rgeos)
library(GISTools)

require(maps)
usa <- map("state", fill = TRUE)  # produces map of US by state

require(sp)
require(maptools)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])  
# ^ use USA map that we made to produce a spatial polygon
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

poly.counts(sp.hc, usa) # count tweets by state for hillary
poly.counts(sp.tc, usa) # count tweets by state for ted
poly.counts(sp.mr, usa) # count tweets by state for marco
poly.counts(sp.bs, usa) # count tweets by state for bernie
poly.counts(sp.dt, usa) # count tweets by state for donald


# create a choropleth map of tweet counts by state
usa$statecountHC <- poly.counts(sp.hc, usa)
usa$statecountTC <- poly.counts(sp.tc, usa)
usa$statecountMR <- poly.counts(sp.mr, usa)
usa$statecountBS <- poly.counts(sp.bs, usa)
usa$statecountDT <- poly.counts(sp.dt, usa)

library(tmap)
library(RColorBrewer)

qtm(usa, "statecountHC")
qtm(usa, "statecountTC")
qtm(usa, "statecountMR")
qtm(usa, "statecountBS")
qtm(usa, "statecountDT")

summary(usa)



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

# crs.geo <- CRS("+init=EPSG:32633")
# proj4string(bs_points_B) <- crs.geo

require(maps)
usa <- map("state", fill=FALSE)

require(sp)
require(maptools)
IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

poly.counts(bs_points_B, usa)

install.packages("choroplethr")
install.packages("choroplethrMaps")
library(choroplethr)

# see: http://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r for latlong2state function

state_vals <- data.frame(latlong2state(bs_coordinates_B))
names(state_vals) <- c("region")
state_cnts <- count(state_vals, region)
names(state_cnts) <- c("region", "value")

state_choropleth(na.omit(state_cnts))
# -------------------------------------# end of Jenn's code # --------------------------------------#
