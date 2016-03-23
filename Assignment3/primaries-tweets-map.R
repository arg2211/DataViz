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
# hillary clinton
worldmap$countrycountHC
staticmapworldHC <- qtm(worldmap, fill = "countrycountHC", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Hillary's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 5, 80, 200), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(4, "PuBuGn"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
staticmapworldHC

# ted cruz
worldmap$countrycountTC
staticmapworldTC <- qtm(worldmap, fill = "countrycountTC", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Ted's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 2, 3, 5, 100, 110), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(6, "Greens"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
staticmapworldTC


# marco rubio
worldmap$countrycountMR
staticmapworldMR <- qtm(worldmap, fill = "countrycountMR", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Marco's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 4, 100, 200), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(4, "Oranges"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
staticmapworldMR

# bernie sanders
worldmap$countrycountBS
staticmapworldBS <- qtm(worldmap, fill = "countrycountBS", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Bernie's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 2, 5, 10, 200), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(5, "Blues"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
staticmapworldBS

# donald trump
worldmap$countrycountDT
staticmapworldDT <- qtm(worldmap, fill = "countrycountDT", # use worldmap that we created and fill with DT's tweet counts by country
                        fill.title = "Trump's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 3, 10, 100, 205), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(5, "Purples"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
staticmapworldDT



# create choropleth maps for each candidate, mapping proportion (%) of total tweets in each state
qtm(usa, "statepropHC")
qtm(usa, "statepropTC")
qtm(usa, "statepropMR")
qtm(usa, "statepropBS")
qtm(usa, "statepropDT")

# hillary clinton
usa$statepropHC
staticmapusaHC <- qtm(usa, fill = "statepropHC", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Hillary", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 1, 2, 4, 10, 20), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "PuBuGn"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
staticmapusaHC

# ted cruz
usa$statepropTC
staticmapusaTC <- qtm(usa, fill = "statepropTC", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Ted", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 6, 8, 10), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Greens"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
staticmapusaTC

# marco rubio
usa$statepropMR
staticmapusaMR <- qtm(usa, fill = "statepropMR", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Marco", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 8, 12, 16), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Oranges"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
staticmapusaMR

# bernie sanders
usa$statepropBS
staticmapusaBS <- qtm(usa, fill = "statepropBS", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Bernie", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 8, 10, 14), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Blues"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
staticmapusaBS

# donald trump
usa$statepropDT
staticmapusaDT <- qtm(usa, fill = "statepropDT", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Donald", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 6, 10, 15), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Purples"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
staticmapusaDT


# save maps as .jpg files
save_tmap(staticmapworldHC, filename="./jpeg-maps/staticmap.world.HC.jpg")
save_tmap(staticmapworldTC, filename="./jpeg-maps/staticmap.world.TC.jpg")
save_tmap(staticmapworldMR, filename="./jpeg-maps/staticmap.world.MR.jpg")
save_tmap(staticmapworldBS, filename="./jpeg-maps/staticmap.world.BS.jpg")
save_tmap(staticmapworldDT, filename="./jpeg-maps/staticmap.world.DT.jpg")
save_tmap(staticmapusaHC, filename="./jpeg-maps/staticmap.usa.HC.jpg")
save_tmap(staticmapusaTC, filename="./jpeg-maps/staticmap.usa.TC.jpg")
save_tmap(staticmapusaMR, filename="./jpeg-maps/staticmap.usa.MR.jpg")
save_tmap(staticmapusaBS, filename="./jpeg-maps/staticmap.usa.BS.jpg")
save_tmap(staticmapusaDT, filename="./jpeg-maps/staticmap.usa.DT.jpg")
