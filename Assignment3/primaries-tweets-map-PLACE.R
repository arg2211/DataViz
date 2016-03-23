# this file is exactly the same as the other except it uses place_lon and place_lat
# instead of lon and lat

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

# find out how many tweets have these key words (in " ")
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


# extract place_long & place_lat from data into a spatialpoint object for each candidate
# for hillary clinton
geop.hc <- cbind(hc$place_lon, hc$place_lat)
geop.hc2 <- na.omit(geop.hc)  # get rid of all NAs
spp.hc <- SpatialPoints(geop.hc2)  # need to make matrix into spatialpoints (sp package)
plot(spp.hc)  # plot spatial points that we created from long & lat
class(spp.hc)  # check that the value is sp

# for ted cruz
geop.tc <- cbind(tc$place_lon, tc$place_lat)
geop.tc2 <- na.omit(geop.tc)  # get rid of all NAs
spp.tc <- SpatialPoints(geop.tc2)  # need to make matrix into spatialpoints (sp package)
plot(spp.tc)  # plot spatial points that we created from long & lat
class(spp.tc)  # check that the value is sp

# for marco rubio
geop.mr <- cbind(mr$place_lon, mr$place_lat)
geop.mr2 <- na.omit(geop.mr)  # get rid of all NAs
spp.mr <- SpatialPoints(geop.mr2)  # need to make matrix into spatialpoints (sp package)
plot(spp.mr)  # plot spatial points that we created from long & lat
class(spp.mr)  # check that the value is sp

# for bernie sanders
geop.bs <- cbind(bs$place_lon, bs$place_lat)
geop.bs2 <- na.omit(geop.bs)  # get rid of all NAs
spp.bs <- SpatialPoints(geop.bs2)  # need to make matrix into spatialpoints (sp package)
plot(spp.bs)  # plot spatial points that we created from long & lat
class(spp.bs)  # check that the value is sp

# for donald trump
geop.dt <- cbind(dt$place_lon, dt$place_lat)
geop.dt2 <- na.omit(geop.dt)  # get rid of all NAs
spp.dt <- SpatialPoints(geop.dt2)  # need to make matrix into spatialpoints (sp package)
plot(spp.dt)  # plot spatial points that we created from long & lat
class(spp.dt)  # check that the value is sp


# what is this?? 
#crs.geo <- CRS("+init=EPSG:32633")    
#proj4string(tw_points_B) <- crs.geo
# what is this?? ^ 

# now we have our data points - we need the map (polygons)
library(ggplot2) # need for map_data function

# for map of states in usa
all_states <- map_data("state")
plot(all_states)

# for map of countries in world
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
poly.counts(spp.hc, usa)
poly.counts(spp.tc, usa)
poly.counts(spp.mr, usa)
poly.counts(spp.bs, usa)
poly.counts(spp.dt, usa)

# count tweets by country for each candidate
poly.counts(spp.hc, worldmap)
poly.counts(spp.tc, worldmap)
poly.counts(spp.mr, worldmap)
poly.counts(spp.bs, worldmap)
poly.counts(spp.dt, worldmap)

# sum of all tweets for each candidate in usa
sum(poly.counts(spp.hc, usa)) # = 5466
sum(poly.counts(spp.tc, usa)) # = 4949
sum(poly.counts(spp.mr, usa)) # = 2887
sum(poly.counts(spp.bs, usa)) # = 5508
sum(poly.counts(spp.dt, usa)) # = 10906

# sum of all tweets for each candidate in world
sum(poly.counts(spp.hc, worldmap)) # = 6095
sum(poly.counts(spp.tc, worldmap)) # = 5253
sum(poly.counts(spp.mr, worldmap)) # = 2875
sum(poly.counts(spp.bs, worldmap)) # = 6109
sum(poly.counts(spp.dt, worldmap)) # = 11993

# create a variable that includes the # of tweets in each state/usa
usa$pstatecountHC <- poly.counts(spp.hc, usa)
usa$pstatecountTC <- poly.counts(spp.tc, usa)
usa$pstatecountMR <- poly.counts(spp.mr, usa)
usa$pstatecountBS <- poly.counts(spp.bs, usa)
usa$pstatecountDT <- poly.counts(spp.dt, usa)

# create a variable that includes the # of tweets in each country/world
worldmap$pcountrycountHC <- poly.counts(spp.hc, worldmap)
worldmap$pcountrycountTC <- poly.counts(spp.tc, worldmap)
worldmap$pcountrycountMR <- poly.counts(spp.mr, worldmap)
worldmap$pcountrycountBS <- poly.counts(spp.bs, worldmap)
worldmap$pcountrycountDT <- poly.counts(spp.dt, worldmap)

worldmap$pcountrycountHC # look at country counts

# create a variable that includes the proportion (%) of tweets in each state compared to total # of tweets for candidate
usa$pstatepropHC <- poly.counts(spp.hc, usa)/sum(poly.counts(spp.hc, usa))*100
usa$pstatepropTC <- poly.counts(spp.tc, usa)/sum(poly.counts(spp.tc, usa))*100
usa$pstatepropMR <- poly.counts(spp.mr, usa)/sum(poly.counts(spp.mr, usa))*100
usa$pstatepropBS <- poly.counts(spp.bs, usa)/sum(poly.counts(spp.bs, usa))*100
usa$pstatepropDT <- poly.counts(spp.dt, usa)/sum(poly.counts(spp.dt, usa))*100

library(tmap)
library(RColorBrewer)

# create choropleth maps for each candidate, mapping # of tweets in each state
# don't need this for assignment after all...
qtm(usa, "pstatecountHC")
qtm(usa, "pstatecountTC")
qtm(usa, "pstatecountMR")
qtm(usa, "pstatecountBS")
qtm(usa, "pstatecountDT")

library(tm)

# create choropleth maps for each candidate, mapping # of tweets in each state
# hillary clinton
worldmap$pcountrycountHC
pstaticmapworldHC <- qtm(worldmap, fill = "pcountrycountHC", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Hillary's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 2, 50, 150, 5400), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(5, "PuBuGn"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
pstaticmapworldHC

# ted cruz
worldmap$pcountrycountTC
pstaticmapworldTC <- qtm(worldmap, fill = "pcountrycountTC", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Ted's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 5, 50, 100, 4900), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(5, "Greens"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
pstaticmapworldTC

# marco rubio
worldmap$pcountrycountMR
pstaticmapworldMR <- qtm(worldmap, fill = "pcountrycountMR", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Marco's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 5, 15, 40, 2700), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(5, "Oranges"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
pstaticmapworldMR

# bernie sanders
worldmap$pcountrycountBS
pstaticmapworldBS <- qtm(worldmap, fill = "pcountrycountBS", # use worldmap that we created and fill with candidate's tweet counts by country
                        fill.title = "Bernie's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 5, 20, 100, 5600), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(5, "Blues"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
pstaticmapworldBS

# donald trump
worldmap$pcountrycountDT
pstaticmapworldDT <- qtm(worldmap, fill = "pcountrycountDT", # use worldmap that we created and fill with DT's tweet counts by country
                        fill.title = "Trump's Tweet Count", # legend title
                        borders.alpha = 0.5, # changes thickness of borders around countries
                        fill.style = "fixed", # allows us to customize legend breaks
                        fill.breaks = c(0, 1, 5, 20, 50, 100, 500, 10600), # specify legend breaks
                        fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                        fill.palette = brewer.pal(7, "Purples"), # use RColorBrewer scheme to color countries by tweet count
                        style = "white", # use white style
                        layout.asp = NA)
pstaticmapworldDT


# create choropleth maps for each candidate, mapping proportion (%) of total tweets in each state
# hillary clinton
usa$pstatepropHC
pstaticmapusaHC <- qtm(usa, fill = "pstatepropHC", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Hillary", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 1, 2, 4, 10, 12), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "PuBuGn"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
pstaticmapusaHC

# ted cruz
usa$pstatepropTC
pstaticmapusaTC <- qtm(usa, fill = "pstatepropTC", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Ted", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 6, 8, 13), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Greens"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
pstaticmapusaTC

# marco rubio
usa$pstatepropMR
pstaticmapusaMR <- qtm(usa, fill = "pstatepropMR", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Marco", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 8, 10, 11), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Oranges"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
pstaticmapusaMR

# bernie sanders
usa$pstatepropBS
pstaticmapusaBS <- qtm(usa, fill = "pstatepropBS", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Bernie", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 8, 10, 14), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Blues"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
pstaticmapusaBS

# donald trump
usa$pstatepropDT
pstaticmapusaDT <- qtm(usa, fill = "pstatepropDT", # use use that we created and fill with candidate's tweet counts by state
                      fill.title = "Percent of Tweets about Donald", # legend title
                      borders.alpha = 0.5, # changes thickness of borders around countries
                      fill.style = "fixed", # allows us to customize legend breaks
                      fill.breaks = c(0, 2, 4, 6, 10, 11), # specify legend breaks
                      fill.auto.palette.mapping = FALSE, # allows up to change colors of map
                      fill.palette = brewer.pal(5, "Purples"), # use RColorBrewer scheme to color states by tweet count
                      style = "white", # use white style
                      layout.asp = NA)
pstaticmapusaDT


# save maps as .jpg files
save_tmap(pstaticmapworldHC, filename="./jpeg-maps/pstaticmap.world.HC.jpg")
save_tmap(pstaticmapworldTC, filename="./jpeg-maps/pstaticmap.world.TC.jpg")
save_tmap(pstaticmapworldMR, filename="./jpeg-maps/pstaticmap.world.MR.jpg")
save_tmap(pstaticmapworldBS, filename="./jpeg-maps/pstaticmap.world.BS.jpg")
save_tmap(pstaticmapworldDT, filename="./jpeg-maps/pstaticmap.world.DT.jpg")
save_tmap(pstaticmapusaHC, filename="./jpeg-maps/pstaticmap.usa.HC.jpg")
save_tmap(pstaticmapusaTC, filename="./jpeg-maps/pstaticmap.usa.TC.jpg")
save_tmap(pstaticmapusaMR, filename="./jpeg-maps/pstaticmap.usa.MR.jpg")
save_tmap(pstaticmapusaBS, filename="./jpeg-maps/pstaticmap.usa.BS.jpg")
save_tmap(pstaticmapusaDT, filename="./jpeg-maps/pstaticmap.usa.DT.jpg")
