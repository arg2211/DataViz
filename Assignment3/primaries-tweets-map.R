setwd("~/GitHub/DataViz/Assignment3")

library(streamR)
tweets.02.09.df <- parseTweets("./primaries-data/tweets.02.09.2016.summary.json")
tweets.02.20.df <- parseTweets("./primaries-data/tweets.02.20.2016.summary.json")
tweets.02.23.df <- parseTweets("./primaries-data/tweets.02.23.2016.summary.json")
tweets.03.01.df <- parseTweets("./primaries-data/tweets.03.01.2016.summary.json")
tweets.03.05.df <- parseTweets("./primaries-data/tweets.03.05.2016.summary.json")
tweets.03.06.df <- parseTweets("./primaries-data/tweets.03.06.2016.summary.json")
tweets.03.08.df <- parseTweets("./primaries-data/tweets.03.08.2016.summary.json")
tweets.03.15.df <- parseTweets("./primaries-data/tweets.03.15.2016.summary.json")

tweets.primaries <- rbind(tweets.02.09.df, tweets.02.20.df, tweets.02.23.df, tweets.03.01.df, tweets.03.05.df, tweets.03.06.df, tweets.03.08.df, tweets.03.15.df)
tweets.primaries <- tweets.primaries[, c(1, 15, 16, 37:40)]
save(tweets.primaries, file = "tweets.primaries.rda")
load("./primaries-data/tweets.primaries.rda")


