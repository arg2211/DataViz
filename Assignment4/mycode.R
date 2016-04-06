setwd("~/GitHub/DataViz/Assignment4")

# load data into R
mar16 <- read.csv("./data/tweets.03.16.2016.summary.csv", header = TRUE)
mar17 <- read.csv("./data/tweets.03.17.2016.summary.csv", header = TRUE)
mar18 <- read.csv("./data/tweets.03.18.2016.summary.csv", header = TRUE)
mar19 <- read.csv("./data/tweets.03.19.2016.summary.csv", header = TRUE)
mar20 <- read.csv("./data/tweets.03.20.2016.summary.csv", header = TRUE)
mar21 <- read.csv("./data/tweets.03.21.2016.summary.csv", header = TRUE)
mar22 <- read.csv("./data/tweets.03.22.2016.summary.csv", header = TRUE)
mar23 <- read.csv("./data/tweets.03.23.2016.summary.csv", header = TRUE)
mar24 <- read.csv("./data/tweets.03.24.2016.summary.csv", header = TRUE)
mar25 <- read.csv("./data/tweets.03.25.2016.summary.csv", header = TRUE)

# join all data frames together
tweets <- rbind(mar16,mar17,mar18,mar19,mar20,mar21,mar22,mar23,mar24,mar25)

# keep only the columns I want in data frame
tweets2 <- tweets[, c(1:4,10)]

# create .csv of all tweets with only columns I want
write.csv(tweets2, file = "pared-tweets.csv")

# ------------------------------------------------------------------------ #

# load data into R
t <- read.csv("./data/pared-tweets.csv", header = TRUE)

# change encoding of $text of tweets
t$text <- sapply(t$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))

# filter tweets by candidate
hc <- subset(t, subset = grepl("hillary|clinton|hillaryclinton|imwithher|hillary2016|clinton2016|hillyes|giveemhill|sheswithus|boys4hillary|neverhillary|hrc", text, ignore.case=TRUE))
tc <- subset(t, subset = grepl("ted|cruz|tedcruz|cruzcrew|choosecruz|cruztovictory|lyingted|lyinted|unitewithcruz|nevercruz|cruztovictory|cruz2016|tedcruz2016", text, ignore.case=TRUE))
bs <- subset(t, subset = grepl("bernie|sanders|berniesanders|feelthebern|bernie2016|sanders2016|berniecrat|votebernie|stillsanders|blackoutforbernie", text, ignore.case=TRUE))
dt <- subset(t, subset = grepl("donald|trump|donaldtrump|drumpf|donalddrumpf|realdonaldtrump|trump2016|makeamericagreatagain|trumptrain|nevertrump|womenfortrump|alwaystrump", text, ignore.case=TRUE))

# save separated-by-candidate tweets as .csv files (for easy Excel viewing)
write.csv(hc, file = "./data/hc.csv")
write.csv(tc, file = "./data/tc.csv")
write.csv(bs, file = "./data/bs.csv")
write.csv(dt, file = "./data/dt.csv")

