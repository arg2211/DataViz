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

# create date
t$date <- as.Date(t$created_at, format="%a %b %d %H:%M:%S")

# delete new columns if necessary
t <- t[, c(1:7)]

# create topics columns
t$economy <- ifelse(grepl("economy|business|finance|income|tax", t$text, ignore.case = TRUE), 1, 0)
t$immigration <- ifelse(grepl("immigration|bordercontrol|illegalalien[s]|buildawall|refugee", t$text, ignore.case = TRUE), 1, 0)
t$healthcare <- ifelse(grepl("healthcare|medical|medicare|obamacare|healthinsurance", t$text, ignore.case = TRUE), 1, 0)
t$military <- ifelse(grepl("military|war|soldier|army|terrorist", t$text, ignore.case = TRUE), 1, 0)
t$guncontrol <- ifelse(grepl("guncontrol|firearm|secondamendment|beararms|gunviolence", t$text, ignore.case = TRUE), 1, 0)
t$china <- ifelse(grepl("china|trade|madeinchina|boycottchina|madeinamerica", t$text, ignore.case = TRUE), 1, 0)
t$trade <- ifelse(grepl("trade|globalization|freetrade|transpacificpartnership|globaltrade", t$text, ignore.case = TRUE), 1, 0)
t$race <- ifelse(grepl("racism|blacklivesmatter|massincarceration|racialjustice|whiteprivilege", t$text, ignore.case = TRUE), 1, 0)
t$climatechange <- ifelse(grepl("globalwarming|climatechange|greenhousegas|cleanenergy|pollution", t$text, ignore.case = TRUE), 1, 0)
t$religion <- ifelse(grepl("religio|christ|atheis|muslim|islam", t$text, ignore.case = TRUE), 1, 0)

#
d$TGR[d$genderr==1 ] <- "againstTGR"
d$TGR[d$genderr==0 ] <- "forTGR"

t$topic[t$economy==1] <- "economy"
t$topic[t$military==1] <- "military"
t$topic[t$guncontrol==1] <- "guncontrol"
t$topic[t$race==1] <- "race"
t$topic[t$religion==1] <- "religion"
t$topic[t$trade==1] <- "trade"
t$topic[t$china==1] <- "china"
t$topic[t$climatechange==1] <- "climatechange"
t$topic[t$immigration==1] <- "immigration"
t$topic[t$healthcare==1] <- "healthcare"

t$topicsum <- rowSums(t[8:17])
t$topic[t$topicsum>1] <- "multiple"

# highlight lines, then press shift+ctrl+c to comment out entire sections!!!!
#econonmy <- subset(t, subset = grepl("economy|business|finance|income|taxes", text, ignore.case = TRUE))
#immigration <- subset(t, subset = grepl("immigration|bordercontrol|illegalalien|buildawall|refugees", text, ignore.case = TRUE))
#healthcare <- subset(t, subset = grepl("healthcare|medical|medicare|obamacare|healthinsurance", text, ignore.case = TRUE))
#military <- subset(t, subset = grepl("military|war|iraq|army|terrorists", text, ignore.case = TRUE))
#guncontrol <- subset(t, subset = grepl("guncontrol|firearms|secondamendment|righttobeararms|gunviolence", text, ignore.case = TRUE))
#china <- subset(t, subset = grepl("china|trade|madeinchina|boycottchina|madeinamerica", text, ignore.case = TRUE))
#trade <- subset(t, subset = grepl("trade|globalization|freetrade|transpacificpartnership|globaltrade", text, ignore.case = TRUE))
#race <- subset(t, subset = grepl("racism|blacklivesmatter|massincarceration|racialjustice|whiteprivilege", text, ignore.case = TRUE))
#climatechange <- subset(t, subset = grepl("globalwarming|climatechange|greenhousegasses|cleanenergy|pollution", text, ignore.case = TRUE))
#religion <- subset(t, subset = grepl("religion|christianity|atheism|bible|god", text, ignore.case = TRUE))


# filter tweets by candidate
hc <- subset(t, subset = grepl("hillary|clinton|hillaryclinton|imwithher|hillary2016|clinton2016|hillyes|giveemhill|sheswithus|boys4hillary|neverhillary|hrc", text, ignore.case=TRUE))
tc <- subset(t, subset = grepl("ted|cruz|tedcruz|cruzcrew|choosecruz|cruztovictory|lyingted|lyinted|unitewithcruz|nevercruz|cruztovictory|cruz2016|tedcruz2016", text, ignore.case=TRUE))
bs <- subset(t, subset = grepl("bernie|sanders|berniesanders|feelthebern|bernie2016|sanders2016|berniecrat|votebernie|stillsanders|blackoutforbernie", text, ignore.case=TRUE))
dt <- subset(t, subset = grepl("donald|trump|donaldtrump|drumpf|donalddrumpf|realdonaldtrump|trump2016|makeamericagreatagain|trumptrain|nevertrump|womenfortrump|alwaystrump", text, ignore.case=TRUE))

tc2 <- subset(t, subset = grepl("ted|cruz", text, ignore.case=TRUE))

# create republican and democrat groups
dems <- rbind(hc,bs)
reps <- rbind(tc,dt)

# save separated-by-candidate tweets as .csv files
write.csv(hc, file = "./data/hc.csv")
write.csv(tc, file = "./data/tc.csv")
write.csv(bs, file = "./data/bs.csv")
write.csv(dt, file = "./data/dt.csv")
write.csv(dems, file = "./data/dems.csv")
write.csv(reps, file = "./data/reps.csv")

# find frequency of tweets by topic for each file
sum(t$economy)
 
file <- c("hc", "tc", "bs", "dt", "reps", "dems")
topic <- c("economy", "immigration", "healthcare", "military", "guncontrol", 
           "china", "trade", "race", "climatechange", "religion")

#for hillary clinton
count.hc <- colSums(hc[,c(8:17)])
c.hc <- data.frame(topic, count.hc)
row.names(c.hc) <- c(1:10)
#for ted cruz
count.tc <- colSums(tc[,c(8:17)])
c.tc <- data.frame(topic, count.tc)
row.names(c.tc) <- c(1:10)
# for bernie sanders
count.bs <- colSums(bs[,c(8:17)])
c.bs <- data.frame(topic, count.bs)
row.names(c.bs) <- c(1:10)
# for donald trump
count.dt <- colSums(dt[,c(8:17)])
c.dt <- data.frame(topic, count.dt)
row.names(c.dt) <- c(1:10)
# for dems
count.dems <- colSums(dems[,c(8:17)])
c.dems <- data.frame(topic, count.dems)
row.names(c.dems) <- c(1:10)
# for reps
count.reps <- colSums(reps[,c(8:17)])
c.reps <- data.frame(topic, count.reps)
row.names(c.reps) <- c(1:10)

# combine all counts in one
c.all <- data.frame(topic, count.hc, count.tc, count.bs, count.dt, count.dems, count.reps)
row.names(c.all) <- c(1:10)

# write csv files
write.csv(c.hc, file = "./data/counthc.csv")
write.csv(c.tc, file = "./data/counttc.csv")
write.csv(c.bs, file = "./data/countbs.csv")
write.csv(c.dt, file = "./data/countdt.csv")
write.csv(c.dems, file = "./data/countdems.csv")
write.csv(c.reps, file = "./data/countreps.csv")

# -------------------- # ---------------------- # ------------------------ #

dates <- c("16mar2016","17mar2016","18mar2016","19mar2016","20mar2016","21mar2016",
           "22mar2016","23mar2016","24mar2016","25mar2016")
dates <- as.Date(dates, "%d%b%Y")

day16.hc <- colSums(hc[hc$date=="2016-03-16",c(8:17)], na.rm = TRUE)
day17.hc <- colSums(hc[hc$date=="2016-03-17",c(8:17)], na.rm = TRUE)
day18.hc <- colSums(hc[hc$date=="2016-03-18",c(8:17)], na.rm = TRUE)
day19.hc <- colSums(hc[hc$date=="2016-03-19",c(8:17)], na.rm = TRUE)
day20.hc <- colSums(hc[hc$date=="2016-03-20",c(8:17)], na.rm = TRUE)
day21.hc <- colSums(hc[hc$date=="2016-03-21",c(8:17)], na.rm = TRUE)
day22.hc <- colSums(hc[hc$date=="2016-03-22",c(8:17)], na.rm = TRUE)
day23.hc <- colSums(hc[hc$date=="2016-03-23",c(8:17)], na.rm = TRUE)
day24.hc <- colSums(hc[hc$date=="2016-03-24",c(8:17)], na.rm = TRUE)
day25.hc <- colSums(hc[hc$date=="2016-03-25",c(8:17)], na.rm = TRUE)

byday.hc <- rbind(day16.hc,day17.hc,day18.hc,day19.hc,day20.hc,day21.hc,day22.hc,day23.hc,day24.hc,day25.hc)
colnames(byday.hc) <- topic
rownames(byday.hc) <- as.character(dates)
byday.hc <- data.frame(byday.hc)

write.csv(byday.hc, file = "byday.hc.csv")


t2 <- t[!is.na(t$topic),]
write.csv(t2, file = "t2.csv")

library(ggplot2)

ggplot(mpg, aes(manufacturer, fill=class)) + geom_bar()
ggplot(t2, aes(date, fill= topic)) + geom_bar()

View(mpg)