setwd("~/GitHub/DataViz/Assignment4")

# load summary csv files from internet - TAKES TOO LONG
# file16 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.16.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=tN%2FvRPjQITHUNu6wC22ymlJDwyc%3D"), header = TRUE)
# file17 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.17.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=uBVexgjkjid1Q3NhITR87CyBQOY%3D"), header = TRUE)
# file18 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.18.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=xu5Fu3greYYj1Keo23WFHJWqHQk%3D"), header = TRUE)
# file19 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.19.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=e2BULXJqBevcVbMirixEfiCrq8s%3D"), header = TRUE)
# file20 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.20.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=f%2BCKM6QZ6RqBEXziJ1W%2BCxYri24%3D"), header = TRUE)
# file21 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.21.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=yY4KuwIAaYNcQu6rSt34KN6c3aw%3D"), header = TRUE)
# file22 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.22.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=4CJp9D39p0xhk4b7Ke3%2Fy4xI%2Fh8%3D"), header = TRUE)
# file23 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.23.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=bflOdf221IVd5BbRIez2mqesYGc%3D"), header = TRUE)
# file24 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.24.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=UFfvICNkyN7tuXLtI0p3tuJNESc%3D"), header = TRUE)
# file25 <- read.csv(url("https://primary-tweets-summaries-csv.s3.amazonaws.com/tweets.03.25.2016.summary.csv?AWSAccessKeyId=AKIAJUOIB5T3ZG6K7TBQ&Expires=1460320147&Signature=Rk3l%2BU9wsqrg%2BoM8NZKPV61tpVg%3D"), header = TRUE)

# load data into R from downloaded files
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
t$economy <- ifelse(grepl("econom|business|financ|income|tax", t$text, ignore.case = TRUE), 1, 0)
t$immigration <- ifelse(grepl("immigration|bordercontrol|illegalalien|buildawall|refugee", t$text, ignore.case = TRUE), 1, 0)
t$healthcare <- ifelse(grepl("healthcare|medical|medicare|obamacare|healthinsurance", t$text, ignore.case = TRUE), 1, 0)
t$military <- ifelse(grepl("military|war|soldier|army|terrorist", t$text, ignore.case = TRUE), 1, 0)
t$guncontrol <- ifelse(grepl("gun|weapon|secondamendment|beararms|firearm", t$text, ignore.case = TRUE), 1, 0)
t$china <- ifelse(grepl("china|chinese|xijinping|mandarin|madeinamerica", t$text, ignore.case = TRUE), 1, 0)
t$trade <- ifelse(grepl("trade|globalization|freemarket|transpacificpartnership|international", t$text, ignore.case = TRUE), 1, 0)
t$race <- ifelse(grepl("racism|blacklivesmatter|massincarceration|racial|whiteprivilege", t$text, ignore.case = TRUE), 1, 0)
t$climatechange <- ifelse(grepl("globalwarming|climatechange|greenhousegas|cleanenergy|pollution", t$text, ignore.case = TRUE), 1, 0)
t$religion <- ifelse(grepl("religio|christ|atheis|muslim|islam", t$text, ignore.case = TRUE), 1, 0)

# assign topics under one column
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

t2 <- t[!is.na(t$topic),]
write.csv(t2, file = "t2.csv")