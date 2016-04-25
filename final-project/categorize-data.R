setwd("~/GitHub/DataViz/final-project")

# load data
feb <- load("./data/feb.rda")
mar <- load("./data/march.rda")
#apr <- load("./data/april.rda")

# convert .rda values to dataframes
feb.df <- get(feb)
mar.df <- get(mar)
#apr.df <- get(apr)

# subset tweets by candidates
# for february
hc.feb <- subset(feb.df, subset = grepl("hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc", text, ignore.case=TRUE))
tc.feb <- subset(feb.df, subset = grepl("ted|cruz", text, ignore.case=TRUE))
bs.feb <- subset(feb.df, subset = grepl("bernie|sanders|feelthebern", text, ignore.case=TRUE))
dt.feb <- subset(feb.df, subset = grepl("donald|trump|drumpf|makeamericagreatagain", text, ignore.case=TRUE))
# for march
hc.mar <- subset(mar.df, subset = grepl("hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc", text, ignore.case=TRUE))
tc.mar <- subset(mar.df, subset = grepl("ted|cruz", text, ignore.case=TRUE))
bs.mar <- subset(mar.df, subset = grepl("bernie|sanders|feelthebern", text, ignore.case=TRUE))
dt.mar <- subset(mar.df, subset = grepl("donald|trump|drumpf|makeamericagreatagain", text, ignore.case=TRUE))
# for april
# hc.apr <- subset(apr.df, subset = grepl("hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc", text, ignore.case=TRUE))
# tc.apr <- subset(apr.df, subset = grepl("ted|cruz", text, ignore.case=TRUE))
# bs.apr <- subset(apr.df, subset = grepl("bernie|sanders|feelthebern", text, ignore.case=TRUE))
# dt.apr <- subset(apr.df, subset = grepl("donald|trump|drumpf|makeamericagreatagain", text, ignore.case=TRUE))

# combine dataframes for each candidate
clinton.df <- rbind(hc.feb,hc.mar)
cruz.df <- rbind(tc.feb,tc.mar)
sanders.df <- rbind(bs.feb,bs.mar)
trump.df <- rbind(dt.feb,dt.mar)


fake <- c("hillary is the best and trump sucks",
          "ted is cool, hillary is awful",
          "bernie and trump sitting in a tree",
          "bernie sanders and clinton are dems",
          "clinon and bernie sanders and cruz")
fake.df <- as.data.frame(fake)
library(plyr)
fake.df <- rename(fake.df, c("fake"="text"))

# need another way to assign a single variable to each tweet so the variable can capture
# multiple candidates in one tweet
# set up regex for candidate input
candidates <- c("Hillary Clinton","Ted Cruz","Bernie Sanders","Donald Trump","Democrats","Republicans")
candidates_new <- list(c("hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc","Hillary Clinton"),
                       c("ted|cruz","Ted Cruz"),
                       c("bernie|sanders|feelthebern","Bernie Sanders"),
                       c("donald|trump|drumpf|makeamericagreatagain","Donald Trump"))

output_2 <- character(nrow(fake.df))
for(i in seq_along(candidates_new)){
  output_2[grepl(x = fake.df$text, ignore.case = TRUE, 
                 pattern = candidates_new[[i]][1])] <- candidates_new[[i]][2]
} 
fake.df$candidate <- output_2
