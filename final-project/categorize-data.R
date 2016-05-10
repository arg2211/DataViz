setwd("~/GitHub/DataViz/final-project")

# load data
load("./data/feb.rda")
load("./data/march.rda")

# feb <- load("./data/feb.rda")
# mar <- load("./data/march.rda")
# apr <- load("./data/april.rda")

# convert .rda values to dataframes
# feb.df <- get(feb)
# mar.df <- get(mar)
# apr.df <- get(apr)

# keep only first two columns
feb.df <- Feb[,1:2]
mar.df <- March[,1:2]

# merge data frames together to get one huge df - ADD APRIL WHEN DONE
all.df <- rbind(feb.df, mar.df)

# remove objects we don't need
rm(Feb, March)

# ------------------ # assign topics to each tweet # ------------------ #

all.df$reproductive_rights = grepl('reproduct|abortion|birthcontrol|thepill|contracept
                                   |steriliz|genitalmutilation|uterus|sex(ual)?education
                                   |prochoice|prolife|plannedparenthood|standwithpp|ppfa
                                   |roevwade|antichoice|maternalhealthcare|affordablechildcare
                                   |hobbylobby|standwithwendy', all.df$text, ignore.case = TRUE)

all.df$periods = grepl('menstrua|periodsham|pms|periods|happytobleed|periodprejudice
                       |periodsarenotaninsult|periodsforpence|livetweetyourperiod', all.df$text, ignore.case = TRUE)

all.df$violence_against_women = grepl('violenceagainstwomen|sexualharassment|sexualviolence
                                      |sexualassault|sexualabuse|title9|titleIX|domesticviolence
                                      |domesticabuse|sexslave|humantraffic|yesmeansyes|nomeansno
                                      |consent|bringbackourgirls|survivorprivilege|rapeculture
                                      |rapecultureiswhen|whyistayed|freemarissa|victimblaming
                                      |marissaalexander|redmylips|catcall|itsonus', all.df$text, ignore.case = TRUE)

all.df$sexualization = grepl('sexualiz|objectif|sexobject|sexuallyobjectif|beautystandard
                             |standardofbeauty|hypersexual|freethenipple|notbuyingit
                             |mediawelike', all.df$text, ignore.case = TRUE)

all.df$professional_gap = grepl('underrepresent|wom[ae]nintech|wom[ae]ninpolitics|glassceiling
                                |wom[ae]ninstem|girlsinstem|stemgirls|stemwom[ae]n|wom[ae]ninscience
                                |noceiling|changetheratio|wom[ae]nvote|suffrage|herstory
                                |ask4more|leanin|wearesilent
                                |wagegap|equalpay|equalwage
                                |parentalleave|pregnancyleave|maternityleave', all.df$text, ignore.case = TRUE)

all.df$women_of_color = grepl('rememberrenisha|renishamcbride|youoksis|howmediawriteswoc
                              |notyourasiansidekick|solidarityisforwhitewomen|fasttailedgirls
                              |intersectional|wom[ae]nofcolor', all.df$text, ignore.case = TRUE)

all.df$feminism = grepl('feminis|feminazi|askhermore|yesallwomen
                        |allinforher|girlscount|girlsrising|girlrising|girlschange
                        |empower|standwithwomen|fem2|femfuture|noreallythisbullshit
                        |noreallythisisbullshit|freethefive|tothegirls|imagirl|womenshould
                        |girlscharge|banbossy|sexis', all.df$text, ignore.case = TRUE)

all.df$men = grepl('notallmen|yesallmen|allmen|heforshe|allmencan|misogyn|dudesgreetingdudes', all.df$text, ignore.case = TRUE)

#all.df$beauty_standards = grepl('loveyourlines|beautystandard|')
#olderwomenvoices

# ------------------- # assign candidates to each tweet # ----------------------- #

all.df$hillary = grepl('hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc', all.df$text, ignore.case = TRUE)
all.df$ted = grepl('ted|cruz', all.df$text, ignore.case = TRUE)
all.df$bernie = grepl('bernie|sanders|feelthebern', all.df$text, ignore.case = TRUE)
all.df$donald = grepl('donald|trump|drumpf|makeamericagreatagain', all.df$text, ignore.case = TRUE)

# ------------------ # assign dem or rep to each tweet # -------------------- #

all.df$democrat = grepl('hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc
                        |bernie|sanders|feelthebern', all.df$text, ignore.case = TRUE)
all.df$republican = grepl('ted|cruz
                          |donald|trump|drumpf|makeamericagreatagain', all.df$text, ignore.case = TRUE)

# export huge df
write.csv(all.df, "./shinyapp-final/alltweets.csv")

# ------------------ # 
all.df <- read.csv("./shinyapp-final/alltweets.csv")


# downsample
library(caret)

small.df <- all.df[seq(1, nrow(all.df), 10), ]

# ------------------ # create separate data sets for each candidate # ------------------- #
# subset tweets by candidate
# for all months
hc <- subset(small.df, subset = grepl("hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc", text, ignore.case=TRUE))
tc <- subset(small.df, subset = grepl("ted|cruz", text, ignore.case=TRUE))
bs <- subset(small.df, subset = grepl("bernie|sanders|feelthebern", text, ignore.case=TRUE))
dt <- subset(small.df, subset = grepl("donald|trump|drumpf|makeamericagreatagain", text, ignore.case=TRUE))




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

# combine dataframes for each candidate - ADD IN APRIL WHEN DONE
clinton.df <- rbind(hc.feb,hc.mar)
cruz.df <- rbind(tc.feb,tc.mar)
sanders.df <- rbind(bs.feb,bs.mar)
trump.df <- rbind(dt.feb,dt.mar)

# ------------------------ # ------------------------ # ------------------------- #

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
