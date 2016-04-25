# how to read in multiple .csv files at once that have same columns
# and concatenate into one data frame (df)

getwd()

#install.packages("readr")
library(readr)

# put all summary.csv files for february in a folder and set wd to that folder
setwd("~/GitHub/DataViz/final-project/data/feb")
# locate the files
files <- list.files()
# read the files into a list of dfs
feb <- lapply(files, read_csv)
# concatenate into one big df
Feb <- do.call(rbind, feb)
# keep only the columns I want
Feb <- Feb[, c(2:4,10,44,33,37,42)]
# save Feb as .rda file
save(Feb, file = "feb.rda")

# repeat for march files
setwd("~/GitHub/DataViz/final-project/data/mar")
# locate the files
files <- list.files()
# read the files into a list of dfs
mar <- lapply(files, read_csv)
# concatenate into one big df
Mar <- do.call(rbind, mar)
# keep only the columns I want
Mar <- Mar[, c(2:4,10,44,33,37,42)]
save(Mar, file = "march.rda")

# repeat for april files (ONCE APRIL IS OVER)
setwd("~/GitHub/DataViz/final-project/data/april")
# locate the files
files <- list.files()
# read the files into a list of dfs
apr <- lapply(files, read_csv)
# concatenate into one big df
Apr <- do.call(rbind, apr)
# keep only the columns I want
Apr <- Apr[, c(2:4,10,44,33,37,42)]
save(Apr, file = "april.rda")

