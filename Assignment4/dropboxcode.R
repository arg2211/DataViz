# ---------------------------- # from shinyapps.io articles # -------------------------- #
install.packages("rdrop2")
library(rdrop2)
outputDir <- "responses"

saveData <- function(data) {
  data <- t(data)
  # Create a unique file name
  fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  filesInfo <- drop_dir(outputDir)
  filePaths <- filesInfo$path
  data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  data
}

# --------------------- # from forum # ---------------------- #
GET

response <- GET(url = "https://www.dropbox.com/s/yts35d8j0qalf1x/t2.csv?dl=0") 
load(rawConnection(response$content))

load_data_dropbox <- function() {
  files_info <- drop_dir(FILE_NAME)
  file_paths <- files_info$path
  data <- lapply(file_paths, drop_read_csv, stringsAsFactors = FALSE) %>% 
    do.call(rbind, .)
  data
}