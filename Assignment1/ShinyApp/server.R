library(tm)
library(wordcloud)
library(SnowballC)
library(dplyr)

library(streamR)
tweets.df <- parseTweets("tweets.json", simplify = TRUE)

shinyServer(function(input, output) {
  
  candidates <- c("Jeb Bush","Ben Carson","Chris Christie","Hillary Clinton","Ted Cruz","Carly Fiorina", 
                  "John Kasich","Marco Rubio","Bernie Sanders","Donald Trump")
  candidates_new <- list(c("(Jeb|Bush|Jebbush)","Jeb Bush"),
                         c("(Ben|Carson|Bencarson)", "Ben Carson"),
                         c("(Chris|Christie|Christchristie)","Chris Christie"),
                         c("(Hillary|Clinton|Hillaryclinton)","Hillary Clinton"),
                         c("(Ted|Cruz|Tedcruz)","Ted Cruz"),
                         c("(Carly|Fiorina|Carlyfiorina)","Carly Fiorina"),
                         c("(John|Kasich|Johnkasich)","John Kasich"),
                         c("(Marco|Rubio|Marcorubio)","Marco Rubio"),
                         c("(Bernie|Sanders|BernieSanders)","Bernie Sanders"),
                         c("(Donald|Trump|Donaldtrump)","Donald Trump"))
                         
  output_2 <- character(nrow(tweets.df))
  for(i in seq_along(candidates_new)){
    output_2[grepl(x = tweets.df$text, ignore.case = TRUE, 
                   pattern = candidates_new[[i]][1])] <- candidates_new[[i]][2]
  } 
  tweets.df$candidate <- output_2
  
  output$freq_plot<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df$candidate == input$var)
    times <- as.POSIXct(sub_tweets$created_at, format="%a %b %d %H:%M:%S %z %Y")
    sub_times <- subset(times, times > min(times) + input$RangeA[1]*60)
    sub_times <- subset(sub_times, sub_times < min(times) + input$RangeA[2]*60)
    
    hist(sub_times,
         breaks = 'mins',
         freq=TRUE,
         main = paste(input$var),
         xlab = "Minutes",
         ylab = "Number of Tweets",
         col = "#FFD92F",
         border = "white"
         )
    
  })
  
output$wordcloud<- renderPlot({
  tweets.df$text <- sapply(tweets.df$text,function(row) iconv(row, "latin1", "ASCII", sub="")) 
  tweets.df <- filter(tweets.df, grepl(paste0(input$var), text))
  TweetCorpus <- paste(unlist(tweets.df$text), collapse =" ")
  TweetCorpus <- Corpus(VectorSource(TweetCorpus))
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  TweetCorpus <- tm_map(TweetCorpus, removePunctuation)
  TweetCorpus <- tm_map(TweetCorpus, removeWords, stopwords('english'))
  TweetCorpus <- tm_map(TweetCorpus, removeWords, c("httpstco", "http", "whi", "https", 
                                                    "httpst", "httpstcooequgdenda", "http$"))
  TweetCorpus <- tm_map(TweetCorpus, stemDocument)
  TweetCorpus <- tm_map(TweetCorpus, content_transformer(tolower),lazy=TRUE)
  TweetCorpus <- tm_map(TweetCorpus, PlainTextDocument)
  wordcloud(TweetCorpus, max.words = 250, random.order = FALSE,
            scale=c(5,.6),colors=brewer.pal(8, "Set2"))
})

output$text1 <- renderText({
  paste0("You have selected ", input$var)
})

})
