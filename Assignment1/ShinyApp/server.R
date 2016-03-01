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
    #x1 <- c(as.POSIXct(input$range, origin = "?")) #where x axis values should be placed
    #x2 <- c(?) #what x axis values should be labeled
    #axis(side = 1, at = x1, labels = x2, tick = TRUE, col = "black", tck = -.05) #x axis
    #axis(side = 2, at = NULL, labels = NULL, tick = TRUE, col = "black") #y axis
    
    hist(sub_times,
         breaks = 'mins',
         freq=TRUE,
         main = paste(input$var),
         #xlim = c(0,1000),
         #xlim = c(min(input$range),max(input$range)),
         #ylim = c(0,10000),
         xlab = "Minutes",
         ylab = "Number of Tweets",
         col = "#FFD92F",
         border = "white"
         #axes = FALSE
         )
    
# !!! THE FOLLOWING 2 LINES WORK TO PRODUCE A STATIC X AXIS FROM THE FIRST 
# !!! TIME A TWEET WAS RECORDED TO LAST TIME TWEET WAS RECORDED
    #r <- as.POSIXct(range(times), "mins")
    #axis.POSIXct(1, at = seq(r[1], r[2], by = "min"), format = "%H:%M")
    
# !!! I ALSO COULDN'T GET THE Y AXIS TO DISPLAY PROPER NUMBERS
    #axis(2)
    
    
# !!! THE CODE BELOW DOES NOT WORK BUT I TRIED!
# !!! (TRYING TO CHANGE THE X AXIS SO IT UPDATES WITH THE SLIDER)      
    
    #r <- as.POSIXct(range(times), "mins")
    #axis.POSIXct(1, at = seq(r[1], r[2], by = "min"), format = "%H:%M")
    
    #r_mod <- as.POSIXct(input$RangeA, origin="2016-02-07", "UTC", format = "%M", "mins")
    #r_mod <- as.POSIXct(c(input$RangeA),  format="%M")# "mins") #as.POSIXct(input$RangeA, "mins")
    #r_mod <- as.POSIXct(input$RangeA, format="%a-%b-%d %H:%M:%S", origin="2016-02-07", "UTC")
    #axis.POSIXct(1, at = seq(r_mod[1], r_mod[2], by = "min"), format = "%H:%M")
    
    #r_mod_1 <- format(as.POSIXct(as.numeric(min(input$RangeA))*60, origin="2016-02-07", "UTC"), "%H:%M")
    #r_mod_2 <- format(as.POSIXct(as.numeric(max(input$RangeA))*60, origin="2016-02-07", "UTC"), "%H:%M")
    #axis.POSIXct(1, at = seq(r_mod_1[1], r_mod_2[1], by = "min"), format = "%H:%M")
    
    #r_mod_1 <- format(as.POSIXct((input$RangeA), origin="2016-02-07", "UTC"), "%H:%M")
    
    #min1 <-as.numeric(min(input$range))
    #max1 <-as.numeric(max(input$range))
    
    #r1<-as.POSIXct(min(input$range))
    #r2<-as.POSIXct(max(input$range))
    #axis.POSIXct(1, at = seq(r1, r2, by = "min"), format = "%H:%M")
    
    #r3 <- min(r)
    #r4<- max(r)
    
    #axis.POSIXct(side=1, at=seq(min(input$range), max(input$range), 60), labels=NULL)

    #OR use par()??
    #par(xaxp = c(min(input$range), max$input$range))
    #axis(1)
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
