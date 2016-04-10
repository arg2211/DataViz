

setwd("~/GitHub/DataViz/Assignment4")
tweets.df <- read.csv("./shinyapp/t2.csv", header = TRUE)

# remove march 26th dates
tweets.df <- tweets.df[!tweets.df$date=="2016-03-26",]

# set up regex for candidate input
candidates <- c("Hillary Clinton","Ted Cruz","Bernie Sanders","Donald Trump","Democrats","Republicans")
candidates_new <- list(c("hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc","Hillary Clinton"),
                       c("ted|cruz","Ted Cruz"),
                       c("bernie|sanders|feelthebern","Bernie Sanders"),
                       c("donald|trump|drumpf|makeamericagreatagain","Donald Trump")
                       )

output_2 <- character(nrow(tweets.df))
for(i in seq_along(candidates_new)){
  output_2[grepl(x = tweets.df$text, ignore.case = TRUE, 
                 pattern = candidates_new[[i]][1])] <- candidates_new[[i]][2]
} 
tweets.df$candidate <- output_2

tweets.df$party2[tweets.df$candidate=="Hillary Clinton"] <- "Democrats"
tweets.df$party2[tweets.df$candidate=="Bernie Sanders"] <- "Democrats"
tweets.df$party2[tweets.df$candidate=="Ted Cruz"] <- "Republicans"
tweets.df$party2[tweets.df$candidate=="Donald Trump"] <- "Republicans"

hc.df <- subset(tweets.df, subset = grepl("hillary|clinton|imwithher|hillyes|giveemhill|sheswithus|hrc", text, ignore.case=TRUE))
tc.df <- subset(tweets.df, subset = grepl("ted|cruz", text, ignore.case=TRUE))
bs.df <- subset(tweets.df, subset = grepl("bernie|sanders|feelthebern", text, ignore.case=TRUE))
dt.df <- subset(tweets.df, subset = grepl("donald|trump|drumpf|makeamericagreatagain", text, ignore.case=TRUE))
dems.df <- rbind(hc.df,bs.df)
reps.df <- rbind(tc.df,dt.df)

dems.df <- dems.df[!is.na(dems.df$party2),]
dems.df <- dems.df[dems.df$party2=="Democrats",]

reps.df <- reps.df[!is.na(reps.df$party2),]
reps.df <- reps.df[reps.df$party2=="Republicans",]


library(tm)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  output$c_plot_stacked<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df$candidate == input$var)
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "stack") +
      labs(x = "Date", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
                                  "2016-03-19","2016-03-20","2016-03-21",
                                  "2016-03-22","2016-03-23","2016-03-24",
                                  "2016-03-25"), 
                       labels = c("3/16","3/17","3/18","3/19","3/20",
                                  "3/21","3/22","3/23","3/24","3/25")) +
      scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
                                   "#FFDC00","#2ECC40","#3D9970","#111111",
                                   "#7FDBFF","#0074D9","#aaaaaa"),
                        name = "Topics",
                        labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
                                   "Immigration","Military","Multiple Topics","Race",
                                   "Religion","Trade")
                        )
  })
  
  output$c_plot_dodge<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df$candidate == input$var)
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "dodge") +
      labs(x = "Date", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
                                  "2016-03-19","2016-03-20","2016-03-21",
                                  "2016-03-22","2016-03-23","2016-03-24",
                                  "2016-03-25"), 
                       labels = c("3/16","3/17","3/18","3/19","3/20",
                                  "3/21","3/22","3/23","3/24","3/25")) +
      scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
                                   "#FFDC00","#2ECC40","#3D9970","#111111",
                                   "#7FDBFF","#0074D9","#aaaaaa"),
                        name = "Topics",
                        labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
                                   "Immigration","Military","Multiple Topics","Race",
                                   "Religion","Trade")
      )
  })
  
  output$c_plot_topic_facetwrap<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df$candidate == input$var)
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar() + facet_wrap(~topic) +
      labs(x = "", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
                                  "2016-03-19","2016-03-20","2016-03-21",
                                  "2016-03-22","2016-03-23","2016-03-24",
                                  "2016-03-25"), 
                       labels = c("","","","","",
                                  "","","","","")) +
      scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
                                   "#FFDC00","#2ECC40","#3D9970","#111111",
                                   "#7FDBFF","#0074D9","#aaaaaa"),
                        name = "Topics",
                        labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
                                   "Immigration","Military","Multiple Topics","Race",
                                   "Religion","Trade")
      )
  })

    
  output$about1 <- renderText({
    paste0("Welcome!")
  })
  output$about2 <- renderText({
    paste0("This Shiny app visualizes the volume of tweets about 
           the remaining four presidential candidates: Hillary Clinton,
           Ted Cruz, Bernie Sanders, and Donald Trump.")
  })
  output$about3 <- renderText({
    paste0("The tweets used here were collected daily from March 16, 2016 to March 25, 2016.")
  })
  output$about4 <- renderText({
    paste0("Click on the panel tabs to view different plots about the tweets.")
  })
  
  output$p_plot_stacked<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df$party2 == input$var2)
        ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "stack") +
          labs(x = "Date", y = "Number of Tweets") +
          theme_minimal() +
          scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
                                      "2016-03-19","2016-03-20","2016-03-21",
                                      "2016-03-22","2016-03-23","2016-03-24",
                                      "2016-03-25"), 
                           labels = c("3/16","3/17","3/18","3/19","3/20",
                                      "3/21","3/22","3/23","3/24","3/25")) +
          scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
                                       "#FFDC00","#2ECC40","#3D9970","#111111",
                                       "#7FDBFF","#0074D9","#aaaaaa"),
                            name = "Topics",
                            labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
                                       "Immigration","Military","Multiple Topics","Race",
                                       "Religion","Trade"))
          
    
  })

  output$p_plot_dodge<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df$party2 == input$var2)
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "dodge") +
      labs(x = "Date", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
                                  "2016-03-19","2016-03-20","2016-03-21",
                                  "2016-03-22","2016-03-23","2016-03-24",
                                  "2016-03-25"), 
                       labels = c("3/16","3/17","3/18","3/19","3/20",
                                  "3/21","3/22","3/23","3/24","3/25")) +
      scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
                                   "#FFDC00","#2ECC40","#3D9970","#111111",
                                   "#7FDBFF","#0074D9","#aaaaaa"),
                        name = "Topics",
                        labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
                                   "Immigration","Military","Multiple Topics","Race",
                                   "Religion","Trade"))
    
    
  })
  
  output$p_plot_topic_facetwrap<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df$party2 == input$var2)
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar() + facet_wrap(~topic) +
      labs(x = "", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
                                  "2016-03-19","2016-03-20","2016-03-21",
                                  "2016-03-22","2016-03-23","2016-03-24",
                                  "2016-03-25"), 
                       labels = c("","","","","",
                                  "","","","","")) +
      scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
                                   "#FFDC00","#2ECC40","#3D9970","#111111",
                                   "#7FDBFF","#0074D9","#aaaaaa"),
                        name = "Topics",
                        labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
                                   "Immigration","Military","Multiple Topics","Race",
                                   "Religion","Trade"))
    
    
  })
  
    
  output$t_plot<- renderPlot({
    ggplot(tweets.df, aes(topic, fill = party2)) + geom_bar(position = "dodge") +
      scale_fill_manual(values = c("red","blue"), 
                        name = "Political Parties",
                        labels = c("Democratic Candidates","Republican Candidates")) +
      labs(x = "Topic", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
                                "immigration","military","multiple","race","religion","trade"),
                       labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
                                "Immigration","Military","Multiple Topics","Race",
                                "Religion","Trade"))
    
  })
  
  output$t_plot2<- renderPlot({
    ggplot(tweets.df, aes(topic, fill = party2)) + geom_bar(position = "fill") +
      scale_fill_manual(values = c("red","blue"), 
                        name = "Political Parties",
                        labels = c("Democratic Candidates","Republican Candidates")) +
      labs(x = "Topic", y = "Proportion of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
                                "immigration","military","multiple","race","religion","trade"),
                       labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
                                "Immigration","Military","Multiple Topics","Race",
                                "Religion","Trade"))
    
  })
  
  output$r_plot<- renderPlot({
    ggplot(reps.df, aes(topic, fill = candidate)) + geom_bar(position = "dodge") +
      scale_fill_manual(values = c("#00C0E5","#002A33"), name = "Republican Candidates") +
      labs(x = "Topic", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
                                "immigration","military","multiple","race","religion","trade"),
                       labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
                                "Immigration","Military","Multiple Topics","Race",
                                "Religion","Trade"))
  })
  
  
  output$r_plot2<- renderPlot({
    ggplot(reps.df, aes(topic, fill = candidate)) + geom_bar(position = "fill") +
      scale_fill_manual(values = c("#00C0E5","#002A33"), name = "Republican Candidates") +
      labs(x = "Topic", y = "Proportion of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
                                "immigration","military","multiple","race","religion","trade"),
                       labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
                                "Immigration","Military","Multiple Topics","Race",
                                "Religion","Trade"))
  })
  
  output$d_plot<- renderPlot({
    ggplot(dems.df, aes(topic, fill = candidate)) + geom_bar(position = "dodge") +
      scale_fill_manual(values = c("#E50028","#660011"), name = "Democratic Candidates") +
      labs(x = "Topic", y = "Number of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
                                "immigration","military","multiple","race","religion","trade"),
                       labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
                                "Immigration","Military","Multiple Topics","Race",
                                "Religion","Trade"))
  })
  
  output$d_plot2<- renderPlot({
    ggplot(dems.df, aes(topic, fill = candidate)) + geom_bar(position = "fill") +
      scale_fill_manual(values = c("#E50028","#660011"), name = "Democratic Candidates") +
      labs(x = "Topic", y = "Proportion of Tweets") +
      theme_minimal() +
      scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
                                "immigration","military","multiple","race","religion","trade"),
                       labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
                                "Immigration","Military","Multiple Topics","Race",
                                "Religion","Trade"))
  })
  
  })
  

  
  
