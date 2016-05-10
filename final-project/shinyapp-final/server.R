
# to run locally
setwd("~/GitHub/DataViz/final-project")
tweets.df <- read.csv("./shinyapp-final/alltweets.csv", header = TRUE)

# to run remotely via shinyapps.io
# tweets.df <- read.csv("t2.csv", header = TRUE)


# get rid of NAs
# dems.df <- dems.df[!is.na(dems.df$party2),]
# dems.df <- dems.df[dems.df$party2=="Democrats",]
# 
# reps.df <- reps.df[!is.na(reps.df$party2),]
# reps.df <- reps.df[reps.df$party2=="Republicans",]


library(tm)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  output$c_plot_stacked<- renderPlot({
    #sub_tweets <- subset(tweets.df, tweets.df$candidate == input$var)
    sub_tweets <- subset(tweets.df, tweets.df[[input$var]])
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "stack") +
      labs(x = "Date", y = "Number of Tweets") +
      theme_minimal()
#       scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
#                                   "2016-03-19","2016-03-20","2016-03-21",
#                                   "2016-03-22","2016-03-23","2016-03-24",
#                                   "2016-03-25"), 
#                        labels = c("3/16","3/17","3/18","3/19","3/20",
#                                   "3/21","3/22","3/23","3/24","3/25")) +
#       scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
#                                    "#FFDC00","#2ECC40","#3D9970","#111111",
#                                    "#7FDBFF","#0074D9","#aaaaaa"),
#                         name = "Topics"
#                         labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                    "Immigration","Military","Multiple Topics","Race",
#                                    "Religion","Trade")
#      )
  })
  
  output$c_plot_dodge<- renderPlot({
    #sub_tweets <- subset(tweets.df, tweets.df$candidate == input$var)
    sub_tweets <- subset(tweets.df, tweets.df[[input$var]])
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "dodge") +
      labs(x = "Date", y = "Number of Tweets") +
      theme_minimal()
#       scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
#                                   "2016-03-19","2016-03-20","2016-03-21",
#                                   "2016-03-22","2016-03-23","2016-03-24",
#                                   "2016-03-25"), 
#                        labels = c("3/16","3/17","3/18","3/19","3/20",
#                                   "3/21","3/22","3/23","3/24","3/25")) +
#       scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
#                                    "#FFDC00","#2ECC40","#3D9970","#111111",
#                                    "#7FDBFF","#0074D9","#aaaaaa"),
#                         name = "Topics",
#                         labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                    "Immigration","Military","Multiple Topics","Race",
#                                    "Religion","Trade")
#      )
  })
  
  output$c_plot_topic_facetwrap<- renderPlot({
    #sub_tweets <- subset(tweets.df, tweets.df$candidate == input$var)
    sub_tweets <- subset(tweets.df, tweets.df[[input$var]])
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar() + facet_wrap(~topic) +
      labs(x = "", y = "Number of Tweets") +
      theme_minimal()
#       scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
#                                   "2016-03-19","2016-03-20","2016-03-21",
#                                   "2016-03-22","2016-03-23","2016-03-24",
#                                   "2016-03-25"), 
#                        labels = c("","","","","",
#                                   "","","","","")) +
#       scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
#                                    "#FFDC00","#2ECC40","#3D9970","#111111",
#                                    "#7FDBFF","#0074D9","#aaaaaa"),
#                         name = "Topics",
#                         labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                    "Immigration","Military","Multiple Topics","Race",
#                                    "Religion","Trade")
#       )
  })
  
  output$blank1 <- renderText({
    paste0("-")
  })  
  output$blank2 <- renderText({
    paste0("-")
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
  output$about5 <- renderText({
    paste0("Created by Amanda Gates")
  })
  
  output$p_plot_stacked<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df[[input$var2]])
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "stack") +
      labs(x = "Date", y = "Number of Tweets") +
      theme_minimal()
#       scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
#                                   "2016-03-19","2016-03-20","2016-03-21",
#                                   "2016-03-22","2016-03-23","2016-03-24",
#                                   "2016-03-25"), 
#                        labels = c("3/16","3/17","3/18","3/19","3/20",
#                                   "3/21","3/22","3/23","3/24","3/25")) +
#       scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
#                                    "#FFDC00","#2ECC40","#3D9970","#111111",
#                                    "#7FDBFF","#0074D9","#aaaaaa"),
#                         name = "Topics",
#                         labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                    "Immigration","Military","Multiple Topics","Race",
#                                    "Religion","Trade"))
#     
    
  })
  
  output$p_plot_dodge<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df[[input$var2]])
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar(position = "dodge") +
      labs(x = "Date", y = "Number of Tweets") +
      theme_minimal()
#       scale_x_discrete(breaks = c("2016-03-16","2016-03-17","2016-03-18",
#                                   "2016-03-19","2016-03-20","2016-03-21",
#                                   "2016-03-22","2016-03-23","2016-03-24",
#                                   "2016-03-25"), 
#                        labels = c("3/16","3/17","3/18","3/19","3/20",
#                                   "3/21","3/22","3/23","3/24","3/25")) +
#       scale_fill_manual(values = c("#B10DC9","#F012BE","#85144b","#c2300e",
#                                    "#FFDC00","#2ECC40","#3D9970","#111111",
#                                    "#7FDBFF","#0074D9","#aaaaaa"),
#                         name = "Topics",
#                         labels = c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                    "Immigration","Military","Multiple Topics","Race",
#                                    "Religion","Trade"))
#     
    
  })
  
  output$p_plot_topic_facetwrap<- renderPlot({
    sub_tweets <- subset(tweets.df, tweets.df[[input$var2]])
    ggplot(sub_tweets, aes(date, fill = topic)) + geom_bar() + facet_wrap(~topic) +
      labs(x = "", y = "Number of Tweets") +
      theme_minimal() 
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
    sub_tweets <- subset(tweets.df, tweets.df[[input$var3]])
    ggplot(sub_tweets, aes(topic, fill = input$var3)) + geom_bar(position = "dodge") +
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
  
#   output$t_plot2<- renderPlot({
#     ggplot(tweets.df, aes(topic, fill = party2)) + geom_bar(position = "fill") +
#       scale_fill_manual(values = c("red","blue"), 
#                         name = "Political Parties",
#                         labels = c("Democratic Candidates","Republican Candidates")) +
#       labs(x = "Topic", y = "Proportion of Tweets") +
#       theme_minimal() +
#       scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
#                                 "immigration","military","multiple","race","religion","trade"),
#                        labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                 "Immigration","Military","Multiple Topics","Race",
#                                 "Religion","Trade"))
#     
#   })
#   
#   output$r_plot<- renderPlot({
#     ggplot(reps.df, aes(topic, fill = candidate)) + geom_bar(position = "dodge") +
#       scale_fill_manual(values = c("#00C0E5","#002A33"), name = "Republican Candidates") +
#       labs(x = "Topic", y = "Number of Tweets") +
#       theme_minimal() +
#       scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
#                                 "immigration","military","multiple","race","religion","trade"),
#                        labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                 "Immigration","Military","Multiple Topics","Race",
#                                 "Religion","Trade"))
#   })
#   
#   
#   output$r_plot2<- renderPlot({
#     ggplot(reps.df, aes(topic, fill = candidate)) + geom_bar(position = "fill") +
#       scale_fill_manual(values = c("#00C0E5","#002A33"), name = "Republican Candidates") +
#       labs(x = "Topic", y = "Proportion of Tweets") +
#       theme_minimal() +
#       scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
#                                 "immigration","military","multiple","race","religion","trade"),
#                        labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                 "Immigration","Military","Multiple Topics","Race",
#                                 "Religion","Trade"))
#   })
#   
#   output$d_plot<- renderPlot({
#     ggplot(dems.df, aes(topic, fill = candidate)) + geom_bar(position = "dodge") +
#       scale_fill_manual(values = c("#E50028","#660011"), name = "Democratic Candidates") +
#       labs(x = "Topic", y = "Number of Tweets") +
#       theme_minimal() +
#       scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
#                                 "immigration","military","multiple","race","religion","trade"),
#                        labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                 "Immigration","Military","Multiple Topics","Race",
#                                 "Religion","Trade"))
#   })
#   
#   output$d_plot2<- renderPlot({
#     ggplot(dems.df, aes(topic, fill = candidate)) + geom_bar(position = "fill") +
#       scale_fill_manual(values = c("#E50028","#660011"), name = "Democratic Candidates") +
#       labs(x = "Topic", y = "Proportion of Tweets") +
#       theme_minimal() +
#       scale_x_discrete(breaks=c("china","climatechange","economy","guncontrol","healthcare",
#                                 "immigration","military","multiple","race","religion","trade"),
#                        labels=c("China","Climate Change","Economy","Gun Control","Healthcare",
#                                 "Immigration","Military","Multiple Topics","Race",
#                                 "Religion","Trade"))
#   })
  
  })




