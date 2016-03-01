#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel(title = h2("Tweets per Candidate", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a specific candidate, visualize the volume of tweets"),
      
      selectInput("var", 
                  label = "Choose a candidate:",
                  choices = c("Jeb Bush","Ben Carson","Chris Christie","Hillary Clinton","Ted Cruz","Carly Fiorina", 
                              "John Kasich","Marco Rubio","Bernie Sanders","Donald Trump"),
                  selected = "Jeb Bush"),
      
        sliderInput("RangeA", 
                    label = "Range of Time (Minutes)",
                    min = 0, max = 30, value = c(0, 30),
                    timeFormat ="POSIXt")
     ),

  # this is the code that puts all output on one page    
#    mainPanel(
#      plotOutput("wordcloud"),
#      plotOutput("freq_plot"),
#      textOutput("text1")
#    )
    
  # this puts each output in tabs
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Word Cloud", plotOutput("wordcloud")), 
                  tabPanel("Frequency Plot", plotOutput("freq_plot")) 
                  )
    
  )
)))



