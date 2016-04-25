#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(navbarPage( "Presidential Candidate Tweet Visualizations",
  
                    
  tabPanel("About",
           textOutput("about1"),
           textOutput("blank1"),
           textOutput("about2"),
           textOutput("about3"),
           textOutput("about4"),
           textOutput("blank2"),
           textOutput("about5")
           ),
                    
  # this puts each output in tabs
  navbarMenu("Compare Tweet Volume by Topic",
    tabPanel("Democrats vs. Republicans",
             plotOutput("t_plot"),
             plotOutput("t_plot2")
             ),
    tabPanel("Trump vs. Cruz",
             plotOutput("r_plot"),
             plotOutput("r_plot2")
    ),
    tabPanel("Sanders vs. Clinton",
             plotOutput("d_plot"),
             plotOutput("d_plot2")
    )
  ),

  navbarMenu("Compare Tweet Volume by Day",  
  tabPanel("Compare Candidates", 
                           
                               sidebarLayout(
                                 sidebarPanel(
                                   helpText("Choose a specific candidate and visualize the number of tweets about them.
                                            You may also change the plot style."),
                                   
                                   selectInput("var", 
                                               label = "Select a Candidate:",
                                               choices = c("hillary","ted","bernie","donald"),
                                               selected = "hillary"),
                                   radioButtons("graphtype","Select a Plot Style:",
                                                choices = c("Stacked Bars", "Side-by-Side Bars","Separate Plots by Topic"),
                                                selected = "Stacked Bars")
                                   
                                              ),
                                 
                           
                                mainPanel( 
                                  
                                  conditionalPanel(
                                    condition = "input.graphtype == 'Stacked Bars'", plotOutput("c_plot_stacked")),
                                  conditionalPanel(
                                    condition = "input.graphtype == 'Side-by-Side Bars'", plotOutput("c_plot_dodge")),
                                  conditionalPanel(
                                    condition = "input.graphtype == 'Separate Plots by Topic'", plotOutput("c_plot_topic_facetwrap"))
                                  
                                  )
                           
                                ) 
                          ),

tabPanel("Compare Parties", 
                           
                           sidebarLayout(
                             sidebarPanel(
                               helpText("Choose a specific political party and visualize the volume of tweets by topic.
                                        You may also change the plot style."),
                               
                               selectInput("var2", 
                                           label = "Select a Political Party:",
                                           choices = c("Democrats","Republicans"),
                                           selected = "Democrats"),
                               radioButtons("graphtype2","Select a Plot Style:",
                                            choices = c("Stacked Bars", "Side-by-Side Bars","Separate Plots by Topic"),
                                            selected = "Stacked Bars")
                               
                                          ),
                             
                             mainPanel( 
                               
                               conditionalPanel(
                                 condition = "input.graphtype2 == 'Stacked Bars'", plotOutput("p_plot_stacked")),
                               conditionalPanel(
                                 condition = "input.graphtype2 == 'Side-by-Side Bars'", plotOutput("p_plot_dodge")),
                               conditionalPanel(
                                 condition = "input.graphtype2 == 'Separate Plots by Topic'", plotOutput("p_plot_topic_facetwrap"))
                               
                               )
                             
                                        ) 
                          )
)
))



