library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(visNetwork)
library(rintrojs)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(readxl)
library(readr)
library(shinydashboard)
library(shinydisconnect)

shinyUI(fluidPage(
  theme = shinytheme("cyborg"), #Select a theme for shiny
  navbarPage(
    "Netflix Movies and TV Shows",
    #create a tab named "Home" to make brief introduction regarding this app
    tabPanel("Home", setBackgroundColor(color = c("#202020","#404040"),gradient="linear",direction = "bottom"),
             sidebarLayout(
               sidebarPanel(
                 h3("Netflix Movies and TV Shows Recommenders"),
                 p("This system seeks to predict or filter preferences according to the your choices."),
                 h3(""),
                 h4("How can you utilise this system?"),
                 p("First, decide whether you want to watch movie or TV show"),
                 p("Secondly, provide us the clear features of your ideal movies or TV shows"),
                 p("Thirdly, choose one movie or TV show from the recommendation shown"),
                 p("Last but not least, sit back and enjoy the show!")
               ),
               mainPanel(
                     img(src="Poster.png", height="100%", width="100%",align="center"),
                     h6("Compilation of movies and TV shows",align = "center")
               )
             ),
             
             ),
    
    tabPanel("Discoverer",
              titlePanel("What to watch?"),
             sidebarLayout(
               sidebarPanel(
                 radioButtons("movieortv","Movie or TV show",c("Movie","TV show")),
                 
                 selectInput("year","Year",c("1920s(1920-1929)",
                                             "1930s(1930-1939)",
                                             "1940s(1940-1949)",
                                             "1950s(1950-1959)",
                                             "1960s(1960-1969)",
                                             "1970s(1970-1979)",
                                             "1980s(1980-1989)",
                                             "1990s(1990-1999)",
                                             "2000s(2000-2009)",
                                             "2010s(2010-2019)",
                                             "2020s(2020-now)")),
                 
                 selectInput("duration","Movie Duration",c("30-60",
                                                           "61-90",
                                                           "91-120",
                                                           "121-150",
                                                           "151-180",
                                                           "181-210",
                                                           "211-240",
                                                           "241-270",
                                                           "271-300",
                                                           "301-330")),
                 
                 selectInput("genre","Genre",c(genre)),
                 
                 textInput("name","Movie or TV Show Name",value="The Matrix"),
                 
                 submitButton("Search",icon("search",lib="glyphicon"))),
               
               mainPanel(
                 fluidRow(
                   #DTOutput("table")
                   #It will display data table that show a list of movie or TV shows
                 )
                 )
               )
             ),
    
    tabPanel("Recommendation",
             titlePanel("Movies that you may like"),
             #Going to show the top 5 movies that most related to user based on their search history
             fluidRow(
               #Title column size
               column(1,""),
               column(2,
                      "Movie 1"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Movie 2"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Movie 3"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Movie 4"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Movie 5"),style="height=100px;background-color: white;color:black",
               column(1,"")
             ),
             fluidRow(
               #Title column size
               column(1,""),
               column(2,
                      "Content 1"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Content 2"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Content 3"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Content 4"),style="height=100px;background-color: white;color:black",
               column(2,
                      "Content 5"),style="height=100px;background-color: white;color:black",
               column(1,"")
             )
    ),
             
    tabPanel("About", 
             sidebarPanel(
               h3("Purpose of this App"),
               p("We hope to provide a mechanism assisting users in classifying and filtering out their desired movie or TV shows."),
               p("We wish to support users in the process of selecting movie or TV show as to ease their life as well."),
               p("We aim to enhance the users' experience and ensure their enjoyment while watching movie or TV show."),
               br(),
               h3("Data Source"),
               p("The data is sourced from Kaggle website, IMDB Movies Dataset by Harshit Shankhdnar."),
               p("The data could be accessed ",a("here",href="https://www.kaggle.com/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows")),
             ),
             mainPanel(
               fluidRow(
                 shiny::HTML("<br>
                           <center>
                           <h1>About The Team</h1>
                           </center>"),
                 style = "height:50px;"),
               
               fluidRow(
                 shiny::HTML("<br><br>
                                  <center>
                                  <h3>University of Malaya (UM)</h3>
                                  </center>"),
                 shiny::HTML("<center><h4>We are students from University of Malaya currently undertaking Introduction to Data Science (WIA1007). 
                                  We have analyzed the top 1000 movies and TV shows by IMDB Rating and integrated it into our project as to ease the users while probing for their own ideal movie or TV show.
                                  </h4></center><br>")
               ),
               
               fluidRow(
                 column(1),
                 #Goh Yi Xian
                 column(2,
                        div(class="panel panel-default",
                            div(class="panel-body",
                                width="600px",align="center",
                                div(
                                  tags$img(src="yixian.jpg",
                                           width="80px",height="140px")
                                ),
                                div(
                                  tags$h5("Goh Yi Xian"),
                                  tags$h6("Project Lead")
                                )
                            )
                        )
                 ),
                 #Lim Jia Yu
                 column(2,
                        div(class = "panel panel-default",
                            div(class="panel-body",
                                width="600px",align="center",
                                div(
                                  tags$img(src="jiayu.jpg",
                                           width="80px",height="140px")
                                ),
                                div(
                                  tags$h5("Lim Jia Yu"),
                                  tags$h6("Web Developer")
                                )
                            )
                        )
                 ),
                 #Jesson Law Cong Ji
                 column(2,
                        div(class="panel panel-default",
                            div(class="panel-body",
                                width="600px",align="center",
                                div(
                                  tags$img(src="congji.jpg",
                                           width="80px",height="140px")
                                ),
                                div(
                                  tags$h5("Jesson Law Cong Ji"),
                                  tags$h6("Backend Developer")
                                )
                            )
                        )
                 ),
                 #Huo Yi Qing
                 column(2,
                        div(class="panel panel-default",
                            div(class="panel-body",
                                width="600px",align="center",
                                div(
                                  tags$img(src="yiqing.jpg",
                                           width="80px",height="140px")
                                ),
                                div(
                                  tags$h5("Huo Yi Qing"),
                                  tags$h6("Server Developer")
                                )
                            )
                        )
                 ),
                 #Lyu Bin Bin
                 column(2,
                        div(class="panel panel-default",
                            div(class="panel-body",
                                width="600px",align="center",
                                div(
                                  tags$img(src="binbin.jpg",
                                           width="80px",height="140px")
                                ),
                                div(
                                  tags$h5("Lyu Bin Bin"),
                                  tags$h6("Website Designer")
                                )
                            )
                        )
                 ),
                 fluidRow(style="height:150px")
               )
               
             )
             )
    )
  )
)




