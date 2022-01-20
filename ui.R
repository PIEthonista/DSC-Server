# import the required library
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
library(DT)
library(tibble)

# read the data set
#mydata <- data.frame(read.csv("imdb_top_1000.csv"))
mydata <- data.frame(read.csv("https://raw.githubusercontent.com/PIEthonista/DSC-Data-Hosting/main/imdb_top_1000.csv"))
# filter data and get the important points needed for customer
# displaydata <- mydata[-c(1,4,9,15,16)]
displaydata <- mydata[-c(1,9,15,16)]
#filter out drama data set
dramavalidity <- data.frame(str_detect(displaydata$Genre, "Drama"))
dramafilter <- cbind(displaydata, drama = dramavalidity[,1])
drama <- dramafilter[!(dramafilter$drama=="FALSE"),]
dramaa <- drama[,-12]
#filter out movie data set
movie <- dramafilter[!(dramafilter$drama=="TRUE"),]
moviee <- movie [,-12]


shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("readable"), #Select a theme for shiny
  navbarPage(
    HTML(paste("<p><font color = '#6495ED' font size = '5' font face = 'Incised901 Nd BT'>",
               "Netflix Movies and TV Shows",
               "</font>")),
    #create a tab named "Home" to make brief introduction regarding this app
    tabPanel("Home", icon = icon("home"),
             sidebarLayout(
               sidebarPanel(
                 h3("Netflix Movies and TV Shows Recommenders"),
                 span("This system seeks to predict or filter preferences according to the your choices.",
                      style = "color:#4682B4"),
                 br(),
                 br(),
                 h4("How can you utilise this system?"),
                 span("First, decide whether you want to watch movie or TV show",
                      style = "color:#4682B4"),
                 br(),
                 span("Secondly, provide us the clear features of your ideal movies or TV shows",
                      style = "color:#4682B4"),
                 br(),
                 span("Thirdly, choose one movie or TV show from the recommendation shown",
                      style = "color:#4682B4"),
                 br(),
                 span("Last but not least, sit back and enjoy the show!",
                      style = "color:#4682B4")
               ),
               mainPanel(
                 #uiOutput("img"),
                 img(src="Poster.png", height="100%", width="100%",align="center"),
                 h6("Compilation of movies and TV shows",align = "center",
                    style = "color:#6495ED"),
               )
             ),
             
    ),
    
    tabPanel("Discoverer", icon = icon("search"),
             titlePanel("What to watch?"),
             sidebarLayout(
               sidebarPanel(
                 textInput("name","Your name: "),
                 br(),
                 radioButtons("movieordrama","Movie or TV Show", c("Movie","TV Show")),
                 selectInput("year","Year",c("-",
                                             "1920s(1920-1929)",
                                             "1930s(1930-1939)",
                                             "1940s(1940-1949)",
                                             "1950s(1950-1959)",
                                             "1960s(1960-1969)",
                                             "1970s(1970-1979)",
                                             "1980s(1980-1989)",
                                             "1990s(1990-1999)",
                                             "2000s(2000-2009)",
                                             "2010s(2010-2019)",
                                             "2020s(2020-2029)")),
                 
                 selectInput("duration","Duration",c("-",
                                                     "30-60",
                                                     "61-90",
                                                     "91-120",
                                                     "121-150",
                                                     "151-180",
                                                     "181-210",
                                                     "211-240",
                                                     "241-270",
                                                     "271-300",
                                                     "301-330")),
                 
                 selectInput("genre","Genre",c("-",
                                               "Action",
                                               "Adventure",
                                               "Animation",
                                               "Biography",
                                               "Comedy",
                                               "Crime",
                                               "Drama",
                                               "Family",
                                               "Fantasy",
                                               "Film-Noir",
                                               "History",
                                               "Horror",
                                               "Music",
                                               "Musical",
                                               "Mystery",
                                               "Romance",
                                               "Sci-Fi",
                                               "Sport",
                                               "Thriller",
                                               "War",
                                               "Western")),
                 
                 checkboxInput("kids","Suitable for kids",value=FALSE),
                 
                 #submitButton("Search",icon("search",lib="glyphicon"))),
                 actionButton("search", "Search",icon("search",lib="glyphicon"))),
               
               mainPanel(
                 fluidRow(
                   h1(textOutput("greeting")),
                   dataTableOutput("Drama")
                   #It will display data table that show a list of movie or TV shows
                 )
               )
             )
    ),
    
    tabPanel("Recommendation", icon = icon("star"),
             titlePanel("Movies that you may like"),
             #Going to show the top 5 movies that most related to user based on their search history
             br(),
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
               p("We hope to provide a mechanism assisting users in classifying and filtering out their desired movie or TV shows.",
                 style = "color:#4682B4"),
               p("We wish to support users in the process of selecting movie or TV show as to ease their life as well.",
                 style = "color:#4682B4"),
               p("We aim to enhance the users' experience and ensure their enjoyment while watching movie or TV show.",
                 style = "color:#4682B4"),
               br(),
               h3("Data Source"),
               p("The data is sourced from Kaggle website, IMDB Movies Dataset by Harshit Shankhdnar.",
                 style = "color:#4682B4"),
               p("The data could be accessed ",a("here",href="https://www.kaggle.com/harshitshankhdhar/imdb-dataset-of-top-1000-movies-and-tv-shows"),
                 style = "color:#4682B4"),
             ),
             mainPanel(
               fluidRow(
                 shiny::HTML("<br>
                           <center>
                           <h1>About The Team</h1>
                           </center>"),
                 style = "height:50px;color:#6495ED"),
               
               fluidRow(
                 shiny::HTML("<br><br>
                                  <center>
                                  <h3>University of Malaya (UM)</h3>
                                  </center>"),
                 shiny::HTML("<center><h4>We are students from University of Malaya currently undertaking Introduction to Data Science (WIA1007). 
                                  We have analyzed the top 1000 movies and TV shows by IMDB Rating and integrated it into our project as to ease the users while probing for their own ideal movie or TV show.
                                  </h4></center><br>"),
                 style = "color:#4682B4"),
               
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
                                  tags$img(src="huoyiqing.jpg",
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
                                  tags$img(src="bingbing.jpg",
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