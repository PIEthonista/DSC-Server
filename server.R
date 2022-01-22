library(shiny)
library(dplyr)
library(stringr)

shinyServer(function(input, output, session) {
  #output the entered name
  string <- reactive(paste("Good day and welcome ", input$name, "!"))
  output$greeting <- renderText(string())
  
  #-----------------------------ERROR SECTION/NOT WORKING HAIHHHH
  # Concept refer to: https://gist.github.com/aagarw30/69feeeb7e813788a753b71ef8c0877eb
  movieordrama <- c("Movie")
  year <- c("-")
  duration <- c("-")
  genre <- c("-")
  kids <- c(FALSE)
  storage <- reactiveValues(df=data.frame(movieordrama,year,duration,genre,kids))
  
  getMode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  observeEvent(input$search, {
    movieordrama=input$movieordrama
    year=input$year
    duration=input$duration
    genre=input$genre
    kids=input$kids
    temp <- data.frame(movieordrama, year, duration, genre, kids)
    storage$df <- rbind(storage$df, temp)
    #print(storage$df)  #to test if above code section is run
    #print(getMode(storage$df$movieordrama))
    #print(getMode(storage$df$year))
    #print(getMode(storage$df$duration))
    #print(getMode(storage$df$genre))
    #print(getMode(storage$df$kids))
    
  })
  
  data <- eventReactive(input$search, {
    switch(input$movieordrama,
           "Movie" = moviee,
           "TV Show" = dramaa)
    
    data1 <- if(input$movieordrama=="Movie"){
      switch(input$year,
             "-" = moviee,  
             "1920s" = filter(moviee, between(moviee[,2],1920,1929)),
             "1930s" = filter(moviee, between(moviee[,2],1930,1939)),
             "1940s" = filter(moviee, between(moviee[,2],1940,1949)),
             "1950s" = filter(moviee, between(moviee[,2],1950,1959)),
             "1960s" = filter(moviee, between(moviee[,2],1960,1969)),
             "1970s" = filter(moviee, between(moviee[,2],1970,1979)),
             "1980s" = filter(moviee, between(moviee[,2],1980,1989)),
             "1990s" = filter(moviee, between(moviee[,2],1990,1999)),
             "2000s" = filter(moviee, between(moviee[,2],2000,2009)),
             "2010s" = filter(moviee, between(moviee[,2],2010,2019)),
             "2020s" = filter(moviee, between(moviee[,2],2020,2029))
      )
      
    } else if (input$movieordrama == "TV Show"){
      switch (input$year,
              "-" = dramaa,
              "1920s" = filter(dramaa, between(dramaa[,2],1920,1929)),
              "1930s" = filter(dramaa, between(dramaa[,2],1930,1939)),
              "1940s" = filter(dramaa, between(dramaa[,2],1940,1949)),
              "1950s" = filter(dramaa, between(dramaa[,2],1950,1959)),
              "1960s" = filter(dramaa, between(dramaa[,2],1960,1969)),
              "1970s" = filter(dramaa, between(dramaa[,2],1970,1979)),
              "1980s" = filter(dramaa, between(dramaa[,2],1980,1989)),
              "1990s" = filter(dramaa, between(dramaa[,2],1990,1999)),
              "2000s" = filter(dramaa, between(dramaa[,2],2000,2009)),
              "2010s" = filter(dramaa, between(dramaa[,2],2010,2019)),
              "2020s" = filter(dramaa, between(dramaa[,2],2020,2029))
      )
    }
    
    c1 <- c("30 min","31 min","32 min","33 min","34 min","35 min","36 min","37 min","38 min","39 min","40 min",
            "41 min","42 min","43 min","44 min","45 min","46 min","47 min","48 min","49 min","50 min",
            "51 min","52 min","53 min","54 min","55 min","56 min","57 min","58 min","59 min","60 min")
    c2 <- c("61 min","62 min","63 min","64 min","65 min","66 min","67 min","68 min","69 min","70 min",
            "71 min","72 min","73 min","74 min","75 min","76 min","77 min","78 min","79 min","80 min",
            "81 min","82 min","83 min","84 min","85 min","86 min","87 min","88 min","89 min","90 min")
    c3 <- c("91 min","92 min","93 min","94 min","95 min","96 min","97 min","98 min","99 min","100 min",
            "101 min","102 min","103 min","104 min","105 min","106 min","107 min","108 min","109 min","110 min",
            "111 min","112 min","113 min","114 min","115 min","116 min","117 min","118 min","119 min","120 min")
    c4 <- c("121 min","122 min","123 min","124 min","125 min","126 min","127 min","128 min","129 min","130 min",
            "131 min","132 min","133 min","134 min","135 min","136 min","137 min","138 min","139 min","140 min",
            "141 min","142 min","143 min","144 min","145 min","146 min","147 min","148 min","149 min","150 min")
    c5 <- c("151 min","152 min","153 min","154 min","155 min","156 min","157 min","158 min","159 min","160 min",
            "161 min","162 min","163 min","164 min","165 min","166 min","167 min","168 min","169 min","170 min",
            "171 min","172 min","173 min","174 min","175 min","176 min","177 min","178 min","179 min","180 min")
    c6 <- c("181 min","182 min","183 min","184 min","185 min","186 min","187 min","188 min","189 min","190 min",
            "191 min","192 min","193 min","194 min","195 min","196 min","197 min","198 min","199 min","200 min",
            "201 min","202 min","203 min","204 min","205 min","206 min","207 min","208 min","209 min","210 min")
    c7 <- c("211 min","212 min","213 min","214 min","215 min","216 min","217 min","218 min","219 min","220 min",
            "221 min","222 min","223 min","224 min","225 min","226 min","227 min","228 min","229 min","230 min",
            "231 min","232 min","233 min","234 min","235 min","236 min","237 min","238 min","239 min","240 min")
    c8 <- c("241 min","242 min","243 min","244 min","245 min","246 min","247 min","248 min","249 min","250 min",
            "251 min","252 min","253 min","254 min","255 min","256 min","257 min","258 min","259 min","260 min",
            "261 min","262 min","263 min","264 min","265 min","266 min","267 min","268 min","269 min","270 min")
    c9 <- c("271 min","272 min","273 min","274 min","275 min","276 min","277 min","278 min","279 min","280 min",
            "281 min","282 min","283 min","284 min","285 min","286 min","287 min","288 min","289 min","290 min",
            "291 min","292 min","293 min","294 min","295 min","296 min","297 min","298 min","299 min","300 min")
    c10 <- c("301 min","302 min","303 min","304 min","305 min","306 min","307 min","308 min","309 min","310 min",
             "311 min","312 min","313 min","314 min","315 min","316 min","317 min","318 min","319 min","320 min",
             "321 min","322 min","323 min","324 min","325 min","326 min","327 min","328 min","329 min","330 min")
    
    
    data2 <- switch(input$duration,
                    "-" = data1,
                    "30-60"  = filter(data1, data1[,4] %in% c1),
                    "61-90"  = filter(data1, data1[,4] %in% c2),
                    "91-120" = filter(data1, data1[,4] %in% c3),
                    "121-150"= filter(data1, data1[,4] %in% c4),
                    "151-180"= filter(data1, data1[,4] %in% c5),
                    "181-210"= filter(data1, data1[,4] %in% c6),
                    "211-240"= filter(data1, data1[,4] %in% c7),
                    "241-270"= filter(data1, data1[,4] %in% c8),
                    "271-300"= filter(data1, data1[,4] %in% c9),
                    "301-330"= filter(data1, data1[,4] %in% c10)
    )
    
    data3 <- switch (input$genre,
                     "-" = data2,
                     "Action" = filter(data2, str_detect(data2[,5],"Action")),
                     "Adventure" = filter(data2, str_detect(data2[,5],"Adventure")),
                     "Animation" = filter(data2, str_detect(data2[,5],"Animation")),
                     "Biography" = filter(data2, str_detect(data2[,5],"Biography")),
                     "Comedy" = filter(data2, str_detect(data2[,5],"Comedy")),
                     "Crime" = filter(data2, str_detect(data2[,5],"Crime")),
                     "Drama" = filter(data2, str_detect(data2[,5],"Drama")),
                     "Family" = filter(data2, str_detect(data2[,5],"Family")),
                     "Fantasy" = filter(data2, str_detect(data2[,5],"Fantasy")),
                     "Film-Noir" = filter(data2, str_detect(data2[,5],"Film-Noir")),
                     "History" = filter(data2, str_detect(data2[,5],"History")),
                     "Horror" = filter(data2, str_detect(data2[,5],"Horror")),
                     "Music" = filter(data2, str_detect(data2[,5],"Music")),
                     "Musical" = filter(data2, str_detect(data2[,5],"Musical")),
                     "Mystery" = filter(data2, str_detect(data2[,5],"Mystery")),
                     "Romance" = filter(data2, str_detect(data2[,5],"Romance")),
                     "Sci-Fi" = filter(data2, str_detect(data2[,5],"Sci-Fi")),
                     "Sport" = filter(data2, str_detect(data2[,5],"Sport")),
                     "Thriller" = filter(data2, str_detect(data2[,5],"Thriller")),
                     "War" = filter(data2, str_detect(data2[,5],"War")),
                     "Western" = filter(data2, str_detect(data2[,5],"Western"))
    )
    
    data3 <- if(input$kids==TRUE){
      filter(data3, grepl("UA", data3[,3]) |
               grepl("U", data3[,3]) |
               grepl("PG", data3[,3]) |
               grepl("G", data3[,3]) |
               grepl("Passed", data3[,3]) |
               grepl("GP", data3[,3]) |
               grepl("Approved", data3[,3]) |
               grepl("U/A", data3[,3]))
    } else {
      data3
    }
    
    data3<- data3 %>%
            select(,-7) %>%
            rename(Title=Series_Title, Year=Released_Year, IMDB=IMDB_Rating)
    
  })
  
  output$Drama <- DT::renderDataTable({
    DT::datatable(data(),rownames = FALSE)
  })
  
#=========================================================================================================================================
    recommendationtest <- eventReactive(input$search, {
    
    recommendation1 <- switch(getMode(storage$df$movieordrama),
                              "Movie" = recommendationmovie,
                              "TV Show" = recommendationdrama)
    recommendation2 <- if(getMode(storage$df$movieordrama=="Movie")){
      switch(getMode(storage$df$year),
             "-" = recommendationmovie,  
             "1920s" = filter(recommendationmovie, between(recommendationmovie[,3],1920,1929)),
             "1930s" = filter(recommendationmovie, between(recommendationmovie[,3],1930,1939)),
             "1940s" = filter(recommendationmovie, between(recommendationmovie[,3],1940,1949)),
             "1950s" = filter(recommendationmovie, between(recommendationmovie[,3],1950,1959)),
             "1960s" = filter(recommendationmovie, between(recommendationmovie[,3],1960,1969)),
             "1970s" = filter(recommendationmovie, between(recommendationmovie[,3],1970,1979)),
             "1980s" = filter(recommendationmovie, between(recommendationmovie[,3],1980,1989)),
             "1990s" = filter(recommendationmovie, between(recommendationmovie[,3],1990,1999)),
             "2000s" = filter(recommendationmovie, between(recommendationmovie[,3],2000,2009)),
             "2010s" = filter(recommendationmovie, between(recommendationmovie[,3],2010,2019)),
             "2020s" = filter(recommendationmovie, between(recommendationmovie[,3],2020,2029))
      )
      
    } else if (getMode(storage$df$movieordrama == "TV Show")){
      switch (getMode(storage$df$year),
              "-" = recommendationdrama,
              "1920s" = filter(recommendationdrama, between(recommendationdrama[,2],1920,1929)),
              "1930s" = filter(recommendationdrama, between(recommendationdrama[,2],1930,1939)),
              "1940s" = filter(recommendationdrama, between(recommendationdrama[,2],1940,1949)),
              "1950s" = filter(recommendationdrama, between(recommendationdrama[,2],1950,1959)),
              "1960s" = filter(recommendationdrama, between(recommendationdrama[,2],1960,1969)),
              "1970s" = filter(recommendationdrama, between(recommendationdrama[,2],1970,1979)),
              "1980s" = filter(recommendationdrama, between(recommendationdrama[,2],1980,1989)),
              "1990s" = filter(recommendationdrama, between(recommendationdrama[,2],1990,1999)),
              "2000s" = filter(recommendationdrama, between(recommendationdrama[,2],2000,2009)),
              "2010s" = filter(recommendationdrama, between(recommendationdrama[,2],2010,2019)),
              "2020s" = filter(recommendationdrama, between(recommendationdrama[,2],2020,2029))
      )
    }
    
     c1 <- c("30 min","31 min","32 min","33 min","34 min","35 min","36 min","37 min","38 min","39 min","40 min",
             "41 min","42 min","43 min","44 min","45 min","46 min","47 min","48 min","49 min","50 min",
             "51 min","52 min","53 min","54 min","55 min","56 min","57 min","58 min","59 min","60 min")
     c2 <- c("61 min","62 min","63 min","64 min","65 min","66 min","67 min","68 min","69 min","70 min",
             "71 min","72 min","73 min","74 min","75 min","76 min","77 min","78 min","79 min","80 min",
             "81 min","82 min","83 min","84 min","85 min","86 min","87 min","88 min","89 min","90 min")
     c3 <- c("91 min","92 min","93 min","94 min","95 min","96 min","97 min","98 min","99 min","100 min",
             "101 min","102 min","103 min","104 min","105 min","106 min","107 min","108 min","109 min","110 min",
             "111 min","112 min","113 min","114 min","115 min","116 min","117 min","118 min","119 min","120 min")
     c4 <- c("121 min","122 min","123 min","124 min","125 min","126 min","127 min","128 min","129 min","130 min",
             "131 min","132 min","133 min","134 min","135 min","136 min","137 min","138 min","139 min","140 min",
             "141 min","142 min","143 min","144 min","145 min","146 min","147 min","148 min","149 min","150 min")
     c5 <- c("151 min","152 min","153 min","154 min","155 min","156 min","157 min","158 min","159 min","160 min",
             "161 min","162 min","163 min","164 min","165 min","166 min","167 min","168 min","169 min","170 min",
             "171 min","172 min","173 min","174 min","175 min","176 min","177 min","178 min","179 min","180 min")
     c6 <- c("181 min","182 min","183 min","184 min","185 min","186 min","187 min","188 min","189 min","190 min",
             "191 min","192 min","193 min","194 min","195 min","196 min","197 min","198 min","199 min","200 min",
             "201 min","202 min","203 min","204 min","205 min","206 min","207 min","208 min","209 min","210 min")
     c7 <- c("211 min","212 min","213 min","214 min","215 min","216 min","217 min","218 min","219 min","220 min",
             "221 min","222 min","223 min","224 min","225 min","226 min","227 min","228 min","229 min","230 min",
             "231 min","232 min","233 min","234 min","235 min","236 min","237 min","238 min","239 min","240 min")
     c8 <- c("241 min","242 min","243 min","244 min","245 min","246 min","247 min","248 min","249 min","250 min",
             "251 min","252 min","253 min","254 min","255 min","256 min","257 min","258 min","259 min","260 min",
             "261 min","262 min","263 min","264 min","265 min","266 min","267 min","268 min","269 min","270 min")
     c9 <- c("271 min","272 min","273 min","274 min","275 min","276 min","277 min","278 min","279 min","280 min",
             "281 min","282 min","283 min","284 min","285 min","286 min","287 min","288 min","289 min","290 min",
             "291 min","292 min","293 min","294 min","295 min","296 min","297 min","298 min","299 min","300 min")
     c10 <- c("301 min","302 min","303 min","304 min","305 min","306 min","307 min","308 min","309 min","310 min",
              "311 min","312 min","313 min","314 min","315 min","316 min","317 min","318 min","319 min","320 min",
              "321 min","322 min","323 min","324 min","325 min","326 min","327 min","328 min","329 min","330 min")
    
    
    recommendation3 <- switch(getMode(storage$df$duration),
                    "-" = recommendation2,
                    "30-60"  = filter(recommendation2, recommendation2[,5] %in% c1),
                    "61-90"  = filter(recommendation2, recommendation2[,5] %in% c2),
                    "91-120" = filter(recommendation2, recommendation2[,5] %in% c3),
                    "121-150"= filter(recommendation2, recommendation2[,5] %in% c4),
                    "151-180"= filter(recommendation2, recommendation2[,5] %in% c5),
                    "181-210"= filter(recommendation2, recommendation2[,5] %in% c6),
                    "211-240"= filter(recommendation2, recommendation2[,5] %in% c7),
                    "241-270"= filter(recommendation2, recommendation2[,5] %in% c8),
                    "271-300"= filter(recommendation2, recommendation2[,5] %in% c9),
                    "301-330"= filter(recommendation2, recommendation2[,5] %in% c10)
    )
    
    recommendation4 <- switch (getMode(storage$df$genre),
                     "-" = recommendation3,
                     "Action" = filter(recommendation3, str_detect(recommendation3[,6],"Action")),
                     "Adventure" = filter(recommendation3, str_detect(recommendation3[,6],"Adventure")),
                     "Animation" = filter(recommendation3, str_detect(recommendation3[,6],"Animation")),
                     "Biography" = filter(recommendation3, str_detect(recommendation3[,6],"Biography")),
                     "Comedy" = filter(recommendation3, str_detect(recommendation3[,6],"Comedy")),
                     "Crime" = filter(recommendation3, str_detect(recommendation3[,6],"Crime")),
                     "Drama" = filter(recommendation3, str_detect(recommendation3[,6],"Drama")),
                     "Family" = filter(recommendation3, str_detect(recommendation3[,6],"Family")),
                     "Fantasy" = filter(recommendation3, str_detect(recommendation3[,6],"Fantasy")),
                     "Film-Noir" = filter(recommendation3, str_detect(recommendation3[,6],"Film-Noir")),
                     "History" = filter(recommendation3, str_detect(recommendation3[,6],"History")),
                     "Horror" = filter(recommendation3, str_detect(recommendation3[,6],"Horror")),
                     "Music" = filter(recommendation3, str_detect(recommendation3[,6],"Music")),
                     "Musical" = filter(recommendation3, str_detect(recommendation3[,6],"Musical")),
                     "Mystery" = filter(recommendation3, str_detect(recommendation3[,6],"Mystery")),
                     "Romance" = filter(recommendation3, str_detect(recommendation3[,6],"Romance")),
                     "Sci-Fi" = filter(recommendation3, str_detect(recommendation3[,6],"Sci-Fi")),
                     "Sport" = filter(recommendation3, str_detect(recommendation3[,6],"Sport")),
                     "Thriller" = filter(recommendation3, str_detect(recommendation3[,6],"Thriller")),
                     "War" = filter(recommendation3, str_detect(recommendation3[,6],"War")),
                     "Western" = filter(recommendation3, str_detect(recommendation3[,6],"Western"))
    )
    
    recommendation5 <- if(getMode(storage$df$kids)==TRUE){
      filter(recommendation4, grepl("UA", recommendation4[,4]) |
               grepl("U", recommendation4[,4]) |
               grepl("PG", recommendation4[,4]) |
               grepl("G", recommendation4[,4]) |
               grepl("Passed", recommendation4[,4]) |
               grepl("GP", recommendation4[,4]) |
               grepl("Approved", recommendation4[,4]) |
               grepl("U/A", recommendation4[,4]))
    } else {
      recommendation4
    }
    
    recommendation <- recommendation5 %>%
                      rename(Title=Series_Title, Year=Released_Year, IMDB=IMDB_Rating)
    
  })
#=========================================================================================================================================
# Random generation for NAs
  observeEvent(input$search,{
    recommendation <- data.frame(recommendationtest())
    #for NAs
    random<-sample(1:nrow(moviee), 5, replace=FALSE)
    #for not NAs
    random2 <- sample(1:nrow(recommendation), 5, replace=FALSE)

    if(!is.na(recommendation[random2[1],1])){
      src1 = recommendation[random2[1],1]
      output$picture1 <- renderText({c('<img src="',src1,'" width="188" height="274">')})
      output$title1 <- renderText({c('<font size="5">',recommendation[random2[1],2],'</font>')})
      output$rating1 <- renderText({c('Rated ',recommendation[random2[1],7])})
      output$desc1 <- renderText({recommendation[random2[1],8]})
    } else {
      src1 = mydata[random[1],1]
      output$picture1 <- renderText({c('<img src="',src1,'" width="188" height="274">')})
      output$title1 <- renderText({c('<font size="5">',recommendation[random[1],2],'</font>')})
      output$rating1 <- renderText({c('Rated ',mydata[random[1],7])})
      output$desc1 <- renderText({mydata[random[1],8]})
    }
    
    if(!is.na(recommendation[random2[2],1])){
      src2 = recommendation[random2[2],1]
      output$picture2 <- renderText({c('<img src="',src2,'" width="188" height="274">')})
      output$title2 <- renderText({c('<font size="5">',recommendation[random2[2],2],'</font>')})
      output$rating2 <- renderText({c('Rated ',recommendation[random2[2],7])})
      output$desc2 <- renderText({recommendation[random2[2],8]})
    } else {
      src2 = mydata[random[2],1]
      output$picture2 <- renderText({c('<img src="',src2,'" width="188" height="274">')})
      output$title2 <- renderText({c('<font size="5">',recommendation[random[2],2],'</font>')})
      output$rating2 <- renderText({c('Rated ',mydata[random[2],7])})
      output$desc2 <- renderText({mydata[random[2],8]})
    }
    
    if(!is.na(recommendation[random2[3],1])){
      src3 = recommendation[random2[3],1]
      output$picture3 <- renderText({c('<img src="',src3,'" width="188" height="274">')})
      output$title3 <- renderText({c('<font size="5">',recommendation[random2[3],2],'</font>')})
      output$rating3 <- renderText({c('Rated ',recommendation[random2[3],7])})
      output$desc3 <- renderText({recommendation[random2[3],8]})
    } else {
      src3 = mydata[random[3],1]
      output$picture3 <- renderText({c('<img src="',src3,'" width="188" height="274">')})
      output$title3 <- renderText({c('<font size="5">',recommendation[random[3],2],'</font>')})
      output$rating3 <- renderText({c('Rated ',mydata[random[3],7])})
      output$desc3 <- renderText({mydata[random[3],8]})
    }
    
    if(!is.na(recommendation[random2[4],1])){
      src4 = recommendation[random2[4],1]
      output$picture4 <- renderText({c('<img src="',src4,'" width="188" height="274">')})
      output$title4 <- renderText({c('<font size="5">',recommendation[random2[4],2],'</font>')})
      output$rating4 <- renderText({c('Rated ',recommendation[random2[4],7])})
      output$desc4 <- renderText({recommendation[random2[4],8]})
    } else {
      src4 = mydata[random[4],1]
      output$picture4 <- renderText({c('<img src="',src4,'" width="188" height="274">')})
      output$title4 <- renderText({c('<font size="5">',recommendation[random[4],2],'</font>')})
      output$rating4 <- renderText({c('Rated ',mydata[random[4],7])})
      output$desc4 <- renderText({mydata[random[4],8]})
    }
    
    if(!is.na(recommendation[random2[5],1])){
      src5 = recommendation[random2[5],1]
      output$picture5 <- renderText({c('<img src="',src5,'" width="188" height="274">')})
      output$title5 <- renderText({c('<font size="5">',recommendation[random2[5],2],'</font>')})
      output$rating5 <- renderText({c('Rated ',recommendation[random2[5],7])})
      output$desc5 <- renderText({recommendation[random2[5],8]})
    } else {
      src5 = mydata[random[5],1]
      output$picture5 <- renderText({c('<img src="',src5,'" width="188" height="274">')})
      output$title5 <- renderText({c('<font size="5">',recommendation[random[5],2],'</font>')})
      output$rating5 <- renderText({c('Rated ',mydata[random[5],7])})
      output$desc5 <- renderText({mydata[random[5],8]})
    }
    #Haven't code if recommendation list is empty how to random choose row
  })
  
  output$Test <- DT::renderDataTable({
    DT::datatable(recommendationtest(),rownames = FALSE)
  })
  
})