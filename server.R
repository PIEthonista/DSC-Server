library(shiny)
library(dplyr)
library(stringr)

shinyServer(function(input, output, session) {
  #output the entered name
  string <- reactive(paste("Good day and welcome ", input$name, "!"))
  output$greeting <- renderText(string())
  
  #-----------------------------ERROR SECTION/NOT WORKING HAIHHHH
  # Concepr refer to: https://gist.github.com/aagarw30/69feeeb7e813788a753b71ef8c0877eb
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
  #test <- observeEvent(c(input$search,input$movieordrama,input$year,input$duration,input$genre,input$kids), {
    movieordrama=input$movieordrama
    year=input$year
    duration=input$duration
    genre=input$genre
    kids=input$kids
    temp <- data.frame(movieordrama, year, duration, genre, kids)
    storage$df <- rbind(storage$df, temp)
    print("===============================")
    print(storage$df)  #to test if above code section is run
    print(getMode(storage$df$movieordrama))
    print(getMode(storage$df$year))
    print(getMode(storage$df$duration))
    print(getMode(storage$df$genre))
    print(getMode(storage$df$kids))
  })
    
    data <- eventReactive(input$search, {
      switch(input$movieordrama,
             "Movie" = moviee,
             "TV Show" = dramaa)
      
      data1 <- if(input$movieordrama=="Movie"){
        switch(input$year,
               "-" = moviee,  
               "1920s(1920-1929)" = filter(moviee, between(moviee[,2],1920,1929)),
               "1930s(1930-1939)" = filter(moviee, between(moviee[,2],1930,1939)),
               "1940s(1940-1949)" = filter(moviee, between(moviee[,2],1940,1949)),
               "1950s(1950-1959)" = filter(moviee, between(moviee[,2],1950,1959)),
               "1960s(1960-1969)" = filter(moviee, between(moviee[,2],1960,1969)),
               "1970s(1970-1979)" = filter(moviee, between(moviee[,2],1970,1979)),
               "1980s(1980-1989)" = filter(moviee, between(moviee[,2],1980,1989)),
               "1990s(1990-1999)" = filter(moviee, between(moviee[,2],1990,1999)),
               "2000s(2000-2009)" = filter(moviee, between(moviee[,2],2000,2009)),
               "2010s(2010-2019)" = filter(moviee, between(moviee[,2],2010,2019)),
               "2020s(2020-2029)" = filter(moviee, between(moviee[,2],2020,2029))
        )
        
      } else if (input$movieordrama == "TV Show"){
        switch (input$year,
                "-" = dramaa,
                "1920s(1920-1929)" = filter(dramaa, between(dramaa[,2],1920,1929)),
                "1930s(1930-1939)" = filter(dramaa, between(dramaa[,2],1930,1939)),
                "1940s(1940-1949)" = filter(dramaa, between(dramaa[,2],1940,1949)),
                "1950s(1950-1959)" = filter(dramaa, between(dramaa[,2],1950,1959)),
                "1960s(1960-1969)" = filter(dramaa, between(dramaa[,2],1960,1969)),
                "1970s(1970-1979)" = filter(dramaa, between(dramaa[,2],1970,1979)),
                "1980s(1980-1989)" = filter(dramaa, between(dramaa[,2],1980,1989)),
                "1990s(1990-1999)" = filter(dramaa, between(dramaa[,2],1990,1999)),
                "2000s(2000-2009)" = filter(dramaa, between(dramaa[,2],2000,2009)),
                "2010s(2010-2019)" = filter(dramaa, between(dramaa[,2],2010,2019)),
                "2020s(2020-2029)" = filter(dramaa, between(dramaa[,2],2020,2029))
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
      
    })
    
    output$Drama <- DT::renderDataTable({
      DT::datatable(data(),rownames = FALSE)
    })
    
})
