# Title     : Aplikacja webowa do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2017 umieszczonych na portalu ceur-ws.org
# Created by: yonarsky
# Created on: 20.12.2020

if(require(shiny)){
  library(wordcloud2)
  library(pdftools)
  library(stringr)
  library(tidyRSS)
  library(tm)
  library(httr)
  library(XML)
  library(stringi)
  library(solrium)
  library(textstem)
   # Global variables can go here
   n <- 1
   titleApp <- h1("Aplikacja webowa do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2017 umieszczonych na portalu", a(href="http://ceur-ws.org/Vol-1846/", "ceur-ws.org", target="_blank"));
   # Define the UI
   ui <- fluidPage(
      titlePanel(title = titleApp, windowTitle = "Aplikacja webowa do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2017 umieszczonych na portalu ceur-ws.org"),
      sidebarLayout(
        sidebarPanel(
          h3("Wybierz zakres stron do analizy"),
          sliderInput("strony", "od", min = 1,  max = 319, value = c(1, 50)),
          helpText("Uwaga! Wybranie zbyt dużej liczby stron, może powodowodować powolne działanie aplikacji."),
          actionButton("analizuj", "Analizuj"),
          hr(),
          h3("Rozmiar"),
          sliderInput("rozmiar", "od", min = 1,  max = 10, step = 0.1, value = 1),
          h3("Liczba słów [%]"),
          sliderInput("lSlow", "od", min = 1,  max = 100, value = 50, post = " %"),
        ),
        mainPanel(
          wordcloud2Output('wordcloud2')
          )
        )
   )


   # Define the server code
   server <- function(input, output) {
     observeEvent(input$analizuj, {
      output$wordcloud2 <- renderWordcloud2({
        wordcloud2(demoFreq, size=input$rozmiar)
    })
        #wordcloud2(d, size=input$size)
      })
   }
   # Return a Shiny app object
   # Sys.setlocale("LC_CTYPE","chs") #if you use Chinese character
   ## Do not Run!
   shinyApp(ui = ui, server = server)
   }
