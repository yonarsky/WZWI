# Title     : Aplikacja webowa do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2017 umieszczonych na portalu ceur-ws.org
# Created by: yonarsky
# Created on: 20.12.2020

if(require(shiny)){
  library(wordcloud2)
   # Global variables can go here
   n <- 1
   titleApp <- h1("Aplikacja webowa do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2017 umieszczonych na portalu", a(href="http://ceur-ws.org/Vol-1846/", "ceur-ws.org", target="_blank"));
   # Define the UI
   ui <- fluidPage(
      titlePanel(title = titleApp, windowTitle = "Aplikacja webowa do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2017 umieszczonych na portalu ceur-ws.org"),
      sidebarLayout(
        sidebarPanel(
          h3("Wybierz zakres stron do analizy"),
          sliderInput("pages", "od",
                  min = 1,  max = 319, value = c(1, 50)),
          helpText("Uwaga! Wybranie zbyt dużej liczby stron, może powodowodować powolne działanie aplikacji.")
        ),
        mainPanel(
          wordcloud2Output('wordcloud2')
          )
        )
   )


   # Define the server code
   server <- function(input, output) {
      output$wordcloud2 <- renderWordcloud2({
        wordcloud2(demoFreq, size=input$pages[1])
        #wordcloud2(d, size=input$size)
      })
   }
   # Return a Shiny app object
   # Sys.setlocale("LC_CTYPE","chs") #if you use Chinese character
   ## Do not Run!
   shinyApp(ui = ui, server = server)
   }
