# Title     : Aplikacja webowa do akwizycji i analizy danych z artykułów z konferencji Petri Nets and Software Engineering 2017 umieszczonych na portalu ceur-ws.org
# Created by: yonarsky
# Created on: 20.12.2020

if(require(shiny)){
  library(wordcloud2)
   # Global variables can go here
   n <- 1

   # Define the UI
   ui <- fluidPage(
      titlePanel("Lorem ipsum"),
      sidebarLayout(
        sidebarPanel(
          numericInput('size', 'Size of wordcloud', n)
        ),
        mainPanel(
          wordcloud2Output('wordcloud2')
          )
        )
   )


   # Define the server code
   server <- function(input, output) {
      output$wordcloud2 <- renderWordcloud2({
        wordcloud2(demoFreq, size=input$size)
        #wordcloud2(d, size=input$size)
      })
   }
   # Return a Shiny app object
   # Sys.setlocale("LC_CTYPE","chs") #if you use Chinese character
   ## Do not Run!
   shinyApp(ui = ui, server = server)
   }
