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

  wyslijSolr <- function (dane) {
    #wysyłanie wiadomości do Solr
    polaczenie <- SolrClient$new(host = "127.0.0.1", port = 8983, path = "/solr/pnse17/select")
    wielkosc <- length(dane)
    data1 <- matrix(nrow = wielkosc, ncol = 2)

    colnames(data1) <- c("id","content")

    counter <- 1
    for (val in dane) {
      data1[counter,1] <- counter
      data1[counter,2] <- val
      counter <- counter + 1
    }
    dokumenty <- data.frame(data1)
    solrium::add(x = dokumenty, conn = polaczenie, name = 'pnse17');
  }

  pobierzSolr <- function () {
    #pobieranie wiadomości z Solr
    polaczenie <- SolrClient$new(host = "127.0.0.1", port = 8983, path = "/solr/pnse17/select")
    dokumenty2 <- as.data.frame.list(solr_search(conn = polaczenie, params = list(q="*:*", rows= -1)));
  }
  analizaDokumentu <- function (od, do) {
    #pobieranie dokumentu do analizy
    uri <- "http://www.informatik.uni-hamburg.de/TGI/events/pnse/pnse17/pnse17_proceedings.pdf"
    download.file(uri,"analizowany_dokument.pdf", method = "internal", mode = "wb")
    pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = "analizowany_dokument.pdf"), language = "en", id = "id1")

    #wczytywanie analizowanego dokumentu
    tekst <- pdf_text(pdf="analizowany_dokument.pdf")[od:do]
    tekst2 <- str_replace_all(tekst, "[\r\n]", " ")
    tekst3 <- str_squish(tekst2)

    wyslijSolr(tekst3);
    #przygotowanie tekstu do analizy
    dokumenty2 <- pobierzSolr();
    dokumenty <- Corpus(VectorSource(stri_enc_toutf8(dokumenty2$content)))
    dokumenty <- tm_map(dokumenty, removePunctuation)
    dokumenty <- tm_map(dokumenty, removeNumbers)
    dokumenty <- tm_map(dokumenty, content_transformer(tolower))
    dokumenty <- tm_map(dokumenty, removeWords, c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))

    usun.znaki <- function (x) gsub("[–„”’⊗‡∪≤∃�“•∈→−δ∩∗∅—∀∼�π_⊂σ�𝑎⊆𝑐𝑡∧𝑏𝑠≈Ωµτ↓φ⇔∞≥⇒◦∆𝑒𝑚↔⇐≺𝑑⇤‘α×𝑓ø¬⊥]", "", x)
    dokumenty <- tm_map(dokumenty, usun.znaki)

    #lematyzacja
    for (d in 1:length(dokumenty)) {
      dokumenty[[d]]$content <- lemmatize_strings(dokumenty[[d]]$content)
      dokumenty[[d]]$content <- stri_enc_toutf8(dokumenty[[d]]$content)
    }

    tdm <- TermDocumentMatrix(dokumenty)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(words = names(v), freq = v)
  }
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
       analizaDokumentu(input$strony[1], input$strony[2])
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
