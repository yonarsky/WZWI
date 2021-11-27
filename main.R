# Title     : Web application in R for data acquisition and analysis from scientific articles from International Workshop on Petri Nets and Software Engineering 2017. (http://ceur-ws.org/Vol-1846/)
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

  sendSolr <- function (data) {
    #send messages to Apache Solr
    connection <- SolrClient$new(host = "127.0.0.1", port = 8983, path = "/solr/pnse17/select")

    solrium::delete_by_query(conn = connection, name = 'pnse17', query = '*:*')

    len <- length(data)
    data1 <- matrix(nrow = len, ncol = 2)

    colnames(data1) <- c("id","content")

    counter <- 1
    for (val in data) {
      data1[counter,1] <- counter
      data1[counter,2] <- val
      counter <- counter + 1
    }
    documents <- data.frame(data1)
    solrium::add(x = documents, conn = connection, name = 'pnse17', overwrite = TRUE)
  }

  getSolr <- function () {
    #dwonload messages from Apache Solr
    connection <- SolrClient$new(host = "127.0.0.1", port = 8983, path = "/solr/pnse17/select")
    documents2 <- as.data.frame.list(solr_search(conn = connection, params = list(q="*:*", rows= -1)));
  }
  docAnalyze <- function (od, do) {
    #download pdf document
    uri <- "http://www.informatik.uni-hamburg.de/TGI/events/pnse/pnse17/pnse17_proceedings.pdf"
    download.file(uri,"document_pdf.pdf", method = "internal", mode = "wb")
    pdf <- readPDF(control = list(text = "-layout"))(elem = list(uri = "document_pdf.pdf"), language = "en", id = "id1")

    #wczytywanie analizowanego dokumentu
    text <- pdf_text("document_pdf.pdf")[od:do]
    text2 <- str_replace_all(text, "[\r\n]", " ")
    text3 <- str_squish(text2)

    sendSolr(text3);
    #przygotowanie tekstu do analizy
    documents2 <- getSolr();
    documents <- Corpus(VectorSource(stri_enc_toutf8(documents2$content)))
    documents <- tm_map(documents, removePunctuation)
    documents <- tm_map(documents, removeNumbers)
    documents <- tm_map(documents, content_transformer(tolower))
    documents <- tm_map(documents, removeWords, c(stopwords("SMART"), "thy", "thou", "thee", "the", "and", "but"))

    removeChars <- function (x) gsub("[–„”’⊗‡∪≤∃�“•∈→−δ∩∗∅—∀∼�π_⊂σ�𝑎⊆𝑐𝑡∧𝑏𝑠≈Ωµτ↓φ⇔∞≥⇒◦∆𝑒𝑚↔⇐≺𝑑⇤‘α×𝑓ø¬⊥]", "", x)
    documents <- tm_map(documents, removeChars)

    #lemmatisation
    for (d in seq_along(documents)) {
      documents[[d]]$content <- lemmatize_strings(documents[[d]]$content)
      documents[[d]]$content <- stri_enc_toutf8(documents[[d]]$content)
    }

    tdm <- TermDocumentMatrix(documents)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(words = names(v), freq = v)
    return(d)
  }

   titleApp <- h1("Web application in R for data acquisition and analysis from scientific articles from International Workshop on Petri Nets and Software Engineering 2017 ", a(href="http://ceur-ws.org/Vol-1846/", "(ceur-ws.org)", target="_blank"));

   ui <- fluidPage(
      titlePanel(title = titleApp, windowTitle = "Web application in R for data acquisition and analysis from scientific articles from International Workshop on Petri Nets and Software Engineering 2017."),
      sidebarLayout(
        sidebarPanel(
          sliderInput("pages", "Numbers of pages", min = 1,  max = 319, value = c(1, 50)),
          actionButton("analyze", "Analyze"),
          hr(),
          sliderInput("size", "Size", min = 1,  max = 5, step = 0.05, value = 1)
        ),
        mainPanel(
          wordcloud2Output('wordcloud2')
          )
        )
   )

   server <- function(input, output) {
     observeEvent(input$analyze, {
       d <- as.data.frame.list(docAnalyze(input$pages[1], input$pages[2]))
      output$wordcloud2 <- renderWordcloud2({
        wordcloud2(d, size = input$size)
    })
      })
   }

   ## Do not Run!
   ## Do Run!
   ## ple ple
   shinyApp(ui = ui, server = server)
   }
