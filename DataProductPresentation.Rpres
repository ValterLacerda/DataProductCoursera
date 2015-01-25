DataProductPresentation
========================================================
author: Valter Lacerda de Andrade Jr
date: 2015-01-25

1º Slide - Text Analysis
========================================================

This application was developed to try undestand the concentration of
words in any website. The concept try to see "what is most important in the search of people when this people try to see anything."

- Search Web site
- Counting words
- Show for anybody the results

2º Slide - Introduction
========================================================

We have a lot off kinds of web sites in the web and usualy when we try to undestand what the context or what is very important in the 
web site we need to analyse de context off the text, for this kind of necessity I though this solution to help the people to see fast what the most important word for help then understant the subject of the web site.


3º Slide - Explain the solution - Ui.R
========================================================

In this slide is possible to understand what this application because i configured de painel using this code.


library(shiny)

langs <- c("english","french","german","italian","portuguese","russian",
           "spanish")

shinyUI(fluidPage(
    
    
    headerPanel("Text Analysis"),
    
    sidebarPanel(
        
        textInput("url", "URL (with http://):", value = "http://en.wikipedia.org/wiki/Siege_of_Constantinople_(717%E2%80%9318)"),
        
        selectInput("lang", "Select the language:", choices = langs,
                    selected = "english"),
        
        sliderInput("minFreq", "Minimum frequency:", 
                    min = 1, max = 50, value = 3),
        sliderInput("maxFreq", "Maximum number of words:",
                    min = 1, max = 500, value = 100),
        
        submitButton("Go")
    ),
    
    mainPanel(
        
        h3("Word Cloud"),
        h4("Minimum frequency:"),
        verbatimTextOutput("minFreq"),
        h4("Maximum word frequency:"),
        verbatimTextOutput("maxFreq"),
        h4("Selected language:"),
        verbatimTextOutput("langSelected"),
        h4("Word cloud:"),
        plotOutput("myplot")
    )
    
))


4º Slide - Explain the solution - server.R
========================================================

In this slide is possible to see what the app do.

library(shiny)
library(XML)
library(RCurl)
library(tm)
library(wordcloud)
library(RColorBrewer)


shinyServer(function(input, output, session) {
    
    terms <- reactive({
        
        #input$update
                        
                html = getURL(input$url)
                doc.html = htmlTreeParse(html, useInternal = TRUE)
                doc.text = unlist(xpathApply(doc.html, '//p', xmlValue))
                
                doc.text = gsub('\\n', ' ', doc.text)
                doc.text = gsub('\\r', ' ', doc.text)
                doc.text = gsub('\\t', ' ', doc.text)
                
                # Transforming in a Corpora
                myCorpus <- Corpus(VectorSource(doc.text))
                myCorpus <- tm_map(myCorpus, content_transformer(tolower))
                myCorpus <- tm_map(myCorpus, removePunctuation)
                myCorpus <- tm_map(myCorpus, removeNumbers)
                myCorpus <- tm_map(myCorpus, removeWords, stopwords(input$lang))
                
                # Transforming in a Term Document Matrix
                myDtm <- TermDocumentMatrix(myCorpus, 
                                            control = list(minWordLength = 3))
                
                # Building the Wordcloud
                m <- as.matrix(myDtm)
                v <- sort(rowSums(m), decreasing = TRUE)
                
                
    
        
    })
    
  
    #output$pageTitle <- renderText({html})
    output$minFreq <- renderText({input$minFreq})
    output$maxFreq <- renderText({input$maxFreq})
    output$langSelected <- renderText({input$lang})
    output$myplot <- renderPlot({
        
        v <- terms()
        wordcloud(names(v), v, min.freq = input$minFreq, 
                  max.words = input$maxFreq, colors = brewer.pal(8,"Dark2"),
                  scale=c(10,.5))
        
    })
    
    
})


5º Slide - Conclusion
========================================================

The application really try to resolve the problem to understant the context inside the web sites, but i didn't create a mesaure to undertant what this solution can resolve the problem.





