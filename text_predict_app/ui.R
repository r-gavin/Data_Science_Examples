#    http://shiny.rstudio.com/
#
#    ui.R for shiny app - Data Science Capstone Project
#

library(shiny)

# Define UI for NLP Predictor
shinyUI(fluidPage(
    
    titlePanel("Data Science Capstone Project - NLP Predictor"),
    p("This application demonstrates the algorithm I built for predicting the
      next word or phrase when given user input. The user interface of the 
      application was built using the",strong(em("shiny package.")),"The 
      underlying algorithm was built using the following packages:", 
      strong("dplyr, stringr, reshape2, ngram."),"The", strong("ngram"), 
      "package in particular was used to build",em("n-grams"),"from the corpus
      provided to us. The corpus consisted of text taken from blogs, news sources,
      and Twitter from the web."),
    p("What is presented here also represents my final work for the Coursera 
      Data Science Specialization - Capstone Project. A discussion of the cleaning
      process and exploratory analysis can be found",
      a(href="http://rpubs.com/r-gavin/353988", "here.")),
    h3(em("The Predictor")),
    p("The prediction starts with the user typing their desired word, phrase, 
      or sentences. Taking this as input, the algorithm then offers up to three
      choices to either complete what is being typed or to add another word. 
      The choices are presented as buttons. Clicking on a particular button 
      \"chooses\" that choice. Once a button is clicked, the chosen \"completion\" 
      is added to the user's line of inputed text. The user can then continue 
      typing their string of text and pick any suggestion (from the algorithm) 
      they may choose. This simple process continues until the user is finished 
      typing."),
    p(strong("Give the algorithm and the predictor a try below!!!")),
    textInput("usr_text",
              h4("User text typed here:"),
              value = ""),
    fluidRow(
        column(4,
               actionButton("choice1",textOutput("str_suggest1")),
               actionButton("choice2",textOutput("str_suggest2")),
               actionButton("choice3",textOutput("str_suggest3"))
               )
    ),
    br(),
    p("In the event that the user's text goes beyond the size of the input box, 
      the user's text is presented below:"),
    strong(textOutput("fin_text")),
    br(),
    p("Seeing as this project is more a demonstration than a finished, customer
      facing product, there are some shortcomings to the alogrithm. Some are listed
      below:"),
    tags$ul(tags$li("If trying to complete the very first inputed word, a",
                    strong("NA"),"will appear at the beginning of the line."),
            tags$li("If simply choosing to insert a word after a period, the 
                    insertion will replace the period and the word before the 
                    period."),
            tags$li("Comma placement is preserved, other punctuation isn't 
                    guaranteed. Apostrophes, quotation marks, i.e., are not
                    preserved."),
            tags$li("Capitalization isn't preserved."))
))
