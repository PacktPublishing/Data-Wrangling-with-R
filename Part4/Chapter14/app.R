"---
Title: Chapter14_Build an Application with Shiny in R
author: Gustavo R Santos
date: 2022-08-31
---
  
  ## Packt Book
  ## Data Wrangling With R
  ### Chapter 13 - Building a Model with R
  
  This document is part of the Packt Book *Data Wrangling with R*.

---"


###############################
###### Package Loads ##########
#------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(plotly)
library(tidyverse)
library(randomForest)

##############################
##### Loading the model #####
#------------------------------------------------------------------

model <- readRDS("rf_model.rds")


###################################################
##### Function to prepare text for prediction #####
#------------------------------------------------------------------

# Define spam words
spam_words <- c('you', 'your', 'will', 'free', 'our', 'all', 'mail', 'email', 
                'business', 'remove', '000', 'font', 'money', 'internet', 
                'credit', 'over', 'order', '3d', 'address', 'make', 'make', 'people',
                're', 'receive')

prepare_input <- function (text){
  "This function takes a string text as input, counts the quantities of !, $, (), uppercase letters, longest sequence of uppercase, words in the spam list.
  * Input: string
  * Returns: data frame for input in the RF model"
  
  # Counts of the punctuation
  exclamation <- str_count(text, pattern="[!]")
  parenthesis <-  str_count(text, pattern="[()]")
  dollar_sign <-  str_count(text, pattern="[$]")
  
  # Counts of UPPERCASE
  total_uppercase <- str_count(text, "[A-Z]")
  
  # Remove punctuation for total words count
  text_no_puncuation <- str_remove_all(text, pattern="[:punct:]|[$]*")
  
  #longest_uppercase
  all_words <- str_split(text_no_puncuation, " ")
  all_words <- all_words[[1]]
  
  # Create a vector with all the uppercase counts
  char_counts <- c()
  all_words_no_numbers <- str_remove_all(all_words, pattern="[0-9]")
  for (word in all_words_no_numbers) {
    if (word == toupper(word)) {
      char_counts <- c(char_counts, nchar(word))
    } #enf if
  }#end for
  
  # Get only the longest uppercase word size
  if (max(char_counts) < 0) {
    longest_upper <- 0} else {longest_upper <- max(char_counts)}
  
  
  # Count how many spam words are in the text
  # Create a counter of spam words
  top_w <- 0
  # For each word in the list all_words...
  for (word in all_words) {
    # if word is in the spam list, count +1, else 0.
    if (tolower(word) %in% spam_words) {
      top_w <- top_w + 1
    } #enf if
  }#end for
  
  # Determine length of the text for percentage calculations
  text_length <- length(all_words)
  
  # Create a data frame with all counts in percentages (divided by the text length)
  input <- data.frame(top_w_pct= 100*top_w / text_length,
                      char_freq_exclam= 100*exclamation / text_length,
                      char_freq_parenthesis= 100*parenthesis / text_length,
                      char_freq_dollar= 100*dollar_sign / text_length,
                      capital_run_length_total= total_uppercase,
                      capital_run_length_longest= longest_upper )
  
  return(input)
}

###############################
####### User Interface ########
#------------------------------------------------------------------

ui <- fluidPage(theme = shinytheme("united"),
                # This is the panel with all the tabs on top of the pages
                navbarPage(
                  theme = 'united',
                  'Data Wrangling with R',
                  
                  # Tab About
                  #----------------------------------
                  tabPanel("About the Project",
                           mainPanel(fluidRow(
                             h3("> The Project"),
                             p("This project is part of the book Data Wrangling with R, published with", 
                               strong(a("Packt Publishing.", href="https://www.packtpub.com/")), 
                               "It consists in a classification model to predict what is the chance of a given e-mail to be
                               marked as spam or not, based on the key words from the training dataset. The user must go to the tab 
                               SPAM CLASSIFIER and input a text. The model will read it and classify as", em("SPAM"), "or", em("NOT SPAM"),
                               ", together with the proability of each classification."),
                             h3("> The Dataset"),
                             p("The dataset used in this project is the", em('Spambase'), "from the UCI Machine Learning Repository."),
                             h4("Dataset Credits"),
                             p(strong("Creators:"), "Mark Hopkins, Erik Reeber, George Forman, Jaap Suermondt", br(),
                             strong("Donor:"), "George Forman"),
                             p(strong("URL Address:"), a("Spambase Dataset from UCI", href="https://archive.ics.uci.edu/ml/datasets/spambase")),
                             h3("> The Model"),
                             p("The classifications are performed by a Random Forest model trained with the datatet previously mentioned.
                               The possible results are SPAM or NOT SPAM for a given text."),
                             h3("> The Author"),
                             p(strong(a("Gustavo R Santos", href="https://www.linkedin.com/in/gurezende/")), 
                             "is a Data Scientist in the retail industry, working daily with R Language, Python,
                             Databricks, and SQL.")
                           ) # fluidRow-About
                           ) # mainPanel-About
                  ), #tabPanel-About
                  
                  #----------------------------------
                  # Tab SPAM CLASSIFIER
                  #----------------------------------
                  
                  tabPanel("SPAM Classifier",
                           mainPanel(fluidRow(
                             h3("> Instructions"),
                             p("This app receives a text as input. Write down or paste a text in the input box
                               and press the button submit. The model will read it and return a classification
                               as Spam or Not spam, together with the probability of each result."),
                             h4("Input your text here:"),
                             #text Input
                             textAreaInput(inputId = "text", NULL, " ", width = "1000px", height="120px"),
                             submitButton(text= 'Submit'),
                             column(5,
                                    h3("> Prediction"),
                                    p("The probability (%) of this text being classified as spam or not are:"),
                                    h4( tableOutput('prediction') ) 
                                    ),# column1
                             column(7, 
                                    h3( "> Measurements" ),
                                    p("These are the measurements of the variables that affect the classification. 
                                      The higher they are, more are the chances of your text being 
                                      classified as spam."),
                                    plotlyOutput(outputId = 'measurements')
                                    )#column2
                           ) # fluidRow-spam_classifier
                           ) # mainPanel-spam_classifier
                  ) #tabPanel-spam_classifier
                  
                  #----------------------------------
                  
                ) # navbarPage
                
) #MainfluidPage-close





#############################
########## Server ###########
#------------------------------------------------------------------

server <- function(input, output) {
  
  
  # Piece of code for the Prediction
  output$prediction <- renderTable({ 
    # If there is no text, show an empty table
    if (input$text == " ") {input$text} 
    else {
    
    # Prepare data for input in the model
    datatext <- prepare_input(input$text)

    # Predict
    prediction <- predict(model,datatext, type='prob')
    data.frame(prediction*100)
    }#end if
    
  }) #output prediction
  
  # Piece of code for the measurements graphic
  output$measurements <- renderPlotly({
    # If there is no text, show a dummy graphic
    if (input$text == " ") {
      datatext <- data.frame(variables=c('1','2'), values= c(0,0))
      g <- plot_ly(data = datatext, x = ~values, y = ~variables, type = 'bar',
                   name='Bar', alpha = 0.6)
      g <- g %>% layout(width = 400, height = 200)}
    else {
    
    # Prepare data as a dataset
    datatext <- prepare_input(input$text)
    datatext <- data.frame(t(datatext))
    colnames(datatext) <- 'values'
    datatext <- as.data.frame(datatext)
    measurements <- c('Spam words', 'Presence of !!!','Presence of ( )',
                      'Presence of $$$', 'Total UPPER', 'Longest UPPER')
    
    # Create graphic
    g <- plot_ly(data = datatext, x = ~values, y = measurements, type = 'bar',
                 name='Bar', alpha = 0.85, color= 'darkorange')
    g <- g %>% layout(width = 500, height = 200, xaxis=list(range=c(0,100)))
    plotly_build(g)
    
    }#end if
    
  }) #output bar graphic
  
}# close server



##############################
######### Shiny App ##########
#------------------------------------------------------------------

shinyApp(ui = ui, server = server)
