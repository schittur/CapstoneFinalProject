
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Next Word Predictor"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      p("Enter a sentence and press submit button to predict the next word."),
      p("This application uses text mining and predicts the next word from a list of trigrams")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3("Enter a phrase and press [Submit]"),
      textInput("sentence", label="", value = ""),
      submitButton("Submit"), hr(),
      tags$div(
        HTML("<h4>Most probable next word: </h4><h3 style='color:red'>"),
        textOutput("PredictedWords")),
        HTML("</h3></br><hr><h4>All possibilities: </h4>"),
        textOutput("WordsFound"),hr(),
        HTML("</br></br>Server time:"),
        textOutput("servertime")
    )
  )
))
