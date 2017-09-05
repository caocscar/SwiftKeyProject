# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 2px solid #0084b4;}
                        #prob1 {text-align: center;}
                        #prob2 {text-align: center;}
                        #prob3 {text-align: center;}
                        "))
    ),
    # Application title
    titlePanel("Word Prediction and Completion 101"),
    sidebarLayout(position='right',
        sidebarPanel(width=5, 
            strong("INSTRUCTIONS"),
            p("Start typing. You can use the buttons to input your next word or do auto-completion. 
              If the last character is a letter, the app will do current word completion else it will do next word prediction."),
            checkboxInput('checkbox', label='Show Conditional Probability', width='100%'),
            actionButton('erase', label='Erase Text', width='100%')
        ),
        # Show a plot of the generated distribution
        mainPanel(width=7,
            textInput(inputId="text", label="Type Here", value="", width='100%'),
            hr(),
            fluidRow(
                column(4, uiOutput("word1")),
                column(4, uiOutput("word2")),
                column(4, uiOutput("word3"))
            ),
            fluidRow(
                conditionalPanel(
                    condition = "input.checkbox == 1",
                    br(),
                    column(4,verbatimTextOutput(outputId='prob1', placeholder=T)),
                    column(4,verbatimTextOutput(outputId='prob2', placeholder=T)),
                    column(4,verbatimTextOutput(outputId='prob3', placeholder=T))
                )
            )
        )
    )
))

