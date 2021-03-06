# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    tags$head(
        tags$style(HTML("hr {border-top: 2px solid #084594}
                        #action1 {background-color: #c6dbef}
                        #action2 {background-color: #9ecae1}
                        #action3 {background-color: #6baed6}
                        #erase {border-color: #084594}
                        #prob1 {text-align: center}
                        #prob2 {text-align: center}
                        #prob3 {text-align: center}
                        ")),
        tags$script('Shiny.addCustomMessageHandler("refocus",
                     function(message) {
                         document.getElementById("text").focus();
                     });'
        )
    ),
    # Application title
    titlePanel("Word Prediction and Completion 101"),
    sidebarLayout(position='right',
        sidebarPanel(
            strong("INSTRUCTIONS"),
            p("Start typing. Use the buttons to enter the next word or perform word completion. 
              The app decides based on whether the last character is a letter or not."),
            checkboxInput('checkbox', label='Show Conditional Probability', width='100%'),
            actionButton('erase', label='Erase Text', width='100%')
        ),
        # Show a plot of the generated distribution
        mainPanel(
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

