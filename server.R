# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

library(shiny)
library(data.table)
library(stringr)

load('final10.RData')
setkey(DF,'Y','ngram')
choices = 3
ngram = 4

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
   
    P <- eventReactive(input$text, {
        S = input$text[1]
        # string cleaning (lowercase, punctuation, extra&leading&trailing spaces)
        s = sapply(S, tolower)
        s = sub('[a-z]+$', "", s)
        s = str_replace_all(s,"[^a-z0-9' ]","")
        s = str_replace_all(s," {2,}"," ")
        s = trimws(s)
        sentence = str_split(s," ")[[1]]
        print(sentence)
        if ((length(sentence) > 0) & (sentence[1] != "")) {
            if (length(sentence) > ngram-2) {
                lastNwords = sentence[length(sentence)-((ngram-2):0)]
            } else {
                lastNwords = sentence
            }
            phrase3 = paste(lastNwords, collapse=" ")
            # 4gram calculation
            df4 = DF[.(phrase3,4)]
            beta_flag = T
            if (beta_flag) {
                beta = 1 - df4[,sum(ct*discount)/sum(ct)]
                if (!is.na(beta)) {
                    beta_flag = F
                    df4N = df4[,pbo:=discount*ct/sum(ct)][pbo>0,.(Z,pbo)]
                } else {
                    df4N = data.table()
                }
            }
            # 3gram calculation
            phrase2 = sub('^.+? ','',phrase3)
            df3 = DF[.(phrase2,3)]
            if (beta_flag) {
                beta = 1 - df3[,sum(ct*discount)/sum(ct)]
                if (!is.na(beta)) {
                    beta_flag = F
                    df3N = df3[,pbo:=discount*ct/sum(ct)][pbo>0,.(Z,pbo)]
                } else {
                    df3N = data.table()
                }
            } else {
                df3$tf = 1
                df3[Z %in% df4[,Z], tf:=0]
                df3[, ct2:=tf*ct]
                df3[,pbo_lower:=discount*ct2/sum(ct2)]
                alpha3 = beta/df3[,sum(pbo_lower)]
                df3N = df3[,pbo:=alpha3*pbo_lower][pbo>0,.(Z,pbo)]
            }
            # 2gram calculation
            phrase1 = sub('^.+? ','',phrase2)
            df2 = DF[.(phrase1,2)]
            if (beta_flag) {
                beta = 1 - df2[,sum(ct*discount)/sum(ct)]
                if (!is.na(beta)) {
                    beta_flag = F
                    df2N = df2[,pbo:=discount*ct/sum(ct)][pbo>0,.(Z,pbo)]
                } else {
                    df2N = data.table()
                }
            } else {
                df2$tf = 1
                df2[Z %in% c(df4[,Z],df3[,Z]), tf:=0]
                df2[, ct2:=tf*ct]
                df2[,pbo_lower:=discount*ct2/sum(ct2)]
                alpha2 = beta/df2[,sum(pbo_lower)]
                df2N = df2[,pbo:=alpha2*pbo_lower][pbo>0,.(Z,pbo)]
            }
            # 1gram calculation
            df1 = DF[ngram==1]
            if (beta_flag) {
                df1N = df1[,pbo:=discount*ct/sum(ct)][pbo>0,.(Z,pbo)]
            } else {
                df1$tf = 1
                df1[Z %in% c(df4[,Z],df3[,Z],df2[,Z]), tf:=0]
                df1[, ct2:=tf*ct]
                df1[,pbo_lower:=discount*ct2/sum(ct2)]
                alpha1 = beta/df1[,sum(pbo_lower)]
                df1N = df1[,pbo:=alpha1*pbo_lower][pbo>0,.(Z,pbo)]
            }
            # combine top results
            df_combined = rbind(df1N,df2N,df3N,df4N)
            if (str_detect(S, '[a-z]$')) {
                phrase = str_split(S, " ")[[1]]
                last_word = tolower(phrase[length(phrase)])
                regex = paste0('^',last_word)
                dfN = df_combined[grep(regex, df_combined$Z)][order(-pbo)][1:choices]
            } else {
                dfN = df_combined[order(-pbo)][1:choices]
            }
            W = sapply(dfN[,'Z'], as.character)
            Pbo = sapply(dfN[,'pbo'], round, 3)
            P = c(W,Pbo)
        } else if (sentence == "") {
            P = c('The','I','That')
        }
    })
    updateSentence <- function(x) {
        S = input$text[1]
        S = sub('[A-Za-z]+$', "", S)
        newtext = trimws(paste(S, as.character(x)))
        newtext = str_replace_all(newtext," {2,}"," ")
        updateTextInput(session, 'text', value=paste0(newtext," "))
    }
    # event handling for action button presses
    observeEvent(input$action1, {
        updateSentence(P()[1])
        session$sendCustomMessage(type="refocus", message=list(NULL))
    })
    observeEvent(input$action2, {
        updateSentence(P()[2])
        session$sendCustomMessage(type="refocus", message=list(NULL))
    })
    observeEvent(input$action3, {
        updateSentence(P()[3])
        session$sendCustomMessage(type="refocus", message=list(NULL))
    })
    observeEvent(input$erase, {
        updateTextInput(session, 'text', value="")        
        session$sendCustomMessage(type="refocus", message=list(NULL))
    })
    # renders the action button with the appropriate word
    output$word1 <- renderUI({
        actionButton('action1', label=P()[1], width='100%')
    })
    output$word2 <- renderUI({
        actionButton('action2', label=P()[2], width='100%')
    })
    output$word3 <- renderUI({
        actionButton('action3', label=P()[3], width='100%')
    })
    # conditional probabilities
    output$prob1 <- renderText(P()[4])
    output$prob2 <- renderText(P()[5])
    output$prob3 <- renderText(P()[6])
})
