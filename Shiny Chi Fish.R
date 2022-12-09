# install.packages('shiny')
# install.packages('ggplot2')

library(shiny)
library(ggplot2)

#UI
ui <- fluidPage(
  
  titlePanel("Chi-Square/Fisher's Exact Tester"),
  sidebarLayout(sidebarPanel(
    numericInput(inputId = 'fa', label = 'Factor A', 0),
    numericInput('fb', label = 'Factor B', 0),
    numericInput('f1', label = 'Factor 1', 0),
    numericInput('f2', label = 'Factor 2', 0)
  ),
  
  mainPanel(
    dataTableOutput(outputId = 'table'),
    verbatimTextOutput(outputId = 'test')
      
    )
            ))

#Server
server <- function(input, output) {
  
  
  output$table <- renderDataTable({
    a <- matrix(c(input$fa, input$f1, input$fb, input$f2),ncol=2,nrow=2,dimnames = list(c('Factor 1','Factor 2'),c('Factor A','Factor B')))
    a <- as.data.frame(a)
    names(a) <- c('Factor A','Factor B')
    a
     })
  
  #PROBLEM NOW
  output$test <- renderPrint({
    a <- matrix(c(input$fa, input$f1, input$fb, input$f2),ncol=2,nrow=2,dimnames = list(c('Factor 1','Factor 2'),c('Factor A','Factor B')))
    a <- as.data.frame(a)
    cat('Chi-square Test Results:', chisq.test(a)$p.value)
    cat('\nFishers Exact Test Results:', fisher.test(a)$p.value)
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

