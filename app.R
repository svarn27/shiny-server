library(shiny)##
library(sendmailR)###

ui <- fluidPage(includeHTML('index.html'))

server <- function(input, output, session){
  
}

shinyApp(ui, server)