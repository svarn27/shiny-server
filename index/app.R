library(shiny)

ui <- fluidPage(
  h2("Investment Applications"),
  a("Daily Performance Dashboard", href="/dashboard"),br(),
  a("Performance Analytics", href="/performance"),br(),
  a("Risk Decomposition", href="/risk"),
  br(),
  h2("Enterprise Management Systems"),
  a("Sales Management", href="/tfd")
)

server <- function(input,output, session){
  
  
}

shinyApp(ui=ui, server=server)