library(shiny)

ui <- fluidPage(
  h2("Shiny Applications"),
  a("Dashboard", href="/dashboard"),br(),
  a("Returns", href="/performance"),br(),
  a("Risk", href="/risk")
)

server <- function(input,output, session){
  
  
}

shinyApp(ui=ui, server=server)