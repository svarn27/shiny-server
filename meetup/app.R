library(shiny)

ui <- fluidPage(
  h2("Shiny Applications"),
  a("Dashboard", href="/example1"),br(),
  a("Returns", href="/example2"),br(),
  a("Risk", href="/example3")
)

server <- function(input,output, session){
  
  
}

shinyApp(ui=ui, server=server)