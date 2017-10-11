library(shiny)

ui <- fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "yahoo.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "resume.css")
      ),
       includeHTML("index.html")
      )

server <- function(input,output,session){
  
}

shinyApp(ui=ui,server=server)