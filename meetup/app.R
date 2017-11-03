library(shiny)

ui <- fluidPage(
  h2("Meetup - Data Analysis using R/Shiny"),
  a("Presentation", 
    href="https://docs.google.com/presentation/d/1v7rt7Mz1Gd7gm2M1YoMjGqhw0cOxHzPH-30CMvr2WZ8/edit?usp=sharing"), 
  br(),
  a("Graphing Example", href="/example2"),br(),
  a("Clustering Example", href="/example1"),br(),
  a("Regression Example", href="/example3")
)

server <- function(input,output, session){
  
  
}

shinyApp(ui=ui, server=server)