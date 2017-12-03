library(shiny)##
library(sendmailR)###

ui <- fluidPage(includeHTML('index.html'))

server <- function(input, output, session){
    
    observe({
      if(is.null(input$send) || input$send==0) return(NULL)
      from <- isolate(input$from)
      to <- isolate("shaneav2749@gmail.com")
      subject <- isolate(input$subject)
      msg <- isolate(input$message)
      sendmail(from, to, subject, msg)
    })
    
  
}

shinyApp(ui, server)