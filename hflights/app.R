rm(list=ls())

library(shiny)
library(highcharter)
library(hflights)
library(dplyr)
library(shinydashboard)

#airlineDict <- url("http://www.census.gov/foreign-trade/reference/codes/aircarrier/ac2.txt"))
options(shiny.trace=FALSE)

carriers <- hflights %>% 
  distinct(UniqueCarrier) %>% 
  select(UniqueCarrier)

destinations <- hflights %>%
  filter(UniqueCarrier=="AA") %>%
  distinct(Dest) %>% 
  select(Dest)

#CLIENT SIDE UI
ui <- dashboardPage(
  dashboardHeader(title="Choose a Dataset"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Houston Flights", tabName = "hflights", icon = icon("plane"))
    )
  ),
  dashboardBody(
    fluidPage(
      tabsetPanel(
        tabPanel("Single Carrier Reporting",
                 fluidRow(
                   column(
                     selectInput("carrier", "Choose Carrier:", carriers),
                     width=6
                   ),
                   column(
                     selectInput("unitTime", "Choose Time Horizon:", c("DayOfWeek","Month")),
                     width=6
                   )
                 ),
                 fluidRow(
                   column(
                     selectInput("graphType", "Choose Graph Type:", c("Count", "Percent")),
                     highchartOutput("hcontainer"),
                     width=6),
                   column(
                     selectInput("delayGraph", "Choose Delay Type:", c("Arrival", "Depature","Total")),
                     highchartOutput("hcontainer2"),
                     width=6),
                   width=12
                 ),
                 fluidRow(
                     selectInput("destGraph", "Choose Destination:", c("DFW")),
                     highchartOutput("hcontainer4")
                 )
        ),
        tabPanel("Multi-Carrier Comparison Reporting")
      )
    )
  )
)

#SERVER SIDE
server <- function(input,output,session){
  
  #TOP LEFT GRAPH
  data <- reactive({hflights %>%
      filter(UniqueCarrier == input$carrier)})
  output$hcontainer <- renderHighchart({
    
    graphType <- switch(input$graphType, Count="normal", Percent="percent")
    
    flightData <- switch(input$unitTime, 
                         Month = data() %>%
                           group_by(Dest, Month) %>%
                           summarise(Flights = n()),
                         DayOfWeek =  data() %>%
                           group_by(Dest, DayOfWeek) %>%
                           summarise(Flights = n()))
    
    hc <- highchart() %>%
      hc_title(text=paste0(input$unitTime," - Flights by Destination")) %>%
      hc_legend(maxHeight=40) %>%
      hc_chart(type="column") %>%
      hc_xAxis(categories=unique(flightData[,2])) %>%
      hc_plotOptions(series=list(stacking=graphType))
    for(d in unique(flightData$Dest)){
      
      x <- flightData %>% filter(Dest==d) %>% select(Flights)
      hc <- hc %>%
        
        hc_add_series(data=x$Flights, name=d)
    }
    
    hc
    
  })
  
  
  #TOP RIGHT GRAPH
  output$hcontainer2 <- renderHighchart({
    
    flightData2 <- data() 
    
    flightData2 <- switch(input$delayGraph, 
                         Depature = flightData2 %>% mutate(TotalDelay =  DepDelay), 
                         Arrival = flightData2 %>% mutate(TotalDelay = ArrDelay),
                         Total = flightData2 %>% mutate(TotalDelay = ArrDelay + DepDelay))
    
    flightData2 <- flightData2 %>%
      group_by(Dest) %>%
      summarise( minDelay = round(sd(TotalDelay, na.rm = TRUE)*-1,2),
                 maxDelay = round(sd(TotalDelay, na.rm = TRUE)*1,2),
                 avgDelay = round(mean(TotalDelay, na.rm = TRUE),2))
    
    hc2 <- highchart() %>%
      hc_chart(type="columnrange") %>%
      hc_legend(maxHeight=40) %>%
      hc_title(text=paste0(input$delayGraph, " Delay per Destination")) %>%
      hc_xAxis(categories=unique(flightData2$Dest)) %>%
      hc_add_series(data=list.parse2(flightData2[,c("minDelay","maxDelay")]),
                    name="+/- 1 sd") %>%
      hc_plotOptions(
        series=list(
          point=list(
            events=list(
              click=JS(
                "function(){
                dest = this.category;
                Shiny.onInputChange('destGraph', dest);
                $('#destGraph').val(dest);
  }"
                      )
              )
              )
              )
        #BOTTOM RIGHT
              ) %>%
      hc_add_series_scatter(x=c(1:nrow(flightData2)-1), y=flightData2$avgDelay, name="Average Delay")
    
    
    hc2
  })
  
  output$hcontainer4 <- renderHighchart({
    
    flightData2 <- data() 
    
    flightData2 <- switch(input$delayGraph, 
                          Depature = flightData2 %>% 
                            filter(Dest == input$destGraph) %>% 
                            mutate(TotalDelay =  DepDelay), 
                          Arrival = flightData2 %>%
                            filter(Dest == input$destGraph) %>%
                            mutate(TotalDelay = ArrDelay),
                          Total = flightData2 %>%
                            filter(Dest == input$destGraph) %>%
                            mutate(TotalDelay = ArrDelay + DepDelay))
    
    flightData4 <- switch(input$unitTime, 
                          Month = flightData2 %>%
                            group_by(Month) %>%
                            summarise( avgDelay = round(mean(TotalDelay, na.rm = TRUE),2),
                                       minDelay = round(avgDelay + (sd(TotalDelay, na.rm = TRUE)*-1),2),
                                       maxDelay = round(avgDelay + (sd(TotalDelay, na.rm = TRUE)*1),2)),
                          DayOfWeek = flightData2 %>%
                            group_by(DayOfWeek) %>%
                            summarise( avgDelay = round(mean(TotalDelay, na.rm = TRUE),2),
                                       minDelay = round(avgDelay + (sd(TotalDelay, na.rm = TRUE)*-1),2),
                                       maxDelay = round(avgDelay + (sd(TotalDelay, na.rm = TRUE)*1),2)))
    
    hc4 <- highchart() %>%
      hc_chart(type="arearange") %>%
      hc_title(text=paste0(input$delayGraph, " Delay per ", input$unitTime)) %>%
      hc_xAxis(categories=unique(flightData4[,2])) %>%
      hc_add_series(data=list.parse2(flightData4[,c("minDelay","maxDelay")]),
                    name="+/- 1 sd") %>%
      hc_add_series(type="spline", data=flightData4$avgDelay,
                    name="Average Delay")
    
    
    hc4
  })
  
  output$hcontainer3 <- renderHighchart({
    
    canc <- data() %>%
      summarise( Cancelled = sum(Cancelled)/n())
    
    onTime <- data() %>%
      mutate(TotalDelay <- ArrDelay + DepDelay) %>%
      summarise( onTime = sum(Cancelled)/n())
    
    hc3 <- highchart() %>% 
      hc_chart(
        type = "solidgauge",
        backgroundColor = "#F0F0F0",
        marginTop = 50
      ) %>% 
      hc_title(
        text = "Activity",
        style = list(
          fontSize = "24px"
        )
      ) %>% 
      hc_tooltip(
        borderWidth = 0,
        backgroundColor = 'none',
        shadow = FALSE,
        style = list(
          fontSize = '16px'
        ),
        pointFormat = '{series.name}<br><span style="font-size:2em; color: {point.color}; font-weight: bold">{point.y}%</span>',
        positioner = JS("function (labelWidth, labelHeight) {
                        return {
                        x: 200 - labelWidth / 2,
                        y: 180
                        };
  }")
    ) %>% 
      hc_pane(
        startAngle = 0,
        endAngle = 360,
        background = list(
          list(
            outerRadius = '112%',
            innerRadius = '88%',
            backgroundColor = JS("Highcharts.Color('#F62366').setOpacity(0.1).get()"),
            borderWidth =  0
          ),
          list(
            outerRadius = '87%',
            innerRadius = '63%',
            backgroundColor = JS("Highcharts.Color('#9DFF02').setOpacity(0.1).get()"),
            borderWidth = 0
          ),
          list(
            outerRadius = '62%',
            innerRadius =  '38%',
            backgroundColor = JS("Highcharts.Color('#0CCDD6').setOpacity(0.1).get()"),
            borderWidth = 0
          )
        )
      ) %>% 
      hc_yAxis(
        min = 0,
        max = 100,
        lineWidth = 0,
        tickPositions = list()
      ) %>% 
      hc_plotOptions(
        solidgauge = list(
          borderWidth = '34px',
          dataLabels = list(
            enabled = FALSE
          ),
          linecap = 'round',
          stickyTracking = FALSE
        )
      ) %>% 
      hc_add_series(
        name = "Move",
        borderColor = JS("Highcharts.getOptions().colors[0]"),
        data = list(list(
          color = JS("Highcharts.getOptions().colors[0]"),
          radius = "100%",
          innerRadius = "100%",
          y = 80
        ))
      ) %>% 
      hc_add_series(
        name = "Exercise",
        borderColor = JS("Highcharts.getOptions().colors[1]"),
        data = list(list(
          color = JS("Highcharts.getOptions().colors[1]"),
          radius = "75%",
          innerRadius = "75%",
          y = 65
        ))
      ) %>% 
      hc_add_series(
        name = "Stand",
        borderColor = JS("Highcharts.getOptions().colors[2]"),
        data = list(list(
          color = JS("Highcharts.getOptions().colors[2]"),
          radius = "50%",
          innerRadius = "50%",
          y = 50
        ))
      )
    
    hc3
    })
  
  #Update Input based on Carrier Selected
  observe({
    carrier <- input$carrier
    newDest <- hflights %>%
      filter(UniqueCarrier == carrier) %>%
      distinct(Dest) %>%
      select(Dest)

    updateSelectInput(session, "destGraph", choices = newDest)
  })
    
  }



shinyApp(ui=ui,server=server)