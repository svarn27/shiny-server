#Graph Data
rm(list=ls())

library(highcharter)
library(shiny)
library(dplyr)
library(DT)
library(shinyBS)

options(DT.options = list(dom = 'tpl', rownames = FALSE, pageLength = 10))

# source(paste0(baseDir,"query_function.R"))
# source(paste0(baseDir,"graph_functions.R"))

time_horizons <- c("DAY", "WTD", "MTD", "QTD")

sector_data1 <- read.csv("/srv/shiny-server/market/sector_data.csv", stringsAsFactors = FALSE)[,-1]
country_data1 <- read.csv("/srv/shiny-server/market/country_data.csv", stringsAsFactors = FALSE)[,-1]
sector_data1$Date <- as.Date(sector_data1$Date)

rptDate <- max(sector_data1$Date)
dates <- sector_data1 %>%
          arrange(desc(Date)) %>%
          distinct(Date)
dates <- as.Date(dates$Date)
          

bmks <- unique(sector_data1$Benchmark)

ui = fluidPage(
  fluidRow(
    column(width=3,selectInput("rptDate", "Report as of:", dates)),
    column(width=3,selectInput("horizon", "Report Horizon:", time_horizons))
  ),
  tabsetPanel(
    tabPanel("Portfolio Performance"),
    tabPanel("Benchmark Performance",
             fluidRow(highchartOutput("plot_all_benchmarks", height = "400")),
             #              fluidRow(selectInput("market_distinction",label = "Choose a Market:", 
             #                                   choices = c("Domestic","Developed", "Emerging"))
             #                       ),
             fluidRow(
               column(width=6,highchartOutput("plot_bmk_style")),
               column(width=6,highchartOutput("plot_bmk_size"))
             )
    ),
    tabPanel("Sector Performance",
             radioButtons("sector_graph", "Weight or Return?", c("Return", "Weight"), inline=TRUE),
             highchartOutput("plot_sector_contributions", height = "400", width="95%"),
             conditionalPanel( condition= "input.sector_graph == 'Weight'",
                               highchartOutput("plot_sector_wts", height = "400", width="95%")
             ),
             conditionalPanel( condition=  "input.sector_graph == 'Return'",
                               highchartOutput("plot_sector_heatmap", height = "400", width="95%")
             )
    ),
    tabPanel("Country Performance",
             fluidRow(
               column(width=3, selectInput("bmk", "Select Benchmark:", bmks)),
               column(width=3,radioButtons("country_graph", "Contribution or Return?", c("Contribution", "Return"), inline=TRUE))
             ),
             fluidRow(
               column(width=2, highchartOutput("plot_bmk_total", height="400", width="100%")),
               column(width=10,highchartOutput("plot_country_bar", height = "400", width="95%"))
             ),
             fluidRow(
               column(width=6,highchartOutput("plot_country_wts", height = "400", width="95%")),
               column(width=6,DT::dataTableOutput("country_table"))
             )
    ),
    tabPanel("Factor Performance")
  ),
  bsModal("sectorModal", "Trailing Sector Returns", "irButton", size="large", highchartOutput("plot_sector_trailing")),
  bsModal("countryModal", "Trailing Country Returns", "irButton", size="large", highchartOutput("plot_country_trailing")),
  bsModal("bmkModal", "Trailing Benchmark Returns", "irButton", size="large", fluidRow())
)


server <- function(input,output, session){
  
  #####Get the Necessary Data from db (tables dailysector, dailycountry)
  sector_data <- reactive({
    
    data <- sector_data1[sector_data1$Date == rptDate,]
    
    names(data) <- c("Grouping","Return","Weight","Contribution","Horizon","Benchmark","Date")
    
    return(data)                                                
  })
  country_data <- reactive({
    
    data <- country_data1[country_data1$Date == input$rptDate,]
    data <- data[grep("^[^R]*$", data$Benchmark),]
    
    names(data) <- c("Grouping","Return","Weight","Contribution","Horizon","Benchmark","Date")
    
    return(data)
  })
  
  #####Benchmark Performance #####
  output$plot_all_benchmarks <- renderHighchart({
    
    sector_data <- sector_data()
    
    data <- sector_data[sector_data$Grouping == "Bmk Total" & sector_data$Horizon == input$horizon,]
    
    hc <- hchart(data, "column", x= Benchmark, y= Return ) %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name") %>%
      hc_title(text=paste0(input$horizon, " - Benchmark Returns")) %>%
      hc_xAxis(title=list(text=NULL)) %>%
      hc_plotOptions(
        series = list(
          colorByPoint=TRUE,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("function(){
                           return Highcharts.numberFormat(this.y, 2);
  }"))))
    
    return(hc)
    
})
  output$plot_bmk_style <- renderHighchart({
    
    sector_data <- sector_data()
    
    #sector_data <- sector_data()
    data <- sector_data[grep("Value|Growth", sector_data$Benchmark), ]
    data <- data[data$Grouping == "Bmk Total", ]
    data <- data[data$Horizon == input$horizon,]
    
    #Color of Graphs
    data$style <- NA
    data[grep("Value", data$Benchmark),"style"] <- "Value"
    data[grep("Growth", data$Benchmark),"style"] <- "Growth"
    
    #Color of Graphs
    data$bmk <- NA
    data$bmk <- gsub("Value|Growth","", data$Benchmark)
    
    hchart(data, x=bmk, y=Return, group = style, type="bar") %>%
      hc_title(text=paste0("Style Benchmark Returns - ", input$horizon)) %>%
      hc_xAxis(title=list(text=NULL)) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("function(){
                           return Highcharts.numberFormat(this.y, 2);
  }"))))
    
})
  output$plot_bmk_size <- renderHighchart({
    
    data <- sector_data()
    data <- data[data$Grouping == "Bmk Total", ]
    data <- data[data$Horizon == input$horizon,]
    
    data <- data[data$Benchmark %in% c("MSCI World Ex-United States",
                                       "MSCI World ex USA Small Cap",
                                       "MSCI Emerging Markets",
                                       "MSCI EM (Emerging Markets) Small Cap",
                                       "Russell 1000",
                                       "Russell 2000"),]
    
    data$market <- NA
    data[grep("Emerging", data$Benchmark),"market"] <- "Emerging"
    data[grep("World", data$Benchmark),"market"] <- "Developed"
    data[grep("Russell", data$Benchmark),"market"] <- "Domestic"
    
    data$size <- NA
    data[grep("Small|1000", data$Benchmark),"size"] <- "Small Cap"
    data[is.na(data$size),"size"] <- "Large Cap"
    
    
    hchart(data,x=market,group=size,y=Return,type="bar" ) %>%
      hc_title(text=paste0("Size Benchmark Returns - ", input$horizon)) %>%
      hc_xAxis(title=list(text=NULL)) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("function(){
                           return Highcharts.numberFormat(this.y, 2);
  }"))))

  })
  
  #####Sector Performance #####
  output$plot_sector_heatmap <- renderHighchart({
    
    sector_data <- sector_data()
    
    data <- sector_data[sector_data$Horizon == input$horizon,]
    
    hc <- hchart(data, "heatmap", x = Benchmark, y= Grouping, value=Return, color=value) %>%
      hc_title(text=paste0("Sector Returns - ",input$horizon)) %>%
      hc_xAxis(title=list(text=NULL)) %>%
      hc_yAxis(reverse=TRUE, title=list(text=NULL)) %>%
      hc_tooltip(formatter=JS(paste0(
        "function () {
        return '<b>' + this.series.yAxis.categories[this.point.y] + '</b><br>'
        + '<b>' + this.series.xAxis.categories[this.point.x] + ':</b> ' +
        Highcharts.numberFormat(this.point.value,2) + ' ",input$horizon,"'}"))) %>% 
      hc_legend(enabled=FALSE) %>%
      hc_plotOptions(
        series = list(
          events = list(
            click = JS(
              "function(e){
              
              sector = event.point.series.yAxis.categories[event.point.y]
              bmk = event.point.series.xAxis.categories[event.point.x]
              send = [sector, bmk];
              
              Shiny.onInputChange('sector_trailing', send);     
              
              $('#sectorModal').modal('toggle')
  }"
              )
            ),
          dataLabels = list(
            enabled = TRUE,
            style = list(fontSize="10px"),
            formatter = JS("function(){
                           return Highcharts.numberFormat(this.point.value, 2);
                           }"))))
    
    
    return(hc)
    
    })
  output$plot_sector_contributions <- renderHighchart({
    
    sector_data <- sector_data()
    
    data <- sector_data[sector_data$Horizon == input$horizon & 
                          sector_data$Grouping != "Bmk Total", ]
    
    hc <- hchart(data, "column", x = Benchmark, y = Contribution, group= Grouping) %>%
      hc_title(text=paste0("Sector Contribution - ",input$horizon)) %>%
      hc_subtitle(text="Click a Sector to View Trailing Returns") %>%
      hc_xAxis(categories=unique(data$Benchmark),title=list(text=NULL)) %>%
      hc_plotOptions(series = list(
        stacking = "normal",
        point = list(
          events = list(
            click = JS(
              "function(e){
              sector = this.series.name;
              bmk = this.category
              send = [sector, bmk]
              
              Shiny.onInputChange('sector_trailing', send);     
              
              $('#sectorModal').modal('toggle')
              
  }"
                      )
            )
            )
            )
            )
    
    return(hc)
    
})
  output$plot_sector_wts <- renderHighchart({
    
    sector_data <- sector_data()
    
    data <- sector_data[sector_data$Horizon == input$horizon & 
                          sector_data$Grouping != "Bmk Total", ]
    
    return(plot_wts(data))
    
  })
  output$plot_sector_trailing <- renderHighchart({
    
    sector <- input$sector_trailing[1]
    bmk <- input$sector_trailing[2]
    
    data <- qry(paste0("Select * from dailysector 
                       where Sector = '",sector,"'
                       and Benchmark = '",bmk,"'
                       and Horizon = '",input$horizon,"'"))
    
    hchart(data,x=Date, y=Return, type=ifelse(input$horizon == "DAY", "column", "spline")) %>%
      hc_title(text=ifelse(input$horizon == "DAY", 
                           paste0(sector," Trailing Returns - Daily"), 
                           paste0(sector," Trailing Return - Cumulative, ",input$horizon))) %>%
      hc_subtitle(text=bmk)
    
  })
  
  #####Country Performance ######
  country_table_data <- reactive({
    
    country_data <- country_data() 
    
    country_data <- country_data[country_data$Horizon == "DAY" & 
                                   country_data$Benchmark == "MSCI Emerging Markets",
                                  names(country_data) %in% c("Grouping", "Weight", "Return", "Contribution")]
    
    names(country_data) <- c("Country", "Return", "Weight", "Contribution")
    
    country_data[,c("Return", "Weight", "Contribution")] <- apply(country_data[,c("Return", "Weight", "Contribution")],2,
                                                                  function(x){round(x,2)})
    
    return(country_data)
    
  })
  output$country_table = DT::renderDataTable(country_table_data())
  output$plot_country_bar <- renderHighchart({
    
    country_data <- country_data()
    
    data <- country_data[country_data$Horizon == input$horizon & 
                           country_data$Grouping != "Bmk Total" &
                           country_data$Benchmark == input$bmk, ]
    data <- data[data$Weight > .01 | data$Weight < -.01,  ]
    
    ifelse(input$country_graph == "Contribution",
           hc <- hchart(data, "column", x = Grouping, y = Contribution),
           hc <- hchart(data, "column", x = Grouping, y = Return))
    
    hc <- hc %>%
      hc_title(text=paste0("Country ",input$country_graph," - ",input$horizon)) %>%
      hc_subtitle(text="Click a Country to View Trailing Returns") %>%
      hc_xAxis(categories=data$Grouping,title=list(text=NULL)) %>%
      hc_plotOptions(
        series = list(
          colorByPoint=TRUE,
          dataLabels = list(
            enabled = TRUE,
            formatter = JS("function(){
                           return Highcharts.numberFormat(this.y, 2);
  }")
                      ),
          point = list(
            events = list(
              click = JS(
                "function(e){
                country = this.category;
                
                Shiny.onInputChange('country_trailing', country);     
                
                $('#countryModal').modal('toggle')
                
                }"
                  )
              )
              )
              )
              )
    
    })
  output$plot_country_wts <- renderHighchart({
    
    country_data <- country_data()
    
    data <- country_data[country_data$Horizon == input$horizon & 
                           country_data$Grouping != "Bmk Total" &
                           country_data$Benchmark == input$bmk, ]
    data <- data[data$Weight > .1 | data$Weight < -.1,  ]
    
    hc <- hchart(data, y=Weight,name=Grouping,type= "pie") %>%
      hc_title(text="Country Weights")
    
    return(hc)
    
  })
  output$plot_bmk_total <- renderHighchart({
    
    country_data <- country_data()
    
    data <- country_data %>% 
      filter(
        Horizon == input$horizon, 
        Benchmark == input$bmk,
        Grouping == 'Bmk Total')
    
    hchart(data, x=Benchmark, y=Return, type="column") %>%
      hc_xAxis(title=list(text=NULL)) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            style = list(fontSize="10px"),
            formatter = JS("function(){
                           return Highcharts.numberFormat(this.y, 2);
  }")
          )
            )
          )
    
  })
  output$plot_country_trailing <- renderHighchart({
    
    country <- input$country_trailing
    
    data <- qry(paste0("Select * from dailycountry 
                     where country = '",country,"'
                     and Benchmark = '",input$bmk,"'
                     and Horizon = '",input$horizon,"'"))
    
    hchart(data,x=Date, y=Return, type=ifelse(input$horizon == "DAY", "column", "spline")) %>%
      hc_title(text=ifelse(input$horizon == "DAY", 
                           paste0(country," Trailing Returns - Daily"), 
                           paste0(country," Trailing Return - Cumulative, ",input$horizon)))
    
  })
  
  }

shinyApp(ui=ui,server=server)