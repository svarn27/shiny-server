rm(list=ls())

library(shiny)
library(highcharter)
library(shinyBS)
library(DT)
library(lubridate)
library(dplyr)
library(zoo)

###PERFORMANCE REPORT####
baseDir <- ifelse(as.character(.Platform$OS.type) == "windows",
                  "J:\\Misc\\Shane\\mqa\\",
                  "/srv/shiny-server/utilities/")

source(paste0(baseDir,"query_function.R"))
source(paste0(baseDir,"compound_function.R"))

options(shiny.trace=F,xtable.include.rownames=F)

#     #download button; timestamp function creates a unqiue file name
timeStamp <- function(){return(gsub("[[:punct:]|[:space:]]", "" ,Sys.time()))}

result <- qry("SELECT DISTINCT TRIM(ENTITY_NAME) AS ENTITY_NAME
              FROM pacedb
              WHERE PERF_END_DATE IS NULL
              AND BANK_MARKET_VALUE IS NOT NULL
              AND ENTITY_NAME NOT REGEXP 
              'FRS|American|DC|Perspective|Cash|Account|IMA'
              AND BANK_ID NOT REGEXP 'EPH10317|FRSFG1014802'
              ORDER BY ENTITY_NAME ASC;")

result <- lapply(result,
                 function(x){
                   paste0("<option value='",trimws(x),"'>",trimws(x),"</option>",collapse="")
                 })

dates <- qry("SELECT DISTINCT MONTH_END_DATE
             FROM pacedb
             ORDER BY MONTH_END_DATE ASC")

buckets <- qry("SELECT DISTINCT bucket 
               FROM allmanagers
               WHERE bucket != ''
               ORDER BY bucket")

selectTheme <- function(theme){
  switch(theme,
         null = hc_theme_null(),
         darkunica = hc_theme_darkunica(),
         gridlight = hc_theme_gridlight(),
         fintimes = hc_theme_ft(),
         dotabuff = hc_theme_db(),
         google = hc_theme_google(),
         flat = hc_theme_flat(),
         flatdark = hc_theme_flatdark(),
         simple = hc_theme_smpl(),
         sandsignika = hc_theme_sandsignika(),
         fivethirtyeight = hc_theme_538(),
         economist = hc_theme_economist())
}

ui = fluidPage (
  tags$head(HTML("
                 <script src='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/js/bootstrap-select.min.js'></script>
                 <link rel='stylesheet' href='https://cdnjs.cloudflare.com/ajax/libs/bootstrap-select/1.10.0/css/bootstrap-select.min.css'>
                 <meta http-equiv='Pragma' content='no-cache'>
                 <meta http-equiv='Expires' content='-1'>
                 <meta http-equiv='CACHE-CONTROL' content='NO-CACHE'>
                 ")),
  titlePanel("Performance Report"),
  fluidRow(
    column(3,
           HTML(paste0("
                       <label class='control-label' for='managerselection'>Select Manager:</label>
                       <select class='selectpicker'  data-actions-box='true'
                       data-live-search='true' multiple id='managerselection'
                       title='Select Managers'>",result,"</select>")),
           selectInput('bucketselect', label='Select Bucket:', choices=buckets, selected = NULL, multiple=TRUE),
           div(style="display:inline-block",downloadButton('downloadData', 'Download'),width=6),
           div(style="display:inline-block",HTML("
                                                 <div class='dropdown form-group'>
                                                 <button style='width:100px' class='btn btn-default dropdown-toggle action-button' type='button' data-toggle='dropdown'>
                                                 <span class='glyphicon glyphicon-signal'></span>  Chart
                                                 <span class='caret'></span></button>
                                                 <ul class='dropdown-menu dropdown-menu-form' role='menu'>
                                                 <li><a href='#'><button style='width:150px;text-align: left' id='perfButton' type='button' class='btn btn-default action-button'>
                                                 <span class='glyphicon glyphicon-stats'></span> Performance</button></a></li>
                                                 <li><a href='#'><button style='width:150px;text-align: left' id='irButton' type='button' class='btn btn-default action-button'>
                                                 <span class='glyphicon glyphicon-retweet'></span> &nbsp;Risk vs. Return</button></a></li>
                                                 <li><a href='#'><button style='width:150px;text-align: left' id='barraButton' type='button' class='btn btn-default action-button'>
                                                 <span class='glyphicon glyphicon-bold'></span> &nbsp;Barra Regression</button></a></li>
                                                 
                                                 <label class='control-label' for='corButton'>&nbsp; &nbsp; Rolling</label>
                                                 <li><a href='#'> <button style='width:150px;text-align: left' id='corButton' type='button' class='btn btn-default action-button'>
                                                 <span class='glyphicon glyphicon-th'></span> Correlation</button></a></li>
                                                 <li><a href='#'><button style='width:150px;text-align: left' id='riskButton' type='button' class='btn btn-default action-button'>
                                                 <span class='glyphicon glyphicon-random'></span> &nbsp;Risk</button></a></li>
                                                 
                                                 </button>
                                                 <form align='center'>
                                                 <br>
                                                 <li>
                                                 <div class='form-group shiny-input-container'  style='width: 100%;'>
                                                 <label class='control-label' for='theme'>Theme:</label>
                                                 <div>
                                                 <select id='theme'>
                                                 <option value='FALSE' selected>None</option>
                                                 <option value='fivethirtyeight'>fivethirtyeight</option>
                                                 <option value='economist'>economist</option>
                                                 <option value='darkunica'>darkunica</option>
                                                 <option value='gridlight'>gridlight</option>
                                                 <option value='sandsignika'>sandsignika</option>
                                                 <option value='simple'>simple</option>
                                                 <option value='fintimes'>fintimes</option>
                                                 <option value='dotabuff'>dotabuff</option>
                                                 <option value='google'>google</option>
                                                 <option value='flat'>flat</option>
                                                 <option value='flatdark'>flatdark</option></select>
                                                 <script type='application/json' data-for='theme' data-nonempty=''>{}</script>
                                                 </div>
                                                 </div>
                                                 </li>
                                                 </form>
                                                 </ul>
                                                 </div>
                                                 "),width=4)
           ),
    column(4,
           selectInput('returnType', 'Select Report Type:', c(Rolling = 'Historical', "Multi-Horizon" = 'Multi-Horizon')),
           sliderInput("dateRange", width="100%",
                       label="Date Range:", timeFormat="%b %Y",
                       min = as.Date(dates[1,1]), max = as.Date(dates[nrow(dates),1]), 
                       value = c( as.Date(dates[1,1]), as.Date(dates[nrow(dates),1])) 
           ),
           checkboxInput('gross', 'Gross Returns?')
    ),
    column(4,
           
           conditionalPanel( condition = "input.returnType == 'Multi-Horizon'",
                             selectInput('multihorizon', 'Select Horizons:', multiple = TRUE, 
                                         choices = c('MONTH','QTD','FYTD','CYTD', '1YR','2YR','3YR', '4YR','5YR','6YR','7YR',
                                                     '8YR', 'INCEPT'),
                                         selected = c('MONTH','QTD','FYTD','CYTD', '1YR', '3YR', '5YR','INCEPT'))),
           
           conditionalPanel( condition = "input.returnType != 'Multi-Horizon'",
                             selectInput('riskSelect', 'Select Rolling Risk:', c('1YR','3YR','5YR')),
                             div(style="display:inline-block", selectInput('months', 'Select Period:', 
                                                                           c(Monthly = 1, Quarterly = 3, Annually = 12, "3YR" = 36, "5YR" = 60) )),
                             div(style="display:inline-block",checkboxInput('rolling', 'Rolling Monthly?')))
    )
           ),
  fluidRow(
    column(12, align='center',
           dataTableOutput("table2")
    )
  ),
  bsModal("modalExample", "Risk Charting", "riskButton", size = "large", highchartOutput("plot"),
          column(width=3,
                 selectInput("riskType", label="Select Risk Type:", 
                             choices = c("Tracking Error", "Information Ratio"))
          ),
          column(width = 3,
                 sliderInput("riskLimit", width="80%",
                             label="Risk Requirement:", step=.05,
                             min = 0, max = 10, 
                             value = 0
                 )
          )
  ),
  bsModal("modalExample2", "Performance Charting", "perfButton", size = "large", highchartOutput("plot2"),
          conditionalPanel("input.returnType != 'Multi-Horizon'", 
                           column(width=5,NULL),
                                  #selectInput("bmkSelect", label="Select Benchmark:", choices= c("None"))),
                           column(width=3,
                                  radioButtons("actmgd", label = "Active or Managed?",
                                               choices = list("Active" = "ACTIVE", "Managed" = "MGD_RETURN"))
                           ),
                           column(width=3,
                                  radioButtons("rtnChart", label = "Bar or Line?",
                                               choices = list("Bar" = "BAR", "Line" = "LINE"))
                           )
          )
  ),
  bsModal("modalExample3", "Performance Charting", "corButton", size = "large", br(),highchartOutput("plot3"),downloadButton('downloadCorr', 'Download')),
  bsModal("modelExample4", "Risk vs. Return", "irButton", size="large", fluidRow(
    highchartOutput("plotIR",height = "500px")
  ),
  column(width = 3,
         selectInput("type", label = "Horizon", width = "100%",
                     choices = c("1YR", "3YR", "5YR"))),
  column(width = 3,
         sliderInput("ir", width="80%",
                     label="IR Requirement:", step=.10,
                     min = .1, max = 2, 
                     value = .3
         )
  ),
  fluidRow(
    column(width=12,
           DT::dataTableOutput('tbl')
    )
  )
  ),
  bsModal("modalExample5", "Barra Regression", "barraButton", size="large", 
          highchartOutput("plotBarra", height="500px"), 
          column(width = 2,
                 selectInput("riskGroup", label = "Grouping:",  width = "100%",
                             choices = c("Style", "Country", "Industry")
                 )
          ),
          column(width = 3,
                 selectInput("barraSel", label = "Risk Factor:",  width = "100%",
                             choices = c("Style", "Country", "Industry")
                 )  
          ),
          column(width = 2,
                 selectInput("barraHorizon", label = "Horizon:",  width = "100%",
                             choices = c("Month", "3 Months","6 Months","1 Year")
                 )  
          ),
          column(width=1,
                 checkboxInput("barraLabels", "Labels?", TRUE),
                 checkboxInput("barraLegend", "Legend?", TRUE)
          ),
          column(width = 2,
                 br(), div(align="center",downloadButton('downloadBarra', 'Download'))
          )
  ),
  HTML("<script>
       $(document).ready(function(){
       
       $('#managerselection').selectpicker('deselectAll');
       $('#managerselection').selectpicker('refresh');
       
       $('.dropdown-menu').on('click', function(e) {
       if($(this).hasClass('keep_open')) {
       e.stopPropagation();
       }
       });
       
       })
       
       var bucketList;
       var manArr;
       
       Shiny.addCustomMessageHandler('bucket',
       function(result){
       bucketList = result
       
       $('#managerselection option').removeAttr('selected');
       
       selectors = bucketList;
       select = []
       
       $.each(selectors, function(i, d){
       select.push(d);
       });
       
       manArr = select[0];
       
       $('#managerselection').val(select[0]);
       $('#managerselection').change();
       $('#managerselection').selectpicker('refresh');    		
       
       });
       
       </script>
       ")
  )

server = function(input, output, session){
  
  #refresh returns data if the update button, submit button, or gross return inputs change
  returns <- reactive({
    
    gross <- ifelse(input$gross, "GROSS", "NET")
    type <- input$returnType
    man <- input$managerselection
    months <- input$months
    horizons <- c("MANAGER", "MKT_VAL", input$multihorizon)
    dateRng <- input$dateRange
    rolling <- input$rolling
    risk <- input$riskSelect
    
    start <- dateRng[1]
    end <- dateRng[2]
    
    if(length(man) > 1){
      man <- lapply(man, function(x){paste0("'",x,"'")})
      man <- toString(man)
    } else { man <- paste0("'",man,"'")}
    
    returns <- qry(paste0("SELECT ENTITY_NAME AS MANAGER,
                          MONTH_END_DATE AS MONTH_END,
                          M1_",gross,"_RETURN AS MGD_RETURN,
                          M1_BM1_RETURN AS BMK_RETURN,
                          M1_",gross,"_RETURN - M1_BM1_RETURN AS ACTIVE,
                          concat('$',FORMAT(BANK_MARKET_VALUE,2)) AS MKT_VAL,
                          PRI_BMK_NAME AS BMK_NAME
                          FROM pacedb 
                          WHERE ENTITY_NAME IN (",man,")
                          AND BANK_MARKET_VALUE IS NOT NULL
                          AND  M1_NET_RETURN IS NOT NULL
                          AND  M1_BM1_RETURN IS NOT NULL
                          
                          AND EXTRACT(YEAR_MONTH FROM MONTH_END_DATE) Between EXTRACT(YEAR_MONTH FROM '",start,"')
                          AND EXTRACT(YEAR_MONTH FROM '",end,"')
                          AND BANK_MARKET_VALUE > 100
                          AND PERF_END_DATE IS NULL
                          AND MONTH_END_DATE >= PERF_START_DATE
                          AND BANK_ID != 'LCFGGBN002A0'
                          ORDER BY ENTITY_NAME ASC, MONTH_END_DATE DESC;"))
    
    returns$MANAGER <- sapply(returns$MANAGER, trimws)
    
    if(months > 35){rolling <- TRUE}
    
    ifelse(type == "Historical",
           returns <- compound_trail(returns, months, rolling, risk),
           returns <- compound_multi(returns[-nrow(returns),])[,horizons])
    
    
    return(returns)
    
  })
  
  #Update benchmark options in performance chart
  observe({
    returns <- returns()
    
    updateSelectInput(session,inputId='bmkSelect',
                      label="Select Benchmark:",
                      choices=as.character(unique(returns$BMK_NAME)))
    
  })
  
  #Update dates of the range slider as a new manager is selected
  observe({
    
    man <- input$managerselection
    
    if(length(man) > 1){
      man <- lapply(man, function(x){paste0("'",x,"'")})
      man <- toString(man)
    } else { man <- paste0("'",man,"'")}
    
    dates <- qry(paste0("SELECT DISTINCT
                        MONTH_END_DATE AS MONTH_END
                        FROM pacedb 
                        WHERE ENTITY_NAME IN (",man,")
                        ORDER BY MONTH_END_DATE ASC;"))
    
    #          session$sendCustomMessage(type="dates", message=dates)
    if(nrow(dates) > 0){
      updateSliderInput(session, "dateRange",
                        min = as.Date(dates[1,1]), max = as.Date(dates[nrow(dates),1]), 
                        value = c( as.Date(dates[1,1]), as.Date(dates[nrow(dates),1])))
    }
    
  })
  
  #render tables using returns dataframe
  output$table2 = renderDataTable(
    returns()[,!(names(returns()) %in% c("BMK_NAME","MKT_VAL"))], 
    options = list(
      dom = 'tpl', 
      buttons = list('csv'),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({
        'background-color': '#f6931f', 'color': '#fff'
});",
                       "}")
    ), 
    rownames = FALSE
      )
  
  #render risk chart
  output$plot <- renderHighchart({
    
    returns <- returns()
    
    if(input$returnType == 'Historical'){
      
      riskType <- switch(input$riskType, "Tracking Error" = "TE", "Information Ratio" = "IR")
      
      x<- as.numeric(as.POSIXct( as.Date(returns[!is.na(returns[,riskType]), c('MONTH_END')]), format='%Y-%m-%d'))*1000
      y<- as.numeric(as.character(returns[!is.na(returns[,riskType]), c(riskType)]))
      #y<-ifelse(riskType == "TE", y[!is.na(y)], y)
      MANAGER <- as.character(returns[!is.na(returns[,riskType]), 'MANAGER'])
      data <- data.frame(MANAGER ,x,y)
      data <- data[!is.na(data$y),]
      
      hc <- highchart() %>%
        hc_title(text = paste0(input$riskType,' Over Time')) %>%
        hc_chart(type="spline", zoomType='x') %>%
        hc_tooltip(xDateFormat = '%Y-%m-%d', shared=TRUE) %>%
        hc_plotOptions(spline=list(marker=list(enabled=FALSE))) %>%
        hc_xAxis(type='datetime',
                 labels = list(
                   format = '{value:%Y-%m}',
                   rotation = 90,
                   align = 'left'
                 )
        ) %>%
        hc_yAxis(plotLines = list(
          list(color = "#ccd1d1",
               width = 2,
               value = 0),
          list(color = ifelse(input$riskLimit == 0,"#ccd1d1","#FF0000"),
               dashStyle = "shortdot",
               width = 1,
               value = input$riskLimit))
        ) 
      
      if(length(unique(data$MANAGER))==0){return(hc)}
      
      for(man in unique(data$MANAGER)){
        hc <- hc %>%
          hc_add_series(data = list_parse(data[data$MANAGER == man,c("x","y")]), 
                        name=man)
      }
      hc <- hc %>%
        hc_exporting(enabled = TRUE, filename = "custom-file-name")
      
      
      if (input$theme != FALSE) {
        theme <- selectTheme(input$theme)
        hc <- hc %>% hc_add_theme(theme)
      }
      
      hc
    }
  })
  
  #render performance charts
  output$plot2 <- renderHighchart({
    
    returns <- returns()
    
    #render multihorizon performance chart
    if(input$returnType == 'Multi-Horizon'){
      
      returns[,3:ncol(returns)] <- apply(returns[,3:ncol(returns)], 2, 
                                         function(x){as.numeric(as.character(x))})
      x <- as.character(names(returns[,3:ncol(returns)]))
      
      hc <- highchart() %>%
        hc_title(text = 'Multi-Horizon Returns') %>%
        hc_chart(type="column") %>%
        hc_xAxis(categories = x)
      
      for(z in c(1:nrow(returns)) ){
        man <- as.character(returns[z,1])
        y <- as.data.frame(t(returns[z, 3:ncol(returns)]))
        
        hc <- hc %>% 
          hc_add_series(data=y[,1], name=man)
      }
      
      #render historical performance chart
    } else if(input$returnType == 'Historical' ){
      
      if(input$rtnChart == 'BAR'){
        
        manCount <- length(unique(returns$MANAGER))
        if(manCount == 1){
          returns <- returns %>% arrange(MONTH_END)
          x <- as.Date(returns$MONTH_END)
          yy <- sapply(returns$MKT_VAL, function(x){gsub("\\$|,", "", x)})
          yy <- as.numeric(as.character(yy))
          y <- as.numeric(as.character(returns$BMK_RETURN))
          z <- as.numeric(as.character(returns[,input$actmgd]))
          
          
          hc <- highchart() %>%
            hc_chart( zoomType='x') %>%
            hc_title(text = 'Performance Over Time') %>%
            hc_xAxis(categories=x) %>%
            hc_add_series(type='spline', data=yy, 
                          name='Market Value', yAxis=1) %>%
            hc_add_series(type="column", data=y, name="Benchmark") %>%
            hc_add_series_scatter(x=c(1:length(x)-1),y=z, name=as.character(input$actmgd)) %>%
            hc_plotOptions(spline=list(marker=list(enabled=FALSE, symbol='circle')),
                           scatter=list(tooltip=list(pointFormat = '<b>{point.y}</b><br/>'),
                                        showInLegend=TRUE)) %>%
            hc_yAxis_multiples(
              list(
                title = list(text = "Performance")
              ),
              list(
                title = list(text = "Market Value"),
                opposite = TRUE,
                min=(min(yy)*.75), max=(max(yy)*1.25)
              )
            )       
          
          if (input$theme != FALSE) {
            theme <- selectTheme(input$theme)
            hc <- hc %>% hc_add_theme(theme)
          }
          
          hc
          
        } else {
          managers <- unique(returns$MANAGER)
          returns$MONTH_END <- datetime_to_timestamp(as.Date(returns$MONTH_END,format = "%Y-%m-%d"))
          test <- unique(returns[,c("MONTH_END", "BMK_RETURN", "BMK_NAME")])
          test <- test[test$BMK_NAME == input$bmkSelect,]
          test$BMK_RETURN <- as.numeric(as.character(test$BMK_RETURN))
          
          hc <- highchart() %>%
            hc_chart( zoomType='x') %>%
            hc_xAxis(type='datetime',
                     tickmarkPlacement = 'between',
                     #gridLineWidth = 1,
                     tickInterval = (24 * 3600 * 1000 * 30 * as.numeric(input$months)),
                     labels = list(
                       format = '{value:%Y-%m}'#,
                       #rotation = 90,
                       #align = 'left'               
                     )
            ) %>%
            hc_plotOptions(column=list(pointPlacement='between')) %>%
            hc_tooltip(pointFormat = "<b>Date:</b> {point.x:%Y-%m-%d}<br><b>Active:</b> {point.y:.2f}")  %>% 
            hc_add_series(data=list_parse2(test), type="column", name="Benchmark")
          
          
          for(m in managers){
            
            z <- returns[returns$MANAGER == m, ]
            hc <- hc %>% hc_add_series_scatter(x = z$MONTH_END,
                                               y = as.numeric(as.character(z[,input$actmgd])),
                                               showInLegend = TRUE,
                                               name = m)
            
          }
          
        }
        
      } else if(input$rtnChart == 'LINE'){
        
        x<- as.numeric(as.POSIXct( as.Date(returns[!is.na(returns[,"ACTIVE"]), c('MONTH_END')]), format='%Y-%m-%d'))*1000
        y<- as.numeric(as.character(returns[!is.na(returns[,"ACTIVE"]), c("ACTIVE")]))
        #y<-ifelse(riskType == "TE", y[!is.na(y)], y)
        MANAGER <- as.character(returns[!is.na(returns[,"ACTIVE"]), 'MANAGER'])
        data <- data.frame(MANAGER ,x,y)
        data <- data[!is.na(data$y),]
        
        hc <- highchart() %>%
          hc_title(text = paste0('Active Performance Over Time, ',input$months,"-months Rolling")) %>%
          hc_chart(type="spline", zoomType='x') %>%
          hc_tooltip(xDateFormat = '%Y-%m-%d', shared=TRUE) %>%
          hc_plotOptions(spline=list(marker=list(enabled=FALSE))) %>%
          hc_xAxis(type='datetime',
                   labels = list(
                     format = '{value:%Y-%m}',
                     rotation = 90,
                     align = 'left'
                   )
          ) %>%
          hc_yAxis(plotLines = list(
            list(color = "#ccd1d1",
                 width = 2,
                 value = 0),
            list(color = ifelse(input$riskLimit == 0,"#ccd1d1","#FF0000"),
                 dashStyle = "shortdot",
                 width = 1,
                 value = input$riskLimit))
          ) 
        
        if(length(unique(data$MANAGER))==0){return(hc)}
        
        for(man in unique(data$MANAGER)){
          hc <- hc %>%
            hc_add_series(data = list_parse(data[data$MANAGER == man,c("x","y")]), 
                          name=man)
        }
        
        
      }
    }
    
    hc <- hc %>%
      hc_exporting(enabled = TRUE, filename = "custom-file-name")
    
    if (input$theme != FALSE) {
      theme <- selectTheme(input$theme)
      hc <- hc %>% hc_add_theme(theme)
    }
    
    hc
  })
  
  #the statistics table for the graph; update when returns data changes
  #     observe({
  #        
  #           returnsTab <- returns()
  #           if(nrow(returnsTab) > 0 & names(returnsTab)[2] == "MONTH_END"){
  #               s <- statTable(returnsTab)
  #           
  #               s <- print(gsub("\n","",trimws(print(xtable(s),
  #                          type="html", include.rownames=TRUE,
  #                          html.table.attributes="align='center' width='50%' id='statistictable'"))),
  #                          quote=FALSE)
  #               
  #               session$sendCustomMessage(type="stat", message = s)
  #           }
  #     })
  
  #handler for bucket selector; find managers in selected bucket
  observe({  
    
    bucket <- input$bucketselect
    
    if(length(bucket) > 1){
      bucket <- lapply(bucket, function(x){paste0("'",x,"'")})
      bucket <- toString(bucket)
    } else { bucket <- paste0("'",bucket,"'")}
    
    if(length(bucket) > 0){
      result <- qry(paste0("SELECT PaceID 
                           from allmanagers 
                           where Bucket IN (",bucket,")
                           and Bucket != ''"))
      result <- lapply(result,trimws)
      
      session$sendCustomMessage(type="bucket", message=result)                          
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){paste0("export_file_",timeStamp(),".csv")},
    content = function(file) {
      write.csv(returns(), file,row.names=F)
    }
  )
  
  output$downloadCorr <- downloadHandler(
    filename = function(){paste0("export_file_",timeStamp(),".csv")},
    content = function(file) {
      write.csv(corr(), file,row.names=F)
    }
  )
  #    
  
  ###
  corr <-reactive({
    input$corButton
    
    isolate({
      returnsTab <- returns()
    })
    
    z <- NULL
    
    if(length(unique(returnsTab$MANAGER)) > 1 & !(names(returnsTab)[2] == "MKT_VAL")){
      z <- corrTable(returnsTab)
    }
    
    return(z)
    
  })
  #    #returns correlation table when the correlation button is pressed
  output$plot3 <- renderHighchart({
    
    z <- corr()
    
    if(is.null(z)){return(highchart()%>%hc_title(text="Select Multiple Managers", align="left"))}
    
    hchart(as.matrix(z)) %>%
      hc_plotOptions(
        series = list(
          dataLabels = list(
            enabled = TRUE,
            style = list(fontSize="9px"),
            formatter = JS("function(){
                           return Highcharts.numberFormat(this.point.value, 2);
  }")
              )
            )
          ) %>%
      hc_title(text = 'Portfolio Correlation Matrix') %>%
      hc_exporting(enabled = TRUE, 
                   filename = "custom-file-name")
    #)
  })
  
  output$plotIR <- renderHighchart({
    
    managers <- input$managerselection # "Frontier" #
    
    if(is.null(managers)){return(highchart()%>%hc_title(text="Select Managers"))}
    
    if(!is.null(managers)){
      if(length(managers) > 1){
        man <- lapply(managers, function(x){paste0("'",x,"'")})
        man <- toString(man)
      } else { man <- paste0("'",managers,"'")}
    }
    
    dateRng <- input$dateRange
    end <- (dateRng[2] + months(0:23)-days(1) + months(1))
    
    returns <- qry(paste0("SELECT TRIM(ENTITY_NAME) AS ENTITY_NAME, 
                          MONTH_END_DATE, M1_NET_RETURN, M1_BM1_RETURN
                          FROM pacedb
                          WHERE ENTITY_NAME IN (",man,")
                          AND MONTH_END_DATE <= '",end,"'
                          ORDER BY ENTITY_NAME, MONTH_END_DATE"))
    
    graphDat <- returns 
    
    months <- switch(input$type, '1YR' = 12,
                     '3YR' = 36,
                     '5YR' = 60)
    
    months <- (returns %>% 
                 distinct(MONTH_END_DATE) %>% 
                 arrange(desc(MONTH_END_DATE)) %>%
                 select(MONTH_END_DATE))[months,]
    
    annualizer <- switch(input$type, '1YR' = as.numeric(1/1),
                         '3YR' = as.numeric(12/36),
                         '5YR' = as.numeric(12/60))
    
    graphDat <- graphDat %>%
      filter(MONTH_END_DATE >= months) %>%
      group_by(ENTITY_NAME) %>%         
      summarise(Risk = round(sd(M1_NET_RETURN - M1_BM1_RETURN)*(12 ^ 0.5),2),
                Return = round(((prod(1+M1_NET_RETURN/100)^annualizer-1)*100) - 
                                 ((prod(1+M1_BM1_RETURN/100)^annualizer-1)*100),2),
                IR = round(Return/Risk,2))
    
    output$tbl = DT::renderDataTable(
      graphDat, options = list(dom = 'tpl', buttons = list('csv')), rownames = FALSE
    )
    
    slope <- round(c(0:10) / input$ir, 2)
    slope <- data.frame(x = slope, y = c(0:10))
    regLine <- list_parse2(slope[slope$x <= 10,])
    
    graphHigh <- ceiling(max(c(graphDat$Risk,graphDat$Return)) * 1.25)
    graphLow <- floor(min(c(graphDat$Risk,graphDat$Return)) * 1.25)
    
    hc <- highchart() %>%
      hc_add_series(name = paste("IR = ",input$ir), type = "line", 
                    data = regLine, dashStyle = 'longdash',
                    marker = list(enabled=FALSE),
                    enableMouseTracking = FALSE) %>%
      hc_yAxis(title=list(text="Return")) %>% 
      hc_xAxis(title=list(text="Risk")) %>%
      hc_add_series_scatter(x=graphDat$Risk, 
                            y=graphDat$Return,
                            z=round(graphDat$Return/graphDat$Risk,2),
                            label=graphDat$ENTITY_NAME,
                            dataLabels = list(
                              enabled = TRUE,
                              format = "{point.label}"
                            )) %>% 
      hc_chart(zoomType = "xy") %>% 
      hc_tooltip(useHTML = TRUE,
                 headerFormat = "<table>",
                 pointFormat = paste("<tr><th colspan=\"2\">{point.label}</th></tr>",
                                     "<tr><th>Return:</th><td>{point.y}</td></tr>",
                                     "<tr><th>Risk:</th><td>{point.x}</td></tr>",
                                     "<tr><th>IR:</th><td>{point.z}</td></tr>"),
                 footerFormat = "</table>") %>%                  
      hc_exporting(enabled = TRUE, filename = "custom-file-name")
    
    #   for(x in 1:nrow(graphDat)){
    #     hc <- hc %>% hc_add_serie_scatter(x=graphDat[x,"Risk"], 
    #                                       y=graphDat[x,"Return"],
    #                                       z=round( graphDat[x,"Return"] /graphDat[x,"Risk"] , 2),
    #                                       label=graphDat[x,"ENTITY_NAME"],
    #                                       dataLabels = list(
    #                                         enabled = TRUE,
    #                                         format = "{point.label}"
    #                                       ))
    #   } 
    
    
    if (input$theme != FALSE) {
      theme <- selectTheme(input$theme)
      hc <- hc %>% hc_add_theme(theme)
    }
    
    hc
  })
  
  barraData <- reactive({
    
    dateRng <- input$dateRange
    
    end <- ceiling_date(dateRng[2], "month")
    
    monthsB <- switch(input$barraHorizon, 
                      'Month' = 1,
                      '3 Months' = 3,
                      '6 Months' = 6,
                      '1 Year' = 12)
    
    start <- end %m-% months(monthsB)
    gross <- ifelse(input$gross, "GROSS", "NET")
    
    barra <- qry(paste0("SELECT PaceID, Bucket, ACT_EXP, PARENT_NODE, FACTOR FROM
                        (SELECT BarraID, PaceID, Bucket
                        FROM allmanagers
                        WHERE Strategy='Active') as a
                        INNER JOIN
                        (SELECT MANAGER, 
                        PARENT_NODE, 
                        ACT_EXP,
                        FACTOR
                        FROM barraexposure
                        WHERE MONTH(DATE) = MONTH('",start,"') 
                        AND YEAR(DATE) = YEAR('",start,"')) as b
                        ON a.BarraID = b.MANAGER
                        ORDER BY PaceID
                        "))
    
    returns <- qry(paste0("SELECT ENTITY_NAME AS MANAGER,
                          MONTH_END_DATE AS MONTH_END, 
                          M1_",gross,"_RETURN AS MGD_RETURN,
                          M1_BM1_RETURN AS BMK_RETURN,
                          concat('$',FORMAT(BANK_MARKET_VALUE,2)) AS MKT_VAL
                          FROM pacedb
                          WHERE BANK_MARKET_VALUE IS NOT NULL
                          AND  M1_NET_RETURN IS NOT NULL
                          AND  M1_BM1_RETURN IS NOT NULL
                          AND MONTH_END_DATE >= PERF_START_DATE
                          AND MONTH_END_DATE BETWEEN '",start,"' AND '",end,"'
                          AND BANK_MARKET_VALUE > 100
                          AND PERF_END_DATE IS NULL
                          AND BANK_ID != 'LCFGGBN002A0'
                          ORDER BY ENTITY_NAME ASC, MONTH_END_DATE DESC;"))
    
    returns <- returns %>%
      select(MANAGER, MGD_RETURN, BMK_RETURN) %>%
      group_by(MANAGER) %>%
      summarise(CumMGD = sprintf("%.2f",(prod(1+MGD_RETURN/100)-1)*100),
                CumBMK = sprintf("%.2f",(prod(1+BMK_RETURN/100)-1)*100)) %>%
      mutate(ACTIVE = as.numeric(CumMGD) - as.numeric(CumBMK)) %>%
      select(MANAGER, ACTIVE)
    
    results <- merge(barra,returns,by.x="PaceID",by.y="MANAGER", all.x=TRUE, all.y=FALSE)
    
    group <- switch(input$riskGroup,
                    Country ="/Country" , Industry = "/Industry", Style = "/Risk Indices")
    
    results <- results %>%
      filter(PARENT_NODE == group, Bucket %in% c(input$bucketselect)) %>%
      arrange(-ACT_EXP) %>% select(PaceID, ACT_EXP, ACTIVE, FACTOR, PARENT_NODE) %>%
      filter(ACTIVE != 'NA') %>% arrange(FACTOR) %>%
      select(ACT_EXP, ACTIVE, PaceID, FACTOR, PARENT_NODE)
    
    return(results)
    
  })
  
  barraFactors <- function(){
    
    Style <- qry("select distinct FACTOR from barraexposure
                 WHERE PARENT_NODE = '/Risk Indices'
                 ORDER BY FACTOR")
    Country <- qry("select distinct FACTOR from barraexposure 
                   WHERE PARENT_NODE = '/Country'
                   ORDER BY FACTOR")
    Industry <- qry("select distinct FACTOR from barraexposure 
                    WHERE PARENT_NODE = '/Industry'
                    ORDER BY FACTOR")
    
    result <- list(Style = Style, Country = Country, Industry = Industry)
    return(result)
    
  }
  
  
  observe({
    riskChoice <- input$riskGroup
    barraFactors <- barraFactors()
    
    updateSelectInput(session, "barraSel",
                      choices = barraFactors[[riskChoice]]
    )
    
  })
  
  output$plotBarra <- renderHighchart({
    input$barraButton
    
    factorSel  <- input$barraSel
    results <- barraData()
    
    results <- results[results$FACTOR == factorSel,]
    
    if(is.null(input$bucketselect)){return(highchart()%>%hc_title(text="Select a Bucket", align="left"))}
    if(nrow(results) < 2 ){return(highchart()%>%hc_title(text="No Manager Exposure", align="left"))}
    
    #create regression line
    model <- lm(ACTIVE ~ ACT_EXP,results)
    line <- data.frame(ACT_EXP = c( min(results$ACT_EXP),
                                    mean(results$ACT_EXP),
                                    max(results$ACT_EXP) ))
    line <- cbind( line, data.frame(predict(model, line)) )
    rsq <- summary(model)$r.squared
    
    hc <- highchart() %>%
      hc_xAxis(title=list(text="Exposure"),
               plotLines = list(
                 list(color = "#ccd1d1",
                      width = 2,
                      value = 0))) %>%
      hc_yAxis(title=list(text="Return"),
               plotLines = list(
                 list(color = "#ccd1d1",
                      width = 2,
                      value = 0))) %>%
      hc_add_series(data=list_parse2(line), type="spline", 
                    dashStyle = 'longdash',
                    marker = list(enabled=FALSE),
                    enableMouseTracking = FALSE,
                    name="Regression Line") %>%
      hc_legend(enabled=input$barraLegend) %>%
      hc_title(text=paste0(factorSel," Regression"), align="left") %>%
      hc_subtitle(text = paste0("R-squared:", round(rsq,4)), align="left") %>%
      hc_tooltip(useHTML = TRUE,
                 headerFormat = "<table>",
                 pointFormat = paste("<tr><th colspan=\"2\">{point.label}</th></tr>",
                                     "<tr><th>Exposure:</th><td>{point.x}</td></tr>",
                                     "<tr><th>Return:</th><td>{point.y}</td></tr>"),                                   
                 footerFormat = "</table>") %>%                  
      hc_exporting(enabled = TRUE, filename = "custom-file-name")
    
    for(x in 1:nrow(results)){
      
      ifelse(input$barraLabels,
             hc <- hc %>% hc_add_series_scatter(x=results[x,"ACT_EXP"], 
                                                y=results[x,"ACTIVE"],
                                                label=results[x,"PaceID"],
                                                showInLegend=TRUE,
                                                name=results[x,"PaceID"]) ,
             hc <- hc %>% hc_add_series_scatter(x=results[x,"ACT_EXP"], 
                                                y=results[x,"ACTIVE"],
                                                showInLegend=TRUE,
                                                name=results[x,"PaceID"])
      )
      
    }  
    
    if (input$theme != FALSE) {
      theme <- selectTheme(input$theme)
      hc <- hc %>% hc_add_theme(theme)
    }
    
    hc
    
    
  })
  
  output$downloadBarra <- downloadHandler(
    filename = function(){paste0("export_file_",timeStamp(),".csv")},
    content = function(file) {
      write.csv(barraData(), file,row.names=F)
    }
  )
  
  
}

shinyApp(ui = ui, server = server)