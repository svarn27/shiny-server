#Graph Data
rm(list=ls())

library(shiny)
library(highcharter)
library(dplyr)
library(DT)
library(shinyBS)
library(xtable)
library(reshape2)
#library(readxl)

####DASHBOARD REPORT
data("worldgeojson")

##table script on flush
{
  table_script <- "$('.tree').treegrid();
  $('.tree').treegrid('collapseAll');
  $('.treegrid-1').treegrid('expand');
  
  $(' .tree tbody td:nth-of-type(4)').each(function() {
  if (Number($(this).text()) < 0) {
  $(this).addClass('negRtn');
  } else if(Number($(this).text()) > 0){
  $(this).addClass('posRtn');
  } else {
  $(this).addClass('neuRtn');
  }
  });
  
  $(' .tree tbody td:nth-of-type(7)').each(function() {
  if (Number($(this).text()) < 0) {
  $(this).addClass('negRtn');
  } else if(Number($(this).text()) > 0){
  $(this).addClass('posRtn');
  } else {
  $(this).addClass('neuRtn');
  }
  });
  
  $('.factors td').click(function(){
  
  var bmk = $(this).closest('tr').children('td:first').text();
  var factor = $(this).closest('table').find('th').eq($(this).index() + 3).text();
  
  if(factor != 'Benchmark'){
  var send = [bmk,factor];
  
  Shiny.onInputChange('mydata', send);
  }
  });
  
  $('.tree td').click(function(){
  
  var descRow = $('td').filter(function() {
  return $(this).text() == ' US Small Cap ';
  }).closest('tr').index();
  
  var delcRow = $('td').filter(function() {
  return $(this).text() == ' US Large Cap Growth ';
  }).closest('tr').index();
  
  var dgeRow = $('td').filter(function() {
  return $(this).text() == ' Dedicated Global ';
  }).closest('tr').index();
  
  var emRow = $('td').filter(function() {
  return $(this).text() == ' Emerging Aggregate ex FM ';
  }).closest('tr').index();
  
  var thisRow = $(this).closest('tr').index();
  
  if(thisRow >= descRow){
  var aggregate = 'DESC';
  } else if(thisRow >= delcRow){
  var aggregate = 'DELC';
  } else if(thisRow >= dgeRow){
  var aggregate = 'DGE';
  } else if(thisRow >= emRow){
  var aggregate = 'EM';
  } else {
  var aggregate = 'DVP';
  }
  
  var portfolio = $(this).closest('tr').children('td:first').text();
  var horizon = $(this).parent().children().index($(this));
  
  if(horizon == 6){
  var horizon = 'Month to Date';
  } else if(horizon == 3){
  var horizon = 'One Day';
  } else{
  var horizon = '';
  }
  
  if(horizon != ''){
  var send = [portfolio,horizon,aggregate];
  
  Shiny.onInputChange('port_att', send);
  $('#attModal').modal('toggle');
  }
  });
  "
}

##functions
{
  convert_to_tree <- function(z){
    z <- as.data.frame(z)
    
    # z$Portfolio <- sub("All International Managers","Foreign Equity", z$Portfolio)
    # z$Portfolio <- sub("DE Total Composite","Domestic Equity", z$Portfolio)
    # z$Portfolio <- sub("Small Cap Aggregate","Developed SC Aggregate", z$Portfolio)
    # z$Portfolio <- sub("Developed Active Aggregate ex Small Cap Managers","Developed Std Active Aggregate", z$Portfolio)
    # z$Portfolio <- sub("ACUITAS AGG","US MicroCap", z$Portfolio)
    # z$Portfolio <- sub("Global Aggregate","Dedicated Global", z$Portfolio)
    z$DAY.Active <- z$DAY.MGD.Rtn - z$DAY.BMK.Rtn
    z$MktVal <- paste("$",format(round((z$MktVal / 5000000),0), big.mark=","),sep="")
    
    z <- z %>% select(Portfolio, DAY.BMK.Rtn, DAY.MGD.Rtn, DAY.Active, MTD.BMK.Rtn, MTD.MGD.Rtn, Total.Effect, MktVal)
    names(z) <- c("Portfolio", "Benchmark", "Managed", "Active","Benchmark", "Managed", "Active","NAV ($M)")
    
    y <- paste(capture.output(print(xtable(z), type="html"))[-c(1,2)],sep = "",collapse = "")
    x <- as.numeric(gregexpr("</tr>  <tr>", y)[[1]])
    
    yy <- 0
    for(i in 1:length(x)){
      
      if(z$Portfolio[i] %in% c("Total Fund")){
        y <- sub("</tr>  <tr>", paste0("</tr><tr class='treegrid-",i," aggs treegrid-expanded'>"), y)
        yy <- yy + 1
        
      } else if (z$Portfolio[i] %in% c("Aggregate 1", "Aggregate 2")){
        y <- sub("</tr>  <tr>", paste0("</tr><tr class='treegrid-",i," treegrid-parent-1 aggs2'>"), y)
        yy <- i
        yyy <- i
      } else if (grepl("Sub-Aggregate",z$Portfolio[i], ignore.case = T)){
        y <- sub("</tr>  <tr>", paste0("</tr><tr class='treegrid-",i," treegrid-parent-",yyy," aggs3'>"), y)
        yy <- i
      } else {
        y <- sub("</tr>  <tr>", paste0("</tr><tr class='treegrid-",i," treegrid-parent-",yy,"'>"), y)
      }
    }
    
    y <- sub("<table border=1>","<table width='100%' class='tree'><tr>
             <td colspan='1' width='25%' style='border: 0px'></td>
             <th colspan='3' width='32.5%' align='center'>DAY</th>
             <th colspan='3'  width='32.5%' align='center'>MTD</th>
             <td colspan='1'  width='10%' style='border: 0px'></th></tr>", y)
    
    return(y)
  }
}

options(DT.options = list(dom = 'tpl', rownames = FALSE, pageLength = 10),xtable.include.rownames=F)

#options(shiny.sanitize.errors = TRUE)

baseDir <- ifelse(as.character(.Platform$OS.type) == "windows",
                  "J:\\Misc\\Shane\\mqa\\",
                  "/srv/shiny-server/utilities/")

base_dirr <- ifelse(as.character(.Platform$OS.type) == "windows",
                    "J:/",
                    "/home/ge/data/reporting/")

source(paste0(baseDir,"query_function.R"))
#source(paste0(baseDir,"graph_functions.R"))

rptDate <- "2016-12-29" #as.Date(as.character(qry("select max(Date) from dailysector")))

time_horizons <- qry(paste0("select distinct horizon from dailysector where Date = '",rptDate,"'"))
quintile_time_horizons <- qry(paste0("select distinct horizon from dailyfactorquintiles where RepDate = '",rptDate,"'"))

#dates <- qry("select distinct date from dailysector order by Date DESC")

bmks <- qry(paste0("select distinct Benchmark from dailycountry 
                   WHERE Date='",rptDate,"'
                   AND Benchmark NOT LIKE 'Russell%'
                   AND Benchmark NOT LIKE '% EM %'
                   AND Benchmark NOT LIKE '%USA%'
                   ORDER BY Benchmark"))


# managers_map <- read_excel(paste0(base_dirr,"Report Program Development/archive/GE_Report_Program_Jan2017 - PACE.xlsm"),2)[,c("RMID","FACTSET_BATCHER")]
# morning_perf_map <- read.csv(paste0(base_dirr,"Misc/Shane/dashboard/morning_perf_map.csv"), stringsAsFactors=FALSE)
# morning_perf_map <- merge(morning_perf_map, managers_map, by="RMID", all.x=TRUE, all.y=FALSE)
# morning_perf_map <- morning_perf_map[!duplicated(morning_perf_map),]

####Get bmk attribution files
ui = fluidPage(tags$head(HTML(
  "<link rel='stylesheet'' href='http://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css'>
  <link rel='stylesheet' href='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/css/jquery.treegrid.css'>
  <script type='text/javascript' src='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/js/jquery.treegrid.js'></script>
  <script type='text/javascript' src='https://cdn.rawgit.com/maxazan/jquery-treegrid/master/js/jquery.treegrid.bootstrap3.js'></script>
  <script type='text/javascript' src='https://cdn.rawgit.com/DLarsen/jquery-hottie/blob/master/jquery.hottie.js'></script>
  <script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');
  
  ga('create', 'UA-88914395-1', 'auto');
  ga('send', 'pageview');
  
  Shiny.addCustomMessageHandler('jsCode', function(message) { 
  $('table.tree').replaceWith(message.perf_table); 
  eval(message.value); 
  });
  
  </script>
  <style>
  .modal-lg {width: 95%;}
  .tree table { 
  width: 90%; 
  border-collapse: collapse; 
  margin:50px auto;
  }
  .tree th { 
  background: #3498db; 
  color: white; 
  font-weight: bold; 
  }
  .tree td,.tree  th { 
  padding: 10px; 
  border: 1px solid black; 
  text-align: center; 
  font-size: 16px;
  }
  .tree  td:first-child,
  .tree th:first-child{
  text-align: left; 
  font-weight: bold;
  border: 1px solid black;
  }
  .aggs td, .aggs2 td, 
  .aggs3 td {
  font-weight:bold;
  border-bottom-style: double;
  }
  .aggs td:nth-of-type(1){
  background-color: #2cc16a;
  font-weight: bold;
  color: #fff;
  }
  .aggs2 td:nth-of-type(1){
  background-color: #fdbd0b;
  font-weight: bold;
  color: #fff;
  }
  .aggs3 td:nth-of-type(1){
  background-color: #9751f9;
  font-weight: bold;
  color: #fff;
  }
  .tree tbody td:nth-of-type(4),
  .tree thead th:nth-of-type(4),
  .tree tbody td:nth-of-type(7),
  .tree thead th:nth-of-type(7){
  background: #fdf9a5;
  }
  .posRtn{
  color:#4bbe0e; font-weight: bold;
  }
  .negRtn{
  color:red; font-weight: bold;
  }
  .neuRtn{
  color:black; font-weight: bold;
  }
  
  .tree tbody td:nth-of-type(4):hover {color:#b078c7;}
  .tree tbody td:nth-of-type(7):hover {color:#b078c7;}
  
  .factors {width:90%; text-align:center;}
  .factors th{width:10%; text-align:center;}
  .factors th:nth-child(1){width:15%;}
  .factors td{text-align:center;}
  .factors td:hover {font-weight: bold;}
  </style>
  <meta http-equiv='Pragma' content='no-cache'>
  <meta http-equiv='Expires' content='-1'>
  <meta http-equiv='CACHE-CONTROL' content='NO-CACHE'>
  "
)
),
tags$head(tags$script(HTML(''))),
titlePanel("Daily Performance"),
fluidRow(
  column(width=3,selectInput("rptDate", "Report Date as of Day End:", "2016-12-29")),
  column(width=3,selectInput("horizon", "Report Horizon:", time_horizons))
),
tabsetPanel(
  tabPanel("Portfolio Performance", highchartOutput("plot_port_perf"), HTML("<table class='tree'></table>"),
           HTML("Click 'Active' Column for Manager Attribution")),
  tabPanel("Benchmark Performance",
           fluidRow(highchartOutput("plot_all_benchmarks", height = "400")),
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
             column(width=2,radioButtons("country_map_bar", "Map or Bar Graph?", c("Map", "Bar"), inline=TRUE)),
             conditionalPanel( condition= "input.country_map_bar == 'Bar'",
                               column(width=2,radioButtons("country_graph", "Bar Graph:", c("Return", "Contribution"), inline=TRUE))
             )
           ),
           fluidRow(
             #column(width=2, highchartOutput("plot_bmk_total", height="400", width="100%")),
             column(width=12,
                    conditionalPanel( condition= "input.country_map_bar == 'Bar'",
                                      highchartOutput("plot_country_bar", height = "400", width="95%")
                    ),
                    conditionalPanel( condition= "input.country_map_bar == 'Map'",
                                      highchartOutput("plot_country_map", height = "400", width="95%")
                    )
             )
           ),
           fluidRow(
             column(width=6,highchartOutput("plot_country_wts", height = "400", width="95%")),
             column(width=6,DT::dataTableOutput("country_table"))
           )
  ),
  tabPanel("Factor Performance",
           selectInput("factor_horizon","Select Horizon:", quintile_time_horizons),
           div(align='center',
               htmlOutput("table1"), br(),
               htmlOutput("table2"), br(),
               htmlOutput("table3")
           ),
           HTML("*Factor Returns are Q5 minus the Benchmark Return."),
           highchartOutput("quintile_graph")
  )
),
bsModal("sectorModal", "Trailing Sector Returns", "irButton", size="large", highchartOutput("plot_sector_trailing")),
bsModal("countryModal", "Trailing Country Returns", "irButton", size="large", highchartOutput("plot_country_trailing")),
bsModal("bmkModal", "Trailing Benchmark Returns", "irButton", size="large", fluidRow()),
bsModal("attModal", "Portfolio Attribution", "irButton", size="large", 
        radioButtons("att_type",NULL,c("Sector", "Country", "Company"), select="Sector", inline=TRUE),
        downloadLink('downloadData', 'Download'),
        div(dataTableOutput("att_table"), style='font-size:90%')),
HTML("
     <script type='text/javascript'>
     $(document).ready(function() {
     
     
     });
     </script>")
)


server <- function(input,output, session){
  
  morning_perf_data <- reactive({
    
    rpt_date <- input$rptDate
    rpt_date <- '2016-12-29'
    
    morning_perf <- qry(paste0("select * from morningperf where date='",rpt_date,"' AND Report = 'FE'"))
    
    morning_perf[morning_perf$Portfolio == "Total GE", "Portfolio"] <- "Total Fund"
    morning_perf[morning_perf$Portfolio == "All International Managers", "Portfolio"] <- "Aggregate 1"
    morning_perf[morning_perf$Portfolio == "Global Aggregate", "Portfolio"] <- "Aggregate 2"
    morning_perf[morning_perf$Portfolio %in% c("Developed Active Aggregate ex Small Cap Managers",
                  "Small Cap Aggregate", "Emerging Aggregate ex FM"), "Portfolio"] <- "Sub-Aggregate"
    
    to_replace <- morning_perf[!(morning_perf$Portfolio %in% c("Total Fund","Aggregate 1","Aggregate 2", "Sub-Aggregate")),"Portfolio"]
    
    i <- 1
    for(x in to_replace){
      
      morning_perf[morning_perf$Portfolio == x, "Portfolio"] <- paste0("Portfolio ",i)
      
      i <- i + 1
    }

    return(morning_perf)
  })
  
  observe({
    
    morning_perf <- morning_perf_data()
    morning_perf <- convert_to_tree(morning_perf)
    morning_perf <- HTML(morning_perf)
    
    session$sendCustomMessage(type='jsCode', list(value = table_script, perf_table = morning_perf))
  })
  
  morning_chart <- reactive({
    
    morning_perf <- morning_perf_data()
    
    ###get and clean morning performance
    morning_overview <- morning_perf %>% 
      filter(Portfolio %in% c("Total Fund", "Aggregate 1", "Aggregate 2")) %>%
      mutate(DAY.Active = DAY.MGD.Rtn - DAY.BMK.Rtn) %>%
      select(Portfolio, DAY.BMK.Rtn, DAY.MGD.Rtn, DAY.Active, MTD.BMK.Rtn, MTD.MGD.Rtn,  MTD.Active = Total.Effect) %>%
      melt(id.vars=c("Portfolio"))
    
    #Color of Graphs
    morning_overview$Horizon <- NA
    morning_overview[grep("MTD", morning_overview$variable),"Horizon"] <- "MTD"
    morning_overview[grep("DAY", morning_overview$variable),"Horizon"] <- "DAY"
    
    #Color of Graphs
    morning_overview$Return <- NA
    morning_overview[grep("Active", morning_overview$variable),"Return"] <- "Active"
    morning_overview[grep("MGD", morning_overview$variable),"Return"] <- "Managed"
    morning_overview[grep("BMK", morning_overview$variable),"Return"] <- "Benchmark"
    
    # morning_overview$Portfolio <- sub("All International Managers","Foreign Equity", morning_overview$Portfolio)
    # morning_overview$Portfolio <- sub("DE Total Composite","Domestic Equity", morning_overview$Portfolio)
    # morning_overview$Portfolio <- sub("Global Aggregate","Dedicated Global", morning_overview$Portfolio)
    
    return(morning_overview)
    
  })
  
  values <- reactiveValues()
  values[['horizon']] <- "DAY"
  observe({
    graph_title <- input$perf_graph_title
    
    ifelse(grepl("DAY", graph_title),
           values[['horizon']] <- "MTD",
           values[['horizon']] <- "DAY")
    
  })
  
  #####Get the Necessary Data from db (tables dailysector, dailycountry)
  quintile_data <- reactive({
    
    rep_date <- input$rptDate
    
    quintile_data <-qry(paste0("select * from dailyfactorquintiles WHERE RepDate = '",rep_date,"'
                               And Benchmark in ('MSCI EM','MSCI ACWI','MSCI EM (EM) SC', 'MSCI World ex US',
                               'Russell 1000', 'Russell 2000', 'MSCI World ex US SC')"))
    
  })
  sector_data <- reactive({
    
    rep_date <- input$rptDate
    
    sector_data <- qry(paste0("Select * from dailysector
                              Where Date = '",rep_date,"'
                              AND Sector != 'NA'
                              AND Benchmark NOT LIKE '%USA%'
                              order by Benchmark"))
    
    names(sector_data) <- c("Grouping","Return","Weight","Contribution","Horizon","Benchmark","Date")
    
    return(sector_data)                                                
  })
  country_data <- reactive({
    
    rep_date <- "2016-12-29"# input$rptDate
    
    country_data <- qry(paste0("Select * from dailycountry
                               Where Date = '",rep_date,"'
                               AND Country != 'NA'
                               And Benchmark NOT LIKE 'Russell%'
                               order by Benchmark"))
    
    names(country_data) <- c("Grouping","Return","Weight","Contribution","Horizon","Benchmark","Date")
    
    return(country_data)
  })
  
  ###Portfolio Performance
  morning_rpt <- reactive({
    
    morning_perf <- morning_perf_data()
    
    morning_rpt <- morning_perf[morning_perf$Report == input$de_fe,]
    morning_rpt[,!(names(morning_rpt) %in% c("Date", "Report"))]
  })
  output$morning_perf = renderDataTable(
    morning_rpt(), options = list(ordering=F, paging=F),rownames=F
  )
  
  output$plot_port_perf <- renderHighchart({
    
    morning_overview <- morning_chart()
    
    horizon <- values[['horizon']]
    
    morning_overview <- morning_overview %>% filter(Horizon == horizon) %>%
      hchart("column", hcaes(x = Portfolio, y = value, group = Return)) %>%
      hc_title(text=paste0("Global Equity Overview, ",horizon),style=list(fontSize='24px')) %>%
      hc_xAxis(title=list(text=NULL),labels=list(style=list(fontSize='18px'))) %>%
      hc_yAxis(title=list(text="Return")) %>%
      hc_plotOptions(
        series = list(dataLabels = list(enabled = TRUE,
                                        formatter = JS("function(){
                                                       return Highcharts.numberFormat(this.y, 2);
  }")))) %>%
      hc_exporting(enabled=TRUE,buttons=list(contextButton=list(symbol="circle", 
                                                                onclick=JS("function(e){
                                                                           send = this.options.title.text;
                                                                           Shiny.onInputChange('perf_graph_title', send);  
}"))))
  })
  
  # mtd_att <- reactive({
  #   rpt_date <- as.Date(input$rptDate)
  #   att_file_name <- paste0(base_dirr,"Misc/Shane/dashboard/attribution/MTD_ATT_",format(rpt_date,"%Y%m%d"),".csv")
  #   print(att_file_name)
  #   att <- read.csv(att_file_name, stringsAsFactors=FALSE) 
  #   return(att)
  # })
  # day_att <- reactive({
  #   rpt_date <- as.Date(input$rptDate)
  #   att_file_name <- paste0(base_dirr,"Misc/Shane/dashboard/attribution/DAY_ATT_",format(rpt_date,"%Y%m%d"),".csv")
  #   print(att_file_name)
  #   att <- read.csv(att_file_name, stringsAsFactors=FALSE)
  #   return(att)
  # })
  
  
  # get_total_effect <- function(input_manager, time_horizon, agg){
  #   
  #   #     file_path <- ifelse(as.character(.Platform$OS.type) == "windows",
  #   #                         "J:/Misc/Shane/dashboard/",
  #   #                         "/home/ge/data/reporting/Misc/Shane/dashboard/")
  #   
  #   input_manager <- trimws(input_manager)
  #   time_horizon <- trimws(time_horizon)
  #   aggregate <- trimws(agg)
  #   
  #   #         input_manager <- "Artisan"
  #   #         time_horizon <- "Month to Date"
  #   #     aggregate <- "DVP"
  #   
  #   mtd_att <- mtd_att()
  #   day_att <- day_att()
  #   
  #   base_dir2 <- paste0(base_dirr,"Misc/Shane/dashboard/")
  #   batcher_dir <- paste0(base_dirr,"Factset/Monthly/201702_ALL/")
  #   batcher_files <- list.files(batcher_dir)
  #   date_range <- "01312017_02282017"
  #   
  #   bmk_returns <- switch(time_horizon,
  #                         "Month to Date" = mtd_att,
  #                         "One Day" = day_att
  #   )
  #   
  #   manager_bmk_factset_name <- morning_perf_map %>% 
  #     filter(Portfolio == input_manager) %>% 
  #     select(FACTSET_BATCHER, Bmk_ID)
  #   
  #   input_manager <- as.character(manager_bmk_factset_name$FACTSET_BATCHER)
  #   man_bmk <- as.character(manager_bmk_factset_name$Bmk_ID)
  #   
  #   wrk_files <- batcher_files[grep(input_manager, batcher_files)]
  #   file_name <- wrk_files[grep(paste0("NAME.*",date_range),wrk_files)]
  #   
  #   man_holdings <- paste0(batcher_dir, file_name)
  #   
  #   if(file.exists(man_holdings)){
  #     man_holdings <- read_excel(man_holdings,1)  
  #   } else {
  #     man_holdings <- NULL
  #   }
  #   
  #   validate(
  #     need(!is.null(man_holdings), "Attribution Data not available")
  #   )
  #   
  #   #Note column 11 is the ending weight column in the name attribution file
  #   man_holdings <- man_holdings[11:nrow(man_holdings),c(2,11)] 
  #   names(man_holdings) <- c("Company", "Mgd.Wt")
  #   man_holdings <- man_holdings[!is.na(man_holdings$Mgd.Wt),]
  #   
  #   col_names <- c("Bmk.Wt", "Return", "Sector","Country")
  #   att_data <- bmk_returns[,c("Company.Name",man_bmk, "BMK_RETURN",
  #                              "MSCI_SECTOR_TEST", "MSCI_COUNTRY")]
  #   names(att_data) <- c("Company", col_names)
  #   att_data <- att_data[att_data$Bmk.Wt > 0,]
  #   att_data <- att_data[!is.na(att_data$Company),]
  #   
  #   ge_returns <- bmk_returns[,c("Company.Name","Weight", "BMK_RETURN",
  #                                "MSCI_SECTOR_TEST", "MSCI_COUNTRY")]
  #   names(ge_returns) <- c("Company", col_names)
  #   ge_returns <- ge_returns[!is.na(ge_returns$Company),]
  #   
  #   attribution <- merge(man_holdings, att_data, by="Company", all.x=TRUE, all.y=FALSE)
  #   
  #   for(x in which(is.na(attribution$Bmk.Wt))){
  #     sec_name <- attribution[x,"Company"]
  #     replacement <- ge_returns[ge_returns$Company == sec_name,c("Return", "Sector", "Country")]
  #     
  #     if(nrow(replacement) > 0 ){
  #       attribution[x,c("Return", "Sector", "Country")] <- ge_returns[ge_returns$Company == sec_name,c("Return", "Sector", "Country")]
  #     }
  #   }
  #   
  #   attribution[,c("Mgd.Wt", "Bmk.Wt", "Return")] <- apply(attribution[,c("Mgd.Wt", "Bmk.Wt", "Return")],
  #                                                          2, function(x){as.numeric(as.character(x))})
  #   
  #   attribution[which(is.na(attribution$Bmk.Wt)),"Bmk.Wt"] <- 0
  #   attribution[attribution$Company == "[Cash]", "Return"] <- 0
  #   
  #   attribution <- attribution %>%
  #     filter(!is.na(Return)) %>%
  #     mutate(Act.Wt = Mgd.Wt - Bmk.Wt,
  #            Total.Effect = (Act.Wt * Return)/100)
  #   
  #   attribution[attribution$Company == "[Cash]", c("Sector", "Country")] <- "[Cash]"
  #   
  #   return(attribution)
  #   
  # }
  # att_data <- reactive({
  #   
  #   att_portfolio <- input$port_att[1] 
  #   att_horizon <- input$port_att[2]
  #   att_type <- input$att_type
  #   agg <- input$port_att[3]
  #   
  #   y <- get_total_effect(att_portfolio,att_horizon,agg)
  #   
  #   z <- y
  #   names(y)[1] <- "Grouping"
  #   
  #   
  #   z[,1] <- z[,att_type] 
  #   names(z)[1] <- "Grouping"
  #   
  #   z[is.na(z)] <- 0
  #   
  #   z <- z %>%  mutate(Tot.Bmk.Ret = sum(Bmk.Wt*Return)/100) %>%
  #     group_by(Grouping) %>%
  #     summarise(Mgd.Rtn = sum(Mgd.Wt/sum(Mgd.Wt) * Return, na.rm = TRUE),
  #               Mgd.Wt = sum(Mgd.Wt),        
  #               Bmk.Rtn = sum(Bmk.Wt/sum(Bmk.Wt) * Return, na.rm = TRUE),
  #               Bmk.Wt = sum(Bmk.Wt),
  #               Act.Wt = sum(Act.Wt),
  #               Act.Rtn = Mgd.Rtn - Bmk.Rtn,
  #               Selection = (Bmk.Wt * (Mgd.Rtn - Bmk.Rtn))/100,
  #               Allocation = (Act.Wt * (Bmk.Rtn - mean(Tot.Bmk.Ret)))/100,
  #               Interaction = (Act.Wt * Act.Rtn)/100,
  #               Country = unique(Country)[1],
  #               Sector = unique(Sector)[1]) %>%
  #     select(Grouping, Mgd.Wt, Mgd.Rtn, Bmk.Wt, Bmk.Rtn, Act.Wt, Act.Rtn, 
  #            Allocation, Selection, Interaction)
  #   
  #   z[is.na(z)] <- 0
  #   
  #   z <- z %>% group_by(Grouping) %>% 
  #     mutate(Total.Effect = sum(Selection+Interaction+Allocation)) %>%
  #     arrange(Total.Effect)
  #   z[,2:ncol(z)] <- apply(z[,2:ncol(z)], 2, function(x){round(x,2)})
  #   
  #   if(att_type == "Company"){
  #     z <- left_join(z, y[,c("Grouping", "Country", "Sector")])
  #     z <- z %>% select(Grouping, Mgd.Wt, Mgd.Rtn, Bmk.Wt, Bmk.Rtn, Act.Wt, Act.Rtn, 
  #                       Allocation, Selection, Interaction, Total.Effect, Country, Sector)
  #     
  #   }
  #   
  #   z[z$Grouping == '[Cash]', c("Selection", "Interaction", "Allocation", "Total.Effect")] <- 0
  #   names(z)[1] <- att_type
  #   
  #   return(z)
  #   
  # })
  # output$att_table <- renderDataTable(
  #   att_data(), options = list(pageLength = 10, dom='tp'), rownames=FALSE
  # )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('attribution_data_', gsub("[^0-9]","",Sys.time()), '.csv', sep='')
    },
    content = function(con) {
      write.csv(att_data(), con)
    }
  )
  
  #####Benchmark Performance #####
  output$plot_all_benchmarks <- renderHighchart({
    
    sector_data <- sector_data()
    
    data <- sector_data[sector_data$Grouping == "Bmk Total" & sector_data$Horizon == input$horizon,]
    
    hc <- hchart(data, "column", hcaes(x= Benchmark, y= Return )) %>%
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
    
    hchart(data, hcaes(x=bmk, y=Return, group = style), type="bar") %>%
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
    
    data <- data[data$Benchmark %in% c("MSCI World ex US",
                                       "MSCI World ex US Small Cap",
                                       "MSCI Emerging Markets",
                                       "MSCI EM (Emerging Markets) Small Cap",
                                       "Russell 1000",
                                       "Russell 2000"),]
    
    data$market <- NA
    data[grep("Emerging", data$Benchmark),"market"] <- "Emerging"
    data[grep("World", data$Benchmark),"market"] <- "Developed"
    data[grep("Russell", data$Benchmark),"market"] <- "Domestic"
    
    data$size <- NA
    data[grep("Small|1000", data$Benchmark),"size"] <- "Large Cap"
    data[is.na(data$size),"size"] <- "Small Cap"
    
    
    hchart(data, hcaes(x=market,group=size,y=Return),type="bar" ) %>%
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
    horizon <- input$horizon
    
    data <- sector_data[sector_data$Horizon == horizon,]
    
    hc <- hchart(data, "heatmap", hcaes(x = Benchmark, y= Grouping, value=Return, color=value)) %>%
      hc_title(text=paste0("Sector Returns - ",horizon)) %>%
      hc_xAxis(title=list(text=NULL)) %>%
      hc_yAxis(reverse=TRUE, title=list(text=NULL)) %>%
      hc_tooltip(formatter=JS(paste0(
        "function () {
        return '<b>' + this.series.yAxis.categories[this.point.y] + '</b><br>'
        + '<b>' + this.series.xAxis.categories[this.point.x] + ':</b> ' +
        Highcharts.numberFormat(this.point.value,2) + ' ", horizon ,"'}"))) %>% 
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
    
    hc <- hchart(data, "column", hcaes(x = Benchmark, y = Contribution, group= Grouping)) %>%
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
              
              $('#sectorModal').modal('toggle');
              
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
    
    hc <- hchart(data, "column", hcaes(x = Benchmark, y = Weight, group= Grouping)) %>%
      hc_title(text=paste0("Sector Weights")) %>%
      hc_plotOptions(series = list(stacking = "percent",
                                   pointPadding = 0,
                                   groupPadding = 0,
                                   borderWidth = 0,
                                   shadow = FALSE),
                     area = list(marker=list(enabled=FALSE))) %>%
      hc_legend(enabled=FALSE) %>%
      hc_xAxis(title=list(text=NULL))
    
    return(hc)
    
  })
  output$plot_sector_trailing <- renderHighchart({
    
    sector <- input$sector_trailing[1]
    bmk <- input$sector_trailing[2]
    
    data <- qry(paste0("Select * from dailysector 
                       where Sector = '",sector,"'
                       and Benchmark = '",bmk,"'
                       and Horizon = '",input$horizon,"'"))
    
    hchart(data, hcaes(x=Date, y=Return, type=ifelse(input$horizon == "DAY", "column", "spline"))) %>%
      hc_title(text=ifelse(input$horizon == "DAY", 
                           paste0(sector," Trailing Returns - Daily"), 
                           paste0(sector," Trailing Return - Cumulative, ",input$horizon))) %>%
      hc_subtitle(text=bmk)
    
  })
  
  #####Country Performance ######
  country_table_data <- reactive({
    
    country_data <- country_data() 
    
    country_data <- country_data[country_data$Horizon == input$horizon & country_data$Benchmark == input$bmk,
                                 names(country_data) %in% c("Grouping", "Weight", "Return", "Contribution")]
    
    names(country_data) <- c("Country", "Return", "Weight", "Contribution")
    
    country_data[,c("Return", "Weight", "Contribution")] <- apply(country_data[,c("Return", "Weight", "Contribution")],2,
                                                                  function(x){round(x,2)})
    
    return(country_data)
    
  })
  output$country_table = DT::renderDataTable(country_table_data(),rownames=FALSE)
  output$plot_country_bar <- renderHighchart({
    
    country_data <- country_data()
    
    data <- country_data[country_data$Horizon == input$horizon & 
                           country_data$Grouping != "Bmk Total" &
                           country_data$Benchmark == input$bmk, ]
    data <- data[data$Weight > .01 | data$Weight < -.01,  ]
    
    ifelse(input$country_graph == "Contribution",
           hc <- hchart(data, "column", hcaes(x = Grouping, y = Contribution), name = "Contribution"),
           hc <- hchart(data, "column", hcaes(x = Grouping, y = Return), name = "Return"))
    
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
                color = this.color;
                send = [country,color];
                
                Shiny.onInputChange('country_trailing', send);     
                
                $('#countryModal').modal('toggle')
                
  }"
                    )
              )
              )
              )
              )
    
  })
  output$plot_country_map <- renderHighchart({
    
    country_data <- country_data()
    
    data <- country_data[country_data$Horizon == input$horizon & 
                           country_data$Benchmark == input$bmk, ]
    data[data$Grouping == "United States","Grouping"] <- "United States of America"
    #data <- data[data$Weight > .01 | data$Weight < -.01,  ]
    
    colstops <- data.frame(
      q = c(0,0.5,.9),
      q = c('#fd310b','#fcfd0b', '#2ecc71')) %>%
      list_parse2()
    
    highchart() %>%
      hc_add_series_map(worldgeojson, data, name = "Return",
                        value = "Return", joinBy = c("name", "Grouping"),
                        dataLabels = list(enabled = FALSE)) %>%
      hc_colorAxis(stops=colstops, 
                   min=-(max(abs(data$Return))*.5), 
                   max=(max(abs(data$Return))*.5)  ) %>%
      hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_title(text=paste0("Country Returns, ",input$horizon))
    
  })
  output$plot_country_wts <- renderHighchart({
    
    country_data <- country_data()
    
    data <- country_data[country_data$Horizon == input$horizon & 
                           country_data$Grouping != "Bmk Total" &
                           country_data$Benchmark == input$bmk, ]
    data <- data[data$Weight > .1 | data$Weight < -.1,  ]
    
    hc <- hchart(data, type= "pie", hcaes(y=Weight,name=Grouping)) %>%
      hc_title(text="Country Weights")
    
    return(hc)
    
  })
  output$plot_country_trailing <- renderHighchart({
    
    country <- input$country_trailing[1]
    countryColor <- input$country_trailing[2]
    
    data <- qry(paste0("Select * from dailycountry 
                       where country = '",country,"'
                       and Benchmark = '",input$bmk,"'
                       and Horizon = '",input$horizon,"'"))
    
    hchart(data, hcaes(x=Date, y=Return), type=ifelse(input$horizon == "DAY", "column", "spline")) %>%
      hc_plotOptions(series=list(color=countryColor)) %>%
      hc_title(text=ifelse(input$horizon == "DAY", 
                           paste0(country," Trailing Returns - Daily"), 
                           paste0(country," Trailing Return - Cumulative, ",input$horizon))) 
    
  })
  
  ####Factor Performance
  quintile_tbl <- reactive({
    
    quintile_data <- quintile_data()
    
    quintile_tbl <- quintile_data %>% 
      filter(Quintile == 5) %>%
      select(Benchmark, Horizon, Factor, Return, BmkRtn)
    
    #     quintile_tbl$BenchmarkRtn <- apply(quintile_tbl, 1, function(x){bmk_rtns[bmk_rtns$Benchmark == x["Benchmark"] & 
    #                                                                                bmk_rtns$Horizon == x["Horizon"],"Return"]})
    quintile_tbl$FactorRtn <- round(quintile_tbl$Return - quintile_tbl$BmkRtn,4) 
    
    quintile_tbl <- quintile_tbl %>%
      filter(Horizon == input$factor_horizon) %>%
      select(Benchmark, Factor, FactorRtn) 
  })
  test <- reactive({ 
    
    quintile_tbl <- quintile_tbl()
    test <- dcast(quintile_tbl, Benchmark ~ Factor, value.var="FactorRtn")
    
    return(test)
    
  })
  output$table1 <- renderUI({
    
    test <- test()
    
    table1 <- test[, c("Benchmark", "Book.toPrice", "Earnings.Yield", "Sales.toPrice", "Cashflow.Yield", "EBITDA.toPrice", "Dividend.Yield", "Market.Cap")]
    table1 <- paste(capture.output(print(xtable(table1), type="html", align="center"))[-c(1,2)],sep = "",collapse = "")
    table1 <- sub("<table border=1>","<table class='factors' border=1><tr><th colspan='1' style='border: 0px'></th><th colspan='6' bgcolor='#a3c0f3'>Value</th>
                  <th colspan='1' bgcolor='#e78282'>Size</th></tr>",table1)
    
    return(HTML(table1))
  })
  output$table2 <- renderUI({
    
    test <- test()
    
    table2 <- test %>% select(Benchmark, LT..EPSGrowth, LT..SalesGrowth, Est.LT.EPSGrowth, PEG.FY1Est, ST.MOM, LT.MOM)
    table2 <- paste(capture.output(print(xtable(table2), type="html", align="center"))[-c(1,2)],sep = "",collapse = "")
    table2 <- sub("<table border=1>","<table class='factors' border=1><tr><th colspan='1' style='border: 0px'></th><th colspan='4' bgcolor='#7ad393'>Growth</th>
                  <th colspan='2' bgcolor='#b6b6b6'>Momentum</th></tr>",table2)
    
    return(HTML(table2))
  })
  output$table3 <- renderUI({
    
    test <- test()
    
    table3 <- test[,c("Benchmark", "LT.Debt.Cap", "Op.Margin", "Net.Margin", "ROE", "ROA", "Foreign.Sales", "1YR.VOL", "2YR.VOL")]
    table3 <- paste(capture.output(print(xtable(table3), type="html", align="center"))[-c(1,2)],sep = "",collapse = "")
    table3 <- sub("<table border=1>","<table class='factors'border=1><tr><th colspan='1' style='border: 0px'></th><th colspan='6' bgcolor='#f0f3a3'>Quality</th>
                  <th colspan='2' bgcolor='#f595df'>Volatility</th></tr>",table3)
    
    return(HTML(table3))
  })
  output$quintile_graph <- renderHighchart({
    
    validate(
      need(input$mydata != "", "Click a single Factor Return to see Returns across Quintiles.")
    )
    
    bmk <- trimws(input$mydata[1])
    factor <- trimws(input$mydata[2])
    horizon <- input$factor_horizon
    
    quintile_date <- qry("select max(RepDate) from dailyfactorquintiles")
    
    graph <- qry(paste0("select * from dailyfactorquintiles
                        WHERE Benchmark = '",bmk,"'
                        and Factor = '",factor,"'
                        and RepDate = '",input$repDate,"' 
                        and Horizon = '",horizon,"'"))
    
    hchart(graph, "column", hcaes(x=Quintile, y=Return, name="Return")) %>%
      hc_yAxis(plotLines=list(list(value=0, color="grey", width=2))) %>%
      # hc_add_series_df(data=graph, "column", x=Quintile, y=WtdAvg, name="Value", yaxis=2) %>%
      hc_title(text=paste0(factor," Quintile Returns")) %>%
      hc_subtitle(text=paste0(bmk," - ", horizon)) %>%
      hc_add_series_df(data=graph, type="line",x=c(1:5), y=BmkRtn, name="Benchmark Return", showInLegend=TRUE) %>%
      hc_plotOptions(
        series = list(
          marker = list(
            enabled = FALSE
          )
        )
      )
    
    
  })  
  }

shinyApp(ui=ui,server=server)