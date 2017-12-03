library(httr)
library(shiny)
library(jsonlite)
library(dplyr)

rm(list=ls())

options(shiny.trace=TRUE)
set_config(config(ssl_verifypeer = 0L))

baseDir <- "/srv/shiny-server/utilities/"
source(paste0(baseDir,"query_function.R"))

##css
{
  css <-
  '<style>
  body {
  background: white !important;
  }
  html, body, table {
  font-family: \'Ubuntu\', sans-serif !important;
  }
  .catalog thead th {
  background-color: black;
  border: none;
  color: white;
  padding: 15px;
  font: 15px;
  font-weight: bold;
  }
  .catalog thead th:first-child {
  border-radius: 10px 0 0 0;
  }
  .catalog thead th:last-child {
  border-radius: 0 10px 0 0;
  }
  .catalog tbody tr:last-child td:first-child {
  border-radius: 0 0 0 10px;
  }
  .catalog tbody tr:last-child td:last-child {
  border-radius: 0 0 10px 0;
  }
  .catalog tr:nth-child(even) {background: #DDD}
  .catalog tr:nth-child(odd) {background: #FFF}
  .catalog table {text-align:center;}
  
  table {width:95%;
  table-layout: fixed;
  margin-left:2.5%}
  th, td {
  padding: 5px;
  text-align: center;
  }
  
  /* The switch - the box around the slider */
  .switch {
  position: relative;
  display: inline-block;
  width: 40px;
  height: 24px;
  }
  
  /* Hide default HTML checkbox */
  .switch input {display:none;}
  
  /* The slider */
  .slider {
  position: absolute;
  cursor: pointer;
  top: 0;
  left: 0;
  right: 0;
  bottom: 0;
  background-color: #ccc;
  -webkit-transition: .4s;
  transition: .4s;
  }
  
  .slider:before {
  position: absolute;
  content: "";
  height: 16px;
  width: 16px;
  left: 4px;
  bottom: 4px;
  background-color: white;
  -webkit-transition: .4s;
  transition: .4s;
  }
  
  input:checked + .slider {
  background-color: #2196F3;
  }
  
  input:focus + .slider {
  box-shadow: 0 0 1px #2196F3;
  }
  
  input:checked + .slider:before {
  -webkit-transform: translateX(16px);
  -ms-transform: translateX(16px);
  transform: translateX(16px);
  }
  
  /* Rounded sliders */
  .slider.round {
  border-radius: 34px;
  }
  
  .slider.round:before {
  border-radius: 50%;
  }
  #myInput {
  
  width: 100%; /* Full-width */
  font-size: 16px; /* Increase font-size */
  padding: 12px 20px 12px 40px; /* Add some padding */
  border: 1px solid #ddd; /* Add a grey border */
  margin-bottom: 12px; /* Add some space below the input */
  }
  
  #completed_orders td:nth-child(1):hover{
  
  font-weight:bold;
  text-decoration:underline;
  cursor: pointer;
  
  }
  #myModal .modal-header, h4, .close {
      background-color: #5cb85c;
      color:white !important;
      text-align: center;
      font-size: 30px;
  }
  #myModal .modal-footer {
      background-color: #f9f9f9;
  }
  </style>'
  }

##html##
{
  quant_input2 <- function(id){
    paste0("<div class='input-group'>
           <span class='input-group-btn'>
           <button type='button' class='btn btn-danger btn-number'  data-type='minus' data-field='",id,"'>
           <span class='glyphicon glyphicon-minus'></span>
           </button>
           </span>
           <input type='text' id='",id,"' name='",id,"' class='form-control input-number' value='1' min='1' max='100'>
           <span class='input-group-btn'>
           <button type='button' class='btn btn-success btn-number' data-type='plus' data-field='",id,"'>
           <span class='glyphicon glyphicon-plus'></span>
           </button>
           </span>
           </div>")}
  
  input_w_button <- function(inputId, label, placeHolder, buttonId, glyphicon){
    btn <- paste0("<label class='control-label' for='",inputId,"'>",label,"</label>
                  <div class='input-group' width='10%'>
                  
                  <input id='",inputId,"' type='text' class='form-control' placeholder='",placeHolder,"'>
                  <span class='input-group-btn'>
                  <button class='btn btn-secondary action-button' id='",buttonId,"' type='button'>
                  <span class='glyphicon glyphicon-",glyphicon,"'></span> </button>
                  </span>
                  </div>")
    
    return(HTML(btn))
  }
  
  input_w_button2 <- function(inputId, label, value, buttonId, glyphicon){
    btn <- paste0("<label class='control-label' for='",inputId,"'>",label,"</label>
                  <div class='input-group' width='10%'>
                  
                  <input id='",inputId,"' type='number' class='form-control' value='",value,"'>
                  <span class='input-group-btn'>
                  <button class='btn btn-secondary action-button' id='",buttonId,"' type='button'>
                  <span class='glyphicon glyphicon-",glyphicon,"'></span> </button>
                  </span>
                  </div>")
    
    return(HTML(btn))
  }
  
  dropdownInput <- function(input_names){
    
    input_list <- sapply(input_names, function(i){paste0("<li width = '50%'>", 
                                                         input_w_button2(paste0(i,"_input"),
                                                                         paste0(i," Amount:"),
                                                                         0,
                                                                         paste0(i,"_full"),"plus"),
                                                         "</li>")}) 
    
    input_list <- paste0(input_list, collapse="")
    
    input_list <- paste0("<div class='dropdown' width='100%'>
                         <label>Pmt Type:</label>
                         <button class='btn btn-default dropdown-toggle' type='button' data-toggle='dropdown'>Click for Pmts
                         <span class='caret'></span></button>
                         <ul class='dropdown-menu'><form>",
                         input_list,
                         "</form></ul>
                         </div>")
    input_list <- gsub("\n","",input_list)
    input_list <- gsub("\"0\"/","0",input_list)
    return(HTML(input_list))
    
  }

}

##javascript
{
  on_load <-
  "<script>
  $(document).ready(function(e){

    setTimeout(function() {
      var message =  Math.random();
      Shiny.onInputChange('page_loaded', message);
    },10)

  })
  
  function prodSearch() {
    // Declare variables 
    var input, filter, table, tr, td, i;
    input = document.getElementById('myInput');
    filter = input.value.toUpperCase();
    table = document.getElementById('merchandise');
    tr = table.getElementsByTagName('tr');
    
    // Loop through all table rows, and hide those who don't match the search query
    for (i = 0; i < tr.length; i++) {
      td = tr[i].getElementsByTagName('td')[0];
        if (td) {
          if (td.innerHTML.toUpperCase().indexOf(filter) > -1) {
            tr[i].style.display = '';
          } else {
            tr[i].style.display = 'none';
        }
      } 
    }
  }
  </script>"
  
  javascript2 <- "<script>
  //plugin bootstrap minus and plus
  //http://jsfiddle.net/laelitenetwork/puJ6G/
  $(document).on('click', '.btn-number', function(e){
  e.preventDefault();
  
  fieldName = $(this).attr('data-field');
  type      = $(this).attr('data-type');
  var input = $(\"input[name='\"+fieldName+\"']\");
  var currentVal = parseInt(input.val());
  if (!isNaN(currentVal)) {
  if(type == 'minus') {
  
  if(currentVal > input.attr('min')) {
  input.val(currentVal - 1).change();
  } 
  if(parseInt(input.val()) == input.attr('min')) {
  $(this).attr('disabled', true);
  }
  
  } else if(type == 'plus') {
  
  if(currentVal < input.attr('max')) {
  input.val(currentVal + 1).change();
  }
  if(parseInt(input.val()) == input.attr('max')) {
  $(this).attr('disabled', true);
  }
  
  }
  } else {
  input.val(0);
  }
  });
  $(document).on('focusin','.input-number',function(){
  $(this).data('oldValue', $(this).val());
  });
  $(document).on('change','.input-number',function() {
  
  minValue =  parseInt($(this).attr('min'));
  maxValue =  parseInt($(this).attr('max'));
  valueCurrent = parseInt($(this).val());
  
  name = $(this).attr('name');
  if(valueCurrent >= minValue) {
  $(\".btn-number[data-type='minus'][data-field='\"+name+\"']\").removeAttr('disabled')
  } else {
  alert('Sorry, the minimum value was reached');
  $(this).val($(this).data('oldValue'));
  }
  if(valueCurrent <= maxValue) {
  $(\".btn-number[data-type='plus'][data-field='\"+name+\"']\").removeAttr('disabled')
  } else {
  alert('Sorry, the maximum value was reached');
  $(this).val($(this).data('oldValue'));
  }
  
  
  });
  $(document).on('keydown', '.input-number', function (e) {
  // Allow: backspace, delete, tab, escape, enter and .
  if ($.inArray(e.keyCode, [46, 8, 9, 27, 13, 190]) !== -1 ||
  // Allow: Ctrl+A
  (e.keyCode == 65 && e.ctrlKey === true) || 
  // Allow: home, end, left, right
  (e.keyCode >= 35 && e.keyCode <= 39)) {
  // let it happen, don't do anything
  return;
  }
  // Ensure that it is a number and stop the keypress
  if ((e.shiftKey || (e.keyCode < 48 || e.keyCode > 57)) && (e.keyCode < 96 || e.keyCode > 105)) {
  e.preventDefault();
  }
  });
  

  $(document).ready(function(){
    $('input:checkbox:not(:checked)').each(function() {
    var column = 'table .dealer';
    $(column).hide();
    });
    
    $('input:checkbox').click(function(){
    var column = 'table .dealer';
    $(column).toggle();
    });
  
  });
  
////remove order on x click
  $(document).on('click','.remove', function(){
    var rand = Math.random();
    var rowNum = this.id;
  
    Shiny.onInputChange('removeOrder', [rowNum,rand]);
  });
  
///link to invoice when clicking on invoice numbers, completed orders  
  $(document).on('click','#completed_orders tr', function(){
  
    var invoice_num = $(this).find('td:first').text();
    window.open('https://shanevarn.com/invoices/' + invoice_num.trim() + '.pdf');
  
  });
  
  
  Shiny.addCustomMessageHandler('downloadInvoice',function(message) {eval(message.value);})
  
  
  </script>
  "
  }

##qboFunctions
{
  ###Auth Functions
  getTokenURL <- function(cK, cS, user){
    
    baseURL <- 'https://appcenter.intuit.com/connect/oauth2'
    
    url <- paste0(baseURL,
                  '?client_id=',cK,
                  '&redirect_uri=https://shanevarn.com/quickbooks',
                  '&scope=com.intuit.quickbooks.accounting',
                  '&response_type=code',
                  "&state=",user,"")
    
    return(url)
    
  }
  getAccessToken <- function(cK, cS, code){
    secret <- jsonlite::base64_enc(paste(cK, cS, sep = ":"))
    req <- httr::POST("https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer",
                      httr::add_headers(
                        "Accept" = "application/json",
                        "Authorization" = paste0("Basic ", gsub("\n", "", secret)),
                        "Content-Type" = "application/x-www-form-urlencoded",
                        "Host" = "oauth.platform.intuit.com"),
                      body = paste0('grant_type=authorization_code&',
                                    'code=',code,'&redirect_uri=https://shanevarn.com/quickbooks'), verbose())
    
    req <- content(req, as = "text")
    req <- fromJSON(req)
    return(req)
  }
  refreshToken<- function(cK, cS, rT){
    secret <- jsonlite::base64_enc(paste(cK, cS, sep = ":"))
    req <- httr::POST("https://oauth.platform.intuit.com/oauth2/v1/tokens/bearer",
                      httr::add_headers(
                        "Accept" = "application/json",
                        "Authorization" = paste0("Basic ", gsub("\n", "", secret)),
                        "Content-Type" = "application/x-www-form-urlencoded",
                        "Host" = "oauth.platform.intuit.com"),
                      body = paste0('grant_type=refresh_token&',
                                    "refresh_token=",rT,""))
    
    req <- content(req, as = "text")
    req <- fromJSON(req)
  }
  handleAuth <- function(auth, user){
    
    #Retrieve relevant tokens and corresponding expire times
    auth_token <- auth$access_token
    auth_expire_time <- Sys.time() + as.numeric(auth$expires_in)
    refresh_token <- auth$refresh_token
    refresh_expire_time <- Sys.time() + as.numeric(auth$x_refresh_token_expires_in)  

    #    session$sendCustomMessage("login_successful", paste0("alert('Access Token: ", auth_token,"');"))
    
    #Insert new Expire times into auth table
    qry(paste0("UPDATE auth SET 
               AccessToken = '",auth_token,"', 
               AccessExpire = '",auth_expire_time,"', 
               RefreshToken = '",refresh_token,"', 
               RefreshExpire = '",refresh_expire_time,"'
               WHERE UserName = '",user,"'"))
  }
  
  ###Api Functions
  getInventory <- function(realmId, tkn){
    
    res <- httr::GET(paste0("https://sandbox-quickbooks.api.intuit.com/v3/company/",
                            realmId,"/query?query=select%20*%20from%20Item%20"),
                     httr::add_headers(
                       "Authorization" = paste0("Bearer ", tkn),
                       "Accept" = "application/json",
                       "Content-Type" = "application/json"
                     ), verbose())
    
    qbData <- fromJSON(content(res, as = "text"))
    
    df <- data.frame(Id = c(qbData$QueryResponse$Item$Id),
                     Item = c(qbData$QueryResponse$Item$Name),
                     Quantity = c(qbData$QueryResponse$Item$QtyOnHand),
                     Cost = c(qbData$QueryResponse$Item$PurchaseCost))
  }
  getSales <- function(realmId, tkn){
    
    res <- httr::GET(paste0("https://sandbox-quickbooks.api.intuit.com/v3/company/",
                            realmId,"/query?query=select%20*%20from%20Invoice%20"),
                     httr::add_headers(
                       "Authorization" = paste0("Bearer ", tkn),
                       "Accept" = "application/json",
                       "Content-Type" = "application/json"
                     ), verbose())
    
    qbData <- fromJSON(content(res, as = "text"))$QueryResponse$Invoice
    qbData <- qbData[,c('DocNumber', 'DueDate', "TotalAmt","Deposit","Balance")]
    qbData$Sales_Rep <- sample(c("Josh", "Sandi"),nrow(qbData),TRUE)
    qbData$Status <- sample(c("Complete", "Outstand"),nrow(qbData),TRUE)
    qbData$Deposit <- qbData$TotalAmt - qbData$Balance
    
    return(qbData)
  }
  
}

##appFunctions
{
  merchTable <- function(inventory){
    table_html <- "<table id='merchandise' class='catalog'>
    <thead>
    <th >Item</th>
    <th >Item #</th>
    <th >Retail</th>
    <th >Discount</th>
    <th >Sale Price</th>
    <th width='15%'>Quantity</th>
    <th >Total</th>
    <th class='dealer' style='display:none;'>Cost</th>
    <th class='dealer' style='display:none;'>Profit (%)</th>
    <th class='dealer' style='display:none;'>Profit ($)</th>
    <th >Add</th>
    </thead>"
    
    for(i in seq(1:nrow(inventory))){
      
      item <- inventory$Item[i]
      item_id <- inventory$Id[i]
      cost <- inventory$Cost[i]
      retail_price <- cost * 2
      profit_dollars <- (retail_price - cost) 
      profit_margin <- profit_dollars / cost * 100
      table_html <- paste0(table_html,
                           "<tr>
                           <td>",item,"</td>
                           <td>",item_id,"</td>
                           <td>",numericInput(paste0(item_id,"_retail"),label="",value=retail_price),"</td>
                           <td>",numericInput(paste0(item_id,"_disc"),label="",value=0),"</td>
                           <td>",numericInput(paste0(item_id,"_price"),label="",value=retail_price),"</td>
                           <td>",quant_input2(paste0(item_id,"_quant")),"</td>
                           <td>",div(class='total', numericInput(paste0(item_id,"_total"),label="",value=retail_price)),"</td>
                           <td class='dealer' style='display:none;'>",numericInput(paste0(item_id,"_cost"),label="",value=cost),"</td>
                           <td class='dealer' style='display:none;'>",numericInput(paste0(item_id,"_profmargin"),label="",value=profit_margin),"</td>
                           <td class='dealer' style='display:none;'>",numericInput(paste0(item_id,"_prof"),label="",value=profit_dollars),"</td>
                           <td>",actionButton(paste0(item_id,"_add"),'', icon=icon('plus',lib='glyphicon')),"</td>
                           </tr>")
    }
    
    table_html <- paste0(table_html, '</table>')
    }
  }

##observers
{
  generateObservers <- function(id, input, output, session, orders){
    lapply(id,function(id){
      
      observeEvent(input[[ paste0(id, "_disc") ]],{
        discount <- as.numeric(gsub('%','',input[[paste0(id, "_disc")]]))/100
        retail <- input[[paste0(id, "_retail")]]
        sale <- (retail - (discount*retail))
        
        updateNumericInput(session,paste0(id, "_price"),value=sale)
      })
      
      observeEvent(input[[ paste0(id, "_total") ]],{
        
        total <- input[[ paste0(id, "_price") ]]
        cost <- input[[ paste0(id, "_cost") ]]
        quantity <- as.numeric(input[[ paste0(id, "_quant") ]])
        profit_dollars <- (total - cost) * quantity
        profit_margin <- ((total - cost)/cost) * 100
        
        updateNumericInput(session,paste0(id, "_prof"),value=profit_dollars)
        updateNumericInput(session,paste0(id, "_profmargin"),value=profit_margin)
      })
      
      observeEvent(input[[ paste0(id, "_price") ]],{
        price <- input[[ paste0(id, "_price") ]]
        quantity <- as.numeric(input[[ paste0(id, "_quant") ]])
        updateNumericInput(session,paste0(id, "_total"),value=as.numeric(price*quantity))
      })
      
      observeEvent(input[[ paste0(id, "_quant") ]],{
        price <- input[[ paste0(id, "_price") ]]
        quantity <- as.numeric(input[[ paste0(id, "_quant") ]])
        
        updateNumericInput(session,paste0(id, "_total"),value=as.numeric(price*quantity))
      })
      
      observeEvent(input[[ paste0(id, "_add") ]],{
        inventory <- id
        retail <- input[[ paste0(id, "_retail") ]]
        sales <- input[[ paste0(id, "_price") ]]
        discount <- as.numeric(gsub('%','',input[[paste0(id, "_disc")]]))
        quantity <- as.numeric(input[[ paste0(id, "_quant") ]])
        total <- as.numeric(input[[ paste0(id, "_total") ]])
        order_nm <- input[["order_name"]]
        sales_rep <- input[["sales_rep"]]
        
        if(order_nm != ''){
          
          add_order <- data.frame(
            "Inventory_Number" = inventory, 
            "Retail" = retail, 
            "Sales" = sales,
            "Discount" = discount,
            "Quantity" = quantity, 
            "Total" = total,
            "Time" = format(Sys.time(), tz=timezone,usetz=TRUE),
            "Order_Name" = order_nm,
            "Sales_Rep" = sales_rep)
          
          # add_order <- data.frame("Inventory_Number" = 431, 
          #                         "Retail" = 2000, 
          #                         "Sales" = 2000,
          #                         "Discount" = 0,
          #                         "Quantity" = 1, 
          #                         "Total" = 2000,
          #                         "Time" = format(Sys.time(), tz=timezone,usetz=TRUE),
          #                         "Order_Name" = "Tim",
          #                         "Sales_Rep" = "Josh")
          
          app(add_order, "orders")
          orders_df <- qry(paste0("select * from orders where Order_Name = '",order_nm,"'"))
          
          orders$df <- orders_df
          
          updateTabsetPanel(session, "inTabset",
                            selected = "panel_orders"
          )
          
          # output$orders_table <- renderTable(orders$df)
          
        } else {
          
          showModal(modalDialog(
            title = "Create New Order",
            "You must create a new order before adding items.",
            easyClose = TRUE,
            footer = tagList(
              modalButton("OK")
            )
            
          ))
          
        }
        
        updateTextInput(session,paste0(id, "_quant"),value=1)
      })
    })
  }
}

##Global
{
  timezone <- 'America/New_York'
  sales_reps <- c('Josh', 'Sandi')
  order_names <- qry(paste0("select Order_Name from orders where Sales_Rep = '",sales_reps[1],"'"))$Order_Name
  curr_orders <<- lapply(sales_reps, function(i){
    qry(paste0("select distinct Order_Name 
                    from orders
                    Where Order_Name != ''
                    And Sales_Rep = '",i,"'"))$Order_Name
  })
  names(curr_orders) <- sales_reps
  tax_rt <- 7.5
}

ui <- fluidPage(div(id='ui', style='display:none;',
  tags$head(HTML(on_load),HTML(javascript2),
            HTML("<link href=\"https://fonts.googleapis.com/css?family=Ubuntu\" rel=\"stylesheet\">
                 <link href=\"https://gitcdn.github.io/bootstrap-toggle/2.2.2/css/bootstrap-toggle.min.css\" rel=\"stylesheet\">
                 <script src=\"https://gitcdn.github.io/bootstrap-toggle/2.2.2/js/bootstrap-toggle.min.js\"></script>
                 "),
            HTML(css)),
  tags$script("Shiny.addCustomMessageHandler('clear_url',  function(x) {eval(x)});
               Shiny.addCustomMessageHandler('login_successful', function(x) {eval(x)});"),
  titlePanel('RSS Dealer Dashboard'),
  fluidRow(
    column(width=3,selectInput('sales_rep', "Sales Rep:", sales_reps)),
    column(width=3,selectInput('order_name', 'Order Name:', order_names)),
    column(width=3,input_w_button("order_input","New Order:","Add order...","add_order","plus")),
    column(width=3,
           HTML("<label class='control-label' for='clr_order'> </label><br>"),
           actionButton('clr_order',label="Clear Order",icon=icon("trash", lib="glyphicon"))
    )
  ),
  tabsetPanel(id = "inTabset",
              tabPanel(title = div(style="color:black;", 'Merchandise'), value='panel_sales',
                       fluidRow(
                         column(width=3, h2("Merchandise", align='center')),
                         column(width=8,br(),
                                HTML("<div align='right'><label class='switch'>
                                     <input type='checkbox'>
                                     <span class='slider round'></span>
                                     </label></div>"))),
                       HTML("<input type='text' id='myInput' onkeyup='prodSearch()' placeholder='Search for Product..'>"),
                       br(),
                       uiOutput("inventory"),
                       br(),br()),
              tabPanel(title = div(style="color:black;", 'Orders'), value='panel_orders',
                       fluidRow(
                         column(width=10, h2("Current Orders", align='center')),
                         column(width=2,br(),
                                HTML("<div align='left'><label class='switch'>
                                    <input type='checkbox'>
                                    <span class='slider round'></span>
                                  </label></div>"))),
                       div(align='center',tableOutput('orders_table')),
                       hr(style="border-width:2px"),
                       fluidRow(column(width=5, NULL),
                                column(width=4, uiOutput('total_table'),br(),
                                       div(align='right',actionButton('complete_order', label="Complete Order",
                                                                      icon=icon("ok-circle", lib='glyphicon')))
                                ))),
              tabPanel(title = div(style="color:black;", 'Analytics'), value='panel_analytics'),
              tabPanel(title = div(style="color:black;", 'Inventory'), value='panel_inventory',
                       fluidRow(
                         column(width=1,NULL),
                         column(width=1, h2("Inventory")),
                         column(width=3,
                                HTML("<br>&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp
                                   <input type='checkbox' id='inventory_type' checked data-toggle='toggle' data-on='Current' 
                                   data-off='Transactions' data-onstyle='default'  data-width='120'>")
                         ),
                         column(width=5,br(),div(align='center',actionButton('input_inventory', "Upload Inventory")))
                       ),
                       column(width=1, NULL),
                       tableOutput('inventory_table')
              ),
              tabPanel(title = div(style="color:black;", 'Sales'), value='panel_complete',
                       fluidRow(column(width=3, h2("Sale Records", align='center'))),
                       tableOutput('completed_orders'),
                       br(),br(),br())
                         )),
  ####User Login
 div(id='login',
      column(width=3,NULL),
      column(width=6,
        br(),br(),
        HTML("<h4><span class='glyphicon glyphicon-lock'></span> RSS Dealer Authentification</h4>"),
        textInput("user","Username:"),
        passwordInput("pass","Password:"),
        actionButton('loginBtn', 'Login'),
        uiOutput("qbLogin")
      ),
      column(width=3,NULL)
    )
  )

server <- function(input,output,session){
  
  #Auth token tracker
  token = reactiveValues() 
  
  #Reactive order tracker
  orders = reactiveValues()
  
  #Login and Authorize
  observeEvent(input$loginBtn,{
    
    user <- input$user
    pass <- input$pass
    
    userInfo <- qry(paste0("select *
                from auth
                where UserName = '",user,"' 
                and Password = '",pass,"'"))
    
    if(nrow(userInfo) > 0){
      
      cS <- userInfo$ConsumerSecret
      cK <- userInfo$ConsumerKey
      rT <- userInfo$RefreshToken
        
      if(userInfo$AccessExpire < Sys.time()){
        
        res <- refreshToken(cK,cS,rT)
        auth <- handleAuth(res, user)
        
        session$sendCustomMessage("login_successful", "$('#ui').show();$('#login').hide();")
        token$auth_token <- auth$access_token
       
        #output$qbLogin <- renderUI(a("Click Here to Authorize Your QuickBooks Access", href=getTokenURL(cK,cS, user)))
        
      } else {
        
        token$auth_token <- userInfo$AccessToken
        token$realm <- userInfo$RealmId
        
        output$qbLogin <- renderUI(("Login Successul!"))
        session$sendCustomMessage("login_successful", "$('#ui').show();$('#login').hide();")
        
      }
      
    } else {
      
      output$qbLogin <- renderUI(h2("Login Failed"))
    }

  })
  
  ##Watch URL for token code and use to get auth token
  observeEvent(session$clientData$url_search,{
    
    ## gets all the parameters in the URL. Your authentication code should be one of them
    query <- parseQueryString(session$clientData$url_search)
    
    if(length(as.character(query$code)) > 0){
      
      token$code <- code <- as.character(query$code)
      token$user <- user <- as.character(query$state)
      token$realm <- realm <- as.character(query$realmId)
      
      userInfo <- qry(paste0("select ConsumerKey, ConsumerSecret from auth where UserName = '",user,"'"))
      token$cs <- cS <- userInfo$ConsumerSecret
      token$ck <- cK <- userInfo$ConsumerKey
      
  #    session$sendCustomMessage("login_successful", paste0("alert('code: ",code,", user:",user,", realm:",realm ,", ck: ",cK,", cs: ",cS,"');"))
      
      #Call Function to get API token
      auth <- getAccessToken(cK, cS, code)
      auth <- handleAuth(auth, user)
      
      #Clear user browser url for cleanliness sake
      session$sendCustomMessage("clear_url", "window.history.replaceState({}, document.title, '/quickbooks');")
      session$sendCustomMessage("login_successful", "$('#ui').show();$('#login').hide();")
      
      token$auth_token <- auth$access_token
      #refresh_token <- auth$refresh_token 
  #    session$sendCustomMessage("login_successful", paste0("alert('",auth_token,"');"))
    }
  })
  
  ##Get Inventory
  inventory <- reactive({

    tkn <- token$auth_token
    realm <- isolate(token$realm)
    
    if(!is.null(tkn)){
      
      df <- getInventory(realm, tkn) %>% filter(!is.na(Quantity))
      
    } else { 
      df <- NULL
    }
    
    return(df)
    
  })
  
  ##Render Merchandise table
  output$inventory <- renderUI({
    
      df <- inventory()
      
      if(!is.null(df)){
        observers <- generateObservers(df$Id, input, output, session, orders)
        df <- merchTable(df)
      } else {
        df <- NULL
      }
      
    return(HTML(df))
    
  })
  
  #Order tab tables
  observeEvent(input$order_name,{
    order_nm <- input$order_name
    sales_rep <- input$sales_rep
    
    orders_df <- qry(paste0("select * from orders 
                            where Order_Name = '",order_nm,"'
                            and Sales_Rep = '",sales_rep,"'"))
    
    orders$df <- orders_df
    
  })
  
  observeEvent(input$removeOrder,{
    row_remove <- as.numeric(trimws(gsub("_remove","",input$removeOrder[1])))
    order_nm <- input$order_name
    sales_rep <- input$sales_rep
    
    df <- qry(paste0("select * from orders
                     where Order_Name = '",order_nm,"'
                     and Sales_Rep = '",sales_rep,"'"))[-row_remove,]
    
    qry(paste0("delete from orders
               where Order_Name = '",order_nm,"'
               and Sales_Rep = '",sales_rep,"'"))
    
    app(df, "orders")
    
    orders$df <- df
  })
  
  observeEvent(orders$df,{
    orders_table <- orders$df
    #orders_table$row_names <- NULL
    orders_table$Time <- NULL
    orders_table$Sales_Rep <- NULL
    orders_table$Order_Name <- NULL
    orders_table$Inventory_Number <- as.integer(orders_table$Inventory_Number)
    orders_table$Quantity <- as.integer(orders_table$Quantity)
    
    names(orders_table) <- c("Item #","Retail",
                             "Sales", "Discount",
                             "Quantity", "Total")
    
    if(nrow(orders_table) > 0 ){
      
      orders_table$Remove <- sapply(1:nrow(orders_table), function(id){
        as.character(actionButton(inputId = paste0(id,"_remove"),label="", class='remove',
                                  icon=icon("remove", lib="glyphicon")))
      })
    }
    #Orders Tab
    output$orders_table <- renderTable({orders_table},sanitize.text.function = function(x) x,
                                       html.table.attributes="class='catalog'")
    
    session$sendCustomMessage(type="ordersTable", sample(1:10,1))
    
    #Invoice
    invoice_tab <- orders_table
    invoice_tab$Retail <- NULL
    invoice_tab$Discount <- NULL
    invoice_tab$Remove <- NULL
    invoice_tab$Method <- sapply(invoice_tab$`Item #`, function(i){
      as.character(selectInput(inputId = paste0(i,"_method"),width = '100%',
                               label=NULL,choices=c("Take", "PickUp","Deliver")))
    })
    
    
    #invoice_tab <- print(xtable(invoice_tab), type="html", html.table.attributes="")
    output$invoice_table <- renderTable({invoice_tab},width = '100%', 
                                        sanitize.text.function = function(x) x,
                                        html.table.attributes="class='catalog'")
    
    order_total <- sum(orders_table$Total)
    tax <- (order_total * tax_rt/100)
    
    css <- "style='text-align: right;'"
    total_table <- paste0("<table>",
                          "<tr><td>Sub-Total:</td><td ",css,">$",order_total,"</td></tr>",
                          "<tr><td>Tax (",tax_rt,"%):</td><td ",css,">$",tax,"</td></tr>",
                          "<tr><td>Total:</td><td ",css,">$",(order_total + tax),"</td></tr>",
                          "</table>")
    
    output$total_table <- renderUI(HTML(total_table))
    output$invoice_total <- renderUI(HTML(total_table))
    
  })
  
  #Add order name button
  observeEvent(input$add_order,{
    if(input[["order_input"]]  != ''){
      new_nm <- input[["order_input"]]
      sales_rep <- input$sales_rep
      curr_orders[[sales_rep]] <<- c(curr_orders[[sales_rep]], new_nm)
      
      updateTextInput(session,"order_input", value='')
      updateSelectInput(session,"order_name", 
                        choices= curr_orders[[sales_rep]], 
                        selected = new_nm)
    }
  })
  
  #Change of Sales Rep event
  observeEvent(input$sales_rep,{
    sales_rep <- input$sales_rep
    updateSelectInput(session,"order_name", choices=curr_orders[[sales_rep]])
  })
  
  #Clear Order Modal
  observeEvent(input$clr_order,{
    order_nm <- input[["order_name"]]
    sales_rep <- input$sales_rep
    showModal(clear_order_modal(order_nm,sales_rep))
  })
  clear_order_modal <- function(order_nm, sales_rep) {
    modalDialog(
      title = "Clear Order",
      span(paste0("Are you sure you would like to clear the 
                  current order, ",order_nm," for Sales Rep ",sales_rep,"?")),
      footer = tagList(
        actionButton("clr_final", "Clear Order"),
        modalButton("Cancel")
      )
      )
  }
  observeEvent(input$clr_final,{
    order_nm <- input$order_name
    sales_rep <- input$sales_rep
    
    qry(paste0("delete from orders where Order_Name ='",order_nm,"'
               and Sales_Rep = '",sales_rep,"'"))
    
    curr_orders[[sales_rep]] <<- curr_orders[[sales_rep]][curr_orders[[sales_rep]] != order_nm]
    updateSelectInput(session,"order_name", choices=curr_orders[[sales_rep]])
    removeModal()
  })
  
  #Invoice Modal
  observeEvent(input$complete_order,{
    showModal(complete_order_modal(orders$df))
  })
  complete_order_modal <- function(orders_df) {
    modalDialog(size='l',
                title = "Complete Order",
                fluidRow(
                  column(width=6,
                         textInput("customer_name", "Name:"),
                         textAreaInput("customer_add", "Address:",height = '100%'),
                         textAreaInput("customer_comment", "Comments:", width='100%')),
                  column(width=6,
                         textInput("customer_phone1", "Phone 1:"),
                         textInput("customer_phone2", "Phone 2:"),
                         textInput("customer_email", "Email:"),
                         column(width=4,dropdownInput(c("Cash", "Credit","Check")))
                  )
                ),hr(style="border-width:2px"),
                fluidRow(
                  column(width=7,tableOutput("invoice_table")),
                  column(width=5,
                         HTML("<label class='control-label' for='invoice_total'>Amount Due:</label><br>"),
                         uiOutput("invoice_total"),
                         numericInput("amt_collect","Amount Collected:", value=0),
                         uiOutput("amt_due"))
                ),
                footer = tagList(
                  downloadButton('downloadReport',"Submit Order"),
                  modalButton("Cancel")
                )
    )
  }
  observeEvent(input$amt_collect,{
    orders_df <- orders$df
    amount_due <- sum(orders_df$Total) * (1+(tax_rt/100)) - input$amt_collect
    
    amount_due <- ifelse(is.na(amount_due), 0, amount_due)
    
    output$amt_due <- renderUI(HTML(paste0(
      "<label class='control-label' for='amt_due'>Outstanding Balance:</label><br>",
      "$",amount_due)))
  })
  
  #Pmt Type Dropdown
  observe({
    cash_amt <-as.numeric(input$Cash_input)
    credit_amt <- as.numeric(input$Credit_input)
    check_amt <- as.numeric(input$Check_input)
    
    collected <- sum(cash_amt,credit_amt,check_amt)
    
    updateNumericInput(session, inputId= "amt_collect",value=collected)
    
  })
  observeEvent(input$Cash_full,{
    orders_df <- orders$df 
    amt <- ifelse(input$Cash_input == 0, sum(orders_df$Total) * (1+(tax_rt/100)),0)
    
    updateNumericInput(session, inputId = "Cash_input", value = amt)
    updateNumericInput(session, inputId = "Credit_input", value = 0)
    updateNumericInput(session, inputId = "Check_input", value = 0)
  })
  observeEvent(input$Credit_full,{
    orders_df <- orders$df 
    amt <- ifelse(input$Credit_input == 0, sum(orders_df$Total) * (1+(tax_rt/100)),0)
    
    updateNumericInput(session, inputId = "Credit_input", value = amt)
    updateNumericInput(session, inputId = "Cash_input", value = 0)
    updateNumericInput(session, inputId = "Check_input", value = 0)
  })
  observeEvent(input$Check_full,{
    orders_df <- orders$df 
    amt <- ifelse(input$Check_input == 0,sum(orders_df$Total) * (1+(tax_rt/100)), 0)
    
    updateNumericInput(session, inputId = "Check_input", value = amt)
    updateNumericInput(session, inputId = "Credit_input", value = 0)
    updateNumericInput(session, inputId = "Cash_input", value = 0)
  })
  
  #Update completed orders when swapped to sales tab
  observeEvent(input$inTabset,{
    if(input$inTabset == "panel_complete"){

      tkn <- token$auth_token
      realm <- token$realm
      
      if(!is.null(tkn)){
        
        df <- getSales(realm, tkn)
        
      } else { 
        df <- NULL
      }
      
      df
      
      output$completed_orders <- renderTable({df}, html.table.attributes="class='catalog'")
    }
  })
  
  #Inventory Tab
  observeEvent({input$inTabset; input$inventory_type},{
    if(input$inTabset == "panel_inventory"){
      
      if(input$inventory_type){
        z <- qry("select Item, `Item.`, sum(Total_Cost), avg(Unit_Cost), sum(Quantity)
                 from inventory Group By Item, `Item.`")
      } else {
        z <- qry("select * from inventory order by Timestamp DESC")
      }
      
      output$inventory_table <- renderTable({z},
                                            html.table.attributes="class='catalog'")
    }
    })
  
  #Inventory Modal
  observeEvent(input$input_inventory,{
    showModal(inventory_modal())
  })
  inventory_modal <- function(){
    modalDialog(size='l',
                title = "Upload Inventory",
                column(width=1, NULL),
                column(width=5,
                       fileInput("file1", "Choose Inventory File:",
                                 accept = c(".xls", "xlsx",".csv")
                       )),
                column(width=4,
                       textInput("invoice_num", "Input Order Invoice #:")),
                tableOutput("contents"),br(),hr(),
                footer = tagList(
                  actionButton("submit_order", "Submit Order"),
                  modalButton("Close")
                )
    )
  }
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile)|is.null(input$invoice_num))
      return(NULL)
    
    # file_path <- paste(inFile$datapath, ".csv", sep="")
    # file.rename(inFile$datapath, file_path)
    z <- read.csv(inFile$datapath)
    z$Invoice_Number <- "CQ5R35Z"
    z$Transaction <- "Purchase"
    z$Timestamp <- format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S')
    app(z, "inventory")
    return(z)
    
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      #invoice_number <- as.character(orders$invoice_number)
      paste0('Invoice.pdf')
    },
    content = function(file) {
      
      src <- normalizePath('report.Rmd')
      img_src <- normalizePath('tfd.jpg')
      # params <- list(sales_rep = input$sales_rep)#,
      # credit_amt = input$credit_input,
      # check_amt = input$check_input,
      # cust_name = input$customer_name,
      # address = input$customer_add,
      # phone1 = input$customer_phone1,
      # phone2 = input$customer_phone2,
      # email = input$customer_email,
      # comment = input$customer_comment,
      # total_price = sum(orders_df$Total),
      # tax = total_price * (tax_rt/100),
      # amount_due = sum(orders_df$Total) * (1+(tax_rt/100)),
      # amount_collected = input$amt_collect,
      # outstanding = amount_due - amount_collected,
      # sales_rep = input$sales_rep,
      # cash_coll = input$Cash_input,
      # credit_coll = input$Credit_input,
      # check_coll = input$Check_input)
      
      orders_df <- orders$df
      time_stamp <- format(Sys.time(), tz=timezone,usetz=TRUE)
      invoice_number <- gsub('[^0-9.]',"",time_stamp)
      order_date <- as.Date(time_stamp, format="%Y-%m-%d")
      
      order_status <- sapply( orders_df$id, function(i){ input[[paste0(i,"_method")]] })
      order_status <- all("Take" == order_status)
      order_status <- ifelse(order_status, "Complete", "Outstanding")
      
      #write customer details
      cash_amt <-input$cash_input
      credit_amt <- input$credit_input
      check_amt <- input$check_input
      cust_name <- input$customer_name
      address <- input$customer_add
      phone1 <- input$customer_phone1
      phone2 <- input$customer_phone2
      email <- input$customer_email
      comment <- input$customer_comment
      total_price <- sum(orders_df$Total)
      tax <- total_price * (tax_rt/100)
      amount_due <- sum(orders_df$Total) * (1+(tax_rt/100))
      amount_collected <- input$amt_collect
      outstanding <- amount_due - amount_collected
      sales_rep <- input$sales_rep
      cash_coll <- input$Cash_input
      credit_coll <- input$Credit_input
      check_coll <- input$Check_input
      
      #order summary
      order_summary <- data.frame(
        Invoice_Number = invoice_number,
        Sales_Rep = sales_rep,
        Order_Date = order_date,
        Customer = cust_name,
        Address = address,
        Phone1 = phone1,
        Phone2 = phone2,
        Email = email,
        Comment = comment,
        Subtotal = total_price,
        Tax = tax,
        Total = amount_due,
        Amount_Collected = amount_collected,
        Outstanding = outstanding,
        Cash_Collected = cash_coll,
        Credit_Collected = credit_coll,
        Check_Collected = check_coll,
        Order_Status = order_status)
      
      app(order_summary, "order_summary")
      
      #update inventory
      for(i in seq(1:nrow(orders_df))){
        inventory_num <- orders_df$Inventory_Number[i]
        quantity <- orders_df$Quantity[i]
        method <- input[[paste0(inventory_num,"_method")]]
        item_name <- qry(paste0("Select Distinct Item from inventory where `Item.` = '",inventory_num,"'"))
        cost <- qry(paste0("select distinct Unit_Cost
                           from inventory where `Item.` = '",inventory_num,"' limit 1"))
        total_cost <- cost * quantity
        timestamp <- format(as.POSIXlt(Sys.time()),'%Y-%m-%d %H:%M:%S')
        
        # if(method == "Take"){
        #   qry(paste0("UPDATE inventory 
        #       SET In_Stock = In_Stock - ",quantity,"
        #       WHERE `Item.` = '",inventory_num,"'"))
        # }
        
        qry(paste0("INSERT INTO 
                   inventory (Item, `Item.`,Total_Cost, Unit_Cost, Quantity, 
                   Invoice_Number, Transaction, Timestamp)
                   VALUES ('",item_name,"','",inventory_num,"','",total_cost,"','",cost,"','-",quantity,"',
                   '",invoice_number,"','Sale','",timestamp,"')"))
        
        #   qry(paste0("UPDATE inventory 
        #       SET Available = Available - ",quantity,"
        #       WHERE `Item.` = '",inventory_num,"'"))
      }
      
      #order detail
      #orders_df$Invoice_Number <- invoice_number 
      app(orders_df, "order_detail")
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(img_src, 'tfd.jpg', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render(input = 'report.Rmd',
                    output_format =  pdf_document()
                    # params = params
                    
      )
      
      #clear order
      order_nm <- input$order_name
      sales_rep <- input$sales_rep
      
      qry(paste0("delete from orders where Order_Name ='",order_nm,"'
               and Sales_Rep = '",sales_rep,"'"))
      
      curr_orders[[sales_rep]] <<- curr_orders[[sales_rep]][curr_orders[[sales_rep]] != order_nm]
      updateSelectInput(session,"order_name", choices=curr_orders[[sales_rep]])
      
      removeModal()
      
      #invoice_number <- as.character(orders$invoice_number)
      file.copy(out, '/srv/shiny-server/invoices', overwrite=TRUE)
      file.rename('/srv/shiny-server/invoices/report.pdf', paste0('/srv/shiny-server/invoices/',invoice_number,'.pdf'))
      
      file.rename(out, file)
    }
        )
  
  
}

shinyApp(ui,server)