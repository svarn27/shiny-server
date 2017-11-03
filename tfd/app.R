library(shiny)
library(xtable)
library(knitr)

options(shiny.trace=TRUE)

baseDir <- "/srv/shiny-server/utilities/"
source(paste0(baseDir,"query_function.R"))

##Global Variables
{
timezone <- 'America/New_York'
inventory <- qry("select * from inventory where Transaction ='Purchase'")
sales_reps <- c('Josh', 'Sandi')
curr_orders <<- lapply(sales_reps, function(i){
                  qry(paste0("select distinct Order_Name 
                    from orders
                    Where Order_Name != ''
                    And Sales_Rep = '",i,"'"))$Order_Name
                })
names(curr_orders) <- sales_reps
tax_rt <- 7.5
}

##observer functions
{
  
  generateObservers <- function(id, n, input, output, session, orders){
    lapply(id,function(id){
      
      observeEvent(input[[ paste0(id, "_disc") ]],{
        discount <- as.numeric(gsub('%','',input[[paste0(id, "_disc")]]))/100
        retail <- input[[paste0(id, "_retail")]]
        sale <- (retail - (discount*retail))
        
        updateNumericInput(session,paste0(id, "_price"),value=sale)
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
  
  Shiny.addCustomMessageHandler('jsCode', function(e){
  eval(e);
  })
  </script>"
  
  javascript2 <- "<script>
  $(document).ready(function(){
  //plugin bootstrap minus and plus
  //http://jsfiddle.net/laelitenetwork/puJ6G/
  $('.btn-number').click(function(e){
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
  $('.input-number').focusin(function(){
  $(this).data('oldValue', $(this).val());
  });
  $('.input-number').change(function() {
  
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
  $(\".input-number\").keydown(function (e) {
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

  $('input:checkbox:not(:checked)').each(function() {
      var column = 'table .dealer';
      $(column).hide();
  });
  
  $('input:checkbox').click(function(){
      var column = 'table .dealer';
      $(column).toggle();
  });

})
</script>
  "
  }

##table function
{
  table_html <- "<table id='merchandise' class='catalog'>
  <thead>
  <th >Item</th>
  <th >Item #</th>
  <th >Retail Price</th>
  <th >Discount</th>
  <th >Sale Price</th>
  <th width='15%'>Quantity</th>
  <th >Total</th>
  <th class='dealer'>Cost</th>
  <th class='dealer'>Profit (%)</th>
  <th class='dealer'>Profit ($)</th>
  <th >Add</th>
  </thead>"
  item_list <- c()
  
  for(i in seq(1:nrow(inventory))){
    
    item <- inventory$Item[i]
    item_id <- inventory$Item.[i]
    cost <- inventory$Unit_Cost[i]
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
                         <td>",numericInput(paste0(item_id,"_total"),label="",value=retail_price),"</td>
                         <td class='dealer'>",cost,"</td>
                         <td class='dealer'>",numericInput(paste0(item_id,"_profmargin"),label="",value=profit_margin),"</td>
                         <td class='dealer'>",numericInput(paste0(item_id,"_prof"),label="",value=profit_dollars),"</td>
                         <td>",actionButton(paste0(item_id,"_add"),'', icon=icon('plus',lib='glyphicon')),"</td>
                         </tr>")
  }
  
  table_html <- paste0(table_html, '</table>')
  }

# Define UI
{
ui <- fluidPage(tags$head(HTML(on_load),HTML(javascript2),
                          HTML("<link href=\"https://fonts.googleapis.com/css?family=Ubuntu\" rel=\"stylesheet\">"),
                          HTML(css)),
                titlePanel('Business Management System'),
                    fluidRow(
                    column(width=3,selectInput('sales_rep', "Sales Rep:", sales_reps)),
                    column(width=3,selectInput('order_name', 'Order Name:', curr_orders)),
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
                           HTML(table_html)),
                  tabPanel(title = 'Orders', value='panel_orders',
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
                  tabPanel(title = 'Analytics', value='panel_analytics'),
                  tabPanel(title = 'Inventory', value='panel_inventory',
                           fluidRow(
                             column(width=1,NULL),
                             column(width=2, h2("Inventory")),
                             column(width=9,br(),div(align='center',actionButton('input_inventory', "Upload Inventory")))
                           ),
                           column(width=1, NULL),
                           tableOutput('inventory_table')
                  ),
                  tabPanel(title = 'Sales', value='panel_complete',
                           fluidRow(column(width=3, h2("Sale Records", align='center'))),
                           tableOutput('completed_orders'),
                           br(),br(),br())
                )
  )
}

server <- function(input,output,session){
  
  #Reactive order tracker
  orders = reactiveValues()
  
  #Generage Observers on Page Load
  observeEvent(input$page_loaded,{
    observers <<- generateObservers(inventory$Item., length(item_list), input, output, session, orders)
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
                                    as.character(actionButton(inputId = paste0(id,"_remove"),label="",
                                                              icon=icon("remove", lib="glyphicon")))
                                  })
    }
    #Orders Tab
    output$orders_table <- renderTable({orders_table},sanitize.text.function = function(x) x,
                                       html.table.attributes="class='catalog'")
    
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
        actionButton("submit_order", "Submit Order"),
        downloadButton('downloadReport',"Download Invoice"),
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
  observeEvent(input$submit_order,{
    
    orders_df <- orders$df
    time_stamp <- format(Sys.time(), tz=timezone,usetz=TRUE)
    invoice_number <- gsub('[^0-9.]',"",time_stamp)
    order_date <- as.Date(time_stamp, format="%Y-%m-%d")
      
    order_status <- sapply( orders_df$id, function(i){ input[[paste0(i,"_method")]] })
    #print(order_status)
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
    orders_df$Invoice_Number <- invoice_number 
    app(orders_df, "order_detail")
    
    #clear order
    order_nm <- input$order_name
    sales_rep <- input$sales_rep
    
    qry(paste0("delete from orders where Order_Name ='",order_nm,"'
               and Sales_Rep = '",sales_rep,"'"))
    
    curr_orders[[sales_rep]] <<- curr_orders[[sales_rep]][curr_orders[[sales_rep]] != order_nm]
    updateSelectInput(session,"order_name", choices=curr_orders[[sales_rep]])
    
    removeModal()
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
      df <- qry("select Invoice_Number, Customer, Order_Date,
          Total,	Amount_Collected,	Outstanding,
          Sales_Rep,	Order_Status from order_summary
          order by Order_Date DESC")
      
      # df <- print(xtable(df), 
      #       type="html", html.table.attributes="")
      # 
      # actionButton("do", "Click Me")
      
      output$completed_orders <- renderTable({df},
                                             html.table.attributes="class='catalog'")
    }
  })
  
  #Inventory Tab
  observeEvent(input$inTabset,{
    if(input$inTabset == "panel_inventory"){
      output$inventory_table <- renderTable({qry("select * from inventory 
                                                 order by Timestamp DESC")},
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
  
  # observeEvent(input$toPDF,{
  #   
  #   session$sendCustomMesssage(type='toPDF', message=javascript)
  #   
  # })
    
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('Invoice.pdf')
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
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      file.copy(img_src, 'tfd.jpg', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render(input = 'report.Rmd',
                    output_format =  pdf_document()#,
                   # params = params
                    
      )
      file.rename(out, file)
    }
  )
  
}

shinyApp(ui, server)