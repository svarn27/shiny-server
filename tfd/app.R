library(shiny)
library(xtable)

options(shiny.trace=TRUE)

baseDir <- "/srv/shiny-server/utilities/"
source(paste0(baseDir,"query_function.R"))

##Global Variables
{
timezone <- 'America/New_York'
inventory <- qry("select * from inventory")
sales_reps <- c('Josh', 'Sandi')
curr_orders <<- lapply(sales_reps, function(i){
                  qry(paste0("select distinct Order_Name 
                    from orders
                    Where Order_Name != ''
                    And Sales_Rep = '",i,"'"))$Order_Name
                })
names(curr_orders) <- sales_reps
tax_rt <- 7.5

# orders_df <- data.frame(inventory_num=numeric(),
#                         retail=numeric(),
#                         sale=numeric(),
#                         discount=numeric(),
#                         quantity=numeric(),
#                         total=numeric(),
#                         order_name=character(),
#                         timestamp=character())
# 
# 
# names(orders_df) <- c("Inventory_Number", "Retail", "Sales","Discount","Quantity", "Total", "Order_Name","Time")
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
  table {width:90%;
table-layout: fixed;
margin-left:2.5%}
  th, td {
  padding: 5px;
  text-align: left;
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
  
  new_order_btn <- 
    "<label class='control-label' for='order_name'>New Order:</label>
                  <div class='input-group' width='10%'>
                  
                  <input id='order_input' type='text' class='form-control' placeholder='Add order...'>
                  <span class='input-group-btn'>
                  <button class='btn btn-secondary action-button' id='add_order' type='button'>
                  <span class='glyphicon glyphicon-plus'></span> </button>
                  </span>
                  </div>"
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
  
  javascript <- 
  "<script>$(document).ready(function(){
  
  var quantitiy=0;
  $('.quantity-right-plus').click(function(e){
  
  // Stop acting like a button
  e.preventDefault();
  // Get the field name
  var quantity = parseInt($('#quantity').val());
  
  // If is not undefined
  
  $('#quantity').val(quantity + 1);
  
  
  // Increment
  
  });
  
  $('.quantity-left-minus').click(function(e){
  // Stop acting like a button
  e.preventDefault();
  // Get the field name
  var quantity = parseInt($('#quantity').val());
  
  // If is not undefined
  
  // Increment
  if(quantity>0){
  $('#quantity').val(quantity - 1);
  }
  });
  
});</script>"  
  
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
  });})</script>
  "
  }

##table function
{
  table_html <- "<table>
  <tr>
  <th >Item</th>
  <th >Item #</th>
  <th >Retail Price</th>
  <th >Discount</th>
  <th >Sale Price</th>
  <th width='15%'>Quantity</th>
  <th >Total</th>
  <th >Add to Order</th>
  </tr>"
  item_list <- c()
  
  for(i in seq(1:nrow(inventory))){
    
    item <- inventory$Item[i]
    item_id <- inventory$Item.[i]
    retail_price <- inventory$Retail[i]
    table_html <- paste0(table_html,
                         "<tr>
                         <td>",item,"</td>
                         <td>",item_id,"</td>
                         <td>",numericInput(paste0(item_id,"_retail"),label="",value=retail_price),"</td>
                         <td>",numericInput(paste0(item_id,"_disc"),label="",value=0),"</td>
                         <td>",numericInput(paste0(item_id,"_price"),label="",value=retail_price),"</td>
                         <td>",quant_input2(paste0(item_id,"_quant")),"</td>
                         <td>",numericInput(paste0(item_id,"_total"),label="",value=retail_price),"</td>
                         <td>",actionButton(paste0(item_id,"_add"),'Add to Order'),"</td>
                         </tr>")
  }
  
  table_html <- paste0(table_html, '</table>')
  }

#sales_list <- as.data.frame(read_excel('C:/Users/Varn_Shane/Documents/salessheet.xlsx',1))

# Define UI
ui <- fluidPage(tags$head(HTML(on_load),HTML(javascript2),HTML(css)),
                titlePanel('Business Management System'),
                    fluidRow(
                    column(width=3,selectInput('sales_rep', "Sales Rep:", sales_reps)),
                    column(width=3,selectInput('order_name', 'Order Name:', curr_orders)),
                    column(width=3,HTML(new_order_btn)),
                    column(width=2,
                           HTML("<label class='control-label' for='clr_order'> </label><br>"),
                           actionButton('clr_order',label="Clear Order:",icon=icon("trash", lib="glyphicon")))
                    ),
                tabsetPanel(id = "inTabset",
                  tabPanel(title = 'Merchandise', value='panel_sales',
                           HTML(table_html)),
                  tabPanel(title = 'Orders', value='panel_orders',
                           h2("Current Order", align='center'),
                           div(align='center',tableOutput('orders_table')),
                           hr(style="border-width:2px"),
                           fluidRow(column(width=4, NULL),
                                    column(width=4, uiOutput('total_table'),
                                           actionButton('complete_order', label="Complete Order",
                                                        icon=icon("ok-circle", lib='glyphicon'))
                                           ))),
                  tabPanel(title = 'Analytics', value='panel_analytics', 
                           NULL),
                  tabPanel(title = 'Inventory', value='panel_inventory',
                           actionButton('input_inventory', "Upload Inventory"),
                           tableOutput('inventory_table')
                  ),
                  tabPanel(title = 'Sales', value='panel_complete',
                           tableOutput('completed_orders'))
                )
)

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
    
    #Orders Tab
    output$orders_table <- renderTable(orders_table)
    
    #Invoice
    invoice_tab <- orders_table
    invoice_tab$Retail <- NULL
    invoice_tab$Discount <- NULL
    invoice_tab$Method <- sapply(invoice_tab$`Item #`, function(i){
      HTML(as.character(selectInput(inputId = paste0(i,"_method"),label=NULL,choices=c("Take", "PickUp","Deliver"))))
    })
    
    invoice_tab <- print(xtable(invoice_tab), type="html", html.table.attributes="")
    output$invoice_table <- renderUI(HTML(invoice_tab))
    
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
          column(width=5,
            textInput("customer_name", "Name:"),
            textAreaInput("customer_add", "Address:",height = '100%'),
            textAreaInput("customer_comment", "Comments:", width='100%')),
          column(width=6,
            textInput("customer_phone1", "Phone 1:"),
            textInput("customer_phone2", "Phone 2:"),
            textInput("customer_email", "Email:"),
            column(width=4,selectInput("customer_type", "Select Method:", 
                        c("Take With", "Pick Up", "Delivery"))),
            column(width=4, selectInput("pmt_type", "Pmt:",
                                        c("Cash", "Check", "Credit"))),
            column(width=4, selectInput("pmt_full", "Pmt Full?:",
                                        c("In Full", "Partial")))
            )
        ),hr(style="border-width:2px"),
        fluidRow(
        column(width=6,uiOutput("invoice_table")),
        column(width=6,
               HTML("<label class='control-label' for='invoice_total'>Amount Due:</label><br>"),
               uiOutput("invoice_total"),
               #uiOutput('StripeCheckOut'),
               numericInput("amt_collect","Amount Collected:", value=0),
               uiOutput("amt_due"))
        ),
      footer = tagList(
        actionButton("submit_order", "Submit Order"),
        modalButton("Cancel")
      )
    )
  }
  observeEvent(input$amt_collect,{
    orders_df <- orders$df
    amount_due <- sum(orders_df$Total) * (1+(tax_rt/100)) - input$amt_collect
    output$amt_due <- renderUI(HTML(paste0(
      "<label class='control-label' for='amt_due'>Outstanding Balance:</label><br>",
      "$",amount_due)))
  })
  observeEvent(input$submit_order,{
    
    orders_df <- orders$df
    time_stamp <- format(Sys.time(), tz=timezone,usetz=TRUE)
    invoice_number <- gsub('[^0-9.]',"",time_stamp)
    order_date <- as.Date(time_stamp, format="%Y-%m-%d")
      
    #write customer details
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
    cust_type <- input$customer_type
    pmt_type <- input$pmt_type
    pmt_full <- input$pmt_full
    
    #order summary
    order_summary <- data.frame(
               Invoice_Number = invoice_number,
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
               Sales_Rep = sales_rep,
               Delivery_Type = cust_type,
               Payment_Type = pmt_type,
               Payment_Full = pmt_full)
    
    app(order_summary, "order_summary")
    
    #order detail
    orders_df$Invoice_Number <- invoice_number 
    app(orders_df, "order_detail")
    
    #update inventory
    for(i in seq(1:nrow(orders_df))){
      inventory_num <- orders_df$Inventory_Number
      quantity <- orders_df$Quantity
      
      qry(paste0("UPDATE inventory 
          SET In_Stock = In_Stock - ",quantity,"
          WHERE `Item.` = '",inventory_num,"'"))
      
      qry(paste0("UPDATE inventory 
          SET Available = Available - ",quantity,"
          WHERE `Item.` = '",inventory_num,"'"))
    }
    #clear order
    order_nm <- input$order_name
    sales_rep <- input$sales_rep
    
    qry(paste0("delete from orders where Order_Name ='",order_nm,"'
               and Sales_Rep = '",sales_rep,"'"))
    
    curr_orders[[sales_rep]] <<- curr_orders[[sales_rep]][curr_orders[[sales_rep]] != order_nm]
    updateSelectInput(session,"order_name", choices=curr_orders[[sales_rep]])
    
    removeModal()
  })
  
  #Swap to order tab when item added
  observeEvent(input$inTabset,{
    if(input$inTabset == "panel_complete"){
      df <- qry("select Invoice_Number, Customer, Order_Date,
          Total,	Amount_Collected,	Outstanding,
          Sales_Rep,	Delivery_Type from order_summary")
      
      # df <- print(xtable(df), 
      #       type="html", html.table.attributes="")
      # 
      # actionButton("do", "Click Me")
      
      output$completed_orders <- renderTable(df)
    }
  })
  
  #Inventory Tab
  observeEvent(input$inTabset,{
    if(input$inTabset == "panel_inventory"){
      output$inventory_table <- renderTable(qry("select * from inventory"))
    }
  })
  
  #Inventory Modal
  observeEvent(input$input_inventory,{
    showModal(inventory_modal())
  })
  inventory_modal <- function(){
    modalDialog(size='l',
                title = "Upload Inventory",
                fluidRow(
                  fileInput("file1", "Choose Inventory File:",
                            accept = c(".xls", "xlsx",".csv")
                  ),
                  tableOutput("contents")
                ),
                footer = tagList(
                  actionButton("submit_order", "Submit Order"),
                  modalButton("Close")
                )
    )
  }
  output$contents <- renderTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    # file_path <- paste(inFile$datapath, ".csv", sep="")
    # file.rename(inFile$datapath, file_path)
   z <- read.csv(inFile$datapath)
   app(z, "inventory")
   return(z)
    
  })
  
}

shinyApp(ui, server)