# Bojan Duric
# server side to pull and manipulate date
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(rbokeh)
library(leaflet)
library(DT)
library(shiny)

shinyServer(function(input, output) {

# my functions
formatMoney  <- function(x, ...) {
  paste0("$", formatC(as.numeric(x), format="f", digits=0, big.mark=","))
}

formatPercent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

### read data

sales <- readRDS('sales_source.rds') %>% mutate(sales_date = as.Date(sales_date, format = "%m/%d/%Y")) %>%
  select(-customer_zip)

quotes <- readRDS('quotes_source.rds')%>% mutate(close_date = as.Date(close_date, format = "%m/%d/%Y"),
                                                                                                   create_date = as.Date(create_date, format = "%m/%d/%Y"))

goals <- readRDS('goals_source.rds')%>% mutate(sales_date = as.Date(sales_date, format = "%m/%d/%Y"))

customers <- readRDS('customers.rds')%>% mutate(date_open = as.Date(date_open, format = "%m/%d/%Y"))


#FILTERS
s.date <- reactive({ as.Date(input$s.date, format='%Y-%m-%d')})
e.date <- reactive({ as.Date(input$e.date, format='%Y-%m-%d')})


# month name
r.month_name <- reactive({
  ifelse(
    lubridate::floor_date(s.date(), 'month')==lubridate::floor_date(e.date(), 'month'),
    paste0(lubridate::month(e.date(), label = TRUE, abbr = TRUE)),
    paste(lubridate::month(s.date(), label = TRUE, abbr = TRUE),
          lubridate::month(e.date(), label = TRUE, abbr = TRUE), sep="-"))
})
# date range data by department and category

#df.m.sales <- sales %>% filter(floor_date(sales_date, 'month') == floor_date(r.date, 'month')) 

m.sales <- reactive({
  sales %>% filter(sales_date >= s.date(), sales_date <= e.date()) %>%
  #group_by(my_month=floor_date(sales_date, 'month')) %>%
  dplyr::summarise( Sales = sum(sales, na.rm = TRUE),
                    No.ofCustomers = n_distinct(customer, na.rm = TRUE),
                    No.ofDeals = n_distinct(order_id, na.rm = TRUE))
})
#df.m.closed_quotes <- quotes %>% filter(floor_date(close_date, 'month') == floor_date(r.date, 'month'))

m.closed_quotes <-  reactive({
  quotes %>% filter(close_date >= s.date(), close_date <= e.date()) %>% 
  #group_by(my_month=floor_date(close_date, 'month')) %>%
  dplyr::summarise( Sales = sum(sales, na.rm = TRUE),
                    No.ofCustomers = n_distinct(customer, na.rm = TRUE),
                    No.ofQuotes = n_distinct(quote_id, na.rm = TRUE))
})

#df.m.open_quotes <- quotes %>% filter(floor_date(create_date, 'month') == floor_date(r.date, 'month'))

m.open_quotes <- reactive({
  quotes %>% filter(create_date >= s.date(), create_date <= e.date()) %>% 
  #group_by(my_month=floor_date(create_date, 'month')) %>%
  dplyr::summarise( Sales = sum(sales, na.rm = TRUE),
                    No.ofCustomers = n_distinct(customer, na.rm = TRUE),
                    No.ofQuotes = n_distinct(quote_id, na.rm = TRUE))
})

#df.m.goals <- goals %>% filter(floor_date(sales_date, 'month') == floor_date(r.date, 'month'))

m.goals <- reactive({
  goals %>% filter(sales_date >= floor_date(s.date(), 'month'), 
                   sales_date <= e.date()) %>% 
  #group_by(my_month=floor_date(sales_date, 'month')) %>%
  dplyr::summarise( Goal = sum(goal, na.rm = TRUE))
})
#####################################################################################################
#                                                                                                   #
#                                    INFO BOXES                                                     #
#                                                                                                   #
#####################################################################################################


################################# DATE RANGE ########################################################

## Bookings Month ValueBox
output$bookingsMonthBox<-renderInfoBox({
  
  infoBox(value = formatMoney(m.sales()$Sales), 
           title = paste0("Bookings in ", r.month_name()),
           subtitle = paste0("Booked vs ", formatMoney(m.goals()$Goal), " goal"),
           icon = icon("usd"),
           color = ifelse(m.sales()$Sales < m.goals()$Goal, "orange", "blue")
  )
  })

## Bookings progress to Goal ValueBox
output$bookingsProgressBox<-renderInfoBox({
  
  infoBox(value = formatPercent(m.sales()$Sales/m.goals()$Goal),  
          title = 'Progress toward Goal',
          subtitle = "Percent of Goal",
          icon = icon("percent")
          )
  
})

## Month No. of Deals ValueBox
output$dealsMonthBox<-renderInfoBox({
  
  infoBox(value = formatC(m.sales()$No.ofDeals, format="f", digits=0, big.mark=","),
          title = "Number of Deals",
          subtitle = paste0("Number of Deals in ", r.month_name()),
          icon = icon("file-text-o"))
  
})

## Month No. of Customers ValueBox
output$customersMonthBox<-renderInfoBox({
  
  infoBox(value = formatC(m.sales()$No.ofCustomers, format="f", digits=0, big.mark=","),
          title = "Number of Customers",
          subtitle = paste0("Number of Customers in ", r.month_name()),
          icon = icon("users"))
  
})

## Quotes Closing Month ValueBox
output$quotesClosingMonthBox<-renderInfoBox({
  
  infoBox(value = formatMoney(m.closed_quotes()$Sales),
          title = "Opportunities Closing", 
          subtitle = paste0("Value with Close Date in ", r.month_name()),
          icon = icon("filter"))
  
})

## Quotes Closing Month ValueBox
output$quotesCreatedMonthBox<-renderInfoBox({
  
  infoBox(value = formatMoney(m.open_quotes()$Sales),
          title = "Opportunities Created",
          subtitle = paste0("Value of Created Opportunities in ", r.month_name()),
          icon = icon("plus"))
  
})

## Month No. of Quotes Closing ValueBox
output$numClosedQuotesMonthBox<-renderInfoBox({
  
  infoBox(value = formatC(m.closed_quotes()$No.ofQuotes, format="f", digits=0, big.mark=","), 
          title = "Opportunities Closing",
          subtitle = paste0("Number of Opportunities Closing in ", r.month_name()),
          icon = icon("hashtag"))
  
})

## Month No. of Quotes Created ValueBox
output$numOpenedQuotesMonthBox<-renderInfoBox({
  
  infoBox(value = formatC(m.open_quotes()$No.ofQuotes, format="f", digits=0, big.mark=","), 
          title = "Opportunities Created",
          subtitle = paste0("Number of Opportunities Created in ", r.month_name()),
          icon = icon("hashtag"))
  
})

#####################################################################################################
#                                                                                                   #
#                               GRAPHS                                                              #
#                                                                                                   #
#####################################################################################################
#
# group by month 
monthlySales <- reactive({
  sales %>% filter(sales_date >= s.date(), sales_date <= e.date()) %>%
  group_by(Month = floor_date(sales_date, 'month')) %>%
  summarise(Sales = sum(sales))
})

ttmSales <- reactive({
  sales %>% filter(floor_date(sales_date, 'month') >= floor_date(ymd(e.date())-years(1), 'month'), 
                   floor_date(sales_date, 'month') <= floor_date(e.date(), 'month')-1) %>%
    group_by(Month = floor_date(sales_date, 'month')) %>%
    summarise(ttm_Sales = sum(sales)) %>%
    mutate(Month_Num = month(Month))
})

monthlyGoal <- reactive({
  goals %>% filter(year(sales_date) == year(e.date())) %>%
  group_by(Month = floor_date(sales_date, 'month')) %>%
  summarise(Goal = sum(goal))
})

# group by month 
# monthlySales <- sales %>% filter(year(sales_date) == year(r.date)) %>%
#   group_by(Month = month(sales_date)) %>%
#   summarise(Sales = sum(sales))
# 
# monthlyGoal <- goals %>% filter(year(sales_date) == year(r.date)) %>%
#   group_by(Month = month(sales_date)) %>%
#   summarise(Goal = sum(goal))

graphMonthData <- reactive({
  left_join(monthlyGoal(), monthlySales(), by = "Month") %>% 
    mutate(Month_Num = month(Month)) %>%
  left_join(ttmSales(), by = "Month_Num")
})
###
### Cumulative Bookings Graph
# lmResults <- reactive({
#   regress.exp <- "Sales~Month_Num"
#   lm(regress.exp, data = graphMonthData())
# })


output$cumBookings <- renderRbokeh({
figure(width = 600, height = 350, legend_location = "top_left") %>% 
  ly_lines(Month_Num , cumsum(Goal), data= graphMonthData() , legend = 'goal', color = 'red') %>%
  ly_lines(Month_Num , cumsum(ttm_Sales), data= graphMonthData() , legend = 'TTM', color = 'blue') %>%
  ly_points(Month_Num, cumsum(Sales), data= graphMonthData(),
            hover = c(Month = month.abb[Month_Num], Sales = formatMoney(Sales), Goal = formatMoney(Goal))) %>%
  #ly_abline(lmResults(), type = 2, legend = "trajectory", width = 2) %>%
  x_axis(label = 'Month of Year',  grid = FALSE) %>%
  y_axis(label = 'Cumulative Bookings', number_formatter = 'numeral', format = '$0,000') 
})

###
### Monthly Bookings and Goal Graph

graphMonthly <- reactive({graphMonthData() %>% gather(key = type, value = sales, Sales, Goal)})

output$monthlyBookings <- renderRbokeh({
figure(width = 600, height = 350, legend_location = "top_left") %>% 
  ly_bar(Month.x, sales, data= graphMonthly(), color = 'type', hover = TRUE, position = 'dodge') %>%
  x_axis(label = "Date", grid = FALSE) %>%
  y_axis(label = 'Monthly Bookings & Goals', number_formatter = 'numeral', format = '$0,000') %>%
  theme_axis('x', major_label_orientation = 45)
})
###
### Monthly Quotes Closing Graph

monthlyClosedQuotes <- reactive({
  quotes %>% filter(close_date >= s.date(), close_date <= e.date()) %>%
  group_by(my_month=floor_date(close_date, 'month'))
})

output$scatterQuotes <- renderRbokeh({
figure(width = 600, height = 350, legend_location = "top_left") %>% 
  ly_points(close_date, sales, data= monthlyClosedQuotes(), size = Probability/8,
            hover= c( Close_Date = close_date, Sales = formatMoney(sales), 
                      Created_by = sales_rep, Quote_Age = close_date - create_date,
                      WinProbability = Probability)) %>%
  x_axis(label = "Date", grid = FALSE) %>%
  y_axis(label = 'Quote Value', number_formatter = 'numeral', format = '$0,000') 
})  

###
### Geo Map Sales Graph

geoCustomerSales <- reactive({
  sales %>% filter(sales_date >= s.date(), sales_date <= e.date()) %>%
  group_by(customer, Month = floor_date(sales_date, 'month')) %>%
  summarise(Sales = sum(sales, na.rm = TRUE),
            Avg.Sales = mean(sales, na.rm = TRUE),
            No.ofOrders = n_distinct(order_id)) %>%
    left_join(customers, by = c('customer'= 'Customer'))
})
customer_popup <- reactive({
  paste0("<strong>Customer: </strong>", 
                         geoCustomerSales()$customer, 
                      "<br><strong>Sales: </strong>", 
                      formatMoney(geoCustomerSales()$Sales))
})

### Sales Map
output$mapSales <- renderLeaflet({
leaflet(geoCustomerSales()) %>% 
  setView(lng = -98.585522, lat =  39.8333333, zoom = 4) %>%
  addTiles() %>%
  addCircles(lng = ~longitude, lat = ~latitude, weight = 1, fillOpacity = 0.5,
             radius = ~sqrt(Sales)*100, popup = ~customer_popup())
})
### Customers Clusters
output$mapCustomersCluster <- renderLeaflet({
leaflet(geoCustomerSales()) %>% 
  setView(lng = -98.585522, lat =  39.8333333, zoom = 4) %>%
  addTiles() %>%
  addCircleMarkers(lng = ~longitude, lat = ~latitude, weight = 1, fillOpacity = 0.5,
                  clusterOptions = markerClusterOptions(), popup = ~customer_popup())
})
###
### Geo Map Quotes Graph

geoCustomerQuotes <- reactive({
  quotes %>% filter(close_date >= s.date(), close_date <= e.date()) %>%
  group_by(customer, Month = floor_date(close_date, 'month')) %>%
  summarise(Sales = sum(sales, na.rm = TRUE),
            Avg.Sales = mean(sales, na.rm = TRUE),
            No.ofOrders = n_distinct(quote_id)) %>%
    left_join(customers, by = c('customer'= 'Customer'))
})


state_popup <- reactive({
  paste0("<strong>Customer: </strong>", 
                      geoCustomerQuotes()$customer, 
                      "<br><strong>Quotes Value: </strong>", 
                      formatMoney(geoCustomerQuotes()$Sales),
                      "<br><strong>No. of Quotes: </strong>", 
                      formatC(geoCustomerQuotes()$No.ofOrders, format="f", digits=0, big.mark=","))
})

output$mapQuotes <- renderLeaflet({
leaflet() %>% 
  setView(lng = -73.9, lat =  40.8, zoom = 6) %>%
  addTiles() %>%
  addCircles(data = geoCustomerQuotes(), 
             lng = ~longitude, lat = ~latitude, weight = 1, fillOpacity = 0.5,
              radius = ~Sales*8,
              popup=~state_popup())
})

#####################################################################################################
#                                                                                                   #
#                                TOP N                                                              #
#                                                                                                   #
#####################################################################################################
#

## Top Orders
top_orders <- reactive({
  sales %>% filter(sales_date >= s.date(), sales_date <= e.date()) %>%
    filter(rank(desc(sales))<=input$topNinput) %>% arrange(desc(sales))
})
output$topDeals <-DT::renderDataTable({
DT::datatable(top_orders(), colnames = c('Date', 'Order Id', 'Department', 'Category', 'Customer', 'Sales Rep', 'Sales'),
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'excel', 'pdf', 'print'))) %>% 
  formatCurrency(7, digits = 0)
})

## Top Sales Reps
top_salesReps <- reactive({
  sales %>% filter(sales_date >= s.date(), sales_date <= e.date()) %>% 
    group_by(sales_rep, sales_department) %>%
  summarise(Sales = sum(sales, na.rm = TRUE),
            No.of.Orders = n_distinct(order_id, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(rank(desc(Sales))<=input$topNinput) %>% arrange(desc(Sales))
})
output$topReps <-DT::renderDataTable({
DT::datatable(top_salesReps(), colnames = c('Sales Rep', 'Department', 'Sales', 'No. of Orders'),
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'excel', 'pdf', 'print'))) %>% 
  formatCurrency(3, digits = 0) %>% formatRound('No.of.Orders', 0)
})
## Top Customers
top_customers <- reactive({
  sales %>% filter(sales_date >= s.date(), sales_date <= e.date()) %>% 
    group_by(customer, sales_rep, sales_department) %>%
  summarise(Sales = sum(sales, na.rm = TRUE),
            No.of.Orders = n_distinct(order_id, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(rank(desc(Sales))<=input$topNinput) %>% arrange(desc(Sales))
})
output$topCustomers <-DT::renderDataTable({
DT::datatable(top_customers(), colnames = c('Customer', 'Sales Rep', 'Department', 'Sales', 'No. of Orders'),
              extensions = 'Buttons', options = list(
  dom = 'Bfrtip',
  buttons = c('copy', 'excel', 'pdf', 'print'))) %>% 
  formatCurrency(4, digits = 0) %>% formatRound('No.of.Orders', 0)
})

#####################################################################################################
#                                                                                                   #
#                                   CUSTOMER RFM                                                    #
#                                                                                                   #
#####################################################################################################
#

mostRecentDate<-max(sales$sales_date)
rfmSales <-sales %>% group_by(customer) %>%
  dplyr::summarise(Recency = round(as.numeric(difftime(mostRecentDate, max(sales_date, na.rm=TRUE), units='days')), 0),
                   Frequency = n_distinct(order_id, na.rm=TRUE),
                   Monetary = sum(sales, na.rm=TRUE),
                   LastPurchaseDate = max(sales_date, na.rm=TRUE)) 

rfmSales$Rscore <- findInterval(rfmSales$Recency, quantile(rfmSales$Recency, c(0.0, 0.25, 0.50, 0.75, 1.0)))
rfmSales$Fscore <- findInterval(rfmSales$Frequency, quantile(rfmSales$Frequency, c(0.0, 0.25, 0.50, 0.75, 1.0)))
rfmSales$Mscore <- findInterval(rfmSales$Monetary, quantile(rfmSales$Monetary, c(0.0, 0.25, 0.50, 0.75, 1.0)))

#adjust Frequency score in reverse order since least days is best
rfmSales$Rscore <- abs(rfmSales$Rscore-6)

rfmSales <- rfmSales[with(rfmSales, order(-Mscore, -Fscore, -Rscore)), ]

rfmSales$RFMscore <- paste(rfmSales$Rscore, rfmSales$Fscore, rfmSales$Mscore, sep="-")

# add descriptive segments based on score
rfmSales$RscoreDescription <- ifelse(rfmSales$Rscore > 3, '', 
                                     ifelse(rfmSales$Rscore == 3, 'Loosing', 
                                            ifelse(rfmSales$Rscore == 2, 'Almost Lost', 'Lost')))

rfmSales$FscoreDescription <- ifelse(rfmSales$Fscore > 3, 'Loyal', 
                                     ifelse(rfmSales$Fscore == 3, 'Occasional Buyer', 'One Timer'))

rfmSales$MscoreDescription <- ifelse(rfmSales$Mscore == 5, 'Big Spenders', 
                                     ifelse(rfmSales$Mscore == 4, 'Spenders', 
                                            ifelse(rfmSales$Mscore == 3, 'Moderate Spenders', 
                                                   ifelse(rfmSales$Mscore == 2, 'Small Buys','Cheap'))))

rfmSales$RFMscoreDescription <-paste(rfmSales$RscoreDescription, rfmSales$FscoreDescription, rfmSales$MscoreDescription, sep=" ")

# RFM Tables

output$rfmTable <-DT::renderDataTable({
DT::datatable(rfmSales[, c(1, 6:9, 13, 5, 3:4 )], filter = 'top',
              colnames = c('Customer', 'R Score', 'F Score', 'M Score', 'R-F-M', 
                           'RFM Description','Last Purchase Date', 'No. of Orders', 'Sales'),
              extensions = 'Buttons', options = list(
                dom = 'Bfrtip',
                buttons = c('copy', 'excel', 'pdf', 'print'))) %>% 
  formatCurrency(9, digits = 0)
})

# bar chart by number of custoemrs
rfmChartData <- rfmSales %>% group_by(RFMscore) %>% summarise(customers = n_distinct(customer, na.rm=TRUE))

output$rfmChart <- renderRbokeh({
figure(width = 1600, legend_location = "top_left") %>% 
  ly_bar(RFMscore, customers, data= rfmChartData, hover = TRUE, position = 'dodge') %>%
  x_axis(label = "RFM", grid = FALSE) %>%
  y_axis(label = 'Number of Customers', number_formatter = 'numeral', format = '0,000') %>%
  theme_axis('x', major_label_orientation = 45)
})


}) #shinyServer