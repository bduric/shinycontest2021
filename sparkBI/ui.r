library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(skin = "black",
                
                dashboardHeader(title = tags$a(href='http://atlantika16.com',
                                               tags$img(src='your_logo.png', height=50))
                                
                                ),
                dashboardSidebar(
                  sidebarMenu(menuItem("Welcome", tabName = 'index', icon = icon("lightbulb-o")),
                              menuItem("Sales Manager", tabName = 'salesManager', icon = icon("dollar")),
                              menuItem("Top N", tabName = 'topN', icon = icon("rocket")),
                              menuItem("Customers Manager", tabName = 'customerRFM', icon = icon("users")),
                              br(),
                              br(),
                              br(),
                              menuItem("Controls", icon = icon("filter"),
                                       menuSubItem(icon=NULL, dateInput('s.date', 'From:', value= as.Date('2017-01-01', format='%Y-%m-%d'))),
                                       menuSubItem(icon=NULL, dateInput('e.date', 'To:', value= as.Date('2017-02-06', format='%Y-%m-%d')))
                                        ),
                              br(),
                              br(),
                              br(),
                              menuSubItem('Follow us', href='https://twitter.com/bduric16', icon = shiny::icon("twitter")),
                              menuSubItem('Connect', href='https://www.linkedin.com/in/bojanduric?trk=nav_responsive_tab_profile', icon = shiny::icon("linkedin"))
                  )
                ),
                dashboardBody(
                  tags$head(includeScript("google-analytics.js")),
                  # tags$script(HTML('
                  #  $(function() {
                  #   $("body").addClass("sidebar-collapse");
                  #  })
                  # ')),
                  tabItems(
                    tabItem(tabName = 'index',
                            includeHTML("index.html")
                    ),
                    tabItem( tabName = 'salesManager',
                             fluidRow (
                               box(  status = "primary", width = 12,
                               infoBoxOutput('bookingsMonthBox', width = 3),
                               infoBoxOutput('dealsMonthBox', width = 3),
                               infoBoxOutput('bookingsProgressBox', width = 3),
                               infoBoxOutput('customersMonthBox', width = 3)
                               )
                             ),
                             
                             fluidRow(
                               tabBox(
                                 tabPanel('Cummulative Bookings vs Goal',
                                          rbokeh::rbokehOutput('cumBookings')),
                                 tabPanel('Monthly Bookings vs Goal',
                                          rbokeh::rbokehOutput('monthlyBookings'))),
                               tabBox(
                                 tabPanel('Map Sales',
                                          leaflet::leafletOutput('mapSales')),
                                 tabPanel('Customers Geo-Clusters',
                                          leaflet::leafletOutput('mapCustomersCluster')))
                             ),
                             
                             fluidRow(
                               box(  status = "warning", width = 12,
                               infoBoxOutput('quotesClosingMonthBox', width = 3),
                               infoBoxOutput('numClosedQuotesMonthBox', width = 3),
                               infoBoxOutput('numOpenedQuotesMonthBox', width = 3),
                               infoBoxOutput('quotesCreatedMonthBox', width = 3)
                               )
                             ),
                             
                             fluidRow (
                               box('Map Quotes',
                                   leaflet::leafletOutput('mapQuotes')),
                               box('Quotes by Date',
                                   rbokeh::rbokehOutput('scatterQuotes'))
                             )
                             
                    ),
                    tabItem( tabName = 'topN',
                             fluidRow(
                               box(width = 2,
                                   sliderInput("topNinput", "Enter top n:",
                                               min = 10, max = 100, value = 20
                                      )),
                                      tabBox(width = 10,
                                           tabPanel('Top Deals',
                                                      DT::dataTableOutput('topDeals')),
                                             tabPanel('Top Customers',
                                                      DT::dataTableOutput('topCustomers')),
                                             tabPanel('Top Sales Reps',
                                                      DT::dataTableOutput('topReps'))
                                        ))),
                    tabItem(tabName = 'customerRFM',
                            fluidRow(
                              box(width=12, title="No. of Customers by R-F-M segment",
                                  rbokeh::rbokehOutput('rfmChart'))
                            ),
                            fluidRow(box(width=12, title="R-F-M Table",
                              DT::dataTableOutput('rfmTable')
                            ))
                    )
                  ) #tabItems
                ) #dashBoard Body
  ) # dashBoard Page
) #shinyUI
