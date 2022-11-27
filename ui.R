#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(shinydashboard)
library(flexdashboard)
library(plotly)
library(shinydashboardPlus)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "box.css")
  ),
  actionButton("s", "", icon = icon("camera"), lib = "font-awesome", 
               style = "position: absolute; top: 5px; right: 25px; z-index:10000;"),
  navbarPage("Poppys",
             theme = shinytheme("flatly"),
             tags$head(tags$style(HTML('.navbar-static-top {background-color: #325fab;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: #325fab;}'))),
                  tabPanel("Costs",
                            sidebarLayout(
                              sidebarPanel(
                               tags$style(".well {background-color: #325fab; color: #f4f4f4;}"),
                               div(img(src = "logo.png", width = '95%'),style ="text-align: center;"),
                               br(),
                               br(),
                               width = 2,
                               selectizeInput("year_cost", "Year", choices = c("2021" = 2021, "2022" = 2022), selected = 2022, multiple = TRUE,options = NULL),
                               checkboxGroupInput("month_cost",
                                                  "Months:",
                                                  month)

                             ),
                             mainPanel(
                               tabBox(title = HTML("<h4>Weather</h4><br>"), 
                                      width = 12,
                                      tags$style(".small-box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }"),
                                      
                                      fluidRow(
                                        column(
                                          width = 7,
                                          radioButtons("plot", "Select graph:",
                                                       choices = list("Rain" = 'rain', "Humidity" = 'humidity')),
                                          conditionalPanel(
                                            condition = "input.plot == 'humidity'",
                                            tags$p(strong("Average Humidity per Day", style = "font-size:18px;color:#585a58")),
                                            plotlyOutput("humidity_plot",height = 250)
                                          ),
                                          conditionalPanel(
                                            condition = "input.plot == 'rain'",
                                            tags$p(strong("Overall Count of Rainy Days", style = "font-size:18px;color:#585a58")),
                                            plotlyOutput("rain_plot",height = 250)
                                          )
                                        ),
                                        column(
                                          width = 5,
                                          fluidRow(
                                            align = 'center',
                                            valueBoxOutput("humidity_value"),
                                            valueBoxOutput("rainy_value"),
                                            valueBoxOutput("sunny_value")
                                          ),
                                          fluidRow(
                                            align = 'center',
                                            DT::dataTableOutput("table")
                                          
                                          #)
                                          )
                                        )
                                      )
                                      ),
                               tabBox(title = HTML("<h4>Costs</h4><br>"), 
                                      width = 12,
                                      fluidRow(
                                        column(
                                          width = 7,
                                          tags$p(strong("Inflation Percentage Change", style = "font-size:18px;color:#585a58")),
                                          plotlyOutput("inflation_plot",height = 320),
                                        ),
                                        column(
                                          width = 5,
                                          valueBoxOutput("electricity_price"),
                                          valueBoxOutput("gas_price"),
                                          valueBoxOutput("water_price")
                                        )
                                        )
                                      )
                                  )
                            )
                           ),
                           
                           # fluidRow(
                           # #box(title = HTML("<h4>Weather</h4><br>"), solidHeader = TRUE) #,side = 'left'),
                           #   #plotlyOutput(" inflation_plot",height = 420),
                           #  tabBox(title = HTML("<h4>Weather</h4><br>"), 
                           #         width = 9,
                           #         side = 'left',
                           #         footer = 
                           #           fluidRow(
                           #             column(
                           #               width = 6,
                           #                tags$p(strong("Inflation Percentage Change", style = "font-size:18px;color:#585a58")),
                           #                plotlyOutput("inflation_plot",height = 420)
                           #             ),
                           #             column(
                           #               width = 3,
                           #               valueBoxOutput("humidity_value"),
                           #               valueBoxOutput("rain_value"),
                           #               valueBoxOutput("sunny_value"),
                           #             )
                           #           )
                           #        
                           #         )
                           # ),
                           # fluidRow(
                           #   tabBox(title = HTML("<h4>Costs</h4><br>"), 
                           #          side = 'left',
                           #          footer = 
                           #            fluidRow(
                           #              column(
                           #                width = 6,
                           #                tags$p(strong("Inflation Percentage Change", style = "font-size:18px;color:#585a58")),
                           #                plotlyOutput("inflation_plot",height = 420)
                           #              )
                           #              
                           #            )
                           #          )
                           #box(title = HTML("<h4>Costs</h4><br>")) #,side = 'left')
                           #)
    #                       ),
                  tabPanel("KPIs",
                             sidebarLayout(
                               sidebarPanel(
                                 tags$style(".well {background-color: #325fab; color: #f4f4f4;}"),
                                 div(img(src = "logo.png", width = '95%'),style ="text-align: center;"),
                                 br(),
                                 br(),
                                 width = 2,
                                 selectizeInput("year_kpi", "Year", choices = c("2021" = 2021, "2022" = 2022), selected = 2022, multiple = TRUE,options = NULL),
                                    checkboxGroupInput("month_kpi",
                                                       "Months:",
                                                       month)

                             ),
                            mainPanel(
                                   fluidRow(
                                     column(
                                       width = 1
                                      ),
                                     column(
                                       width = 8, 
                                       fluidRow(
                                         align = 'center',
                                         valueBoxOutput("kpi_total_purchase", width = 4),
                                         valueBoxOutput("kpi_total_visitor", width = 4),
                                         valueBoxOutput("kpi_total_budget", width = 4)
                                       
                                       )
                                     ),
                                     column(
                                       width = 2
                                     )
                                   ),
                                   fluidRow( align="center", 
                                     column(
                                       offset = 1,
                                            width = 2,
                                            tags$p(strong("Average Bounce Rate", style = "font-size:15px;color:#585a58;text-align:center;")),
                                            flexdashboard::gaugeOutput("bounce_rate")
                                            ),
                                     column(width = 2, 
                                            tags$p(strong("Average Write Offs", style = "font-size:15px;color:#585a58")),
                                            flexdashboard::gaugeOutput("write_offs")
                                     ),
                                     column(width = 2, 
                                            tags$p(strong("Total Conversion", style = "font-size:15px;color:#585a58")),
                                            flexdashboard::gaugeOutput("conversion")
                                     ),
                                     column(width = 2, 
                                            tags$p(strong("CAC", style = "font-size:15px;color:#585a58")),
                                            flexdashboard::gaugeOutput("cac")
                                     )
                                   ),
                                   fluidRow(
                                     column(
                                       width = 5, 
                                       tags$p(strong("Google Trends", style = "font-size:18px;color:#585a58")),
                                       plotlyOutput("gtrends_plot",height = 420)
                                     ),
                                     column(
                                       width = 5, 
                                       tags$p(strong("Flower Price Difference", style = "font-size:18px;color:#585a58")),
                                       plotlyOutput("price_plot",height = 420)
                                       #plotlyOutput("kpi_plot",height = 420)
                                     )
                                   )
                                   )
                            )
                  ),
                 tabPanel("Projections",
                          sidebarLayout(
                            sidebarPanel(
                              tags$style(".well {background-color: #325fab; color: #f4f4f4;}"),
                              div(img(src = "logo.png", width = '95%'),style ="text-align: center;"),
                              br(),
                              br(),
                              width = 2,
                              selectizeInput("year_cost", "Year", choices = c("2021" = 2021, "2022" = 2022), selected = NULL, multiple = TRUE,options = NULL),
                              checkboxGroupInput("month",
                                                 "Months:",
                                                 month)
                              
                            ),
                            mainPanel(
                              tags$p(strong("Market Price Forecast (Euros)", style = "font-size:18px;color:#585a58")),
                              div(img(src = "projection.png"),style ="text-align: center;"),
                            
                            )
                          )
                 )
  )
)
                          
                          

