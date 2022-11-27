#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(flexdashboard)
library(shinyscreenshot)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$s, {
    screenshot()
  })
  
  inflation_subset <- reactive({
    s_year <- input$year_cost
    s_month <- input$month_cost
    
    if(is.null(input$year_cost)){
      s_year <- year
    }else{
      s_year <- s_year
    }
    
    if(is.null(input$month_cost)){
      s_month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    }else{
      s_month <- which(month %in% s_month)
    }
    #print(s_year)
    inflation %>%
      filter(month %in% s_month & year %in% s_year)
  })
  
  
  weather_subset <- reactive({
    s_year <- input$year_cost
    s_month <- input$month_cost
    
    if(is.null(input$year_cost)){
      s_year <- year
    }else{
      s_year <- s_year
    }
    
    if(is.null(input$month_cost)){
      s_month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    }else{
      s_month <- which(month %in% s_month)
    }
    #print(s_year)
    weather %>%
      filter(month %in% s_month & year %in% s_year)
  })
  
  weather_last_subset <- reactive({
    s_year <- input$year_cost
    s_month <- input$month_cost
    
    if(is.null(input$year_cost)){
      s_year <- year
    }else{
      s_year <- s_year
    }
    
    if(is.null(input$month_cost)){
      s_month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    }else{
      s_month <- which(month %in% s_month)
    }
    weather <- weather %>%
      filter(month %in% s_month & year %in% s_year)
    
    last_temp <- weather %>%
      select(tempmax, tempmin, temp) %>%
      tail(1) %>%
      mutate(
        tempmax = tempmax,
        temp = temp,
        tempmin = tempmin) %>%
      mutate(
        templast = runif(1,tempmin,tempmax),
      )
    colnames(last_temp) <- c('min','avg','max','last')
    
    last_humidity <- weather %>%
      select(humidity) %>%
      tail(1) %>%
      mutate(
        humiditymax = humidity + abs(rnorm(1,0,2)),
        humidity = humidity,
        humiditymin = humidity - abs(rnorm(1,0,2))) %>%
      mutate(
        humiditylast = runif(1,humiditymin,humiditymax),
      )
    
    colnames(last_humidity) <- c('min','avg','max','last')
    rbind(last_temp, last_humidity)
    
    
  })
  
  gtrends_subset <- reactive({
    s_year <- input$year_kpi
    s_month <- input$month_kpi
    
    if(is.null(input$year_kpi)){
      s_year <- year
    }else{
      s_year <- s_year
    }
    
    if(is.null(input$month_kpi)){
      s_month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    }else{
      s_month <- which(month %in% s_month)
    }
    print(s_year)
    gtrends %>%
      filter(month %in% s_month & year %in% s_year)
  })
  
  prices_subset <- reactive({
    s_year <- input$year_kpi
    s_month <- input$month_kpi
    
    if(is.null(input$year_kpi)){
      s_year <- year
    }else{
      s_year <- s_year
    }
    
    if(is.null(input$month_kpi)){
      s_month <- c(1,2,3,4,5,6,7,8,9,10,11,12)
    }else{
      s_month <- which(month %in% s_month)
    }
    print(s_year)
    prices %>%
      mutate(month = month(date),
             year = year(date)) %>%
      filter(month %in% s_month & year %in% s_year)
    
  })
  
  output$electricity_price <- renderValueBox({
    shinydashboard::valueBox(
      value = sprintf("%s", 78),
      subtitle = sprintf("ENERGY (%.1f%%)", 25),
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$gas_price <- renderValueBox({
    shinydashboard::valueBox(
      value = sprintf("%s", 60),
      subtitle = sprintf("GAS (%.1f%%)", 35.422),
      icon = icon("arrow-up"),
      color = "green"
    )
  })
  
  output$water_price <- renderValueBox({
    shinydashboard::valueBox(
      value = sprintf("%s", 10),
      subtitle = sprintf("WATER (%.1f%%)", -1.2),
      icon = icon("arrow-down"),
      color = "green"
    )
  })
  

  
  output$rainy_value <- renderValueBox({
    shinydashboard::valueBox(
      value = icon("cloud-rain", lib = "font-awesome"),
      subtitle = "RAINY",
      #icon = icon("arrow-down"),
      color = "aqua"
    )
  })
  
  output$sunny_value <- renderValueBox({
    shinydashboard::valueBox(
      value = icon("sun", lib = "font-awesome"),
      subtitle = "SUNNY",
      #icon = icon("arrow-down"),
      color = "green"
    )
  })
  
  output$humidity_value <- renderValueBox({
    shinydashboard::valueBox(
      value = sprintf("%s",last(weather_subset()$humidity)),
      subtitle = sprintf("HUMIDITY (%.1f%%)", -5.422),
      #icon = icon("arrow-down"),
      color = "green"
    )
  })
  
  output$kpi_total_purchase <- renderValueBox({
    shinydashboard::valueBox(
      value = sprintf("%s", 5680),
      subtitle = "TOTAL PURCHASE" ,#sprintf("TOTAL PURCHASE 1 (%.1f%%)", 8.9202),
      icon = icon("caret-up"),
      color = "green"
    )
  })
  
  output$kpi_total_visitor <- renderValueBox({
    shinydashboard::valueBox(
      value = sprintf("%s", 8145),
      subtitle = "TOTAL VISITOR" ,#sprintf("TOTAL PURCHASE 1 (%.1f%%)", 8.9202),
      icon = icon("caret-down"),
      color = "green"
    )
  })
  
  output$kpi_total_budget <- renderValueBox({
    shinydashboard::valueBox(
      value = sprintf("%s", 24000),
      subtitle = "TOTAL BUDGET" ,#sprintf("TOTAL PURCHASE 1 (%.1f%%)", 8.9202),
      icon = icon("caret-up"),
      color = "green"
    )
  })

  output$write_offs = flexdashboard::renderGauge({
    gauge(value = 17.98, 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(12, 100), 
                                 warning = c(12, 10),
                                 danger = c(0, 10)))
  })
  
  output$conversion = flexdashboard::renderGauge({
    gauge(value = 8.27, 
          min = 0, 
          max = 100, 
          sectors = gaugeSectors(success = c(30, 100), 
                                 warning = c(5, 30),
                                 danger = c(0, 5)))
  })
  
  
  output$bounce_rate = flexdashboard::renderGauge({
    gauge(value = 31.35, 
          min = 0,
          max = 100, 
          sectors = gaugeSectors(success = c(0, 30), 
                                 warning = c(30, 50),
                                 danger = c(50, 100)))
  })
  
  output$cac = flexdashboard::renderGauge({
    gauge(value = 4.8, 
          min = 0,
          max = 10, 
          sectors = gaugeSectors(success = c(0, 5), 
                                 warning = c(5, 6),
                                 danger = c(6, 10)))
  })
  
  # output$gauge_plot <- renderPlotly({
  #   plot_ly(
  #     type = "indicator",
  #     mode = "gauge+number",
  #     value = 31.35,
  #     title = list(text = "Avg Bounce Rate", font = list(size = 24)),
  #     #delta = list(reference = 400, increasing = list(color = "RebeccaPurple")),
  #     gauge = list(
  #       axis = list(range = list(0, 100), tickwidth = 2, tickcolor = "darkblue"),
  #       bar = list(color = "darkblue"),
  #       bgcolor = "white",
  #       borderwidth = 2,
  #       bordercolor = "gray",
  #       steps = list(
  #         list(range = c(0, 12), color = "#FF6A6A"),
  #         list(range = c(12, 15), color = "#BFEFFF"),
  #         list(range = c(15, 100), color = "#1874CD")),
  #       threshold = list(
  #         line = list(color = "navy", width = 4),
  #         thickness = 0.75,
  #         value = 99)
  #     )
  #   )
  #   
  # })
  
  output$humidity_plot <- renderPlotly({
    plot <- weather_subset() %>% 
      ggplot(aes(x= datetime, y = humidity)) +
      geom_line(color = '#325fab') +
      theme_minimal() +
      ylab("Avg Humidity") +
      xlab("Date") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12)
      ) 
    
    ggplotly(plot) %>%
      config(displayModeBar = FALSE) %>% 
      layout(legend = list(orientation = 'v', x = .9, y = 1.1, bgcolor = 'rgba(255,255,255,.3)',title=list(text='<b> Topic </b>')))
  })
  
  output$gtrends_plot <- renderPlotly({
    plot <- gtrends_subset() %>% 
      pivot_longer(cols = c(gerbera,flower), values_to = "val", names_to = "var") %>%
      ggplot(aes(x= date, y = val, fill = var)) +
      geom_area(alpha = .5) +
      scale_fill_manual(values = c("#F28E2B", "#E15759")) +
      #scale_fill_brewer(palette="Paired") +
      theme_minimal() +
      ylab("Google Interest") +
      xlab("Date") +
      theme(
        #legend.position = "bottom",
        #legend.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12)
      ) 
    
    ggplotly(plot) %>%
      config(displayModeBar = FALSE) %>% 
      layout(legend = list(orientation = 'v', x = .9, y = 1.1, bgcolor = 'rgba(255,255,255,.3)',title=list(text='<b> Topic </b>')))
  })
  
output$kpi_plot <- renderPlotly({
  plot <- kpi %>%
    select(date, conversion, write_offs, bounce_rate) %>%
    pivot_longer(cols = c(conversion, bounce_rate, write_offs), values_to = "val", names_to = "var") %>%
    ggplot(aes(x= date, y = val, group = var, color = var)) +
    geom_line() +
    scale_colour_manual(values = c("#F28E2B", "#E15759")) +
    #scale_color_brewer(palette = 'Set2') +
    theme_minimal() +
    ylab("Value") +
    xlab("Date") +
    theme(
      #legend.position = "bottom",
      #legend.title = element_text(size = 15, face = "bold"),
      axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
      axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
    )
  
  ggplotly(plot) %>%
    config(displayModeBar = FALSE)  %>% 
    layout(legend = list(orientation = 'v', x = .95, y = 1.1, bgcolor = 'rgba(255,255,255,.3)',title=list(text='<b> KPIs </b>')))
  })

output$price_plot <- renderPlotly({
  
  plot <-  prices_subset() %>% 
    select(date, difference) %>%
    #select(date, market_price, our_price) %>%
    #pivot_longer(cols = c(market_price,our_price), 
    #             values_to = "val", 
    #             names_to = "var") %>%
    ggplot(aes(x= date, y = difference)) +
    #geom_line(color = "#325fab") +
    geom_bar(stat="identity", fill = "#325fab") +
    theme_minimal() +
    ylab("Prices") +
    xlab("Date") +
    theme(
      axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
      axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
      axis.text.y = element_text(size = 12), 
      axis.text.x = element_text(size = 12)
    ) 
  
  ggplotly(plot) %>%
    config(displayModeBar = FALSE)  %>% 
    layout(legend = list(orientation = 'v', x = .95, y = 1.1, bgcolor = 'rgba(255,255,255,.3)',title=list(text='<b> Company </b>')))
})


output$rain_plot <- renderPlotly({
  
  rain_count <- weather_subset() %>%
    select(datetime, rain) %>%
    mutate(month = month(datetime), year = year(datetime)) %>%
    group_by(year, month) %>%
    summarise(count = sum(rain)) %>%
    mutate(date = paste(year,month,sep = '-')) %>%
    mutate(date = ym(date)) %>%
    select(date, count)
  
plot <-  rain_count %>% 
  ggplot(aes(x= date, y = count)) +
  geom_bar(stat="identity", fill = "#325fab") +
  theme_minimal() +
  ylab("Count") +
  xlab("Date") +
  theme(
    axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
    axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
    axis.text.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12)
  ) 

ggplotly(plot) %>%
  config(displayModeBar = FALSE)  %>% 
  layout(legend = list(orientation = 'v', x = .95, y = 1.1, bgcolor = 'rgba(255,255,255,.3)',title=list(text='<b> Company </b>')))
})

output$inflation_plot <- renderPlotly({
  
  plot <- inflation_subset() %>% 
    select(date, hipc_change_percent) %>%
    #select(date, market_price, our_price) %>%
    #pivot_longer(cols = c(market_price,our_price), 
    #             values_to = "val", 
    #             names_to = "var") %>%
    ggplot(aes(x= date, y = hipc_change_percent)) +
    #geom_line(color = "#325fab") +
    geom_bar(stat="identity", fill = "#325fab") +
    #scale_color_brewer(palette="Paired") +
    #scale_colour_manual(values = c("#F28E2B", "#E15759")) +
    theme_minimal() +
    ylab("HPIC") +
    xlab("Date") +
    theme(
      axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
      axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
      axis.text.y = element_text(size = 12), 
      axis.text.x = element_text(size = 12)
    ) 
  
  ggplotly(plot) %>%
    config(displayModeBar = FALSE) # %>% 
    #layout(legend = list(orientation = 'v', x = .95, y = 1.1, bgcolor = 'rgba(255,255,255,.3)',title=list(text='<b> Company </b>')))
})

output$table = DT::renderDataTable({
  #col_years <- colnames(df_selected())[grepl("[0-9]{4}", colnames(df_selected()))]
  datatable(weather_last_subset(), 
            colnames=c("Min", "Avg", "Max", "Last"), rownames = c("Temperature","Humidity"),
            options = list(dom = 't') 
  ) %>%
    formatRound(columns =c('min','avg','max','last'), digits = 2)
})


}
