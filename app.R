# Required Libraries ------------------------------------------------------
library(tidyverse) # The gas to the caR
library(shiny) # Creating shiny app
library(shinyWidgets) # Extra widgets
library(leaflet) # Mapping
library(plotly) # Interactive charts
library(RColorBrewer) # Color palettes
library(shinythemes) # Themes

data_tao <- read_csv("tao.csv")

# Define UI ---------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("cerulean"),
                shinyUI(navbarPage
                        ("Koh Tao - Coral Health",
                          tabPanel("Overview",
                                   column(4, 
                                          wellPanel( 
                                            h4("Dive Site"),
                                            pickerInput("reef", options = list(`actions-box` = TRUE),
                                                        choices = NULL, multiple = TRUE, selected = NULL),
                                            h4("Coral Type"),
                                            radioButtons("coral_type", label = NULL,
                                                         c("Boulder" = "Boulder",
                                                           "Branching" = "Branching",
                                                           "Plate" = "Plate",
                                                           "Soft" = "Soft"), 
                                                         selected = "Boulder"),
                                            h4("Period"),
                                            helpText("Click and drag your selected range to observe changes over the periods"),
                                            sliderInput("year", label = NULL,
                                                        min = 0,
                                                        max = 0,
                                                        value = c(0, 0),
                                                        step = 100,
                                                        timeFormat = "%Y-%m"),
                                            h4("Choose Data to Plot"),
                                            radioButtons("data_type", label = NULL,
                                                         c("Average Lightest Score" = "avg_lightest",
                                                           "Average Darkest Score" = "avg_darkest"),
                                                         selected = "avg_lightest")
                                          ),
                                          tags$style(type = "text/css", "#graph1 {height: calc(37vh) !important;}"),
                                          plotlyOutput("graph1")
                                   ),
                                   mainPanel(
                                     tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                     leafletOutput("map", width = "100%")
                                   )),
                          # Background Tab ----------------------------------------------------------
                          tabPanel("Background",
                                   tags$style(type = "text/css", "#tao_image img {max-width: 100%; width: 100%; height: auto}"),
                                   uiOutput("tao_image", width = "100%"),
                                   
                                   column(4, offset = 4,
                                          br(),
                                          h4("Data"),
                                          p("The data comes from", a("Coral Watch,", href = "https://coralwatch.org", target = "_blank"),
                                            "a citizen science program based at the The University of Queensland, Australia.",
                                            "Coral health is measured through a chart, with lower numbers indicating coral bleaching.",
                                            "More information about the chart and the survey methods can be found",
                                            a("here.", href = "https://coralwatch.org/index.php/monitoring/using-the-chart/", target = "_blank"),
                                            align = "justify"),
                                          p("Only data for the island of Koh Tao has been made use of in this app.", 
                                            "The data quality is pretty good for the more recent years. Data before 2014, however, is literally
                                            all over the map. Apparently, White Rock used to exist on the east coast!",
                                            align = "justify"),
                                          
                                          h4("Approach"),
                                          p("Over the years, thousands of users have surveyed and reported data for hundreds of coordinates,
                                            along with the dive site tag. Each of these coordinates are being plotted on the map, with the 
                                            intensity of the color being the", span("average scores", style = "font-weight:bold"), 
                                            "of the chosen", span("coral type", style = "font-weight:bold"), 
                                            "in the selected", span("period.", style = "font-weight:bold"),
                                            align = "justify"),
                                          p("The histogram below the sidebar presents the counts of health scores broken down to smaller ranges.",
                                            "To see the changes over the years, click and drag the",
                                            span("selected blue horizontal date bar.", style = "font-style:italic"),
                                            align = "justify"),
                                          
                                          br(),
                                          p("This dashboard was made by Harish M."),
                                          p("Feedback and suggestions are welcome @ harishnandhaa1994@gmail.com"))
                          ))))

# Server ------------------------------------------------------------------
server <- function(input, output, session) { 
  
  output$tao_image <- renderUI({
    tags$img(src = "taoImage.jpg")
  })
  
  input_reef <- reactive({
    data_tao %>% distinct(reef) %>% arrange(reef)
  })
  observe({
    updatePickerInput(session = session,
                      inputId = "reef",
                      choices = input_reef()$reef,
                      selected = input_reef()$reef)
  })
  
  input_years <- reactive({
    req(input$reef)
    input_years <- data_tao %>% filter(reef %in% input$reef) %>%
      distinct(month_year) %>% arrange(month_year)
  })
  observe({
    updateSliderInput(session = session,
                      inputId = "year",
                      min = input_years()$month_year[1],
                      max = input_years()$month_year[nrow(input_years())],
                      # value = c(input_years()$month_year[1],
                      value = c(input_years()$month_year[nrow(input_years())] - 730,
                                input_years()$month_year[nrow(input_years())]))
  })
  
  data <- reactive({
    req(input$coral_type)
    
    if (input$year[1] > 0) {
      start_month <- paste0(format(input$year[1], "%Y-%m"), "-01")
      end_month <- paste0(format(input$year[2], "%Y-%m"), "-01")
      
      data_tao %>% filter(reef %in% input$reef &
                            coral_type == input$coral_type &
                            month_year >= start_month &
                            month_year <= end_month) %>% 
        group_by(reef, longitude, latitude) %>%  
        summarize(avg_lightest = round(mean(lightest_number, na.rm = TRUE), 2),
                  avg_darkest = round(mean(darkest_number, na.rm = TRUE), 2))
    }
    else {
      # Dummy data to prevent errors
      data_tao %>% 
        group_by(reef, longitude, latitude) %>%  
        summarize(avg_lightest = 1, 
                  avg_darkest = 2)
    }
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}"
      ) %>%
      setView(lng = 99.8330,
              lat = 10.0945, zoom = 14) 
  })
  
  red_palette <- colorRampPalette(colors = c("#ffe6e6", "#4d0000"), space = "Lab")(600)
  
  observe({
    if (data() %>% nrow() > 0) {
      map_render <- leafletProxy("map", data = data()) %>%
        clearShapes() 
      if (input$data_type == "avg_lightest") {
        # Separate color palettes for dark/light for better visuals
        palette <- colorNumeric(palette = red_palette, domain = seq(1, 5, by = 0.01))
        map_render %>% addCircles(lng = ~longitude,
                                  lat = ~latitude,
                                  color =  ~palette(avg_lightest),
                                  radius = 75,
                                  stroke = FALSE,
                                  fillOpacity = 1,
                                  label = paste0(data()$reef, " - ",
                                                 data()$avg_lightest %>% round(2)))
      } else {
        
        palette <- colorNumeric(palette = red_palette, domain = seq(2, 6, by = 0.01))
        map_render %>% addCircles(lng = ~longitude,
                                  lat = ~latitude,
                                  color =  ~palette(avg_darkest),
                                  radius = 75,
                                  stroke = FALSE,
                                  fillOpacity = 1,
                                  label = paste0(data()$reef, " - ",
                                                 data()$avg_darkest %>% round(2)))
      }
    } else{
      leafletProxy("map", data = data()) %>%
        clearShapes() 
    }
  })
  
  output$graph1 <- renderPlotly({
    if (data() %>% nrow() > 0) {
      
      if (input$data_type == "avg_lightest") {
        mean_data <- mean(data()$avg_lightest, na.rm = TRUE) %>% round(2)
        plot1 <- data() %>% plot_ly(x = ~avg_lightest, type = "histogram",
                                    xbins = list(size = 30))
      } else {
        mean_data <- mean(data()$avg_darkest, na.rm = TRUE) %>% round(2)
        plot1 <- data() %>% plot_ly(x = ~avg_darkest, type = "histogram",
                                    xbins = list(size = 30))
      }
      plot1 %>% layout(xaxis = list(title = "Distribution of Coral Health Scores", range = c(1, 6)),
                       yaxis = list(title = "Observations"),
                       shapes = list(type = "line", y0 = 0, y1 = 1, yref = "paper",
                                     x0 = mean_data, x1 = mean_data, line = list(color = "red")),
                       annotations = list(x = 3, y = 10,text = paste0("Overall Average = ", mean_data),
                                          showarrow = FALSE)) %>% 
        hide_colorbar() %>% config(displayModeBar = F)
    }
  })
}

shinyApp(ui = ui, server = server)