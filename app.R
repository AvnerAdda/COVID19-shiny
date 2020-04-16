library(shiny)
library(tidyverse)
library(shinydashboard)
library(RCurl)
library(leaflet)
library(dplyr)
#
df_global_case <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
df_us_case <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
df_us_case <- subset(df_us_case, select = -c(UID,iso2,iso3,code3,FIPS,Admin2,Combined_Key))
df_us_case <- df_us_case %>%
  rename(
    Province.State = Province_State,
    Country.Region = Country_Region,
    Long = Long_
  )
df_global_case <- df_global_case[!(df_global_case$Country.Region=="US"),]
df_global_case <- rbind(df_global_case,df_us_case)

ui <- bootstrapPage(
  tags$style(
    includeCSS("./COVID19/COVID19/styles.css"),
  ),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(id = "controls",class = "panel panel-default", fixed = TRUE,
               draggable = TRUE, top = 10, right = 10,
               width = 350, height = "auto",
      selectInput("select", label = "Select country",
                  choices = unique(df_global_case['Country.Region'])),
      uiOutput('variables'),
      plotOutput("evolution", height = 200)
      # plotOutput("scatterCollegeIncome", height = 250)
  )
)

server <- function(input, output, session) {
  df_global_case2 <- reactive({
    df_global_case2 <- (df_global_case %>% filter(Country.Region == input$select))
    df_global_case2
  })

  filteredData <- reactive({
    df_global_case2 <- df_global_case[,c("Province.State","Country.Region","Lat","Long",names(df_global_case[ncol(df_global_case)]))]  #%>%
      # group_by(Country.Region, Province.State) %>%
      # summarise_each(funs(sum))
    colnames(df_global_case2) <- c('State','Country','Lat','Long','Cases')
    df_global_case2
  })

  output$map <- renderLeaflet({
    leaflet(filteredData()) %>% addTiles() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(30, 40, 4) #%>%
      # fitBounds(~min(Long), ~min(Lat), ~max(Long), ~max(Lat))
  })

  output$value <- renderPrint({ df_global_case2() })

  colorpal <- reactive({
    colorNumeric('YlOrRd', df_global_case[ncol(df_global_case)])
  })

  output$variables = renderUI({
    conditionalPanel(
      condition = "all(df_global_case2()['Province.State'] == '')",
      selectInput("select2", label = "Select region",
                  choices = df_global_case2()['Province.State'], selected = NULL)
    )
  })

  observe({
    pal <- colorpal()
    leafletProxy("map", data = filteredData()) %>%
      clearShapes()  %>%
      addCircles(radius = ~log10(Cases)*2*10e3,fillColor = ~pal(Cases),
                 fillOpacity = 0.7, popup = ~paste(Country, ' (', State,'): ', Cases, ' cases', sep = '')
      )
  })

  observe({
    proxy <- leafletProxy("map", data = filteredData())

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    pal <- colorpal()
    proxy %>% addLegend(position = "bottomright",
                        pal = pal, values = ~Cases)
  })

  # observeEvent(input$mymap_marker_click, {
  #   p <- input$mymap_marker_click  # typo was on this line
  #   print(p)
  # })
}

shinyApp(ui, server)