require(magrittr)
require(rvest)
require(readxl)
require(dplyr)
require(maps)
require(ggplot2)
require(reshape2)
require(ggiraph)
require(RColorBrewer)
require(leaflet)
require(plotly)
require(geojsonio)
require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(shinythemes)

#setwd()
covid_col = "#820000"
covid_other_col = "#bf3102"


write.csv(read.csv("https://raw.githubusercontent.com/eparker12/nCoV_tracker/master/input_data/coronavirus.csv"),"input_data/coronavirus.csv", row.names = FALSE)

cv_cases = read.csv("input_data/coronavirus.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")

### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot  
      legend.position = "none"
    )
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.position = "none"
    )
  g1
}

country_cases_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, 
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.position = "none"
    )
  # ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  g1
}

country_cases_cumulative = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) + xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.position = "none"
    )
  # ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  g1
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"))  {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  g1 = g + geom_line(alpha=0.8) +
      ylab("cumulative (log10)") + theme_bw() + 
      scale_y_continuous(trans="log10") +
      scale_colour_manual(values=country_cols) +
      theme(
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        legend.position = "none"
      )
  # ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
  g1
}

### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 100th case and 10th death
cv_cases$days_since_case100 = cv_cases$days_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
  cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
  cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_100 = subset(cv_today, cases>=100)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                days_since_case100, days_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
}
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,10,50,100,500)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(0,-25,90,65) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
            title = "<small>Active cases per 100,000</small>") #%>%

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names


ui <- bootstrapPage(
  navbarPage(theme = shinytheme("superhero"), collapsible = TRUE, position = 'static-top',
             "COVID-19", id="nav",
             tabPanel("",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 20, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(h3(htmlOutput("reactive_case_count"), align = "right"), style="color:#bf0202"),
                                        span(h3(textOutput("reactive_death_count"), align = "right"), style="color:#ff0004"),
                                        span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                        span(h6(textOutput("clean_date_reactive"), align = "right"), style="color:#000000"),
                                        span(h6(textOutput("reactive_country_count"), align = "right"), style="color:#000000"),
                                        plotOutput("epi_curve", height="130px", width="100%"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderInput("plot_date",
                                                    label = h5("Select mapping date"),
                                                    min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                    max = as.Date(current_date,"%Y-%m-%d"),
                                                    value = as.Date(current_date),
                                                    timeFormat = "%d %b", 
                                                    animate=animationOptions(interval = 2000, loop = FALSE))
                          ),
                          absolutePanel(id = "controls2", class = "panel panel-default",
                                        top = 80, right = 20, width = 300, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        # span(h4(textOutput("country_clicked_text"), align = "center"), style="color:#000000"),
                                        pickerInput("level_select", "Level:",
                                                    choices = c("Global", "Continent", "Country"),
                                                    selected = c("Country"),
                                                    multiple = FALSE),

                                        pickerInput("region_select", "Country/Region:",
                                                    choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country),
                                                    options = list('actions-box' = TRUE, 'none-selected-text' = "Please make a selection!"),
                                                    selected = cv_today_100$country,
                                                    multiple = TRUE),

                                        pickerInput("outcome_select", "Outcome:",
                                                    choices = c("Cases", "Deaths"),
                                                    selected = c("Cases"),
                                                    multiple = FALSE),

                                        pickerInput("start_date", "Plotting start date:",
                                                    choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"),
                                                    options = list('actions-box' = TRUE),
                                                    selected = "Date",
                                                    multiple = FALSE),
                                        plotOutput("country_plot",height="130px", width="100%"),
                                        plotOutput("country_plot_cumulative",height="130px", width="100%"),
                                        plotOutput("country_plot_cumulative_log",height="130px", width="100%")

                          )
                      )
             )#,
             
             # tabPanel("Plot",
             #          
             #          sidebarLayout(
             #            sidebarPanel(
             #              
             #              pickerInput("level_select", "Level:",   
             #                          choices = c("Global", "Continent", "Country"), 
             #                          selected = c("Country"),
             #                          multiple = FALSE),
             #              
             #              pickerInput("region_select", "Country/Region:",   
             #                          choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
             #                          options = list('actions-box' = TRUE, 'none-selected-text' = "Please make a selection!"),
             #                          selected = cv_today_100$country,
             #                          multiple = TRUE), 
             #              
             #              pickerInput("outcome_select", "Outcome:",   
             #                          choices = c("Cases", "Deaths"), 
             #                          selected = c("Cases"),
             #                          multiple = FALSE),
             #              
             #              pickerInput("start_date", "Plotting start date:",   
             #                          choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
             #                          options = list('actions-box' = TRUE),
             #                          selected = "Date",
             #                          multiple = FALSE), 
             #              "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 100 confirmed cases are included."
             #            ),
             #            
             #            mainPanel(
             #              tabsetPanel(
             #                tabPanel("New", plotlyOutput("country_plot")),
             #                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
             #                tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
             #              )
             #            )
             #          )
             # )
        )          
)

server = function(input, output, session) {
  
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
    #  reactive = cv_cases %>% filter(date == "2020-04-07")
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$id)
    #large_countries = reactive %>% filter(alpha3 %in% worldcountry$id)
    worldcountry_subset = worldcountry[worldcountry$id %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$id, large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last24h = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste(HTML('<b>',prettyNum(sum(reactive_db()$cases), big.mark=","),"</b> cases"))
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$death), big.mark=","), " deaths")
  })
  
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
  })
  
  output$reactive_active_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
  })
  
  output$reactive_case_count_China <- renderText({
    paste0("Mainland China: ", prettyNum(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
  })
  
  output$reactive_case_count_row <- renderText({
    paste0("Other: ", prettyNum(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_24h <- renderText({
    paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
      clearMarkers() %>%
      addProviderTiles("CartoDB.Positron") %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.5, 
                  fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>%
      addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5),
                       fillOpacity = 0.7, color = covid_col, group = "2019-COVID (new)",
                       label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "10px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                       fillOpacity = 0.7, color = covid_col, group = "2019-COVID (cumulative)",
                       label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "10px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                       fillOpacity = 0.7, color = covid_col, group = "2019-COVID (active)",
                       label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                         textsize = "10px", direction = "auto"))  
      
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, input$plot_date)
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, input$plot_date)
  })
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                        selected = cv_today_100$country)
    }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = cv_cases_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = cv_cases_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = cv_cases
      db$region = db$country
    }
    
    if (input$outcome_select=="Cases") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlot({
    country_cases_plot(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlot({
    country_cases_cumulative(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlot({
    country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date)
  })
  
  
  # output$country_clicked_text <- renderText({
  #   if (!is.null(input$mymap_marker_click)){
  #     p <- input$mymap_marker_click  # typo was on this line
  #     filtered = cv_cases %>% filter(latitude == p$lat, longitude == p$lng)
  #     paste(unique(filtered$country))
  #   }
  #   else{
  #     paste0('')
  #   }
  # })
  
  country_clicked <- reactive({
    p <- input$mymap_marker_click  # typo was on this line
    filtered = cv_cases %>% filter(latitude == p$lat, longitude == p$lng)
    filtered$country
  })
  
  continent_clicked <- reactive({
    p <- input$mymap_marker_click  # typo was on this line
    filtered = cv_cases %>% filter(latitude == p$lat, longitude == p$lng)
    print(filtered$continent_level)
    unique(filtered$continent_level)
  })
  
  observeEvent(input$mymap_marker_click,
               if (input$level_select == 'Continent'){
                 updatePickerInput(session = session, inputId = "region_select",
                                   selected = continent_clicked())
               }
               else if (input$level_select == 'Country'){
               updatePickerInput(session = session, inputId = "region_select",
                                  selected = country_clicked())
               }, ignoreInit = TRUE)
  
}

shinyApp(ui, server)
