library(shiny)
library(tidyverse)
library(lubridate)
library(highcharter)
data_confirmados <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/time_series/time_series_2019-ncov-Confirmed.csv")




ui <- fluidPage(
  
  highchartOutput("mymap",width = "100%", height = "600px"),
  p("Patricio Said, #DatosDeMiercoles")
)

server <- function(input, output, session) {
  
  
  output$mymap <- renderHighchart({
    
    data_conf <-   data_confirmados %>% 
      mutate_if(is.numeric, replace_na, 0) %>% 
      pivot_longer(-c(1:4), names_to = "Fecha", values_to = "Confirmados") %>% 
      separate(Fecha, c("dia", "hora"),sep = " " ) %>% 
      select(-hora) %>% 
      group_by(`Province/State`, `Country/Region` ,  Lat,  Long, dia) %>% 
      summarise(Confirmados = max(Confirmados)) %>% 
      ungroup() %>% 
      mutate(dia = mdy(dia)) %>% 
      arrange(dia) %>% 
      mutate(dia = as.character(dia))
    
    dias <- data_conf %>% distinct(dia)
    
    data_conf <- data_conf %>% 
      mutate(Confirmados = ifelse(Confirmados <1, NA, Confirmados)) %>% 
      pivot_wider(names_from = dia, values_from = Confirmados) %>% 
      group_by( `Province/State`,`Country/Region` ,Lat, Long) %>% 
      nest() %>% 
      mutate(sequence = map(data, as.double)) %>% 
      rename(State = `Province/State`, Country= `Country/Region` ,
             lat = Lat,
             lon = Long) %>% 
      select(State, Country, lat, lon, sequence) %>% 
      mutate(grupo = "real")
    
    ficticio <- tibble(
      `Province/State` = "ficticio",
      name  = "ficticio"       ,
      lat  = -2000,
      lon = -200000,
      sequence = list(rep(max(data_confirmados[[ncol(data_confirmados)]]),nrow(dias))),
      grupo = "ficticio"
    )
    
    hc <- hcmap() %>% 
      hc_add_series(data = data_conf , type = "mapbubble", name= "Confirmed",
                    hcaes(y = lat, x = lon , size = sequence), color = "red") %>%
      hc_add_series(data = ficticio , type = "mapbubble",
                    hcaes(y = lat, x = lon ), showInLegend= FALSE,color= '#272822',
                    enableMouseTracking= FALSE) %>%
      hc_mapNavigation(enabled = TRUE) %>% 
      hc_plotOptions(bubble = list( minSize= "0%",
                                    maxSize= 300, 
                                    animation= FALSE,
                                    sizeBy= 'width') ,
                     bar = list(groupPadding = 0, pointPadding =  0, borderWidth = 0))%>% 
      hc_motion(enabled = TRUE, series = c(0,1,2), labels = dias[[1]],
                loop = TRUE, autoPlay = TRUE,  magnet = list(step =  1),
                updateInterval = 400) %>% 
      hc_legend(F) %>% 
      hc_add_theme(hc_theme_monokai()) %>% 
      hc_tooltip( footerFormat= " Confirmed  {point.Country} - {point.State}",  useHTML = TRUE,shared = FALSE)
    hc
  })
}


shinyApp(ui, server)