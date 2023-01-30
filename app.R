# rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.

library(rgdal) # readOGR()
library(shiny)
library(plotly) # toRGB()
library(png) #readPNG()
library(patchwork) # inset_element()
library(shinythemes) #shinythemes()
library(leaflet) #leafletOutput()
library(shinycssloaders) #withSpinner()
library(shinyjs) #useShinyjs

# setwd("C:\\n_app\\n_app\\")

melt_df <- read.csv('melt3_df.csv')
Fertilizer_lst <- c("Benchmark_max_Fertilizer","Benchmark_median_Fertilizer","Benchmark_min_Fertilizer")
Manure_lst <- c("Benchmark_max_Manure","Benchmark_median_Manure","Benchmark_min_Manure")
Fixation_lst <- c("Benchmark_max_Fixation","Benchmark_median_Fixation","Benchmark_min_Fixation")
Deposition_lst <- c("Benchmark_max_Deposition","Benchmark_median_Deposition","Benchmark_min_Deposition")
Harvest_lst <- c("Benchmark_max_Harvest","Benchmark_median_Harvest","Benchmark_min_Harvest")
keep <- c("Benchmark_median_Fertilizer", "Benchmark_median_Manure", 
          "Benchmark_median_Fixation", "Benchmark_median_Deposition", 
          "Benchmark_median_Harvest", "Benchmark_median_Area_km2")
ISO3_lst <- unique(melt_df$ISO3)
CountryNames <- unique(melt_df$Country)

myspdf = readOGR(dsn=paste0(getwd(),'//','TM_WORLD_BORDERS_SIMPL-0.3'), layer = "TM_WORLD_BORDERS_SIMPL-0.3", verbose = FALSE) # https://www.thematicmapping.org/downloads/world_borders.php
nuup <- c("NUE")
surp <- c("Nsurplus")

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator'))

###########################################
ui <- fluidPage( 
  bootstrapPage(theme = shinytheme("superhero"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  titlePanel("Global Nitrogen Database"),
    tabsetPanel(
      tabPanel("Home page", fluid = TRUE,
          titlePanel("Welcome to the Global Nitrogen Database!"),
          mainPanel(
                    tags$b("What is the Global Nitrogen Database?"),
                    p('Input and output estimates of nitrogen on cropland are essential for improving nitrogen management and better understanding the global nitrogen cycle. Here, we compare 13 nitrogen budget datasets covering 115 countries and regions from  1961 to 2015.'),
                    plotOutput("image1"),
                    tags$b("Why use the Global Nitrogen Database?"),
                    p('Although most datasets showed similar spatiotemporal patterns, some annual estimates varied widely among them, resulting in large ranges and uncertainty. In 2010, global medians (in TgN/yr) and associated minimum and maximum ranges were 73 (64, 84) for global harvested crop nitrogen; 161 (139, 192) for total nitrogen inputs; 86 (68,97) for nitrogen surplus; and 46% (40%, 53%) for nitrogen use efficiency. Some of the most uncertain nitrogen budget terms by country showed ranges as large as their medians, revealing areas for improvement. A benchmark nitrogen budget dataset, derived from central tendencies of the original datasets, can be used in model comparisons and inform sustainable nitrogen management in food systems.'),
                    plotOutput("image2"),
                    br(),
                    hr(),
                    tags$b("Reading the graphs:"),
                    plotOutput("map1"),
                    p('When under the |World Map| tab, you are shown 2 drop down tabs in which you can edit to change the information that is displayed. You are able to change the input/output values of either min, median, or max cropland areas. You are also able to change the type of Nitrogen input/output: Fertilizer, manure, etc. After selecting the desired options, when you hover of a certain country a number will be displayed. This number is the estimates of that countries global nitrogen use.'),
                    hr(),
                    plotOutput("thumbnail_USA"),
                    p('When under the |NUE| tab, you are given a interactive world map in which you can select a country. In this graph, instead of selecting a type of Nitrogen input/output, all of them will be displayed within the graph. They will be stacked upon one another with a new line in red that shows the harvest within these types.')
          ),
          sidebarPanel("Useful Links",
                       br(),
                       tags$a(href = "https://www.nature.com/articles/s43016-021-00318-5", "Source: Zhang et al., 2021", target="_blank"),
                       br(), 
                       hr(),
                       tags$a(href = "https://datadryad.org/stash/dataset/doi:10.5061/dryad.vt4b8gtrd", "Dataset Repository: Zou et al., 2021", target="_blank"),
                       br(),
                       hr(),
                       tags$a(href = "https://github.com/KEJackson-94/n_app", "Github for Web App.", target="_blank")
          )),
      
      tabPanel("NUE", fluid = TRUE,
               tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
               useShinyjs(),
               br(),
               span(style = "font-weight: 600; font-size: 25px; width: 90%; color: #FFFFFF;", "World Map Nitrogen Budget"),
               br(),
               span(style = "font-weight: 400; font-size: 20px; width: 50%; color: #FFFFFF;", "Provided by a team from UMECS and Salisbury University"),
               br(),
               fluidRow(
                 column(12, leafletOutput("mymap", height = "330px")%>% withSpinner(color="#0dc5c1"))
               ),
               br(),
               p(),
               fluidRow(
                 column(4, plotlyOutput("myPlot", width= "100%", height = "250px")%>% withSpinner(color="#0dc5c1")),
                 column(4, plotlyOutput("nuePlot", width= "100%", height = "250px")%>% withSpinner(color="#0dc5c1")),
                 column(4, plotlyOutput("surplusPlot", width= "100%", height = "250px")%>% withSpinner(color="#0dc5c1"))
                 
               ),
               
               fluidRow(
                 column(4, plotlyOutput("prevPlot", width= "100%", height = "250px")%>% withSpinner(color="#0dc5c1")),
                 column(4, plotlyOutput("prevnuePlot", width= "100%", height = "250px")%>% withSpinner(color="#0dc5c1")),
                 column(4, plotlyOutput("prevSurpPlot", width= "100%", height = "250px")%>% withSpinner(color="#0dc5c1"))
                 
               ))
      )))

RV<-reactiveValues(Clicks=c())
  
###########################################
server <- function(input, output) {
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("Zhang_et_al_2021-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(melt_df, file)
    }
  )
  
  observeEvent(input$go, {
    screenshot()
  })
  
  output$'image1' <- renderPlot({
    my_image <- readPNG("image1.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                  
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$'image2' <- renderPlot({
    my_image <- readPNG("image2.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$'map1' <- renderPlot({
    my_image <- readPNG("map1.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$'thumbnail' <- renderPlot({
    my_image <- readPNG("thumbnail_USA.png", native = TRUE)
    df_empty <- data.frame()
    ggplot(df_empty) +                
      inset_element(p = my_image,
                    left = 0,
                    bottom = 0,
                    right = 1,
                    top = 1)
  })
  
  output$mymap <- renderLeaflet({
    # Create the map data and add ploygons to it
    leaflet(data=myspdf) %>%
      addTiles() %>%
      setView(lat=10, lng=0, zoom=2) %>%
      addPolygons(fillColor = "green",
                  highlight = highlightOptions(weight = 5,
                                               color = "red",
                                               fillOpacity = 0.7,
                                               bringToFront = TRUE),
                  label = ~ISO3,
                  layerId = ~ISO3) # add a layer ID to each shape. This will be used to identify the shape clicked
  })
  
  output$"myPlot" <-
    renderPlotly({ p <- plot_ly(type="scatter",mode="markers")
    p <- layout(p,title="test")
    p <- add_trace(p, x=0,y=0,name="ABC_test",mode="lines+markers")
    })
  
  output$"nuePlot" <- 
    renderPlotly({ p <- plot_ly(type="scatter",mode="markers")
    p <- layout(p,title="test")
    p <- add_trace(p, x=0,y=0,name="ABC_test",mode="lines+markers")
    })
  
  output$"surplusPlot" <- 
    renderPlotly({ p <- plot_ly(type="scatter",mode="markers")
    p <- layout(p,title="test")
    p <- add_trace(p, x=0,y=0,name="ABC_test",mode="lines+markers")
    })
  
  output$"prevPlot" <- 
    renderPlotly({ p <- plot_ly(type="scatter",mode="markers")
    p <- layout(p,title="test")
    p <- add_trace(p, x=0,y=0,name="ABC_test",mode="lines+markers")
    })
  
  output$"prevnuePlot" <- 
    renderPlotly({ p <- plot_ly(type="scatter",mode="markers")
    p <- layout(p,title="test")
    p <- add_trace(p, x=0,y=0,name="ABC_test",mode="lines+markers")
    })
  
  output$"prevSurpPlot" <- 
    renderPlotly({ p <- plot_ly(type="scatter",mode="markers")
    p <- layout(p,title="test")
    p <- add_trace(p, x=0,y=0,name="ABC_test",mode="lines+markers")
    })
  
  observeEvent(input$mymap_shape_click, {
    click = input$mymap_shape_click#  subset the spdf object to get the lat, lng and country name of the selected shape (Country in this case)
    RV$Clicks<-c(RV$Clicks,click$id)
    print(RV$Clicks)# print(click$id)
    sub = myspdf[myspdf$ISO3==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    nm=sub$ISO3
    if(is.null(click))
      return()
    else
      leafletProxy("mymap") %>%
      setView(lng = lng , lat = lat, zoom = 3) %>%
      clearMarkers() %>% 
      addMarkers(lng =lng , lat = lat, popup = nm) # using lat long from spdf will not change the view on multiple clicks on the same shape
  }
  )
  
  observeEvent(input$mymap_shape_click, {
    click = input$mymap_shape_click
    sub = myspdf[myspdf$ISO3==input$mymap_shape_click$id, c("LAT", "LON", "NAME")]
    lat = sub$LAT
    lng = sub$LON
    nm = sub$ISO3
    if(is.null(click))
      return()
    else
      output$text <- renderText({paste("Latitude= ", lat, 
                                       "Longitude=", lng,
                                       "Country=", nm
      )})}
  )
  
  observeEvent(input$mymap_shape_click, {
    click = input$mymap_shape_click
    output$"myPlot" <- 
      renderPlotly({
        iso_df <- melt_df[melt_df[,"ISO3"]== click$id[1],]
        df_edt <- iso_df[iso_df$data_type %in% keep, ]
        new_names <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition',
                       'Harvest', 'Area')
        for (i in 1:length(keep)){
          df_edt$'data_type'[df_edt$'data_type' == keep[i]] <- new_names[i]}
        df_edt$Year <- as.numeric(as.character(df_edt$Year))
        sapply(df_edt, class)
        keepv2 <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition', 'Harvest') # rename to control order in stack plot
        StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
        
        myPlot <- ggplot(StckPlt_df, aes(x = Year, y = value, group=data_type, color=data_type)) + geom_line()+
          theme(legend.position="none") +
          ggtitle("Nitrigon dataset") +
          xlab("Year (1961-2015)") +
          ylab("Inputs or Outputs (kg N)") +
          labs(title= click$id[1]) +
          theme_classic() + 
          theme(legend.title=element_blank())
      })
  } 
  )
  
  observeEvent(input$mymap_shape_click, {
    click = input$mymap_shape_click
    output$"nuePlot" <- 
      renderPlotly({
        iso_df <- melt_df[melt_df[,"ISO3"]== click$id[1],]
        df_edt <- iso_df[iso_df$data_type %in% nuup, ]
        df_edt$Year <- as.numeric(as.character(df_edt$Year))
        sapply(df_edt, class)
        keepv2 <- c('NUE') # rename to control order in stack plot
        StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
        
        nuePlot <- ggplot(StckPlt_df, aes(x = Year, y = value, group=data_type, color=data_type)) + geom_line()+
          theme(legend.position="none") +
          ggtitle("Nitrigon dataset") +
          xlab("Year (1961-2015)") +
          ylab("NUE") +
          labs(title= click$id[1]) +
          theme_classic() + 
          theme(legend.title=element_blank())
      })
  })
  
  observeEvent(input$mymap_shape_click, {
    click = input$mymap_shape_click
    output$"surplusPlot" <- 
      renderPlotly({
        iso_df <- melt_df[melt_df[,"ISO3"]== click$id[1],]
        df_edt <- iso_df[iso_df$data_type %in% surp, ]
        df_edt$Year <- as.numeric(as.character(df_edt$Year))
        sapply(df_edt, class)
        keepv2 <- c('Nsurplus') # rename to control order in stack plot
        StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
        
        nuePlot <- ggplot(StckPlt_df, aes(x = Year, y = value, group=data_type, color=data_type)) + geom_line()+
          theme(legend.position="none") +
          ggtitle("Nitrigon dataset") +
          xlab("Year (1961-2015)") +
          ylab("N Surplus (kg N)") +
          labs(title= click$id[1]) +
          theme_classic() + 
          theme(legend.title=element_blank())
      })
  })
  
  observeEvent(input$mymap_shape_click, {
    click = input$mymap_shape_click
    if(length(RV$Clicks)>1)
      prev_iso <- (RV$Clicks[length(RV$Clicks)-1])
    else
      prev_iso <- click$id
    output$"prevPlot" <- 
      renderPlotly({
        iso_df <- melt_df[melt_df[,"ISO3"]== prev_iso[1],]
        df_edt <- iso_df[iso_df$data_type %in% keep, ]
        new_names <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition',
                       'Harvest', 'Area')
        for (i in 1:length(keep)){
          df_edt$'data_type'[df_edt$'data_type' == keep[i]] <- new_names[i]}
        df_edt$Year <- as.numeric(as.character(df_edt$Year))
        sapply(df_edt, class)
        keepv2 <- c('4_Fertilizer', '3_Manure', '2_Fixation', '1_Deposition', 'Harvest') # rename to control order in stack plot
        StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
        
        
        myPlot <- ggplot(StckPlt_df, aes(x = Year, y = value, group=data_type, color=data_type)) + geom_line()+
          theme(legend.position="none") +
          ggtitle("Nitrigon dataset") +
          xlab("Year (1961-2015)") +
          ylab("Inputs or Outputs (kg N)") +
          labs(title= prev_iso[1]) +
          theme_classic() + 
          theme(legend.title=element_blank())
      })
  } 
  )
  
  observeEvent(input$mymap_shape_click, {
    print(RV$Clicks[(length(RV$Clicks)-1):length(RV$Clicks)])
    click = input$mymap_shape_click
    if(length(RV$Clicks)>1)
      prev_iso <- (RV$Clicks[length(RV$Clicks)-1])
    else
      prev_iso <- click$id
    output$"prevnuePlot" <- 
      renderPlotly({
        iso_df <- melt_df[melt_df[,"ISO3"] == prev_iso[1],]
        df_edt <- iso_df[iso_df$data_type %in% nuup, ]
        df_edt$Year <- as.numeric(as.character(df_edt$Year))
        sapply(df_edt, class)
        keepv2 <- c('NUE') # rename to control order in stack plot
        StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
        
        compPlot <- ggplot(StckPlt_df, aes(x = Year, y = value, group=data_type, color=data_type)) + geom_line()+
          theme(legend.position="none") +
          ggtitle("Nitrigon dataset") +
          xlab("Year (1961-2015)") +
          ylab("NUE") +
          labs(title= prev_iso[1]) +
          theme_classic() + 
          theme(legend.title=element_blank())
      })
  })
  
  observeEvent(input$mymap_shape_click, {
    print(RV$Clicks[(length(RV$Clicks)-1):length(RV$Clicks)])
    click = input$mymap_shape_click
    if(length(RV$Clicks)>1)
      prev_iso <- (RV$Clicks[length(RV$Clicks)-1])
    else
      prev_iso <- click$id
    output$"prevSurpPlot" <- 
      renderPlotly({
        iso_df <- melt_df[melt_df[,"ISO3"] == prev_iso[1],]
        df_edt <- iso_df[iso_df$data_type %in% surp, ]
        df_edt$Year <- as.numeric(as.character(df_edt$Year))
        sapply(df_edt, class)
        keepv2 <- c('Nsurplus') # rename to control order in stack plot
        StckPlt_df <- df_edt[df_edt$data_type %in% keepv2, ]
        
        compPlot <- ggplot(StckPlt_df, aes(x = Year, y = value, group=data_type, color=data_type)) + geom_line()+
          theme(legend.position="none") +
          ggtitle("Nitrigon dataset") +
          xlab("Year (1961-2015)") +
          ylab("N Surplus (kg N)") +
          labs(title= prev_iso[1]) +
          theme_classic() + 
          theme(legend.title=element_blank())
      })
  })
  
  }
###########################################
shinyApp(ui = ui, server = server)
