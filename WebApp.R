library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)
library(plyr)
library(maps)
library(mapdata)
library(stringi)
library(maptools)
library(htmltools)
library(RColorBrewer)
library(ggplot2)
library(rgdal)



ui <- dashboardPage(
    dashboardHeader(title = "Gender Inequality"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "intro", icon = icon("home")),
            menuItem("GII", tabName = "page1", icon = icon("area-chart")),
            menuItem("Health", tabName = "page2", icon = icon("chart-line"),
                     menuSubItem("Maternal mortality ratio",tabName = "page2_1"),
                     menuSubItem("Adolescent birth rate",tabName = "page2_2")),
            menuItem("Empowerment", icon = icon("line-chart"),
                     menuSubItem("Female Secondary Education",tabName = "Esub1"),
                     menuSubItem("Male Secondary Education",tabName = "Esub2"),
                     menuSubItem("Female Share of Seats",tabName = "Esub3"),
                     menuSubItem("Male Share of Seats",tabName = "Esub4")),
            menuItem("Labor", tabName = 'Labor', icon = icon("bar-chart")),
            menuItem("GII Heat Map", tabName = "heatmap", icon = icon("map"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "intro",
                    column(width = 12, img(src = "gender-inequality.png", width = 600), align = "center"),
                    h1("Gender Inequality Index (GII)"),
                    h3("GII Dimensions & Indicators"),
                    p("GII is a composite metric of gender inequality using three dimensions: reproductive health, empowerment and the labour market. A low GII value indicates low inequality between women and men, and vice-versa."),
                    column(width = 12, img(src = "GIIindex.png", width = 600), align = "center"),
                    h3("GII Reflections"),
                    p("GII reflects gender-based disadvantage in three dimensions— reproductive health, empowerment and the labour market—for as many countries as data of reasonable quality allow. It shows the loss in potential human development due to inequality between female and male achievements in these dimensions. It ranges from 0, where women and men fare equally, to 1, where one gender fares as poorly as possible in all measured dimensions. GII values are computed using the association-sensitive inequality measure suggested by Seth (2009), which implies that the index is based on the general mean of general means of different orders—the first aggregation is by a geometric mean across dimensions; these means, calculated separately for women and men, are then aggregated using a harmonic mean across genders."),
                    p("The lower GII values represent a better performance regarding gender inequality."),
                    a(href="https://hdr.undp.org/data-center/thematic-composite-indices/gender-inequality-index#/indicies/GII", "Click here to view more GII introductions"),
                    
                    h1("Data Source"),
                    p("Data comes from Human Develop Reports."),
                    a(href="https://hdr.undp.org/data-center/documentation-and-downloads", "Click here to view more Human Development Reports data"),
                    
                    h1("Data Cleaning and Wrangling"),
                    p("• Selected only the columns that are related to each indicators"),
                    p("• Transposed the data for convenience"),
                    p("• Tidied the data using pivot_longer"),
                    p("• Dropped the prefix of year columns and converted to date "),
                    
                    h1("Data Exploration"),
                    p("• Visualize the general GII Index using line charts for all countries over time"),
                    p("• Visualize each indicator for GII index using line charts for all countries over time"),
                    p("• Visualize the gender inequality issue using heatmaps for all countries over time"),

                    # h1("Team Members"),
                    # box(
                    #   width=12,# The total width a window is 12.
                    #   column(
                    #     width = 3, 
                    #     img(src='gender-inequality.jpg', align = "left", height= 100) # img1.JPG should be placed in the www folder
                    #   ),
                    #   column(
                    #     width = 9, 
                    #     p("A creative, highly self-motivated team from Carey Business School in Business Analytics program. Good at R, Python, SAS and Tableau.")
                    #   )
                    # ),
                    # br(),
                    
                    # checkboxInput("holiday", label = "Show holidays", value = FALSE),
                    # plotlyOutput("plot2", height = 500)
                    ),
            tabItem(tabName = "page1",
                    selectInput("select", label = h3("Filter by region"), 
                                choices = list("SOUTH ASIA" = "SA", "SUB-SAHARAN" = "SSA", "EUROPE AND CENTRAL ASIA" = "ECA",
                                               "ARAB STATES" = "AS", "LATIN AMERICA AND THE CARIBBEAN" = "LAC", "EAST ASIA AND THE PACIFIC" = "EAP"), 
                                selected = "SA"),
                    checkboxInput("points", label = "Show points", value = FALSE),
                    checkboxInput("giilegend", label = "Show legends", value = TRUE),
                    plotlyOutput("plot_gii", height = 500),
                    h2("GII in different years"),
                    sliderInput("year", "Year:", min = 2010, max = 2019, value = 2018, 
                                step = 1, animate = animationOptions(interval = 2000, loop = FALSE)),
                    plotlyOutput("bar")
            ),
            tabItem(tabName = "page2_1",
                    selectInput("select21", label = h3("Filter by region"), 
                                choices = list("SOUTH ASIA" = "SA", "SUB-SAHARAN" = "SSA", "EUROPE AND CENTRAL ASIA" = "ECA",
                                               "ARAB STATES" = "AS", "LATIN AMERICA AND THE CARIBBEAN" = "LAC", "EAST ASIA AND THE PACIFIC" = "EAP"), 
                                selected = "EAP"),
                    checkboxInput("points21", label = "Show points", value = FALSE),
                    checkboxInput("mmrlegend", label = "Show legends", value = TRUE),
                    plotlyOutput("plot_21")
            ),
            tabItem(tabName = "page2_2",
                    selectInput("select22", label = h3("Filter by region"), 
                                choices = list("SOUTH ASIA" = "SA", "SUB-SAHARAN" = "SSA", "EUROPE AND CENTRAL ASIA" = "ECA",
                                               "ARAB STATES" = "AS", "LATIN AMERICA AND THE CARIBBEAN" = "LAC", "EAST ASIA AND THE PACIFIC" = "EAP"), 
                                selected = "SA"),
                    checkboxInput("points22", label = "Show points", value = FALSE),
                    checkboxInput("abrlegend", label = "Show legends", value = TRUE),
                    plotlyOutput("plot_22")
            ),
            tabItem(tabName = "Esub1",
                    plotlyOutput("plot_se_f")
                    ),
            tabItem(tabName = "Esub2",
                    plotlyOutput("plot_se_m")
                    ),
            tabItem(tabName = "Esub3",
                    plotlyOutput("plot_pr_f")
                    ),
            tabItem(tabName = "Esub4",
                    plotlyOutput("plot_pr_m")
                    ),
            tabItem(tabName = "Labor",
                    h2("Labor force participation rate (lfpr)"),
                    br(),
                    checkboxGroupInput("Area", "Areas to show:",
                                       c("Arab States" = "AS",
                                         "East Asia And The Pacific" = "EAP",
                                         "Europe And Central Asia" = "ECA",
                                         "Latin America And The Caribbean" = "LAC",
                                         "South Asia" = "SA",
                                         "Sub-Saharan Africa" = "SSA")),
                    br(),
                    column(
                      width = 6,
                      plotlyOutput("plot_lfpr_f", height = 500)
                    ),
                    column(
                      width = 6,
                      plotlyOutput("plot_lfpr_m", height = 500)
                    )
                    ),
            tabItem(tabName = "heatmap",
                    leafletOutput("myMap",width="100%"))
        )
    )
)


server <- function(input, output, session) {
  
  #####gii part#######
  GII <- read_csv("GII_HDR2020_040722.csv")
  GII <- GII %>%
    select(country,region,contains("gii"))
  
  GII <- GII %>%
    pivot_longer(cols = gii_1995:gii_2019, names_to = "year",values_to = "gii_value")
  
  GII$year = str_split_fixed(GII$year,"_",2)[,2]
  
  GII <- na.omit(GII)
  GII$year <- as.Date(GII$year, "%Y")
  GII_region=GII %>% group_by(region,year)%>%summarise(gii_value=mean(gii_value))
  
  ####Health part####
  #mmr index
  # read data
  health <- read_csv("GII_HDR2020_040722.csv")
  mmr <- health %>% select(country,region,contains("mmr"))
  mmr1 <- pivot_longer(mmr,
                       c("mmr_1995","mmr_2000","mmr_2005","mmr_2010","mmr_2011",	"mmr_2012","mmr_2013","mmr_2014",
                         "mmr_2015","mmr_2016","mmr_2017"),
                       names_to = "year", values_to = "mmr_value")
  
  # split the column
  mmr1$year = str_split_fixed(mmr1$year,"_",2)[,2]
  mmr1$year <- as.Date(mmr1$year, "%Y")
  mmr1 <- na.omit(mmr1)
  
  #abr index
  # read data
  abr<- health %>% select(country,region,contains("abr"))
  abr1 <- pivot_longer(abr,
                       c("abr_1995","abr_2000","abr_2005","abr_2010","abr_2011",	"abr_2012","abr_2013","abr_2014",
                         "abr_2015","abr_2016","abr_2017","abr_2018","abr_2019"),
                       names_to = "year", values_to = "abr_value")
  
  # split the column
  abr1$year = str_split_fixed(abr1$year,"_",2)[,2]
  abr1$year = as.Date(abr1$year, "%Y")
  abr1 <- na.omit(abr1)
  

  ##############################se_f##############################
  # b-1) Data wrangling for se_f
  data = read_csv(file = "GII_HDR2020_040722.csv")
  data_se_f = select(data, "country","region","se_f_2010","se_f_2011","se_f_2012","se_f_2013","se_f_2014","se_f_2015","se_f_2016","se_f_2017","se_f_2018","se_f_2019")
  data_se_f = pivot_longer(data_se_f, c("se_f_2010","se_f_2011","se_f_2012","se_f_2013","se_f_2014","se_f_2015","se_f_2016","se_f_2017","se_f_2018","se_f_2019"),
                           names_to = "year", values_to = "se_f_value")
  data_se_f$year = str_split_fixed(data_se_f$year,"_",3)[,3]
  data_se_f$year = as.Date(data_se_f$year, "%Y")
  
  ##############################se_m##############################
  # b-2) Data wrangling for se_m
  data_se_m = select(data, "country","region","se_m_2010","se_m_2011","se_m_2012","se_m_2013","se_m_2014","se_m_2015","se_m_2016","se_m_2017","se_m_2018","se_m_2019")
  data_se_m = pivot_longer(data_se_m, c("se_m_2010","se_m_2011","se_m_2012","se_m_2013","se_m_2014","se_m_2015","se_m_2016","se_m_2017","se_m_2018","se_m_2019"),
                           names_to = "year", values_to = "se_m_value")
  data_se_m$year = str_split_fixed(data_se_m$year,"_",3)[,3]
  data_se_m$year = as.Date(data_se_m$year, "%Y")
  
  ##############################pr_f##############################
  # b-3) Data wrangling for pr_f
  data_pr_f = select(data, "country","region","pr_f_2010","pr_f_2011","pr_f_2012","pr_f_2013","pr_f_2014","pr_f_2015","pr_f_2016","pr_f_2017","pr_f_2018","pr_f_2019")
  data_pr_f = pivot_longer(data_pr_f, c("pr_f_2010","pr_f_2011","pr_f_2012","pr_f_2013","pr_f_2014","pr_f_2015","pr_f_2016","pr_f_2017","pr_f_2018","pr_f_2019"),
                           names_to = "year", values_to = "pr_f_value")
  data_pr_f$year = str_split_fixed(data_pr_f$year,"_",3)[,3]
  data_pr_f$year = as.Date(data_pr_f$year, "%Y")
  
  ##############################pr_m##############################
  # b-4) Data wrangling for pr_m
  data_pr_m = select(data, "country","region","pr_m_2010","pr_m_2011","pr_m_2012","pr_m_2013","pr_m_2014","pr_m_2015","pr_m_2016","pr_m_2017","pr_m_2018","pr_m_2019")
  data_pr_m = pivot_longer(data_pr_m, c("pr_m_2010","pr_m_2011","pr_m_2012","pr_m_2013","pr_m_2014","pr_m_2015","pr_m_2016","pr_m_2017","pr_m_2018","pr_m_2019"),
                           names_to = "year", values_to = "pr_m_value")
  data_pr_m$year = str_split_fixed(data_pr_m$year,"_",3)[,3]
  data_pr_m$year = as.Date(data_pr_m$year, "%Y")
    
  
  ###### labor force participation rate for female
  lfpr_f = data %>%
    select(iso3, country, hdicode, region, starts_with("lfpr_f")) %>%
    select(!ends_with("1995")) %>%
    select(!ends_with("2000")) %>%
    select(!ends_with("2005")) %>%
    pivot_longer(c("lfpr_f_2010", "lfpr_f_2011", "lfpr_f_2012",
                   "lfpr_f_2013", "lfpr_f_2014", "lfpr_f_2015",
                   "lfpr_f_2016", "lfpr_f_2017", "lfpr_f_2018", "lfpr_f_2019"),
                 names_to = "year",
                 values_to = "lfpr")
  lfpr_f$year = as.Date(str_split_fixed(lfpr_f$year,"_",3)[,3], "%Y")
  lfpr_f = na.omit(lfpr_f)
  
  
  ###### labor force participation rate for male
  lfpr_m = data %>%
    select(iso3, country, hdicode, region, starts_with("lfpr_m")) %>%
    select(!ends_with("1995")) %>%
    select(!ends_with("2000")) %>%
    select(!ends_with("2005")) %>%
    pivot_longer(c("lfpr_m_2010", "lfpr_m_2011", "lfpr_m_2012",
                   "lfpr_m_2013", "lfpr_m_2014", "lfpr_m_2015",
                   "lfpr_m_2016", "lfpr_m_2017", "lfpr_m_2018", "lfpr_m_2019"),
                 names_to = "year",
                 values_to = "lfpr")
  lfpr_m$year = as.Date(str_split_fixed(lfpr_m$year,"_",3)[,3], "%Y")
  lfpr_m = na.omit(lfpr_m)
   
  
  ##############################heatmap##############################
  # Data Wrangling for Heatmap
  gii_data = select(data, "iso3","country","gii_rank_2019","gii_2010","gii_2011","gii_2012","gii_2013","gii_2014","gii_2015","gii_2016","gii_2017","gii_2018","gii_2019")
  gii_data = pivot_longer(gii_data, c("gii_2010","gii_2011","gii_2012","gii_2013","gii_2014","gii_2015","gii_2016","gii_2017","gii_2018","gii_2019"),
                          names_to = "year", values_to = "gii_value")
  gii_data$year = str_split_fixed(gii_data$year,"_",2)[,2]
  gii_data$year = as.Date(gii_data$year, "%Y")
  loc_data = read_csv(file = "countries_codes_and_coordinates.csv")
  giiloc_data = merge(x=gii_data, y=loc_data, by = "iso3")
  giiloc_data = select(giiloc_data, "iso3","country","gii_rank_2019","year","gii_value","lat","lng")
  
  
  
  output$plot_gii = renderPlotly({
    plot_GII=ggplot(data = GII %>% filter(region==input$select),aes(x=year,y=gii_value,color=country)) + 
      geom_line()+
      #geom_point(color="purple")+
      xlim(as.Date("1995-07-12"),as.Date("2020-07-12"))+
      ylim(c(0.000,0.900))+
      #theme(legend.position = "none") +
      ggtitle("Explore GII in different region")
    
    if(input$points==TRUE){
      plot_GII =plot_GII +
        geom_point(data = GII %>% filter(region==input$select),aes(x=year,y=gii_value),color="purple")
    }
    if(input$giilegend==FALSE){
      plot_GII =plot_GII +
        theme(legend.position = "none")
    }
    ggplotly(plot_GII)
  })
  
  output$bar = renderPlotly({
    gii_bar = GII_region %>%
      filter(as.numeric(format(year,'%Y')) == input$year) %>%
      ggplot(aes(x=region,y=gii_value,fill=region))+
      geom_bar(stat="identity",width = 0.3)+
      ylim(c(0,1))+
      annotate("text",x =3, y =0.5,label=input$year,col="gray",size=15)+
      theme_minimal()
    ggplotly(gii_bar)
  })
  
  output$plot_21 = renderPlotly({
    plot_mmr=ggplot(data = mmr1 %>% filter(region==input$select21),
                    mapping=aes(x=year,y=mmr_value,color=country)) + 
      geom_line()+
      ylim(c(0,2000))+
      ggtitle("mmr index")
    #theme(legend.position = "none")
    
    
    if(input$points21==TRUE){
      plot_mmr =plot_mmr +
        geom_point(data = mmr1 %>% filter(region==input$select21),aes(x=year,y=mmr_value))+
        ylim(c(0,2000))
    }
    if(input$mmrlegend==FALSE){
      plot_mmr =plot_mmr +
        theme(legend.position = "none")
    }
    ggplotly(plot_mmr)
  })
  
  output$plot_22 = renderPlotly({
    plot_abr=ggplot(abr1 %>% filter(region==input$select22),aes(x=year,y=abr_value,color=country)) + 
      geom_line()+
      ylim(c(0,250))+
      xlim(as.Date("1995-07-12"),as.Date("2020-07-12"))+
      ggtitle("abr index")
    if(input$points22==TRUE){
      plot_abr =plot_abr +
        geom_point(data = abr1 %>% filter(region==input$select22),aes(x=year,y=abr_value),color="purple")
    }
    if(input$abrlegend==FALSE){
      plot_abr =plot_abr +
        theme(legend.position = "none")
    }
    ggplotly(plot_abr)
  })
   
    output$plot_se_f = renderPlotly({
      # c-1) Line graph for se_f
      graph_se_f = ggplot(data = data_se_f %>% filter(data_se_f$region=="EAP") ) + geom_line(mapping=aes(x=year,y=se_f_value,color=country))
      ggplotly(graph_se_f)
    })
    
    output$plot_se_m = renderPlotly({
      # c-2) Line graph for se_m
      graph_se_m = ggplot(data = data_se_m %>% filter(data_se_m$region=="EAP") ) + geom_line(mapping=aes(x=year,y=se_m_value,color=country))
      ggplotly(graph_se_m)
    })
    
    output$plot_pr_f = renderPlotly({
      # c-3) Line graph for pr_f
      graph_pr_f = ggplot(data = data_pr_f %>% filter(data_pr_f$region=="EAP") ) + geom_line(mapping=aes(x=year,y=pr_f_value,color=country))
      ggplotly(graph_pr_f)
    })
    
    output$plot_pr_m = renderPlotly({
      # c-4) Line graph for pr_f
      graph_pr_m = ggplot(data = data_pr_m %>% filter(data_pr_m$region=="EAP") ) + geom_line(mapping=aes(x=year,y=pr_m_value,color=country))
      ggplotly(graph_pr_m)
    })
    
    output$plot_lfpr_f = renderPlotly({
      if(is.null(input$Area) == 1){
        p_f = ggplot(data = lfpr_f) +
          aes(x = year, y = lfpr, color = country) +
          geom_line() +
          labs(title = "Female",
               y = "Participation Rate (%)") +
          xlim(as.Date("2010-01-01"), as.Date("2019-12-31")) +
        theme(legend.position = "none")
        ggplotly(p_f)
      }
      else{
        lfpr_f = lfpr_f %>% filter(region == input$Area)
        p_f = ggplot(data = lfpr_f) +
          aes(x = year, y = lfpr, color = country) +
          geom_line() +
          labs(title = "Female",
               y = "Participation Rate (%)") +
          xlim(as.Date("2010-01-01"), as.Date("2019-12-31")) +
        theme(legend.position = "none")
        ggplotly(p_f)
      }
      
    })
    
    output$plot_lfpr_m = renderPlotly({
      if(is.null(input$Area) == 1){
        p_m = ggplot(data = lfpr_m) +
          aes(x = year, y = lfpr, color = country) +
          geom_line() +
          labs(title = "Male",
               y = "Participation Rate (%)") +
          xlim(as.Date("2010-01-01"), as.Date("2019-12-31")) +
          theme(legend.position = "none")
        ggplotly(p_m)
      }
      else{
        lfpr_m = lfpr_m %>% filter(region == input$Area)
        p_m = ggplot(data = lfpr_m) +
          aes(x = year, y = lfpr, color = country) +
          geom_line() +
          labs(title = "Male",
               y = "Participation Rate (%)") +
          xlim(as.Date("2010-01-01"), as.Date("2019-07-31")) +
          theme(legend.position = "none")
        ggplotly(p_m)
      }
    })
    
    ################################heatmap#####################################
    output$myMap = renderLeaflet({
      pal <- colorBin(c("darkgreen","yellow","red"),giiloc_data$gii_rank_2019)
      mapWorld<-map("world", regions = ".", fill=TRUE,col=rainbow(2),ylim=c(-60,90),mar=c(0,0,0,0))
      mapWorld %>% leaflet() %>% addTiles() %>% addProviderTiles(providers$Stamen.Toner, group = "Toner") %>% 
        addLayersControl(baseGroups = c("Toner", "OSM"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addPolygons(color=~pal(giiloc_data$gii_rank_2019), stroke = FALSE, fillOpacity  = 0.8,weight=1) %>%
        addLegend(pal=pal,values=giiloc_data$gii_rank_2019,position="bottomleft",title = "GII rank in 2019")
    })
    

}

shinyApp(ui = ui, server = server)
