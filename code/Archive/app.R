# Internet Access to everyone over last 20 years or so....

palette(
  c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#FFFF33",
    "#A65628",
    "#F781BF",
    "#999999"
  )
)

library(shiny)
library(googleVis)
library(plotly)
library(dygraphs)
## Loading INternet and GDP File

internet.data <-
  readxl::read_xlsx("../data/API_IT.NET.USER.ZS_DS2_en_csv_v2.xlsx")

gdp.data <-
  readxl::read_xlsx("../data/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2.xlsx")


## Transposing the Internet and GDP file

internet_data.transposed <-
  gather(internet.data, "year", "InternetUser", 5:ncol(internet.data))


gdp_data.transposed <-
  gather(gdp.data, "year", "gdp", 5:ncol(internet.data))

## Filtering only the complete cases -- Without NA values

internet_data_filtered <-
  internet_data.transposed[complete.cases(internet_data.transposed), ]

## Joining Internet data with change in GDP

gdp.internet.joined <- internet_data_filtered %>%
  inner_join(gdp_data.transposed,
             by = c("year" = "year", "CountryCode" = "CountryCode"))

## Adding lat and Log data

## I am not using Lat and Log data in this graph
## So we can ignore this dataset
countries.lat.log <-
  read.csv("../data/Country_List_ISO_3166_Codes_Latitude_Longitude.csv")

gdp.internet.joined.withlat <- gdp.internet.joined %>%
  inner_join(countries.lat.log, by = c("CountryCode" = "Alpha.3.code"))

gdp.internet.joined.withlat <-
  gdp.internet.joined.withlat[complete.cases(gdp.internet.joined.withlat), ]


## Checking from what year internet user are available
gdp.internet.joined.withlat %>%
  group_by(year) %>%
  summarise(total.internet.user = sum(InternetUser))

## internet users are available from 1990, I will take data greater than 1988

gdp.internet.joined.withlat <-
  gdp.internet.joined.withlat[gdp.internet.joined.withlat$year > 1990 &
                                gdp.internet.joined.withlat$InternetUser > 0,]


gdp.internet.joined.withlat[gdp.internet.joined.withlat$year == 2016, ] %>%
  arrange(desc(gdp)) %>%
  top_n(20, gdp) %>%
  mutate(label = CountryName.x) %>%
  select(CountryName.x, label,gdp)

ui <- fluidPage(
  headerPanel('Internet access to everyone over past 10 years or so...'),
  sidebarPanel(
    selectInput('year', 'Year', gdp.internet.joined.withlat$year)
  ),
  mainPanel(
    tabPanel("% Population access to Internet", htmlOutput('plot1')),
    tabPanel("Growth over the years", plotOutput("plot2")),
    tabPanel("Growth over the years", plotOutput("plot3"))
    )
  
)

server <- function(input, output) {
  selectedGvisData <- reactive({
    gdp.internet.joined.withlat[gdp.internet.joined.withlat$year == input$year, ]
  })
  
  selectedPlotData <- reactive({
    gdp.internet.joined.withlat[gdp.internet.joined.withlat$year <= input$year, ] %>%
      left_join( gdp.internet.joined.withlat[gdp.internet.joined.withlat$year == input$year, ] %>%
                   arrange(desc(InternetUser)) %>%
                   top_n(5, InternetUser) %>%
                   mutate(label = CountryName.x) %>%
                   select(CountryName.x, label))
  })
  
  
  selectedGDPData <- reactive({
    gdp.internet.joined.withlat[gdp.internet.joined.withlat$year == input$year, ] %>%
      inner_join( gdp.internet.joined.withlat[gdp.internet.joined.withlat$year == input$year, ] %>%
                   arrange(desc(gdp)) %>%
                   top_n(20, gdp) %>%
                   mutate(label = CountryName.x) %>%
                   select(CountryName.x, label))
  })
  
  output$plot1 <- renderGvis({
    gvisGeoChart(
      selectedGvisData(),
      locationvar = 'CountryName.x',
      colorvar = 'InternetUser',
      sizevar = 'year',
      options = list(
        dataMode = "regions",
        title = "Internet reach year",
        colorAxis = "{values:[1,25,50,75,100],
        colors:[\'red', \'pink\', \'orange',\'yellow',\'green']}"
      )
    ) 
    
    
  })
  
  output$plot2 <- renderPlot({
    req(nrow(selectedPlotData()) > 0)
    
    ggplot(selectedPlotData(),
           aes(
             x = year,
             group = CountryName.x,
             color = label
           )) +
      geom_point(aes(y = InternetUser)) +
      geom_line(aes(y = InternetUser)) +
      labs(caption="Internet Access growth over years\n(Top 5 fastest growing nation in selected year are colored") +
      theme(axis.text.x = element_text(angle = 50),
            axis.title.x = element_blank(),plot.caption =  element_text(size = 10,
                                                                     lineheight = 1))
    
  })
  
  output$plot3 <- renderPlot({
    req(nrow(selectedGDPData()) > 0)
    
    ggplot(selectedGDPData(),
           aes(
             x = CountryName.x,
             group = CountryName.x,
             fill = label
           )) +
      geom_bar(aes(y = gdp),stat = "identity",position = position_dodge()) +
      labs(caption="GDP growth \n(Top 20 fastest growing nation in selected year are colored") +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),plot.caption =  element_text(size = 10,
                                                                        lineheight = 1))
    
  })
  
  
}

shinyApp(ui = ui, server = server)
