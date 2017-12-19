library(tidyverse)
library(dplyr)
library(googleVis)

## Loading INternet and GDP File

internet.data <-
  readxl::read_xlsx("data/API_IT.NET.USER.ZS_DS2_en_csv_v2.xlsx")

gdp.data <-
  readxl::read_xlsx("data/API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2.xlsx")


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
  read.csv("data/Country_List_ISO_3166_Codes_Latitude_Longitude.csv")

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

n <- 1
k <- 1

for (i in unique(gdp.internet.joined.withlat %>% arrange(as.numeric(year)) %>% .$year)) {
  g1 <-
    gvisGeoChart(
      gdp.internet.joined.withlat[gdp.internet.joined.withlat$year == i, ],
      locationvar = 'CountryName.x',
      colorvar = 'InternetUser',
      sizevar = 'year',
      options = list(
        dataMode = "regions",
        title = "Internet reach year ",
        colorAxis = "{values:[1,25,50,75,100],
        colors:[\'red', \'pink\', \'orange',\'yellow',\'green']}"
        ,
        width = 200,
        height = 200
      )
    )
  
  if (n == 1) {
    g2 <- g1
    n <- n + 1
  } else if (n<=4) {
        g2 <-
      gvisMerge(g2, g1, horizontal = TRUE, tableOptions = "cellspacing=5")
    
    n <- n + 1
  } else {
    n <- 2
    if (k == 1) {
          g3 <- g2
          g2 <- g1
      k <- k + 1
    } else {
      g3 <- gvisMerge(g3, g2, horizontal = FALSE)
      g2 <- g1
      }
    
  }
  
}

plot(g3)
