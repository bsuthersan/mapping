library(tidyverse)
library(readxl)

#Read in teh data
brent <- read.csv("~/Downloads/Brent postcodes.csv")
hammersmith <- read.csv("~/Downloads/Hammersmith and Fulham postcodes.csv")
rbkc <- read.csv("~/Downloads/Kensington and Chelsea postcodes.csv")
westminster <- read.csv("~/Downloads/Westminster postcodes.csv")

fulldata <- brent %>%
  bind_rows(hammersmith) %>%
  bind_rows(rbkc) %>%
  bind_rows(westminster)

remove(brent, hammersmith, rbkc, westminster)

deprivation <- read_xlsx("~/Downloads/IDACI.xlsx", sheet=2)

fulldata <- fulldata %>%
  left_join(deprivation, by = c("LSOA.Code" = "LSOA code (2011)"))

write.csv(fulldata, "~/Downloads/WLZIDACIData.csv", row.names=F, na="")

library(sp)
library(rgdal)
london <- rgdal::readOGR("~/Downloads/statistical-gis-boundaries-london/MapInfo", layer = "LSOA_2011_London_gen_MHW", stringsAsFactors = FALSE)


data_summarised <- fulldata %>%
  rename(LSOA11CD = LSOA.Code) %>%
  rename(IDACI  = `Income Deprivation Affecting Children Index (IDACI) Decile (where 1 is most deprived 10% of LSOAs)`) %>%
  group_by(LSOA11CD) %>%
  summarise(Average_IDACI = mean(IDACI))

london_filtered <- london[london$LSOA11CD %in% data_summarised$LSOA11CD, ]

london_filtered <- merge(london_filtered, data_summarised, by = "LSOA11CD")

london_filtered <- spTransform(london_filtered, CRS("+init=epsg:4326"))

pal <- colorNumeric(palette = "Blues", domain = london_filtered$Average_IDACI)

leaflet() %>%
  addProviderTiles("CartoDB.Positron", options= providerTileOptions(opacity = 0.99)) %>%
  addPolygons(data = london_filtered, stroke = TRUE, color="black", weight=1, fillOpacity = 0.8, 
              smoothFactor = 0.5, fillColor = ~pal(Average_IDACI),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomright",
            pal = pal,
            values = london_filtered$Average_IDACI,
            title = "IDACI Decile",
            opacity = 10)


