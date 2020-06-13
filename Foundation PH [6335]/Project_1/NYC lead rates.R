# https://data.cityofnewyork.us/Health/Children-Under-6-yrs-with-Elevated-Blood-Lead-Leve/tnry-kwh5
# also see https://rpubs.com/jhofman/nycmaps

# Shows data for NYC of 
# Who: Children under 6 years 
# What: with elevated blood lead levels (BLL)
# Rate: per 1,000 tested
df <- "https://data.cityofnewyork.us/api/views/tnry-kwh5/rows.csv?accessType=DOWNLOAD" %>%
  readr::read_csv() %>%
  select(geo_type:time_period, contains("per 1,000 tested"), contains("Number Tested")) %>%
  mutate(geo_type = recode(geo_type, "Neighborhood (UHF 42)"="Neighborhood"))

names(df) <- c(names(df)[1:5], 
               "Over5",  "Over5_note",
               "Over10", "Over10_note",
               "Over15", "Over15_note",
               "nTested", "nTested_note")

df %>%
  count(geo_type)


library(tigris)
library(dplyr)
library(leaflet)
library(sp)
library(ggmap)
library(maptools)
library(broom)
library(httr)
library(rgdal)
u <- "https://gist.githubusercontent.com/miguelpaz/edbc79fc55447ae736704654b3b2ef90/raw/695b3144ec3eb4bbf289d1667a5135e950c9d787/uhf42.geojson"
u <- httr::GET(u)
nyc_sp <- rgdal::readOGR(httr::content(u,'text'), 'OGRGeoJSON', verbose = F)
rm(u)

ggplot() + 
  geom_polygon(data=tidy(nyc_sp), aes(x=long, y=lat, group=group))

leaflet(nyc_sp) %>%
  addTiles() %>% 
  addPolygons(popup = ~uhf_neigh) %>%
  addProviderTiles("CartoDB.Positron")

# u@data %>% filter(uhfcode == 104)

nyc_join <- df %>% 
  filter(geo_type=="Neighborhood") %>% 
  group_by(geo_area_id) %>%
  summarise(
    Over5 = mean(Over5, na.rm=T),
    Over10 = mean(Over10, na.rm=T),
    Over15 = mean(Over15, na.rm=T)) %>%
  geo_join(nyc_sp, ., "uhfcode", "geo_area_id")

pal <- colorNumeric(palette = "Reds",
                    domain = range(nyc_join@data$Over10, na.rm=T))
pal <- colorQuantile("Reds", range(nyc_join@data$Over5, na.rm=T), n=10)
pal <- colorBin("Reds", domain =range(nyc_join@data$Over5, na.rm=T))

nyc_join %>%
  leaflet() %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(Over10), popup = ~as.character(round(Over10)), 
              label=~uhf_neigh,  fillOpacity=0.5) %>%
  addLegend("bottomright", pal = pal, values = ~Over10)



# 
# tigris::lookup_code("New York", "Bronx")
# tigris::lookup_code("New York", "Brooklyn")
# tigris::lookup_code("New York", "Manhattan")
# tigris::lookup_code("New York", "Queens")
# tigris::lookup_code("New York", "Staten Island")
