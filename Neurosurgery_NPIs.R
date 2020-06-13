library(RSocrata)
library(tigris)
library(leaflet)
library(tidycensus)
library(tidyverse)
library(ggmap)
library(viridis)

latlong2county <- function(long, lat) {
  library(sp)
  library(maps)
  library(maptools)
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(data.frame(long=long, lat=lat), 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}


# # BRFSS Prevalence by MMSA
# rawdata <- "https://chronicdata.cdc.gov/resource/j32a-sa6u.csv" %>% 
#   read.socrata() %>%
#   as_tibble()
# 
# rawdata %>%
#   filter(str_detect(locationdesc, "TX")) %>%
#   count(locationdesc)



# # CMS claims data from https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/Physician-and-Other-Supplier2017
# rawdata <- "https://data.cms.gov/resource/fs4p-t5eq.csv" %>% 
#   read.socrata() %>%
#   as_tibble()
# beepr::beep()
rawdata <- readr::read_delim("~/Downloads/Medicare_Provider_Util_Payment_PUF_CY2017/Medicare_Provider_Util_Payment_PUF_CY2017.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE) %>% filter(npi!="0000000001")

glimpse(rawdata) # nppes_provider_zip

NSG <- rawdata %>% 
  filter(provider_type=="Neurosurgery") %>%
  rename(street=nppes_provider_street1, city=nppes_provider_city, 
         state=nppes_provider_state, country=nppes_provider_country,
         zip=nppes_provider_zip,
         
         lastName=nppes_provider_last_org_name,
         firstName=nppes_provider_first_name,
         gender=nppes_provider_gender) %>%
  select(-nppes_provider_mi, -nppes_provider_street2) %>%
  mutate(zip = str_trunc(zip, 5, "right", ellipsis = ""))
glimpse(NSG)

# rm(rawdata)

sf_zip <- tigris::zctas(cb=F)


#####################################
##        HANDLES GEOCODING        ##
#####################################

# geocoded <- NSG %>%
#   select(npi, street, city, state, zip) %>%
#   unique() %>%
#   left_join(select(sf_zip@data, zip=GEOID10, lat=INTPTLAT10, lon=INTPTLON10)) %>%
#   mutate(
#     lat = as.numeric(lat), 
#     lon = as.numeric(lon)
#   )
# 
# geocoded_missing <- geocoded %>%
#   filter(is.na(lat)|is.na(lon)) %>%
#   mutate(address = glue::glue("{city}, {state}, {zip}")) %>%
#   select(npi, address) %>%
#   unique()
# 
# google_geocoded <- geocoded_missing %>%
#   select(address) %>%
#   unique() %>%
#   mutate_geocode(address, output="more")
# 
# 
# # Re-write `geocoded` with missing values filled in
# geocoded <- left_join(geocoded_missing, google_geocoded) %>%
#   right_join(
#     select(filter(geocoded, is.na(lat)|is.na(lon)), -lon, -lat)
#   ) %>%
#   mutate(zip=NA) %>%
#   select(npi, street, city, state, zip, lat, lon) %>%
#   bind_rows(.,
#     filter(geocoded, !is.na(lat), !is.na(lon))
#   )
# rm(geocoded_missing, google_geocoded)
# 
# 
# 
# 
# 
# geocoded2 <- geocoded %>%
#   mutate(county_name = latlong2county(lon, lat)) %>%
#   left_join(
#     {county.fips %>%
#       rename(county_name=polyname, county_FIPS=fips) %>%
#       mutate(county_name = str_replace(county_name, ":.*", "")) %>%
#       unique()}
#     )
# 
# 
# tictoc::tic("Calling cenus")
# missing_geoid <- geocoded2 %>%  # takes a little over 1 min
#   filter(is.na(county_FIPS)) %>%
#   append_geoid(geoid_type = "county") %>%
#   mutate(county_FIPS=geoid)
# tictoc::toc()
# beepr::beep()
# 
# geo_final <- bind_rows(
#   select(missing_geoid, -geoid),
#   mutate(filter(geocoded2, !is.na(county_FIPS)), county_FIPS=as.character(county_FIPS))
# ) %>%
#   mutate(county_FIPS=str_pad(county_FIPS, 5, pad="0"))
# 
# write_csv(geo_final, "geocode_npi.csv")
# rm(geocoded, missing_geoid, geocoded2)








sf_county <- tigris::counties()





df <- NSG %>% 
  filter(state!="GU", state!="PR", state!="AK", state!="HI") %>%
  select(npi:nppes_entity_code,
         provider_type:average_Medicare_standard_amt) %>%
  left_join(geo_final) %>%
  select(-county_name) %>%
  left_join(
    {as_tibble(county.fips) %>%
        rename(county_FIPS=fips, county=polyname) %>%
        mutate(county_FIPS=str_pad(county_FIPS, 5, pad="0"))}
  ) %>%
  group_by(county_FIPS, npi) %>% 
  summarise(n = sum(line_srvc_cnt)) %>%
  summarise(
    NPIs = n(),
    services = sum(n),
    rate = sum(n)/n()
  )

pal <- colorBin(palette = "YlGnBu", domain = df$NPIs)
geo_join(sf_county, df, "GEOID", "county_FIPS",  how="inner") %>%
  
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addPolygons(color="black", fill=F) %>%
  addPolygons(popup = ~str_glue("{NAME} county<br>Number of NPIs: <b>{NPIs}</b>"), 
              color = ~pal(NPIs),
              # stroke=F,
              fillOpacity = 0.5)





population <- tidycensus::get_acs(geography = "county", 
                                  variables = "B01001_001", geometry = T)
  


tictoc::tic("Start plot")

population %>%
  filter(!str_detect(GEOID, "72..."), !str_detect(GEOID, "02..."),
         !str_detect(GEOID, "15...")) %>%
  select(GEOID, NAME, population=estimate, geometry) %>%
  left_join(df, by=c("GEOID"="county_FIPS")) %>%
  
  mutate(Ratio = NPIs*100000/population) %>% # per 100,000 population
  
  ggplot(aes(fill = Ratio)) +
  geom_sf() +
  coord_sf(crs = 26914) +
  scale_fill_gradientn(colours = terrain.colors(5), na.value = "grey50")
tictoc::toc()






  



  

df <- NSG %>%
  filter(nppes_provider_state %in% state.abb) %>%
  group_by(zip, npi) %>% 
  summarise(n = sum(line_srvc_cnt)) %>%
  summarise(
    NPIs = n(),
    services = sum(n),
    rate = sum(n)/n()
  )
pal <- colorBin(palette = "YlGnBu", domain = df$NPIs)
geo_join(sf_zip, df, "GEOID10", "zip",  how="inner") %>%

  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addPolygons(color="black", fill=F) %>%
  addPolygons(popup = ~as.character(round(rate)), 
              color = ~pal(NPIs),
              # stroke=F,
              fillOpacity = 0.5)


# factpal <- colorFactor(c("grey", "#FF2700"), df$OurTract)

  # addLegend("bottomright", pal = pal, values = ~value,
  #           title = "Families below FPL",
  #           labFormat = labelFormat(suffix="%",
  #                                   transform = function(x) 100 * x
  #           ),
  #           opacity = 1
  # )
  

 
 
 NSG %>%
   filter(hcpcs_code %in% filter(count(NSG, hcpcs_code, hcpcs_description), n>4)$hcpcs_code) %>%
   lm(average_Medicare_payment_amt~nppes_provider_gender+medicare_participation_indicator+hcpcs_code, data=.) %>%
   summary()
 
 
 

 
 
 
 ## INPATIENT
 inpatient <- "https://data.cms.gov/resource/tcsp-6e99.csv" %>% 
   read.socrata() %>%
   as_tibble() 
 

 
 x <- geo_npi %>% mutate(lat = round(lat,3), lon=round(lon, 3)) %>%
   filter(state %in% state.abb) %>% 
   filter(state != "AK", state != "HI") %>% 
   count(lat, lon) 
   
 population %>%
   # filter(!str_detect(GEOID, "72..."), !str_detect(GEOID, "02..."),
   #        !str_detect(GEOID, "15...")) %>%
   filter(!str_detect(GEOID, "72...")) %>%
   select(GEOID, NAME, population=estimate, geometry) %>%
   left_join(df, by=c("GEOID"="county_FIPS")) %>%
   
   mutate(Ratio = NPIs*100000/population) %>%
   
   left_join(TBIs, by=c("GEOID"="CountyFIPS")) %>%
   replace_na(list(NPIs=0, Ratio=0)) %>%
   ggplot(aes(x=Crude_Rate, y=Ratio)) + geom_jitter(alpha=.1) +
   ylim(c(NA,20)) + xlim(c(NA,75)) + geom_smooth()
   
