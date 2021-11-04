#2021.11.04
##load all library
library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)

#read in all the spatial data and reproject it

OSM <- st_read(here('data',
                    'greater-london-latest-free.shp',
                    'gis_osm_pois_a_free_1.shp')) %>%
  st_transform(.,27700) %>%
  #select hotels only
  filter(fclass == 'hotel')


WorldCities <- st_read(here('data',
                            'World_Cities',
                            'World_Cities.shp')) %>%
  st_transform(.,27700)


UK_outline <- st_read(here('data',
                           'gadm36_GBR_shp',
                           'gadm36_GBR_0.shp')) %>%
  st_transform(.,27700)

#London Borough data is already in 277000
Londonborough <- st_read(here('data',
                              'statistical-gis-boundaries-london',
                              'ESRI',
                              'London_Borough_Excluding_MHW.shp')) %>%
  st_transform(.,27700)

#read in the .csv
#and make it into spatial data

Airbnb <- read_csv(here('data',
                        'listings.csv',
                        'listings.csv')) %>%
  st_as_sf(.,coords=c('longitude','latitude'), crs=4326) %>%
  st_transform(.,27700) %>%
  #select entire places that are available all year
  filter(room_type =="Entire home/apt" & availability_365 =='365')

# make a function for the join
# functions are covered in practical 7
# but see if you can work out what is going on
# hint all you have to do is replace data1 and data2
# with the data you want to use

Joinfun <- function(data1,data2){
  
  output <- data1 %>%
    st_join(Londonborough,.) %>%
    add_count(GSS_CODE,name='hotels_in_borough')
  
  return(output)
}

#use the function for hotels
Hotels <- Joinfun(OSM,Londonborough)

#then forairbnb 
Airbnb <- Joinfun(Airbnb,Londonborough)

WorldCities2 <- WorldCities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           WorldCities$CITY_NAME=='Birmingham'|
           WorldCities$CITY_NAME=='London'|
           WorldCities$CITY_NAME=='Edinburgh')

newbb <- c(xmin=-296000,ymin=5408,xmax=655696,ymax=1000000)