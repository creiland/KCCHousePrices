require(dplyr)
require (tidyr)
require(lubridate)
library(httr)
library(jsonlite)
library(rgeos)
library(sp)
library(rgdal)
library(ZillowR)
library(ggmap)
library(nominatim)



options(stringsAsFactors = FALSE)

setwd("C:/Users/creil/Desktop/Info370/Final")

data <- read.csv("data/kc_house_data.csv")

#########################Census Income Data#############################
income <- read.csv("data/ACS_15_5YR_income.csv")

#take relevant columns from income data
income_clean <- select(income, GEO.id2, HC01_EST_VC13, HC01_EST_VC15)

#clean column names
colnames(income_clean) <- c("zipcode", "median_income", "mean_income")

#remove row 1
income_clean <- income_clean[-c(1),]


factorToNum <- function (f){
  return(as.numeric(levels(f))[f])
}

#change the columns from factors to numeric
income_clean$zipcode <- as.numeric(income_clean$zipcode)
income_clean$mean_income <- as.numeric(income_clean$mean_income)
income_clean$median_income <- as.numeric(income_clean$median_income)

#join data by  zipcode
joined <- left_join(data, income_clean, by="zipcode")

#add more specific column names to new columns
colnames(joined)[22:23] <- paste(colnames(joined)[22:23], "_by_zip", sep="")

################################Census Population Data#####################################################
population <- read.csv("data/ACS_15_5YR_population.csv")

#take relevant columns from income data
pop_clean <- select(population, GEO.id2, HD01_VD01)

#clean column names
colnames(pop_clean) <- c("zipcode", "population_by_zip")

#remove row 1
pop_clean <- pop_clean[-c(1),]


#change the columns from factors to numeric
pop_clean$zipcode <- as.numeric(pop_clean$zipcode)
pop_clean$population_by_zip <- as.numeric(pop_clean$population_by_zip)

#join data by  zipcode
joined <- left_join(joined, pop_clean, by="zipcode")

################################Census Employment Data#####################################################
employ <- read.csv("data/ACS_15_5YR_employment.csv")

#take relevant columns from income data
employ_clean <- select(employ, GEO.id2, HC01_EST_VC01)

#clean column names
colnames(employ_clean) <- c("zipcode", "people_employed_by_zip")

#remove row 1
employ_clean <- employ_clean[-c(1),]


#change the columns from factors to numeric
employ_clean$zipcode <- as.numeric(employ_clean$zipcode)
employ_clean$people_employed_by_zip <- as.numeric(employ_clean$people_employed_by_zip)

#join data by  zipcode
joined <- left_join(joined, employ_clean, by="zipcode")

################################Census Education Data#####################################################
ed <- read.csv("data/ACS_15_5YR_education.csv")

#take relevant columns from income data
ed_clean <- select(ed, GEO.id2, HD01_VD01, HD01_VD06, HD01_VD07)

#clean column names
colnames(ed_clean) <- c("zipcode", "people_educated_by_zip", "bachelor_degrees_by_zip", "grad_degrees_by_zip")

#remove row 1
ed_clean <- ed_clean[-c(1),]


#change the columns from factors to numeric
ed_clean$zipcode <- as.numeric(ed_clean$zipcode)
ed_clean$people_educated_by_zip <- as.numeric(ed_clean$people_educated_by_zip)
ed_clean$bachelor_degrees_by_zip <- as.numeric(ed_clean$bachelor_degrees_by_zip)
ed_clean$grad_degrees_by_zip <- as.numeric(ed_clean$grad_degrees_by_zip)

#join data by  zipcode
joined <- left_join(joined, ed_clean, by="zipcode")



################################King County Neighborhood Data################################################

#read in shp file 
KC_neighborhoods <- readOGR(dsn = "data/neighborhood", layer="neighborhood")


GisToDf <- function(shpData, columnToJoin, newColName){
  #copy joined and update coordinates for joined dataset
  joined_copy <- data.frame(joined)
  sp::coordinates(joined_copy) <- ~ long + lat
  proj4string(joined_copy) <- CRS("+proj=longlat")
  
  #honestly not sure what this does but it works
  #I think it combines the coordinate data for the joined_copy and the 
  joined_copy <- spTransform(joined_copy, proj4string(shpData))
  sp::proj4string(joined_copy) <- proj4string(shpData)
  
  #joins the shpdata to the joined dataset
  joined_copy <- sp::over(joined_copy, shpData)
  return(joined_copy)
}


#dataframe for neighborhood data
nh <- GisToDf(KC_neighborhoods)

#join neighborhood to joined
joined$neighborhood <- nh$NEIGHBORHO

#set NAs for neighborhood to "No Neighborhood"
joined$neighborhood <- ifelse(is.na(joined$neighborhood), 
                             'No Neighborhood', joined$neighborhood)


###############################KC Parcel Data#######################################
parcel <- read.csv("data/Parcels_for_King_County_with_Address_with_Property_Information__parcel_address_area.csv")

colnames(parcel) <- tolower(colnames(parcel))
colnames(parcel)[22] <- "long"
colnames(parcel)[4] <- "id"

joined$id <- as.character(joined$id)


#join addresses by parcel number
temp <- left_join(joined, parcel, by=c("id"))

joined$address <- temp$addr_full

###############################MapQuest API for address###################################


#get remaining missing addresses by lat and long using OpenStreetMapApi
mapquest_key <- "	IFSHTrCJv54chVrndkMyXb5V4qsnTG93"

#create new df with just NA addresses
addrNA <- joined %>% filter(is.na(address)) %>% select(lat, long, address)

#add index
addrNA$index <- seq.int(nrow(addrNA))

#limit to 15000 for api calls
addrNA <- filter(addrNA, index < 14500)

#populate dataframe with addresses by reverse geocoding from lat and long
#addrNA$address <- reverse_geocode_coords(addrNA$lat, addrNA$long, key = mapquest_key) %>% select(display_name)


addressCopy <- data.frame(addrNA)


joined <- joined %>% left_join()
###############################Zillow API Data###################################

#ideas for columns: median price of comparable sales, list price, last sale price, 

ZWSID <- "X1-ZWz18a8v9t2x3f_1acr8"
set_zillow_web_service_id(ZWSID)




##############################Airbnb data#######################################


###############################Yelp Data#######################################

