#### SET UP AND LOAD IN DATA TO PREP GEOCODING #### 
library(haven)
library(tidyverse)
library(sf)
library(janitor)

##Read in Pew Rental Data
philly_rentals <- read_dta('pew_data_file/PewLandlordsRentals2021.dta')

#Clean the data to geocode
#philly_rentals_to_gc <- philly_rentals %>%
#  mutate(f_address=paste(LOCATION, ", Philadelphia, PA")) %>%
#  select(PARCEL_NUM, f_address)

#philly_landlords_to_gc <- philly_rentals %>%
#  mutate(f_address=paste(NewMailing, NewMailCit, NewMailST,
#                         NewMailZip, sep=", ")) %>%
#  select(PARCEL_NUM, f_address)

#save csvs to geocode
#write_csv(philly_rentals_to_gc, "philly_rentals.csv")
#write_csv(philly_landlords_to_gc, "philly_landlords.csv")

#### GEOCODED DATA ####
#read in geocoded csvs
philly_rentals_gc <- read.csv('philly_rentals-geocoded.csv')

#clean and turn into sf
philly_rentals_sf <- philly_rentals_gc%>%
  st_as_sf(coords = c("Longitude", "Latitude"),  crs=4326)  %>%
  st_transform(2272)

##left join it back to the rentals and remove non-residential 
#and non-rentals
philly_rentals_sf_clean <- philly_rentals_sf %>%
  #pad 0s and make character
  mutate(PARCEL_NUM=as.character(str_pad(PARCEL_NUM, 9, pad = "0"))) %>%
  left_join(philly_rentals, by="PARCEL_NUM") %>%
  filter(Rental_Fla==1 & is.na(NonRes)) %>% 
  clean_names() %>%
  select(parcel_num, match_addr,
         region, subregion, metro_area, city, geometry,
         owner_1, llc_owner, pew_units
         )

#Philadelphia is 47% renters according to Census
#594778 total households, so should be close to 280,000 rentals
#255k, close enough well take it!!!

#### LANDLORDS ####
#read in geocoded csvs
philly_landlords_gc <- read.csv('philly_landlords-geocoded.csv')

#clean and turn into sf
philly_landlords_sf <- philly_landlords_gc%>%
  st_as_sf(coords = c("Longitude", "Latitude"),  crs=4326)  %>%
  st_transform(2272)

##left join it back to the rentals and remove non-residential 
#and non-rentals
philly_landlords_sf_clean <- philly_landlords_sf %>%
  #pad 0s and make character
  mutate(PARCEL_NUM=as.character(str_pad(PARCEL_NUM, 9, pad = "0"))) %>%
  left_join(philly_rentals, by="PARCEL_NUM") %>%
  filter(Rental_Fla==1 & is.na(NonRes)) %>% 
  clean_names() %>%
  select(parcel_num, match_addr,
         region, subregion, metro_area, city, geometry,
         owner_1, llc_owner, pew_units
  )

#clean the demographics sf to just Philly tracts
Philly_demos <- PA_demos %>%
  filter(grepl("Philadelphia County", NAME)) 

#join the rentals to the SF and creat RCAP/RCAA variables
philly_rentals_clean <- philly_rentals_sf_clean %>%
  st_join(Philly_demos, join=st_intersects) %>%
  mutate(
    #40+ % pov and over 50% not white
    RCAP = ifelse(pct_pov > .4 & pct_not_white > .5, 1,0),
    ###Over 80% white and over 2x national median inc, which is 69560
    RCAA = ifelse(pct_not_white <= .2 & med_hh_inc >=139120, 1, 0)
  )

#join the landlords to the SF and creat RCAP/RCAA variables
philly_landlords_clean <- philly_landlords_sf_clean %>%
  st_join(Philly_demos, join=st_intersects) %>%
  mutate(
    #40+ % pov and over 50% not white
    RCAP = ifelse(pct_pov > .4 & pct_not_white > .5, 1,0),
    ###Over 80% white and over 2x national median inc, which is 69560
    RCAA = ifelse(pct_not_white <= .2 & med_hh_inc >=139120, 1, 0)
  )

#calculate portion of rentals in RCAA
table(philly_rentals_clean$RCAA)
#NONE ARE IN RCAA??

#calculate portion of rentals in RCAP
table(philly_rentals_clean$RCAP)
#93647 are NOT in RCAP, 23939 are in RCAP

#calculate portion of landlords in RCAA
table(philly_landlords_clean$RCAA)
##NONE ARE IN RCAA DO WE NEED A DIFFERENT STANDARD?

#calculate portion of landlords in RCAP
table(philly_landlords_clean$RCAP)
#67153 are NOT in RCAP, 7877 are in RCAP


