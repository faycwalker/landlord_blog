library(tidycensus)
library(tidyverse)
library(sf)
if (!require(pacman)) install.packages('pacman')
library(pacman)
p_load(tidyverse, skimr, janitor, sf, beepr, stringr, stringdist)

##read in data
pittsburgh <- read_csv("assessments.csv")

### CREATE ADDRESS COLUMNS
pittsburgh_res <- pittsburgh %>%
  select(-c(MUNICODE,MUNIDESC,SCHOOLCODE, SCHOOLDESC,
         NEIGHCODE, NEIGHDESC,NEIGHCODE,TAXCODE,
         TAXDESC, TAXSUBCODE, TAXSUBCODE_DESC,
         OWNERCODE, LOTAREA,COUNTYLAND,FAIRMARKETLAND,
         STYLE, STORIES,EXTERIORFINISH,EXTFINISH_DESC, ROOF,
         BASEMENT,BASEMENTDESC, GRADE, GRADEDESC, CONDITION,
         CONDITIONDESC, CDU, CDUDESC, TOTALROOMS,
         BEDROOMS, FULLBATHS, HALFBATHS, HEATINGCOOLING,
         HEATINGCOOLINGDESC, FIREPLACES,BSMTGARAGE,
         FINISHEDLIVINGAREA,CARDNUMBER,ALT_ID,
         CLEANGREEN,ABATEMENTFLAG)) %>%
  filter(CLASSDESC=="RESIDENTIAL") %>%
  mutate(ind_llc=case_when(grepl("CORPORATION",OWNERDESC)~"LLC",
                           grepl("REGULAR",OWNERDESC)~"Individual"),
         parcel_address=paste(PROPERTYHOUSENUM,"",PROPERTYADDRESS,",",
                              PROPERTYCITY,",",PROPERTYSTATE,",",
                              PROPERTYZIP),
         tax_address=paste(CHANGENOTICEADDRESS1,"",
                           CHANGENOTICEADDRESS2,",",
                           CHANGENOTICEADDRESS3,",",
                           CHANGENOTICEADDRESS4)) %>%
  select(PARID,parcel_address,
         tax_address,HOMESTEADFLAG,ind_llc) %>%
  mutate(HOMESTEADFLAG=ifelse(is.na(HOMESTEADFLAG),0,1),
         parcel_address=str_trim(parcel_address, side = c("both")),
         parcel_address=str_squish(parcel_address),
         tax_address=str_trim(tax_address, side = c("both")),
         tax_address=str_squish(tax_address),
         matching_tax_address = ifelse(tax_address ==parcel_address, 1,0))

#create two different datasets to geocode
#tax_addresses <- pittsburgh_res %>%
#  select(PARID, f_address=tax_address)

#parcel_addresses <- pittsburgh_res %>%
#  select(PARID, f_address=parcel_address)

#write.csv(tax_addresses, "tax_addresses.csv")
#write.csv(parcel_addresses, "parcel_addresses.csv")
#clean
parcels_geocoded <- read_csv('parcel_addresses-geocoded.csv') %>%
  select(PARID, parcel_addr=Match_addr, parcel_short=StAddr,
         County=Subregion, MetroArea, Latitude, Longitude,
         geometry) %>%
  mutate(parcel_zip=str_sub(parcel_addr, -5,-1),)
tax_geocoded <- read_csv('tax_addresses-geocoded.csv') %>%
  select(PARID, tax_addr=Match_addr, tax_short=StAddr, 
         t_county=Subregion, t_metro=MetroArea, 
         Latitude, Longitude,
         geometry) %>%
  mutate(tax_zip=str_sub(tax_addr, -5,-1))


#join them all together and create new matching column
joined <- pittsburgh_res %>%
  select(-matching_tax_address) %>%
  left_join(parcels_geocoded, by="PARID") %>%
  left_join(tax_geocoded, by="PARID") %>%
  mutate(matching_tax_address = ifelse(tax_short ==parcel_short, 1,0))  %>%
  select(PARID, parcel_addr, parcel_short, tax_addr, tax_short, matching_tax_address,
         Latitude=Latitude.x, Longitude=Longitude.x, parcel_zip=PROPERTYZIP,
         p_county=County, t_county, HOMESTEADFLAG, ind_llc, tax_zip) %>%
  mutate(
         match_zip=ifelse(parcel_zip==tax_zip, 1,0),
         match_county=ifelse(p_county==t_county, 1,0)
         )

#turn into sf
joined_sf <-  st_as_sf(joined, coords = c("Longitude", "Latitude"),  crs=4326)

### CUT DOWN TO ONLY THOSE WITHOUT HOMESTEAD
rental_sf <- joined_sf %>%
  filter(HOMESTEADFLAG==0)

###304k/519k are homestead exempt
#358k are not matching
###463k are individuals
#215010 rental households in Pittsburgh, how does
#that compare to the census count?

##group by corp/indidvual, come up with count
rental_count <- rental_sf %>%
  as.data.frame() %>%
  group_by(ind_llc) %>%
  summarize(count=n()) %>%
  mutate(percent=count/(sum(count)))

who_owns_what <- rental_sf %>%
  as.data.frame() %>%
  group_by(tax_short,match_county) %>%
  summarize(count=n()) %>%
  mutate(mom_pop=ifelse(count<10 & match_county==1, 1,0))

#How many share zip codes?
#412k match zips, 107k don't match'

#how many share metros?
#478k are in the same county, 41k are not

#How many are mom and pop landlords
#101k are mom and pop, 15k are not

##what are the demographics of the areas they live in
my_vars<- c(pop = "B01003_001",
            pov_total = "B17001_001",
            white_alone = "B03002_003", 
            latino = "B03003_003",black = "B02001_003", 
            white = "B02001_002", asian = "B02001_005", 
            in_pov = "B17001_002",
            occ_status="B25002_001",
            vacant="B25002_003",
            med_year_built="B25035_001",
            total_tenure="B25003_001",
            owners="B25003_002",
            renters="B25003_003")

zcta_demos <- get_acs(geography = "zcta",
                         variables = my_vars)

zcta_demos_clean <- zcta_demos %>%
  mutate(ZIP=gsub("ZCTA5 ","",NAME)) %>%
  select(-moe, -GEOID,-NAME) %>%
  spread(variable, estimate)

tax_zips <- tax_geocoded %>%
  pull(tax_zip) %>%
  unique()

parcel_zips <- parcels_geocoded %>%
  pull(parcel_zip) %>%
  unique()

tax_zip_demos <- zcta_demos_clean %>%
  filter(ZIP %in% tax_zips)

parcel_zip_demos <- zcta_demos_clean %>%
  filter(ZIP %in% parcel_zips)

#What's the average distance between?
tax_geocoded_sf <- st_as_sf(tax_geocoded, coords = c("Longitude", "Latitude"),  crs=4326)
parcels_geocoded_sf <- st_as_sf(parcels_geocoded, coords = c("Longitude", "Latitude"),  crs=4326)
