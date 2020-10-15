
source("R/set_up.R")

#' data with iso codes  
codelist = countrycode::codelist %>%
  select(country.name.en, iso2c, un) %>%
  rename(Country = country.name.en) %>%
  mutate(un = as.character(un),
         iso2c = if_else(Country == "Kosovo", "XK", iso2c)) 


#'import un data from its download format
downloaded_un = import(file.path(dir_raw, "UN_MigrantStockByOriginAndDestination_2019.xlsx"), sheet = "Table 1")


#' CLEAN UN DATA
#' column names
col_names = str_remove(paste0(downloaded_un[11,], downloaded_un[10,]), "NA|Country or area of origin")
col_names = str_replace(col_names,"Major area, region, country or area of destination", "Destination")
names(downloaded_un) <- col_names




raw_un = 
  #'keep relevant observations
  subset(downloaded_un, str_detect(Year, "[0-9]")) %>%
  #'drop  this because they are not needed for the study
  select(-c("Notes", "Sort\r\norder", "Notes", "Type of data (a)", "Other South","Other North", "Total")) %>%
  #' droping these because we are interested at the country level only 
  filter(!Destination %in% c(Destination[1:23], 
                         "NORTHERN AMERICA", 
                         "EUROPE AND NORTHERN AMERICA",
                         "LATIN AMERICA AND THE CARIBBEAN",
                         "EASTERN AND SOUTH-EASTERN ASIA",
                         "NORTHERN AFRICA AND WESTERN ASIA",
                         "CENTRAL AND SOUTHERN ASIA",
                         "EUROPE",
                         "OCEANIA",
                         "Central Asia",
                         "Southern Asia",
                         "Eastern Asia",
                         "Middle Africa",
                         "Northern Africa",
                         "Southern Africa",
                         "Western Africa",
                         "Caribbean",
                         "Central America",
                         "South-Eastern Asia",
                         "Western Asia",
                         "Eastern Europe",
                         "Northern Europe",
                         "Southern Europe",
                         "Western Europe",
                         "South America",
                         #' remove these countries because they were not part of the country codes
                         "Channel Islands",
                         "Melanesia",
                         "Micronesia",
                         "Polynesia",
                         #'remove Australia/New Zealand because they are separated in the facebook data
                         "Australia / New Zealand"
                         )
         ) %>%
  filter(Year == "2019")

#'check that all the Countries exist in the country code

setdiff(sort(unique(raw_un$Code)),unique(countrycode::codelist$un))


#'recycle codes to identify the code of origin countries
codelist_un = select(raw_un, c("Destination", "Code"))


#'reshape to long format and remove combinations where stock is NA
clean_un_longer = raw_un %>%
  #' reshape from wide to long
  pivot_longer(!c("Year", "Destination", "Code"),
               names_to = "Origin",
               values_to = "Stock") %>%
  filter(!is.na(Stock)) %>%
  rename(Code_destination = Code) %>%
  #' get the UN code of the origin country
  left_join(codelist_un, by=c("Origin"="Destination")) %>%
  rename(Code_origin= Code) %>%
  #' get Iso2c for Destination Country
  left_join(codelist, by=c("Code_destination"="un")) %>%
  rename(iso2c_destination = iso2c) %>%
  #' get Iso2c for Origin Country
  left_join(codelist, by=c("Code_origin"="un")) %>%
  rename(iso2c_origin = iso2c) %>%
  select(-c("Country.x", "Country.y")) %>%
  #'define ID for the combination destination-origin
  mutate(Stock = as.numeric(Stock),
         ID = map2_chr( iso2c_destination, iso2c_origin, ~str_flatten(sort(c(.x,.y))) )) %>%
  arrange(ID, desc(Stock)) %>%
  #' keep the combination whith highest stock in the other country (where the migrtion net between countries is negative)
  group_by(ID) %>%
  filter(row_number() ==1) %>%
  ungroup()


rio::export(clean_un_longer, file.path(dir_clean, "un_migration_stock_long.rds"))
  



test_join = left_join(raw_un_longer, raw_sci_international) %>%
  filter(!is.na(fr_loc))


