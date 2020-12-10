##reshape UN stock data 
source("set_up.R")

#' read data with iso codes  --------------------------------------------------
countrycodes = import(file.path(dir_clean, "countrycodes.rds")) 


#'read UN data from its download format
downloaded_un = import(file.path(dir_raw, "UN_MigrantStockByOriginAndDestination_2019.xlsx"), sheet = "Table 1")


#' CLEAN UN STOCK DATA ---------------------------------------------------------------
    
#' re-format column names
    col_names = str_remove(paste0(downloaded_un[11,], downloaded_un[10,]), "NA|Country or area of origin")
    col_names = str_replace(col_names,"Major area, region, country or area of destination", "Destination")
    names(downloaded_un) <- col_names



#reshape UN raw data into a long format
raw_un = 
  #'keep relevant observations -------------------------------------------------
  subset(
    downloaded_un, str_detect(Year, "[0-9]")
         ) %>%
  #'drop  this because they are not needed for the study ----------------------
  select(
    -c("Notes", "Sort\r\norder", "Notes", "Type of data (a)", "Other South","Other North", "Total")
         ) 




#Check compatibility with country codes ---------------------------------------
  
  #'check that all the Countries exist in the country code

  not_in_countrycodes = setdiff(sort(unique(raw_un$Code)),unique(countrycodes$un))
  raw_un$Destination[raw_un$Code %in% not_in_countrycodes] ## all of thes are region and countries confirmed not to exist in wb's data


  #'recycle codes to identify the code of origin countries
  codelist_un = select(raw_un, c("Destination", "Code"))


#'reshape to long format and remove combinations where stock is NA
clean_un_longer = raw_un %>%
  #remove observations missing code and regions -------------------------------
  filter(
    !Code %in% not_in_countrycodes,
         !is.na(Code)
         ) %>%
  #' reshape from wide to long -------------------------------------------------
  pivot_longer(
    !c("Year", "Destination", "Code"),
    names_to = "Origin",
    values_to = "Stock"
    ) %>%
  #drop observations missing Stock
  filter(
    !is.na(Stock)
         ) %>%
  #rename Code so the code of the destination is identifiable -----------------
  rename(Code_destination = Code) %>%
  #Correct format of variables
  mutate(Stock = as.numeric(Stock))


  #' get the UN code of the origin country -------------------------------------
  clean_un_longer$Code_origin = codelist_un$Code[match(clean_un_longer$Origin, codelist_un$Destination)]
  #' get Iso2c for Destination Country
  clean_un_longer$iso2_destination = countrycodes$iso2c[match(clean_un_longer$Code_destination, countrycodes$un)]
  #' get Iso2c for Origin Country
  clean_un_longer$iso2_origin = countrycodes$iso2c[match(clean_un_longer$Code_origin, countrycodes$un)]

  ##Note: this countries are in UN but not in county codes
  unique(clean_un_longer$Origin[is.na(clean_un_longer$iso2_origin)])
  
  #Create unique ID for combination of countries
clean_un_longer_IDS = clean_un_longer %>%
  #drop if iso_origin does not exist in country codes (see line 80)
  filter(!is.na(iso2_origin),
         !is.na(iso2_destination)) %>%
  #'define ID for the combination destination-origin
  mutate(
        #ID bilateral  
        ID = map2_chr( iso2_destination, iso2_origin, ~str_flatten(sort(c(.x,.y)))),
         #ID of the origin-destionation combination
         ID_direction = paste0(iso2_origin, iso2_destination)
         ) %>%
  arrange(ID, Origin) %>%
  #' Create stock at destination and origin -------------------------------
  group_by(Year,ID) %>%
  relocate(Origin, .after = Year) %>%
  mutate(seq = row_number(),
         stocks_destination = case_when(seq == 1 ~ lead(Stock,1),
                                                      TRUE ~ lag(Stock, 1)
                                                      )
                                                   
         )%>%
  rename(stocks_origin = Stock) %>%
  arrange(Year, ID,Origin) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(stocks_bilateral = sum(stocks_destination, stocks_origin, na.rm = T)
         ) %>%
  ungroup() %>%
  ##Calculate flows -----------------------------------------------------------
  group_by(ID_direction) %>%
  arrange(ID_direction, Year) %>%
  mutate(flow_origin = stocks_origin - lag(stocks_origin, 1),
         flow_destination = stocks_destination - lag(stocks_destination, 1)) %>%
  ungroup() %>%
  #arrange table
  arrange(Year, ID, Origin) %>%
  relocate(c(starts_with("stock"),c(starts_with("flow"))), .after = Destination) %>%
  relocate(iso2_destination, .after = iso2_origin) %>%
  relocate(c(ID,ID_direction,seq), .before = Year)


#Export
rio::export(clean_un_longer_IDS, file.path(dir_clean, "un_migration_stock_long.rds"))
  








