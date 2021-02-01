#Clean codelist to keep only variables of interest and make iso codes consistent
#accross data sources

source("set_up.R")

#read wb country list
wb_countries <- wbcountries(lang = "en") %>%
  #select relevant variables --------------------------------------------------
  select(iso2c, country, capital, long, lat, income) %>%
  #keep only countries (drop regions or aggregates)
  filter(!is.na(capital))


#' read and clean data with iso codes  
codelist = countrycode::codelist %>%
  #select key variables ---------------------------------------------------------
  select(
    
    country.name.en, iso2c, un, un.region.name, un.regionsub.name
    
    ) %>%
  #rename for consistency -----------------------------------------------------
  rename(
    Country = country.name.en,
    region = un.region.name,
    region_sub = un.regionsub.name
    ) %>%
  #clean iso codes to join with world bank data --------------------------------
  mutate(iso2c = if_else(Country == "Kosovo", "XK", iso2c),
         region = if_else(Country == "Kosovo", "Europe", region),
         region_sub = if_else(Country == "Kosovo", "Southern Europe", region_sub), #Kosovo doesnt have a UN code
  )



#Check compatibility of world bank and codelist datasets-----------------------

  #Check which are only in world bank data
  in_wb_only = setdiff(unique(wb_countries$iso2c), unique(codelist$iso2c))
  wb_countries$country[wb_countries$iso2c %in% in_wb_only] ## These are only regions (we can exclude)
  
  #check which are only codelists data
  in_codelist_only = setdiff(unique(codelist$iso2c), unique(wb_countries$iso2c))
  codelist$Country[codelist$iso2c %in% in_codelist_only] ##Checked that they dont exist in wb's data


  
  
#Join datasets (keeping only observations that are in WB's)
  
  countrycodes = wb_countries %>%
    left_join(codelist) %>%
    relocate(un, .after = iso2c)
  




#Export
export(countrycodes, file.path(dir_clean, "countrycodes.rds"))


