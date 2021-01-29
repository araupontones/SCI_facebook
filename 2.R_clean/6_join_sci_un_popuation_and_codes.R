##Join all datasets
source("set_up.R")

#read data clean data --------------------------------------------------------------------
un_sci = import(file.path(dir_clean, "joint_un_sci_clean.rds"))

wb_clean = import(file.path(dir_clean, "WB_POP_GDP.rds")) %>%
  filter(Year == 2019)

countrycodes = import(file.path(dir_clean, "countrycodes.rds"))


##Check compatibility -----------------------------------------------------------

#in SCI but bot in WB

setdiff(unique(un_sci$iso2_origin), unique(wb_clean$iso2c)) #"GF" "GP" "MQ" "RE" "YT" NA   "TW" arenot in WB data

#in WB but not in UN_SCI
setdiff(unique(wb_clean$iso2c), unique(un_sci$iso2c_origin))



##Fetch indicators from WB data -----------------------------------------------


un_sci %>%
  #fetch indicators for origin country 
  left_join(select(wb_clean, -c(Year, country)), by=c("iso2_origin" = "iso2c")) %>%
  left_join(select(wb_clean, -c(Year, country)), by=c("iso2_destination" = "iso2c"), 
            suffix = c("_origin", "_destination"))  -> un_sci_wb

#Fetch indicators from countrycodes

un_sci_wb %>%
  left_join(select(countrycodes, -c(country, Country)),by=c("iso2_destination" = "iso2c")) %>%
  left_join(select(countrycodes, -c(country, Country)),by=c("iso2_destination" = "iso2c"),
            suffix = c("_origin", "_destination"))  -> un_sci_wb_cc
  



##Create categories of combination between countries ---------------------------
joint_data_clean = un_sci_wb_cc %>%
  #Region and income
  mutate(flow_region = paste(region_origin, region_destination, sep = " to "),
         flow_subregion = paste(region_sub_origin, region_sub_destination, sep = " to "),
         flow_income = paste(income_origin, income_destination, sep = " to ")) %>%
  relocate(all_of(c("ID","ID_direction","seq")), .after = Year) %>%
  relocate(c(starts_with("Code"), starts_with("iso")), .after = flow_income) %>%
  ##Population bilateral
  rowwise() %>%
  mutate(SP_TOTOAL_bilateral=sum(SP.POP.TOTL_origin, SP.POP.TOTL_destination, na.rm = T), 
         .after = SP.POP.TOTL_destination
         ) %>%
  ungroup() %>%
  #calculate distance betwen countries
  mutate(across(starts_with("lat"), as.numeric),
         across(starts_with("lon"), as.numeric),
         distance_km = geodist(Nfrom = lat_origin,
                               Efrom = long_origin,
                               Nto = lat_destination,
                               Eto = lat_destination,
                               units = "km"))

  

#Export
export(joint_data_clean, file.path(dir_clean, "joint_un_sci_pop_codelist.rds"))
