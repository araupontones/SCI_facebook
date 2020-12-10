##Join all datasets
source("set_up.R")

#read data clean data --------------------------------------------------------------------
un_sci = import(file.path(dir_clean, "joint_un_sci_clean.rds"))

population = import(file.path(dir_clean, "WB_POP_TOTL.rds")) %>%
  filter(Year == 2019)

countrycodes = import(file.path(dir_clean, "countrycodes.rds"))


##Check compatibility -----------------------------------------------------------

#in SCI but bot in WB

setdiff(unique(un_sci$iso2_origin), unique(population$iso2c)) #"GF" "GP" "MQ" "RE" "YT" NA   "TW" arenot in WB data

#in WB but not in UN_SCI
setdiff(unique(population$iso2c), unique(un_sci$iso2c_origin))


##Fetch indicators from WB population 
un_sci$SP_POP_TOTL_origin = population$SP_POP_TOTL[match(un_sci$iso2_origin, population$iso2c)]
un_sci$SP_POP_TOTL_destination = population$SP_POP_TOTL[match(un_sci$iso2_destination, population$iso2c)]

#Fetch indicators from country codes
un_sci$Region_origin = countrycodes$region[match(un_sci$iso2_origin, countrycodes$iso2c)]
un_sci$Region_destination = countrycodes$region[match(un_sci$iso2_destination, countrycodes$iso2c)]

un_sci$Region_sub_origin = countrycodes$region_sub[match(un_sci$iso2_origin, countrycodes$iso2c)]
un_sci$Region_sub_destination = countrycodes$region_sub[match(un_sci$iso2_destination, countrycodes$iso2c)]


un_sci$Income_origin = countrycodes$income[match(un_sci$iso2_origin, countrycodes$iso2c)]
un_sci$Income_destination = countrycodes$income[match(un_sci$iso2_destination, countrycodes$iso2c)]


##Create categories of combination between countries ---------------------------
un_sci_pop = un_sci %>%
  #Region and income
  mutate(flow_region = paste(Region_origin, Region_destination, sep = " to "),
         flow_subregion = paste(Region_sub_origin, Region_sub_destination, sep = " to "),
         flow_income = paste(Income_origin, Income_destination, sep = " to ")) %>%
  relocate(all_of(c("ID","ID_direction","seq")), .after = Year) %>%
  relocate(c(starts_with("Code"), starts_with("iso")), .after = flow_income) %>%
  ##Population bilateral
  rowwise() %>%
  mutate(SP_TOTOAL_bilateral=sum(SP_POP_TOTL_origin, SP_POP_TOTL_destination, na.rm = T), 
         .after = SP_POP_TOTL_destination
         ) %>%
  ungroup()

  

#Export
export(un_sci_pop, file.path(dir_clean, "joint_un_sci_pop_codelist.rds"))
