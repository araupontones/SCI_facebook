#Clean SP.POP.TOTL indicator table from WB,
# Check that all iso2 match with countrycoodes
#drop aggregated regions (i.e. "IBRD only", "High Income", etc.)

source("set_up.R")

#red country codes --------------------------------------------------------------
countrycodes = import(file.path(dir_clean, "countrycodes.rds"))

#read world bank data -----------------------------------------

pop = wbsearch("GDP") #search for indicators name

#Totalpopulation: SP.POP.TOTL
#GDP per capita (current US$): 


wb_data = wb(indicator =c("SP.POP.TOTL", "NY.GDP.PCAP.CD"))
                          

#check iso2c matches with clean country codes ---------------------------------------
unmatched = setdiff(unique(wb_data$iso2c), unique(countrycodes$iso2c)) 
unique(wb_population$country[wb_data$iso2c %in% unmatched]) ## All match except for aggregted regions

names(wb_data)

## keep only countries (drop regions from the dataset)
wb_clean = wb_data %>%
  filter(!iso2c %in% unmatched) %>%
  #make wider so each indicator is a column
  pivot_wider(id_cols = -c(indicator, indicatorID),
              names_from = "indicatorID",
              values_from = "value") %>%
  #rename indicator and select relevant variables
  rename(Year = date) %>%
  select(iso2c,country,Year,SP.POP.TOTL,NY.GDP.PCAP.CD ) %>%
  arrange(country, Year)


#Check correct merge
setdiff(unique(wb_clean$iso2c), unique(countrycodes$iso2c))
not_in_countrycodes = setdiff(unique(countrycodes$iso2c), unique(wb_clean$iso2c))
countrycodes$Country[countrycodes$iso2c %in% not_in_countrycodes] ##All match!


## export
export(wb_clean, file.path(dir_clean, "WB_POP_GDP.rds"))


