#Clean SP.POP.TOTL indicator table from WB,
# Check that all iso2 match with countrycoodes
#drop aggregated regions (i.e. "IBRD only", "High Income", etc.)

source("set_up.R")

#red country codes --------------------------------------------------------------
countrycodes = import(file.path(dir_clean, "countrycodes.rds"))

#read world bank total population data -----------------------------------------

#pop = wbsearch("total population")

wb_population = wb(indicator ="SP.POP.TOTL")

#check iso2c matches with clean country codes ---------------------------------------
unmatched = setdiff(unique(wb_population$iso2c), unique(countrycodes$iso2c)) 
unique(wb_population$country[wb_population$iso2c %in% unmatched]) ## All match except for aggregted regions



## keep only countries (drop regions from the dataset)
wb_population_clean = wb_population %>%
  filter(!iso2c %in% unmatched) %>%
  #rename indicator and select relevant variables
  rename(SP_POP_TOTL= value,
         Year = date) %>%
  select(iso2c,country,Year,SP_POP_TOTL) %>%
  arrange(country, Year)


#Check correct merge
setdiff(unique(wb_population_clean$iso2c), unique(countrycodes$iso2c))
not_in_countrycodes = setdiff(unique(countrycodes$iso2c), unique(wb_population_clean$iso2c))
countrycodes$Country[countrycodes$iso2c %in% not_in_countrycodes] ##All match!


## export
export(wb_population_clean, file.path(dir_clean, "WB_POP_TOTL.rds"))


