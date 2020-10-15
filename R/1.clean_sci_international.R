
source("R/set_up.R")

#' data with iso codes  
codelist = countrycode::codelist %>%
  select(country.name.en, iso2c) %>%
  rename(Country = country.name.en) %>%
  mutate(iso2c = if_else(Country == "Kosovo", "XK", iso2c))

#' raw data from facebook
clean_sci_international = import(file.path(dir_raw, "country_country_aug2020.tsv")) %>%
  left_join(codelist, by = c("user_loc"="iso2c")) %>%
  rename(Country_user = Country) %>%
  left_join(codelist, by=c("fr_loc"= "iso2c")) %>%
  rename(Country_friend = Country) %>%
  #' remove observations where country friend == country user
  filter(Country_friend!=Country_user) %>%
  #"unique identifier of the combination 
  mutate(ID = map2_chr( user_loc, fr_loc, ~str_flatten(sort(c(.x,.y))) )) %>%
  #'keep only one observation of the combination
  group_by(ID) %>%
  filter(row_number()==1) %>%
  ungroup()


rio::export(clean_sci_international, file.path(dir_clean, "facebook_sci_clean.rds"))





