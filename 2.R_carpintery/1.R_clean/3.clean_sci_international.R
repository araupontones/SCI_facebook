
##Clean SCI data from FACEBOOK
source("set_up.R")


##read country codes ---------------------------------------------------------
countrycodes = import(file.path(dir_clean, "countrycodes.rds")) 


#' drop these countries (not in worldbank population data)

drop_iso = c("YT", "RE", "GF", "JE", "MQ", "GP", NA)

#' Read and clean raw data from facebook ----------------------------------------
raw_sci = import(file.path(dir_raw, "country_country_aug2020.tsv")) %>%
  #drop observations with missing iso for user or friend
  filter(
    !is.na(user_loc),
    !is.na(fr_loc),
    #drop duplicated combinations
    user_loc != fr_loc
  )


#check compatibility ----------------------------------------------------------

not_in_codes = setdiff(unique(raw_sci$fr_loc), countrycodes$iso2c)

#Countries in Facebook but not in country codes
#french Guiana, Guadalupe, Hong Kong, Jersey, Macao, martinique, Palestine, Reunion, Taiwan, Mayotte



#Cleand SCI -------------------------------------------------------------------

clean_sci = raw_sci %>%
  #drop observations not in countrycodes ----------------------------------
filter(
  !user_loc %in% not_in_codes,
  !fr_loc %in% not_in_codes,
) %>%
  #"unique identifier of the combination -----------------------------------
mutate(
  ID = map2_chr( user_loc, fr_loc, ~str_flatten(sort(c(.x,.y))) )
) %>%
  #'keep only one observation of the combination
  group_by(ID) %>%
  filter(
    row_number()==1
  ) %>%
  ungroup()

#get country names of friends from country codes
clean_sci$Country_user = countrycodes$Country[match(clean_sci$user_loc, countrycodes$iso2c)]
clean_sci$Country_friend = countrycodes$Country[match(clean_sci$fr_loc, countrycodes$iso2c)]

#order variables
clean_sci = relocate(clean_sci, c(ID, starts_with("Country")), before = user_loc)

#Export
export(clean_sci, file.path(dir_clean, "facebook_sci_clean.rds"))





