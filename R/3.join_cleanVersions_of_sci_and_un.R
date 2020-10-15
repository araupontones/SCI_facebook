source("R/set_up.R")

#'read clean version of un stock
clean_un = import(file.path(dir_clean, "un_migration_stock_long.rds"))


#'read clean version of sci
clean_sci = import(file.path(dir_clean, "facebook_sci_clean.rds"))



#'check compatibility
#'SCI data excludes the following Countries
#'Afghanistan, Western Sahara, China, Cuba, Iraq, Israel, Iran, North Korea, 
#'Russia, Syria, Somalia, South Sudan, Sudan, Venezuela, Yemen, Crimea, Jammu and Kashmir, 
#'Donetsk, Luhansk, Sevastopol, West Bank, and Gaza.

umatched_from_un_to_sci = subset(clean_un, ID %in% setdiff(unique(clean_un$ID), unique(clean_sci$ID)))

#' the clean UN data only keep records for combination that have at least 1 person from the origin country
umatched_from_sci_to_un = subset(clean_sci, ID %in% setdiff(unique(clean_sci$ID), unique(clean_un$ID)))



clean_un_sci = left_join(clean_un, clean_sci) %>%
  filter(!is.na(user_loc)) %>%
  #' remove reduntant columns
  select(-c("Country_user", "Country_friend", ends_with("_loc"), "Year")) %>%
  #' relocate variables just to make the data easier to read
  relocate(ID, Origin, Destination, starts_with("Code"), starts_with("iso"))


export(clean_un_sci, file.path(dir_clean, "joint_un_sci_clean.rds"))
