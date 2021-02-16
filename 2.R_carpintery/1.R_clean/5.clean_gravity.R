#data downloaded from: http://www.cepii.fr/CEPII/en/bdd_modele/download.asp?id=8
#we are reading directly from the zip because the file is too large to commit in github

source("set_up.R")


#define paths of zip directory
zipfile = file.path(dir_raw, "Gravity_rds_V202010.zip")

#define paths to unzip file
unzip_countries = file.path(tempdir(), "Countries_V202010.Rds")
unzip_gravity = file.path(tempdir(), "Gravity_V202010.Rds")


## unzip files in temporary dir
unzip(zipfile,
      exdir = tempdir()
      )


#read gravity files
gravity_countries_raw = import(unzip_countries)
gravity_raw = import(unzip_gravity)



