#'set up

library(pacman)
# devtools::install_github("hadley/emo")
library(emo)

options("scipen"=100, "digits"=4)

p_load(countrycode, tidyverse, rio, gt, gtrendsR, wbstats, ggtext,gmt)

#' Set paths
  
  dir_data = "1.Data"
  dir_raw = file.path(dir_data,"raw_data")
  dir_clean = file.path(dir_data,"clean_data")
  dir_plots = file.path("5.plots")





