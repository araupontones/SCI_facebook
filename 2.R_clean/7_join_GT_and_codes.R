
source("set_up.R")
library("dplyr")
library("stringr")
# devtools::install_github("PMassicotte/gtrendsR")
library("gtrendsR")



# File path
dir_GT <- file.path(dir_data,"GT_data")
dir_GT_2019 <- file.path(dir_GT,"2019")

# Joint SCI and UN stock data
joint_un_sci_clean = import(file.path(dir_clean, "joint_un_sci_clean.rds"))
Geo_vs_country_origin1 <- import(file.path(dir_GT, "ISO_Country.RData"))

################################ Corresponding GT data  ################################

## Yearly aggregation functions
Agg_yearly <- function(GTdata, options = "mean"){
  
  if(options == "mean"){
    Yearly_data <- GTdata[[1]]  %>% 
      mutate(Year =str_extract(date, "^.{4}")) %>% 
      mutate(hits = gsub("[^0-9.-]", "", hits)) %>% 
      group_by(Year, geo) %>% 
      summarise(hits_average = mean(as.numeric(hits))) %>% ungroup()
  }else{
    Yearly_data <- GTdata[[1]]  %>%
      mutate(Year =str_extract(date, "^.{4}")) %>% 
      mutate(hits = gsub("[^0-9.-]", "", hits)) %>%
      select(Year, hits) %>% 
      group_by(Year) %>% 
      filter(!duplicated(hits == 0) | hits != 0) %>% # filter out the zeros and only keep one of them
      summarise(hits_average = median(as.numeric(hits), na.rm = TRUE)) %>% ungroup()
    
    
  }
  
  
  return(Yearly_data)
  
}

# extract the corresponding GT index
GT_index_2019<- function(serach_terms, Origin,
                         language = "en",
                         category =555){
  
  origin_code <- Geo_vs_country_origin1[which(Geo_vs_country_origin1$country ==  Origin),][[1]]
  # Origin <- Geo_vs_country_origin1[which(Geo_vs_country_origin1$country_code ==  origin_code),][[3]]
  
  # Make a dataframe first.
  
  # Approach 1 (Destination as keywords, category as 555)
  GT<-gtrends(keyword = serach_terms,geo=c(origin_code), 
                category = category,
                time='2019-01-01 2019-12-31',hl=language,
                gprop = "web", onlyInterest = TRUE)[1]
  
  if(is.null(GT[[1]])){
    
    GT_1_agg <- data_frame(Year = as.character(2019),
                           GT_index_mean = as.numeric(NA),
                           GT_index_median = as.numeric(NA),
                           Origin_Country = Origin,
                           Destination = serach_terms)
  } else{
    GT_1_agg <- Agg_yearly(GT, options = "mean") %>% ungroup() %>%
      select(Year, hits_average) %>%
      rename(GT_index_mean = hits_average) %>% 
      left_join(Agg_yearly(GT, options = "Median"), by = "Year") %>% 
      rename(GT_index_median = hits_average)
  }
  
  
  GT_agg <- GT_1_agg %>% mutate(Origin_Country = Origin,
                                Destination = serach_terms)
  
  return(GT_agg)
}
GT_Germany_SY <- GT_index_2019("Germany", "Syria")

### Check the country iso code with SCI data

unmatches <- joint_un_sci_clean %>%  select(Origin) %>% distinct() %>% 
  anti_join(Geo_vs_country_origin1 %>% select(country), by = c("Origin" = "country") )# country doesnt matchs

# Country pairs we are interested in
Country_pairs <- joint_un_sci_clean %>% select(Origin, Destination) %>% 
  filter(!Origin %in% unmatches$Origin)

# loop thorough the country pairs and collect the data, do not run!

# y <- data.frame()
# for (i in c(7821:7900)) {
# 
#   M <- GT_index_2019(Origin = Country_pairs[i,1][[1]],
#                      serach_terms = Country_pairs[i,2][[1]])
# 
# 
#   y <- rbind.data.frame(y, M)
# 
#   Sys.sleep(3)
# 
# }

# GT_2019_7820_7900 <- y
# save(GT_2019_7820_7900, file = "GT_2019_7820_7900.RData")
# dir()
# GT_2019_1_1047<- get(load("GT_2019_1_1047.RData"))
# GT_2019_1048_1387<- get(load("GT_2019_1048_1387.RData"))
# GT_2019_1388_2990<- get(load("GT_2019_1388_2990.RData"))
# GT_2019_2991_4608<- get(load("GT_2019_2991_4608.RData"))
# GT_2019_4608_6220 <- get(load("GT_2019_4608_6220.RData"))
# GT_2019_6220_7820 <- get(load("GT_2019_6220_7820.RData"))
# GT_2019_7820_7900 <- get(load("GT_2019_7820_7900.RData"))
# 
# GT_2019 <- rbind.data.frame(GT_2019_1_1047,GT_2019_1048_1387,
#                             GT_2019_1388_2990,GT_2019_2991_4608,
#                             GT_2019_4608_6220,GT_2019_6220_7820,
#                             GT_2019_7820_7900)

# GT_2019 %>% distinct(Origin_Country, Destination)
# save(GT_2019, file = "GT_2019_full.RData")

#### GT index for 2019
GT_2019 <- import(file.path(dir_GT_2019, "GT_2019_full.RData"))

joint_un_sci_gt <- joint_un_sci_clean %>% 
  left_join(GT_2019, by  = c("Origin" = "Origin_Country", "Destination" = "Destination"))

