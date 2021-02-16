source("set_up.R")
library("dplyr")
library("stringr")
# devtools::install_github("PMassicotte/gtrendsR")
library("gtrendsR")
library(DescTools)


######### Merge the current data

# Web population data with GDP
WB_POP_GDP = import(file.path(dir_clean, "WB_POP_GDP.rds"))
# UN, SCI, Population data
joint_un_sci_pop = import(file.path(dir_clean, "joint_un_sci_pop_codelist.rds"))

# OECD inflows
# OECD_data <- read.csv(file.path(dir_GT, "migrationData.csv")) %>% mutate_if(is.factor, as.character)
OECD_data <- read.csv(file.path(dir_GT, "migrationData_new.csv")) %>% mutate_if(is.factor, as.character)
OECD_inflow <- OECD_data_new %>%  
  filter(str_detect(Variable, "^Inflows")) %>% 
  group_by(Country.of.birth.nationality, Country, Year) %>% 
  summarise(Value = sum(Value)) %>% ungroup()
# for 2019
OECD_inflow_2019 <- OECD_inflow %>% filter(Year == 2019)
OECD_inflow_2019_1 <- OECD_inflow_2019  %>% 
  left_join(Geo_vs_country_origin1 %>% select(-country_name), by = c("Country.of.birth.nationality" = "country")) %>% 
  rename(iso_origin = country_code) %>% 
  left_join(Geo_vs_country_origin1 %>% select(-country_name), by = c("Country" = "country")) %>% 
  rename(iso_destination = country_code) 

OECD_inflow_2019_2 <- OECD_inflow_2019_1 %>% 
  select(iso_origin, iso_destination, Value) %>% rename(OECD_inflows = Value)


# A <- gtrends(keyword = "Germany",geo=c("SY"), 
#         category = 555,
#         time='2013-01-01 2019-12-31',hl="en",
#         gprop = "web", onlyInterest = TRUE)
# plot(A)



# joint 1 - GT
joint_un_sci_gt <- joint_un_sci_pop %>% 
  left_join(GT_2019 %>% select(-Year), by  = c("Origin" = "Origin_Country", "Destination" = "Destination"))

# joint 2 - OECD inflows
joint_un_sci_gt_oecd <- joint_un_sci_gt %>% 
  left_join(OECD_inflow_2019_2, by  = c("iso2_origin" = "iso_origin", "iso2_destination" = "iso_destination"))

# categorize the SCI scale
joint_un_sci_gt_oecd$scaled_sci_cat <-  CutQ(joint_un_sci_gt_oecd$scaled_sci, breaks = 3, labels=c("low", "medium", "high"))
# joint_un_sci_gt_oecd$scaled_sci_cat <-  CutQ(joint_un_sci_gt_oecd$scaled_sci, breaks = 3, labels=c(1, 2, 3))
joint_un_sci_gt_oecd$scaled_sci_cat2 <-  CutQ(joint_un_sci_gt_oecd$scaled_sci, breaks = 2, labels=c("low", "high"))


############################################### Analysis part ###################################
library(caTools)
# Select the variables
joint_un_sci_gt_oecd_analyse1 <- joint_un_sci_gt_oecd %>% 
  select(Year, Origin, Destination, flow_destination,OECD_inflows, 
         SP.POP.TOTL_origin, SP.POP.TOTL_destination, SP_TOTOAL_bilateral,
         # NY.GDP.PCAP.CD_origin, NY.GDP.PCAP.CD_destination, 
         flow_income, distance_km, 
         GT_index_mean, GT_index_median, 
         scaled_sci_cat, scaled_sci, scaled_sci_cat2)

joint_un_sci_gt_oecd_analyse1_clean <- joint_un_sci_gt_oecd_analyse1 %>% 
  filter_at(vars(-Year, -Origin, -Destination,), all_vars(!is.na(.)))

### Descriptive stats
str(joint_un_sci_gt_oecd_analyse1_clean)
summary(joint_un_sci_gt_oecd_analyse1_clean)
# plot(joint_un_sci_gt_oecd_analyse1_clean)
head(joint_un_sci_gt_oecd_analyse1_clean)
joint_un_sci_gt_oecd_analyse1_clean1 <- as.data.frame(joint_un_sci_gt_oecd_analyse1_clean)

# Scatter plot of SCI and GT

# Interactions between SCI and GT
scatter.smooth(x=joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci,
               y=joint_un_sci_gt_oecd_analyse1_clean1$GT_index_median, 
               main="SCI ~ GT", xlim = range(0, 10000))
# What we cans ee here is lower SCI index bilateral pairs mostl likely to have higher GT index.
# However, SCI is randomly distributed when GT index is lower.

scatter.smooth(x=joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci_cat,
               y=joint_un_sci_gt_oecd_analyse1_clean1$GT_index_median, 
               main="SCI ~ GT", xlim = range(1, 3))

### Lets check the linear correlation
# linear correlation
cor(joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci,
    joint_un_sci_gt_oecd_analyse1_clean1$GT_index_median)

cor(joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci,
    joint_un_sci_gt_oecd_analyse1_clean1$GT_index_mean)

par(mfrow=c(1, 1))
plot(joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci,
     joint_un_sci_gt_oecd_analyse1_clean1$GT_index_mean,
     type = "l", xlim = c(0,30000))

# it deosnt seems like pearson correlation is a good metric for them
# Lets check the non linear correlation
library(devtools)
# install_github("ProcessMiner/nlcor")
library(nlcor)
nlcor(joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci,
      joint_un_sci_gt_oecd_analyse1_clean1$GT_index_median, plt = F)

nlcor(joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci,
      joint_un_sci_gt_oecd_analyse1_clean1$GT_index_mean, plt = F)

# I dont think there is a non-linear relationship between them

### Lets check the correlation with glm model - so here categorize the SCI index

# GT median
logit_median <- glm(scaled_sci_cat2 ~ GT_index_median, 
             family = "binomial",data=joint_un_sci_gt_oecd_analyse1_clean1)
summary(logit_median)
y = joint_un_sci_gt_oecd_analyse1_clean1$scaled_sci_cat2
# Accuracy
sum(round(predict(logit_median, type = "response")) == as.numeric(y)) / length(y)
# Sensitivity
sum(round(predict(logit_median, type = "response")) == as.numeric(y) & as.numeric(y) == 1) /
  sum(as.numeric(y))
# Precision
sum(round(predict(logit_median, type = "response")) == as.numeric(y) & as.numeric(y) == 1) /
  sum(round(predict(logit_median, type = "response") == 1))

# GT mean
logit_mean <- glm(scaled_sci_cat2 ~ GT_index_mean, 
                    family = "binomial",data=joint_un_sci_gt_oecd_analyse1_clean1)
summary(logit_mean)

# Accuracy
sum(round(predict(logit_mean, type = "response")) == as.numeric(y)) / length(y)
# Sensitivity
sum(round(predict(logit_mean, type = "response")) == as.numeric(y) & as.numeric(y) == 1) /
  sum(as.numeric(y))
# Precision
sum(round(predict(logit_mean, type = "response")) == as.numeric(y) & as.numeric(y) == 1) /
  sum(round(predict(logit, type = "response") == 1))



# With an individual research we dont find much correlation between SCI and GT data, lets check with the other independent variables

# Check the OECD flows density plot to check wether its distribtution is close to normal
par(mfrow=c(1, 4)) # divide graph area in 3 columns

plot(density(train1$OECD_inflows), main="OECD inflows", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(train1$OECD_inflows), 3)))

polygon(density(train1$OECD_inflows), col="darkorange")

plot(density(train1$scaled_sci), main="SCI", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(train1$scaled_sci), 3)))

polygon(density(train1$scaled_sci), col="darkorange")

plot(density(train1$GT_index_mean), main="GT mean", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(train1$GT_index_mean), 3)))

polygon(density(train1$GT_index_mean), col="darkorange")

plot(density(train1$GT_index_median), main="GT median", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(train1$GT_index_median), 3)))

polygon(density(train1$GT_index_median), col="darkorange") # bimodal

###
set.seed(123)
data_split1 = sample.split(joint_un_sci_gt_oecd_analyse1_clean1, SplitRatio = 0.75)
train1 <- subset(joint_un_sci_gt_oecd_analyse1_clean1, data_split1 == TRUE)
test1 <-subset(joint_un_sci_gt_oecd_analyse1_clean1, data_split1 == FALSE)

### OLS regression
#GT mean, SCI
model_1 <- lm(OECD_inflows ~ GT_index_mean + scaled_sci_cat + GT_index_mean*scaled_sci_cat, data = train1)

summary(model_1)

# GT median, SCI
model_2 <- lm(OECD_inflows ~ GT_index_median + scaled_sci_cat + GT_index_median*scaled_sci_cat, data = train1)

summary(model_2)

### GLM regression with possion distribution
# GT median  SCI
# model_3 <- glm( OECD_inflows ~ GT_index_median + scaled_sci_cat2 + GT_index_median*scaled_sci_cat2 , 
#                 family="poisson", data = train1)
model_3 <- glm( OECD_inflows ~ GT_index_median*scaled_sci_cat2 + 
                  log(distance_km) , 
                family="poisson", data = train1)
summary(model_3)


# GT mean, SCI
model_4 <- glm( OECD_inflows ~  GT_index_mean*scaled_sci_cat + 
                  log(distance_km) , family=poisson, data = train1)

summary(model_4)


#### So lets try to group by the origin country and implement the analysis
par(mfrow = c(1,1))
Argentina <- joint_un_sci_gt_oecd_analyse1_clean1 %>%  filter(Origin == "Argentina")
cor(Argentina$GT_index_mean, Argentina$scaled_sci)
scatter.smooth(x=Argentina$scaled_sci,
               y=Argentina$GT_index_mean, 
               main="SCI ~ GT", )


