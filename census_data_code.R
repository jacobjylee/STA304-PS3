#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
library(dplyr)
# Read in the raw data.
setwd("~/Desktop/PS3")
raw_data1 <- read_dta("~/Desktop/PS3/Census_Data/usa_00002.dta.gz")


# Add the labels
raw_data1 <- labelled::to_factor(raw_data1)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- raw_data1 %>% 
  select(region, statefip, sex, age, race, inctot, educd, labforce)
#head(reduced_data)

reduced_data1 <- reduced_data %>%
  #count(age, sex, race, statefip, inctot, educd) %>%
  group_by(age, sex, race, inctot, educd) %>%
  summarize(count=n())
# view(reduced_data1)

# Remove all observations where the individual cannot vote (17 or younger)
reduced_data2 <- reduced_data1[!(reduced_data1$age=="less than 1 year old"),] 
reduced_data3 <- reduced_data2[!(reduced_data2$age=="1"|
                                   reduced_data2$age=="2"|
                                   reduced_data2$age=="3"|
                                   reduced_data2$age=="4"|
                                   reduced_data2$age=="5"|
                                   reduced_data2$age=="6"|
                                   reduced_data2$age=="7"|
                                   reduced_data2$age=="8"|
                                   reduced_data2$age=="9"|
                                   reduced_data2$age=="10"|
                                   reduced_data2$age=="11"|
                                   reduced_data2$age=="12"|
                                   reduced_data2$age=="13"|
                                   reduced_data2$age=="14"|
                                   reduced_data2$age=="15"|
                                   reduced_data2$age=="16"|
                                   reduced_data2$age=="17"),]

names(reduced_data3)[names(reduced_data3)=="sex"] <- "gender"
names(reduced_data3)[names(reduced_data3)=="race"] <- "race_ethnicity"
n#ames(reduced_data3)[names(reduced_data3)=="statefip"] <- "state"
names(reduced_data3)[names(reduced_data3)=="inctot"] <- "household_income"
names(reduced_data3)[names(reduced_data3)=="educd"] <- "education"
head(reduced_data3)

#names(reduced_data3)

reduced_data3 %>% na.omit(reduced_data3)

#as.character(unique(unlist(reduced_data3$race_ethnicity)))
reduced_data4 <- reduced_data3 %>% # Create data with column "vote_trump" ; 1 if voting for Trump, 0 if not.
  mutate(white = 
           ifelse(race_ethnicity=="white", 1, 0))
#view(reduced_data4)

#as.character(unique(unlist(reduced_data4$education)))
reduced_data5 <- reduced_data4 %>%
  mutate(bdorhigher =
           ifelse(education=="bachelor's degree"|
                    education=="master's degree" |
                    education=="professional degree beyond a bachelor's degree"|
                    education=="doctoral degree", 1, 0))
#head(reduced_data5)

reduced_data6 <- reduced_data5 %>%
  mutate(male =
           ifelse(gender=="male",1,0))
#head(reduced_data6)

reduced_data7 <- reduced_data6 %>%
  mutate(above_avg =
           ifelse(as.integer(household_income)>=70000,1,0))
head(reduced_data7)

reduced_data8 <- as.tibble(reduced_data7)

reduced_data9 <- reduced_data8 %>% 
  select(age, white, bdorhigher, male, above_avg)
head(reduced_data9)
  
  



#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

# reduced_data1 <- reduced_data %>%
#   count(age) %>%
#   group_by(age) 
# view(reduced_data1)
# 
# reduced_data <- 
#   reduced_data %>% 
#   filter(age != "less than 1 year old") %>%
#   filter(age != "90 (90+ in 1980 and 1990)")
# 
# reduced_data$age <- as.integer(reduced_data$age)

# Saving the census data as a csv file in my
# working directory
reduced_data10 <- as.tibble(reduced_data9)
write_csv(reduced_data10, "Outputs/census_data.csv")



         