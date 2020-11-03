#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/jacoblee/Desktop/PS3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625/ns20200625.dta") # Read the data

# Add the labels
raw_data1 <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- raw_data1 %>% 
  select(gender,
         race_ethnicity,
         household_income,
         education,
         state,
         vote_2020,
         age)

reduced_data1 <- na.omit(reduced_data) # Remove all entries with N/a

reduced_data2 <- reduced_data1 %>% # Create data with column "vote_trump" ; 1 if voting for Trump, 0 if not.
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

length(as.character(unique(unlist(reduced_data2$state))))



###################################### Making new data frame with binary explanatory variables

# hispanic1 <- reduced_data2 %>% # Create data with column "not_hispanic" ; 1 if not hispanic, 0 if hispanic.
#   mutate(not_hispanic =
#            ifelse(hispanic=="Not Hispanic", 1, 0))
# hispanic2 <- hispanic1 %>%
#   select(not_hispanic)
# write_csv(hispanic2, "outputs/hispanic2.csv")
# #view(hispanic2)

reduced_data3 <- reduced_data2 %>%
  mutate(bdorhigher =
           ifelse(education=="Associate Degree"|
                    education=="College Degree (such as B.A., B.S.)"|
                    education=="Completed some graduate, but no degree"|
                    education=="Masters degree"|
                    education=="Doctorate degree",1, 0))


reduced_data4 <- reduced_data3 %>%
  mutate(male =
           ifelse(gender=="Male",1,0)) #1 if Male, 0 if Female

reduced_data5 <- reduced_data4 %>%
  mutate(white =
           ifelse(race_ethnicity=="White", 1, 0)) # 1 if White, 0 if not

# ~$70,000 is the average income in the USA in 2019.

reduced_data6 <- reduced_data5 %>% # 1 if household_income is at least $70,000, 0 otherwise
  mutate(above_avg =
           ifelse(household_income=="$70,000 to $74,999"|
                    household_income=="$75,000 to $79,999"|
                    household_income=="$80,000 to $84,999"|
                    household_income=="$85,000 to $89,999"|
                    household_income=="$90,000 to $94,999"|
                    household_income=="$95,000 to $99,999"|
                    household_income=="$100,000 to $124,999"|
                    household_income=="$125,000 to $149,999"|
                    household_income=="$150,000 to $174,999"|
                    household_income=="$175,000 to $199,999"|
                    household_income=="$200,000 to $249,999"|
                    household_income=="$250,000 and above",1,0))

head(reduced_data6)
reduced_data7 <- as.tibble(reduced_data6)
reduced_data8 <- reduced_data7 %>%
  select(vote_trump, age, white, bdorhigher, male, above_avg)


# Saving the survey/sample data as a csv file in my
# working directory

reduced_data9 <- as.tibble(reduced_data8)
write_csv(reduced_data9, "outputs/survey_data.csv")

