##############################
## Title: Hackathon Project ##
## Date: 11/7/2025          ##
## Author: E Nowlis         ##
##############################

library(tidyverse)
library(janitor)
library(skimr)
library(readxl)
library(dplyr)
library(data.table)
library(DescTools)

## Data is downloaded from: https://web.archive.org/web/20240725151544/https://datacatalog.cookcountyil.gov/browse?tags=state%27s+attorney+case-level&sortBy=most_accessed

setwd([root])
diversion <- read.csv("Diversion_20251107.csv")
sentencing <- read.csv("Sentencing_20251107.csv")

### Explore the data ###
skim(diversion)
head(diversion)
summary(diversion)

skim(sentencing)
head(sentencing)
summary(sentencing)

### Question 1: Does when you are referred impact your probability of success ? ###

diversion_edit <- diversion %>% 
  mutate(RECEIVED_DATE=as.Date(RECEIVED_DATE,format = "%Y %b %d %I:%M:%S %p")) %>%
  mutate(REFERRAL_DATE=as.Date(REFERRAL_DATE,format = "%Y %b %d %I:%M:%S %p")) %>%
  mutate(as.numeric(case_length = REFERRAL_DATE - RECEIVED_DATE)) %>%
  filter(case_length >= 0 & DIVERSION_RESULT != "", na.rm = TRUE) %>%
  mutate(DIVERSION_RESULT_NUM = if_else(DIVERSION_RESULT == "Failed", 0, 1))

# Run a regression #

fit <- lm(diversion_edit$DIVERSION_RESULT_NUM ~ diversion_edit$case_length)

summary(fit)

diversion_edit %>%
  group_by(year(REFERRAL_DATE)) %>%
  ggplot(mapping=aes(y= year(REFERRAL_DATE), fill=RACE)) +
           geom_bar()

diversion_edit %>%
  group_by(year(REFERRAL_DATE)) %>%
  ggplot(mapping=aes(y= year(REFERRAL_DATE), fill=GENDER)) +
  geom_bar()

