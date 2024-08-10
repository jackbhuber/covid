# ----- LOAD PACKAGES 

library(labelled)
library(tidyverse)
library(tibble)
library(dplyr)

# ----- READ DATA
df <- read.csv("https://raw.githubusercontent.com/jackbhuber/datasets/main/Baylor_Religion_Survey_Wave_VI_2021.csv", header = TRUE)

# ----- VARIABLE LABELS
var_label(df$Q1) <- "Happy"
var_label(df$Q2) <- "Health"

var_label(df$Q6) <- "Health at 16"
var_label(df$Q7) <- "Exercise"

# ----- MAKE THESE VARIABLES NUMERIC
#data$inpatient_days  <- as.float(data$inpatient_days)
#data$icu_days        <- as.float(data$icu_days)
#data$ventilator_days <- as.float(data$ventilator_days)
#data$n_vax <- as.float(data$n_vax)
#data$months_since_mAb_approval <- as.float(data$months_since_mAb_approval)

# ----- MISSING DATA

# Replace NA values in points column with mean of points column
# df <- df %>% mutate(across(Q1, ~replace_na(., mean(., na.rm=TRUE))))

# Replace NA values with Mean in All Numeric Columns
# df %>% mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE))))