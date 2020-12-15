## Final Project Prep for Stats 506, F20
##
## Cleaning up data to use in logistic regression 
## Using NHANES 2017-2018 Demo Data, NHANES 2017-2018 Fertin Data and NHANES
## 2017-2018 Dietary Intake Data
##
## Part 1: merging datasets and keeping variables of interest
## 
## Part 2: formatting factor variables

##
## Author(s): Rithu Uppalapati, rurithu@umich.edu
## Updated: December 15, 2020

# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse)
library(foreign)
# directories: ----------------------------------------------------------------
setwd("/Users/rithuuppalapati/Desktop/stats506/Final_Project")

# data: -----------------------------------------------------------------------
demo <- read.xport("DEMO_J.XPT")
fertin <- read.xport("FERTIN_J.XPT")
diet <- read.xport("DR1TOT_J.XPT")
demo <- merge(demo, fertin, by = "SEQN")
demo <- merge(demo, diet, by = "SEQN")

myvars <- c("SEQN", "RIAGENDR", "RIDAGEYR", "RIDRETH3", "LBDFERSI", "SDMVPSU",
            "SDMVSTRA", "WTINT2YR", "DR1TTFAT")
demo <- demo[myvars]
demo <- na.omit(demo)

# code dummy variable for iron deficiency
# 1 == Iron Deficient
# 0 == Normal Iron Levels
demo$Iron_Def <- ifelse(((demo$RIAGENDR == 1 & demo$LBDFERSI < 24) | 
                           (demo$RIAGENDR == 2 & demo$LBDFERSI < 11)), 1, 0)

names(demo) <- c("id", "gender", "age", "race", "feritin levels(ug/L)", 
                 "idvars", "str", "wt", "total_fat(gm)", "low_iron")

# Formatting: ----------------------------------------------------------------

demo$gender <- recode_factor(demo$gender, `1` = "Male", `2` = "Female")
demo$race <- recode_factor(demo$race, `1` = "Mexican American", 
                           `2` = "Other Hispanic", `3` = "Non-Hispanic White", 
                           `4` = "Non-Hispanic Black", `6` = "Non-Hispanic Asian",
                           `7` = "Other")
write_csv(demo, "demo_full.csv")
