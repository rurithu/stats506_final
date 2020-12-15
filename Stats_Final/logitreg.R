## Final Project Prep for Stats 506, F20
##
## This .R file is split into two parts: 
## Part 1: Creating Summary Statistics and graphs/tables to display them 
## 
## Part 2: Performing Logistic Regressions
##
##
## Author(s): Rithu Uppalapati, rurithu@umich.edu
## Updated: December 15, 2020

# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(survey)
library(srvyr)
library(ggplot2)
library(knitr)
library(kableExtra)
library(dplyr)
# directories: ----------------------------------------------------------------
setwd("/Users/rithuuppalapati/Desktop/stats506/Final_Project")

# data: -----------------------------------------------------------------------
demo_full <- read.csv("demo_full.csv")

# Survey : -------------------------------------------------------------------
nhanes_design <- svydesign(id = ~idvars, strata = ~str, weights = ~wt, 
                           nest = TRUE, data = demo_full)

# Summary Statistics: ---------------------------------------------------------
demo_full_survey <- demo_full %>% 
  as_survey_design(1, strata = str, weight = wt) %>%
  group_by(gender, race) %>%
  summarise(avg_iron = survey_mean(feritin.levels.ug.L., vartype = "ci")) 

# plot summary statistics 
  ggplot(demo_full_survey, aes(x = race, y = avg_iron, group = gender,
             fill = gender, ymax = avg_iron_upp, ymin = avg_iron_low)) + 
  geom_col(stat = "identity", position = "dodge") +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.1) +
  xlab("Race") + 
  ylab("Avg. Fertin Level (ug/L)") + 
  labs(title = "Average Fertin Level by Race and Gender", caption = "Figure 1a.
       Point Estimates with Confidence Levels for sample means of Fertin level")
  
# Proportions of People who are Iron Deficient 
prop_sample <- svyby(formula = ~low_iron, by = ~gender+race,
                       design = nhanes_design, FUN = svymean)

# Proportions Table 
normCI <- qnorm(.975)
prop_tab <- prop_sample %>% 
  arrange(gender) %>%
  mutate(lwr = low_iron - normCI*se, upr = low_iron + normCI*se) 

names(prop_tab) <- c("Gender", "Race", "Proportion of Iron Deficient", 
                     "Std. Err", "95% Lower Confidence", "95% Upper Confidence")
kbl(prop_tab[, 2:6], caption = "Table 1a. Proportion Estimates
    of people who have Iron deficiency",
    digits = 4) %>%
  pack_rows("Female", 1, 7) %>%
  pack_rows("Male", 7, 12)

# Logistic Regression: --------------------------------------------------------
# Logistic Regression by Gender 
logit_gender <- svyglm(low_iron~factor(gender)+age+total_fat.gm., 
                       family=quasibinomial, design=nhanes_design,
                       na.action = na.omit)
fit.logit_gender <- summary(logit_gender)

# Logistic Regression by Race controlling for Gender 
logit_race <- svyglm(low_iron~factor(race)+ factor(gender)+age+total_fat.gm., 
                     family=quasibinomial, design=nhanes_design, 
                     na.action = na.omit)
fit.logit_race <- summary(logit_race)

# Logistic Regression by Race with gender and race interaction term
logit_race_int <- svyglm(low_iron~factor(gender)+factor(race)+
                           age+total_fat.gm.+factor(gender)*factor(race), 
                         family=quasibinomial, design=nhanes_design, 
                         na.action = na.omit)
fit.logit_race_int <- summary(logit_race_int)

# Create Confidence Intervals for Regression Tables 
std.err_logit <- fit.logit_race_int$coefficients[, 2]
coeff_logit <- fit.logit_race_int$coefficients[, 1]
lwr <- coeff_logit - normCI*std.err_logit
upr <- coeff_logit + normCI*std.err_logit
mod_fit <- data.frame(fit.logit_race_int$coefficients)
mod_fit$lwr <- lwr
mod_fit$upr <- upr
mod_fit$t.value <- NULL

# Create Regression Table 
# Formatting 
names(mod_fit) <- c("Coefficient Estimate", "Std. Error", "P-Val", 
                    "95% Lower Confidence", "95% Upper Confidence")
rownames(mod_fit) <- c("Intercept", "Male", "Non-Hispanic Asian",
                       "Non-Hispanic Black", "Non-Hispanic White", 
                       "Other", "Other Hispanic", "Age", "Total Fat", 
                       "Male*Non-Hispanic Asian", "Male*Non-Hispanic Black", 
                       "Male*Non-Hispanic White", "Male*Other", 
                       "Male*Other Hispanic")
# Table 
kbl(mod_fit, caption = "Table 1b. Regression Coefficients of interaction model,
    and significance in predicting low fertin levels", digits = 3)
