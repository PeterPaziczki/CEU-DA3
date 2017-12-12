
# Data Analysis 3 - Pattern Discovery and Regression Analysis 2017/2018 Fall
# TERM PROJECT

# authors:
# Peter Paziczki
# Imre Boda
# Balazs Zankay
# date: 2017 december 16

# Clearing the memory
rm(list=ls())

# Loading necessary libraries
library(ggplot2)
library(data.table)
# library(descr) # for the freq and crosstable functions
# library(fBasics) # for basicStats function
library(lmtest) # for coeftest
library(sandwich) # sandwich is needed for coeftest function
# library(stargazer) # needed to provide a nicer output for interpreting coefficients
library(pander) # pander function makes tables have a nicer output
# library(mfx) # it is needed for computing marginal differences in probit and logit functions

# Checking and setting working directory
getwd()
setwd('/Users/User/Documents/R_projects/CEU-DA3')

# Limiting the number of digits
options(digits=2)

# TERM PROJECT

## Does having a young CEO makes firms do better?

# Task is the following:
  
# using the bisnode_all.csv dataset
# covering companies for 2011-2016, in manufacturing and services
# the task is to get closer to causal interpretation: does having a young CEO makes firms do better?
# using cross sectional analysis – ie focusing on firm features in a given a year only
# defining „do better” and using the definition and the benchmark of one-year sales growth
# selecting control variables based on stories (arguing why using a certain variable)
# interpreting and discussing the findings.
# writing a technical report on what and why we did. Our target audience is the chief data scientist. The technical report
# can use bullet points. 
# can be brief but precise

## Notes

# ====================================

# READING DATA

# bisnode_raw <- read.csv("bisnode_all.csv", na.strings = ".")
bisnode_raw <- fread("bisnode_all.csv")

# ====================================

# INITIAL SETTINGS FOR THE RESEARCH

# Year of interest
research_year <- 2015

# a CEO has to be at least this age
min_CEO_age <- 20

# max age of a young CEO
young_CEO_max_age <- 39

# min number of employees on average
min_employee <- 0 # companies with a low number of employees are not reliable for this research

# minimum number of years CEOs have to spend in their position
min_CEO_exp_days <- 730

# minimum company age
min_comp_age <- 2

# ====================================

## CLEANING

# Kepping columns that we need
bisnode <- bisnode_raw[,c('comp_id', 'begin', 'end', 'curr_assets', 'fixed_assets', 'personnel_exp', 'profit_loss_year', 'sales', 'share_eq', 'tang_assets', 'year', 'founded_year', 'ceo_count', 'female', 'birth_year', 'inoffice_days', 'gender', 'ind', 'labor_avg', 'balsheet_notfullyear', 'exit_year', 'inc_bef_tax')]

# Filtering for the year chosen
bisnode <- bisnode[year == research_year]

# dropping birth_years NAs
bisnode <- bisnode[birth_year != ""]

# computing age
bisnode[, ceo_age := year - birth_year]

# limiting CEO age to minimum years
bisnode <- bisnode[ceo_age >= min_CEO_age]

# creating young_CEO binary variable
bisnode$young_CEO <- as.numeric(bisnode$ceo_age <= young_CEO_max_age)

# dropping industry NAs
bisnode <- bisnode[ind != ""]

# number of days CEO spent in office, dropping those who spent less then a quarter as CEOs
bisnode <- bisnode[inoffice_days >= min_CEO_exp_days]

# dropping CEOs that are under the age limit
bisnode <- bisnode[ceo_age >= min_CEO_age]

# dropping firms where average number of employees is less than the limit we set
bisnode <- bisnode[labor_avg > min_employee]

# dropping firms where average number of employees are NA
bisnode <- bisnode[labor_avg != ""] # more than 40.000 NAs

# computing age
bisnode[, comp_age := year - founded_year]

# dropping firms that are too young
bisnode <- bisnode[comp_age > min_comp_age]

# dropping firms that don't have a balance sheet for full year
bisnode <- bisnode[balsheet_notfullyear == 0]

# dropping those countries that have NAs in financial variables
bisnode <- bisnode[profit_loss_year != ""]
bisnode <- bisnode[profit_loss_year != 0]
bisnode <- bisnode[share_eq != ""]
bisnode <- bisnode[share_eq != 0]
bisnode <- bisnode[curr_assets != ""]
bisnode <- bisnode[fixed_assets != ""]
bisnode <- bisnode[tang_assets != ""]

# ====================================

# INDUSTRY AVERAGES

# Average number of employees by industries
pander(bisnode[, mean(labor_avg),by=ind])
# Adding average number of employees by industries to the table
bisnode[, ind_labor_avg := mean(labor_avg), by = ind]
# Adding the size of company
bisnode$comp_size_big <- as.numeric(bisnode$labor_avg >= bisnode$ind_labor_avg)

# Expenses per employees
bisnode[, pers_exp_emp := personnel_exp / labor_avg]

# ====================================

# MEASURE OF PERFORMANCE
# return on asset / return on equity ... need to choose

# Company performance
bisnode[, comp_performance := profit_loss_year / share_eq] # can change ...

# Industry performance
bisnode[, ind_performance := mean(comp_performance), by = ind]

# CEO performance
bisnode[, ceo_performance := comp_performance - ind_performance]

# ====================================

# PLOTS

# Average performance of companies by industries, firm size and by CEO (young or old)
pander(bisnode[, lapply(.SD, mean, na.rm = TRUE), by = list(ind, comp_size_big, young_CEO), .SDcols = c("comp_performance")])

# Average company peformance per CEOs (young or old)
pander(bisnode[, list(avg_perforamnce = mean(comp_performance)), by = young_CEO])

# ====================================
