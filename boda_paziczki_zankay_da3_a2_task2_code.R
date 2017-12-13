
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
bisnode <- bisnode_raw[,c('comp_id', 'begin', 'end', 'curr_assets', 'fixed_assets', 
                          #'intang_assets', 'tang_assets', 'liq_assets', 
                          'curr_liab', 'inc_bef_tax', 'personnel_exp', 'profit_loss_year', 'sales', 
                          'share_eq', 'year', 'founded_year', 'ceo_count', 
                          'female', 'birth_year', 'inoffice_days', 'gender', 'ind', 'ind2', 
                          #'labor_avg', 
                          'balsheet_notfullyear', 'exit_year', 'origin', 'region_m')]

# Filtering for the year chosen
bisnode <- bisnode[year == research_year]

# dropping birth_years NAs
bisnode <- bisnode[birth_year != ""]
#bd: lost here ~ 4500.

# computing age
bisnode[, ceo_age := year - birth_year]

# limiting CEO age to minimum years
bisnode <- bisnode[ceo_age >= min_CEO_age]

# creating young_CEO binary variable
bisnode[,"young_CEO"] <- as.numeric(bisnode[,ceo_age] <= young_CEO_max_age)

ind2_list <- bisnode [,.N, by = ind2][N > 1000,][,ind2]
bisnode <- bisnode [ind2 %in% ind2_list,]

# 26 Manufacture of computer, electronic and optical products
# 28 Manufacture of machinery and equipment n.e.c.
# 33 Repair and installation of machinery and equipment
# 55 Accommodation
# 56 Food and beverage service activities

#bd lost here: ~1700

# number of days CEO spent in office, dropping those who spent less then a quarter as CEOs
bisnode <- bisnode[inoffice_days >= min_CEO_exp_days]
#bd lost here ~1500

# dropping CEOs that are under the age limit
bisnode <- bisnode[ceo_age >= min_CEO_age]

# dropping firms where average number of employees is less than the limit we set
# bisnode <- bisnode[labor_avg > min_employee]

# dropping firms where average number of employees are NA
# bisnode <- bisnode[labor_avg != ""] # more than 40.000 NAs   #bd: agreed not to use labor_avg, as it would limit the sample

# computing company age
bisnode[, comp_age := year - founded_year]

# dropping firms that are too young
bisnode <- bisnode[comp_age > min_comp_age]
#bd ost here: ~2500

# dropping firms that don't have a balance sheet for full year
bisnode <- bisnode[balsheet_notfullyear == 0]
#bd lost here: ~300

# dropping those countries that have NAs in financial variables
bisnode <- bisnode[inc_bef_tax != ""]
bisnode <- bisnode[inc_bef_tax != 0]
bisnode <- bisnode[curr_assets != ""]
bisnode <- bisnode[curr_assets != 0]
bisnode <- bisnode[fixed_assets != ""]
bisnode <- bisnode[fixed_assets != 0]
#bisnode <- bisnode[tang_assets != ""]   #bd: I think we do not use it
#bisnode <- bisnode[tang_assets != 0]   #bd: I think we don use it
#bd lost in above filters: ~3800

# ====================================

# INDUSTRY AVERAGES

# Average number of employees by industries
# pander(bisnode[, mean(labor_avg),by=ind2]) bd: to be removed as labor is unreliable
# Adding average number of employees by industries to the table
# bisnode[, ind_labor_avg := mean(labor_avg), by = ind2] bd: to be removed as labor is unreliable
# Adding the size of company
# bisnode$comp_size_big <- as.numeric(bisnode$labor_avg >= bisnode$ind_labor_avg) bd: to be removed as labor is unreliable, other measure for size to be defined

bisnode <- bisnode [curr_assets + fixed_assets >=0,]
bisnode [, size_third := quantile ((curr_assets + fixed_assets), 0.33), by = ind2]
bisnode [, size_twothird := quantile ((curr_assets + fixed_assets), 0.66), by = ind2]
bisnode [, size_cat := cut (curr_assets + fixed_assets, c(
                            0, size_third [1],
                            size_twothird [1], Inf),
                            labels = c("small", "medium", "big")),
                            by = ind2]

# bisnode [, unique (size_twothird)] #bd: check
# bisnode [, .N, by = size_cat] #bd: check


# Expenses per employees
# bisnode[, pers_exp_emp := personnel_exp / labor_avg] bd: to be removed as labor is unreliable
#Fixed assets ~= Tangible Asset + Intangible Asset
#Liquid Asset: part of current asset
#Asset ~= Current Asset + Fix Asset

# ====================================

# MEASURE OF PERFORMANCE
# ROCE = EBIT / CAPITA Employed 

# Company performance - ROCE - returnal on capital employed
bisnode[, comp_performance := inc_bef_tax / (curr_assets + fixed_assets - curr_liab)] 

# Industry performance
bisnode[, ind_performance := mean(comp_performance), by = ind2]
#bisnode [, unique(ind_performance)] #checkpoint

# CEO performance
bisnode[, ceo_performance := comp_performance / ind_performance]

#industry and CEO performance per size categories
bisnode [, ind_performance_persize := mean (comp_performance),by = .(ind2, size_cat)]
#bisnode [, unique(ind_performance_persize), by= .(ind2, size_cat)][order(ind2,size_cat),] #checkpoint
bisnode[, ceo_performance_persize := comp_performance / ind_performance_persize]


# ====================================

# PLOTS

# Average performance of companies by industries, firm size and by CEO (young or old)
pander(bisnode[, lapply(.SD, mean, na.rm = TRUE), by = list(ind2, size_cat, young_CEO), .SDcols = c("comp_performance")])

# Average company peformance per CEOs (young or old)
pander(bisnode[, list(avg_perforamnce = mean(comp_performance)), by = young_CEO])

# ====================================
# first round of variable selection

summary (bisnode [, ceo_performance_persize])
unique (bisnode [, region_m])
summary (lm (ceo_performance_persize ~ inoffice_days, data = bisnode))
summary (lm (ceo_performance_persize ~ ceo_count, data = bisnode))
summary (lm (ceo_performance_persize ~ origin, data = bisnode))
summary (lm (ceo_performance_persize ~ personnel_exp, data = bisnode))
# coefficients is very small and/or the significance level is too low, so most likely there is no relationship
summary (lm (ceo_performance_persize ~ region_m, data = bisnode))

bisnode [, unique(gender)]
#result: "male"   "mix"    "female"
bisnode [, unique(female)]
#result 0.00 0.50 1.00 0.33 0.67 0.25 0.29 0.17 0.20 0.40 0.60
# proposal: have dom_gender variable as follows: female <= 0.33: => male, 0.33 < female =< 0.66: mix, 0.66 <= female: female

### Setting up variables that do not exist:
model <- bisnode [, .(assets = curr_assets + fixed_assets,   #probably log analysed
                      ind2,
                      ceo_age,
                      ceo_performance,
                      ceo_performance_persize,   #probably log analysed
                      comp_age,
                      dom_gender = cut (female, c(-0.1, 0.33, 0.66, 1.1), labels = c('male', 'mix', 'female')),
                      size_cat,
                      region_m
                      )]
model [, .N, by = dom_gender]

###################### Modell parameters:

### creating binaries
model [, D_dom_gender_Fem := (dom_gender == 'female')]
model [, D_dom_gender_Mix := (dom_gender == 'mix')]
model [, D_dom_gender_Male := (dom_gender == 'male')]

model [, D_ind2_28 := (ind2 ==28)]
model [, D_ind2_26 := (ind2 ==26)]
model [, D_ind2_33 := (ind2 ==33)]
model [, D_ind2_55 := (ind2 ==55)]
model [, D_ind2_56 := (ind2 ==56)]

model [, D_size_S := (size_cat == 'small')]
model [, D_size_M := (size_cat == 'medium')]
model [, D_size_L := (size_cat == 'big')]

model [, .N, by = region_m]
model [, D_region_C := (region_m == 'Central')]
model [, D_region_E := (region_m == 'East')]
model [, D_region_W := (region_m == 'West')]
model <- model [region_m != ""]
#bd: here we lost 35 samples

model [, D_CEO_y := (ceo_age <= 40)]
model [, D_CEO_o := (ceo_age > 40)]
model [ceo_age >40,.N]  #bd showing 70- 30 distribution  => not sure that in this fricking over segmented model we will have enough number of samples for young CEOs



### Model1 disregarding industries and sizes
#control variables: ceo_performance, D_dom_gender_Fem, D_dom_gender_Mix, D_region_W, D_region_E, D_CEO_y, comp_age

lm_All_op <- lm (log(ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
              D_region_E + D_region_W + comp_age * D_region_E + comp_age * D_region_W, data = model [ceo_performance >0,])
coeftest(lm_All_op, vcov=sandwich)
lm_All_up <- lm (log(-ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                D_region_E + D_region_W + comp_age * D_region_E + comp_age * D_region_W, data = model [ceo_performance <0,])

coeftest(lm_All_up, vcov=sandwich)
BIC (lm_All_op)
BIC (lm_All_up)
#it looks that for underperfroming companies East makes no difference to Central, while for overperformers with 95% confidence there is still
# let us change the model by not differentiating between East and Central, only between West adn the rest
lm2_All_op <- lm (log(ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                   D_region_W + comp_age * D_region_W, data = model [ceo_performance >0,])
coeftest(lm2_All_op, vcov=sandwich)
lm2_All_up <- lm (log(-ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                   D_region_W + comp_age * D_region_W, data = model [ceo_performance <0,])

coeftest(lm2_All_up, vcov=sandwich)
BIC (lm2_All_op)
BIC (lm2_All_up)
# did not impact relevant coefficients, but suggests that there is no regional difference ???
lm3_All_op <- lm (log(ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age,
                    data = model [ceo_performance >0,])
coeftest(lm3_All_op, vcov=sandwich)
lm3_All_up <- lm (log(-ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age,
                  data = model [ceo_performance <0,])

coeftest(lm3_All_up, vcov=sandwich)
BIC (lm3_All_op)
BIC (lm3_All_up)

# does it mean, that we should forget regional differences?
lmtmp <- lm (log(ceo_performance) ~ D_region_W, data = model[ceo_performance >0,])
coeftest (lmtmp, vcov = sandwich)
#yes, we can safely drop this
#so the model is lm_3_All_Up

### Model2: disregarding industries, including sizes
#control variables: ceo_performance, D_dom_gender_Fem, D_dom_gender_Mix, D_size_M, D_size_L, D_CEO_y, comp_age
#bd??? mit csináljunk a ceo_performance-el, amit per size per industry definiáltunk
lm_IndAll_op <- lm (log(ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                   D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [ceo_performance >0,])
coeftest(lm_IndAll_op, vcov=sandwich)
lm_IndAll_up <- lm (log(-ceo_performance) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                   D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [ceo_performance <0,])
coeftest(lm_IndAll_up, vcov=sandwich)
BIC (lm_IndAll_op)
BIC (lm_IndAll_up)
# bd???: azt jelenti, hogy a felülteljesítőknél nem számít a mére, viszont az alulteljesítésnél meg nagyon?
# hátha MOdel 3 megvilágosít

### Model 3: in order to be able to handle the complexity, we divide the model by industy and then by size
## ind = 26
lm_ind26_op <- lm (log(ceo_performance_persize) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                      D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [(ceo_performance_persize >0) & D_ind2_26,])
coeftest(lm_ind26_op, vcov=sandwich)
lm_ind26_up <- lm (log(-ceo_performance_persize) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                      D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [ceo_performance_persize <0 & D_ind2_26,])
coeftest(lm_ind26_up, vcov=sandwich)
model [ceo_performance_persize & D_ind2_26,.N]
model [ceo_performance_persize & D_ind2_26 & ceo_age <= 40, .N]
BIC (lm_ind26_op)
BIC (lm_ind26_up)

lm_ind26_op <- lm (log(ceo_performance_persize) ~ D_CEO_y + comp_age +
                     D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [(ceo_performance_persize >0) & D_ind2_26,])
coeftest(lm_ind26_op, vcov=sandwich)
lm_ind26_up <- lm (log(-ceo_performance_persize) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                     D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [ceo_performance_persize <0 & D_ind2_26,])
coeftest(lm_ind26_up, vcov=sandwich)
model [ceo_performance_persize & D_ind2_26,.N]
model [ceo_performance_persize & D_ind2_26 & ceo_age <= 40, .N]
BIC (lm_ind26_op)
BIC (lm_ind26_up)
# just for fun, let us see in this segment if there is any relationship between age and success
lm_ind26_oplim <- lm (log(ceo_performance_persize) ~ D_CEO_y + D_size_M + D_size_L, data = model [(ceo_performance_persize >0) & D_ind2_26,])
coeftest(lm_ind26_oplim, vcov=sandwich)
#we can flip coins Seemingly in this indusrty segment (computer manufacturing) there is no any relationship

##ind = 28
lm_ind28_op <- lm (log(ceo_performance_persize) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                     D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [(ceo_performance_persize >0) & D_ind2_28,])
coeftest(lm_ind26_op, vcov=sandwich)
lm_ind28_up <- lm (log(-ceo_performance_persize) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
                     D_size_M + D_size_L + comp_age * D_size_M + comp_age * D_size_L, data = model [ceo_performance_persize <0 & D_ind2_28,])
coeftest(lm_ind28_up, vcov=sandwich)
model [ceo_performance_persize & D_ind2_28,.N]
model [ceo_performance_persize & D_ind2_28 & ceo_age <= 40, .N]
#any relation at all?
lm_ind28_oplim <- lm (log(ceo_performance_persize) ~ D_CEO_y + D_size_M + D_size_L, data = model [(ceo_performance_persize >0) & D_ind2_28 & D_size_L,])
coeftest(lm_ind28_oplim, vcov=sandwich)

##basic relation test in other segments
lm_ind33_oplim <- lm (log(ceo_performance_persize) ~ D_CEO_y + D_size_M + D_size_L, data = model [(ceo_performance_persize >0) & D_ind2_33 & D_size_L,])
coeftest(lm_ind33_oplim, vcov=sandwich)

lm_ind55_oplim <- lm (log(ceo_performance_persize) ~ D_CEO_y, data = model [(ceo_performance_persize >0) & D_ind2_55 & D_size_L,])
coeftest(lm_ind55_oplim, vcov=sandwich)

lm_ind56_oplim <- lm (log(ceo_performance_persize) ~ D_CEO_y + D_size_M + D_size_L, data = model [(ceo_performance_persize >0) & D_ind2_56 & D_size_L,])
coeftest(lm_ind56_oplim, vcov=sandwich)

# per does not it matter per industry??????????????????????????????????