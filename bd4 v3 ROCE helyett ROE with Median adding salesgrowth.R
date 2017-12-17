
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
library(stargazer) # needed to provide a nicer output for interpreting coefficients
library(pander) # pander function makes tables have a nicer output
# library(mfx) # it is needed for computing marginal differences in probit and logit functions

# Checking and setting working directory
getwd()
#setwd('/Users/User/Documents/R_projects/CEU-DA3')

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
research_1 <- research_year -1

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
bisnode_tmp <- bisnode_raw [year == research_1]   #in order to store sales data
bisnode_tmp <- bisnode_tmp [, list(comp_id, sales_pastyear = sales)]   #only having in past year sales result
# Filtering for the year chosen
bisnode <- bisnode[year == research_year]

## merging last year sales to current year data
bisnode <- merge (bisnode, bisnode_tmp, by = "comp_id")
#bd: lost ~1500 samples
bisnode <- bisnode [!is.na(sales_pastyear),]
bisnode <- bisnode [!is.na(sales),]
bisnode <- bisnode [sales_pastyear !=0,]
bisnode <- bisnode [, salesgrowth_perc := (sales - sales_pastyear) / sales_pastyear]
#bd: lost ~4700 rows

# dropping birth_years NAs
bisnode <- bisnode[birth_year != ""]
#bd: lost here ~ 4500.

# computing age
bisnode[, ceo_age := year - birth_year]

# limiting CEO age to minimum years
bisnode <- bisnode[ceo_age >= min_CEO_age]

# creating young_CEO binary variable
bisnode[,"young_CEO"] <- as.numeric(bisnode[,ceo_age] <= young_CEO_max_age)
#dropping those samples that belong to an industry that has less than 1000 samples
ind2_list <- bisnode [,.N, by = ind2][N > 900,][,ind2]   #here I had to change limit value to 900,
# otherwise we would loose ind2 = 26 (after dropping sales =0)
bisnode <- bisnode [ind2 %in% ind2_list,]
bisnode [,.N, by = ind2]

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

# computing company age
bisnode[, comp_age := year - founded_year]

# dropping firms that are too young
bisnode <- bisnode[comp_age > min_comp_age]
#bd ost here: ~2500

# dropping firms that don't have a balance sheet for full year
bisnode <- bisnode[balsheet_notfullyear == 0]
#bd lost here: ~300

# dropping those countries that have NAs in financial variables
bisnode <- bisnode[profit_loss_year != ""]
bisnode <- bisnode[profit_loss_year != 0]
# bisnode <- bisnode[inc_bef_tax != ""]
# bisnode <- bisnode[inc_bef_tax != 0]
bisnode <- bisnode[share_eq != ""]
bisnode <- bisnode[share_eq != 0]

#bisnode [, .N] returns 16362 when not removing slaes_pastyear =0
# in this version we removed sales_pastyear == 0, here bisnode [, .N] returns 12907 

###  Creating 3 company size categories based on Assets value 
bisnode [, size_third := quantile ((curr_assets + fixed_assets), 0.33), by = ind2]
bisnode [, size_twothird := quantile ((curr_assets + fixed_assets), 0.66), by = ind2]
bisnode [, size_cat := cut (curr_assets + fixed_assets, c(
  0, size_third [1],
  size_twothird [1], Inf),
  labels = c("small", "medium", "big")),
  by = ind2]

############## MEASURE OF PERFORMANCE ####################
############## selected measure: ROE = Net_Income / Shareholders Equity

bisnode[, comp_performance := profit_loss_year / share_eq] 

### Let us explore ROCE and get rid of extreme values: BEGIN
ggplot (data = bisnode, aes (x = comp_performance)) + geom_histogram()
ggplot (data = bisnode, aes (x = log(comp_performance))) + geom_histogram()
ggplot (data = bisnode, aes (x = size_cat, y = comp_performance)) + geom_boxplot()
# check extreme values
bisnode [comp_performance > 1000,]
bisnode [comp_perfromance <-1000,]
bisnode <- bisnode [comp_performance < 1000 & comp_performance > -1000]

# ROE is usually between 0 - 1. Here we exclude those that are extreme, assuming that it is due to error

bisnode <- bisnode [comp_performance < 10]
bisnode <- bisnode [comp_performance > -10]
bisnode [,.N]
### Let us explore ROCE and get rid of extreme values: END

median_Perf <- quantile(bisnode [, comp_performance],0.5)         #overall average performance
bisnode[, ceo_perfbase := comp_performance  / median_Perf]  #ceo_perfbase: performance comapred to overall average performance


#################### first round of variable selection ##############################

summary (lm (log(ceo_perfbase) ~ inoffice_days, data = bisnode))   # close to 0
stargazer(list(lm (log(ceo_perfbase) ~ inoffice_days, data = bisnode)), digits=3, type="text", out="ceo_perfabce-inoffice_days.doc",
          no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

summary (lm (log(ceo_perfbase) ~ ceo_count, data = bisnode))
stargazer(list(lm (log(ceo_perfbase) ~ ceo_count, data = bisnode)), digits=3, type="text", out="ceo_perfabce-ceo_count.doc",
          no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

summary (lm (log(ceo_perfbase) ~ origin, data = bisnode))
stargazer(list(lm (log(ceo_perfbase) ~ origin, data = bisnode)), digits=3, type="text", out="ceo_perfabce-origin.doc",
          no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))
bisnode[,.N, by = list(origin, young_CEO)]

summary (lm (log(ceo_perfbase) ~ personnel_exp, data = bisnode))   # close to 0

summary (lm (log(ceo_perfbase) ~ region_m, data = bisnode))        # not significant
# coefficients is very small and/or the significance level is too low, so most likely there is no relationship

### For analysis let us create a clean dataset
model <- bisnode [, .(assets = curr_assets + fixed_assets,   #probably log analysed
                      ind2,
                      ceo_age,
                      ceo_perfbase,
                      #ceo_performance,
                      ceo_count, 
                      origin,
                      comp_age,
                      dom_gender = cut (female, c(-0.1, 0.33, 0.66, 1.1), labels = c('male', 'mix', 'female')),
                      size_cat,
                      region_m,
                      salesgrowth_perc
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

model [, D_region_C := (region_m == 'Central')]
model [, D_region_E := (region_m == 'East')]
model [, D_region_W := (region_m == 'West')]
model <- model [region_m != ""]
#bd: here we lost 35 samples
#pp: I dropped them in row 167

model [, D_CEO_y := (ceo_age <= 40)]
model [, D_CEO_o := (ceo_age > 40)]

model [, D_origin_D := (origin == "Domestic")]
model [, D_origin_F := (origin == "Foreign")]
model [, D_origin_mix := (origin == "mix")]

model [, D_ceo_count_1 := (ceo_count ==1)]
model [, D_ceo_count_M := (ceo_count >1)]




################################# Model1 disregarding industries and size ##########################################
#less control variables (selection based on assumption of importance)
#interaction where it makes sense
#control variables: ceo_performance, D_dom_gender_Fem, D_dom_gender_Mix, D_region_W, D_region_E, D_CEO_y, comp_age
#### THIS MODEL IS DISREGARDED: IT MAKES NO SENSE TO INTERACT COMP_AGE AND SIZE IF WE ARE AFTER D_CEO_y COEFF #####
#coeftest (lm(log(ceo_perfbase) ~ D_CEO_y, data = model), vcov =sandwich)
#lm_All_op <- lm (log(ceo_perfbase) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
#                   D_region_E + D_region_W + comp_age * D_region_E + comp_age * D_region_W, data = model [ceo_perfbase >0,])
#coeftest(lm_All_op, vcov=sandwich)
#
#lm_All_up <- lm (log(-ceo_perfbase) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +
#                   D_region_E + D_region_W + comp_age * D_region_E + comp_age * D_region_W, data = model [ceo_perfbase <0,])
#coeftest(lm_All_up, vcov=sandwich)
#BIC (lm_All_op)
#BIC (lm_All_up)
# model seems to be unusable, contining a lot of parameters that have no use


#### Model 1 ver2. gender and region E removed. Interaction is still on.
##it looks that no dependancy on gender, nor difference between East and Central region.
#lm2_All_op <- lm (log(ceo_perfbase) ~ D_CEO_y + comp_age +
#                    D_region_W + comp_age * D_region_W, data = model [ceo_perfbase >0,])
#coeftest(lm2_All_op, vcov=sandwich)
#lm2_All_up <- lm (log(-ceo_perfbase) ~ D_CEO_y + comp_age +
#                    D_region_W + comp_age * D_region_W, data = model [ceo_perfbase <0,])
#
#coeftest(lm2_All_up, vcov=sandwich)
#
#BIC (lm2_All_op)
#BIC (lm2_All_up)
## quick evaluation: in the below average there is relationship btw. ceo youth and performance, not otherwise


################################# Model2 disregarding industries but including size ##########################################
#less control variables (selection based on assumption of importance) region excluded based on above
#interaction where it makes sense
#control variables: ceo_performance, D_dom_gender_Fem, D_dom_gender_Mix, D_size_M, D_size_L, D_CEO_y

coeftest (lm(log(ceo_perfbase) ~ D_CEO_y, data = model), vcov = sandwich)
lm_IndAll_op <- lm (log(ceo_perfbase) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + 
                    #  comp_age +
                      D_size_M + D_size_L ,
                    #  comp_age * D_size_M + comp_age * D_size_L, 
                    data = model [ceo_perfbase >0,])
coeftest(lm_IndAll_op, vcov=sandwich)
lm_IndAll_up <- lm (log(-ceo_perfbase) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + 
                      # comp_age +
                      D_size_M + D_size_L, 
                    # + comp_age * D_size_M + comp_age * D_size_L, 
                    data = model [ceo_perfbase <0,])
coeftest(lm_IndAll_up, vcov=sandwich)
BIC (lm_IndAll_op)
BIC (lm_IndAll_up)

stargazer(list(lm_IndAll_op, lm_IndAll_up), digits=3, type="text", out="ROE-model_2.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

################################# Model3 REMOVED ##########################################

################################# Model4 size and indusrty included ##########################################
#less control variables (selection based on assumption of importance) region excluded based on above
# no interaction
#control variables: see below


lm_IndSize_op <- lm (log(ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_region_W + D_region_E +D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55 +
                      D_size_M + D_size_L + D_ceo_count_1 + D_origin_F + D_origin_D, data = model [ceo_perfbase >0,])
coeftest (lm_IndSize_op, vcov = sandwich)

BIC(lm_IndSize_op)

lm_IndSize_up <- lm (log(-ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_region_W + D_region_E +D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55 +
                       D_size_M + D_size_L + D_ceo_count_1 + D_origin_F + D_origin_D, data = model [ceo_perfbase <0,])
coeftest (lm_IndSize_up, vcov = sandwich)

BIC(lm_IndSize_up)

#removing regional control:
lm_IndSize_op <- lm (log(ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                      D_size_M + D_size_L + D_ceo_count_1 + D_origin_F + D_origin_D, data = model [ceo_perfbase >0,])
coeftest (lm_IndSize_op, vcov = sandwich)

BIC(lm_IndSize_op)

lm_IndSize_up <- lm (log(-ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                      D_size_M + D_size_L + D_ceo_count_1 + D_origin_F + D_origin_D, data = model [ceo_perfbase <0,])
coeftest (lm_IndSize_up, vcov = sandwich)

BIC(lm_IndSize_up)

################## you can forget the two above attempts, this is below the Model 4 ########################
# removing ceo_count and ceo origin

lm_IndSize_op <- lm (log(ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                      D_size_M + D_size_L, data = model [ceo_perfbase >0,])
coeftest (lm_IndSize_op, vcov = sandwich)

BIC(lm_IndSize_op)

lm_IndSize_up <- lm (log(-ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                      D_size_M + D_size_L, data = model [ceo_perfbase <0,])
coeftest (lm_IndSize_up, vcov = sandwich)

BIC(lm_IndSize_up)
################## this is above the Model 4 ########################


################################# Model5 size and indusrty slice attempt #####################################
### slicing per ind2

# azt nem értem, hogy ha 0gy szétvágom az adatokat, akkor miért ugrik azonnal az korrelációs összefüggés??????
model [, comp_performance := ceo_perfbase * median_Perf]
ind26_median <- quantile (model [ind2==26, comp_performance],0.5)

model [, ceo_perf26 := comp_performance / ind26_median]
lm_Ind26_op <- lm (log(ceo_perf26) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [ceo_perf26 >0 & ind2==26,])
coeftest (lm_Ind26_op, vcov = sandwich)
lm_Ind26_up <- lm (log(-ceo_perf26) ~ D_dom_gender_Fem + D_CEO_y + 
                      D_size_M + D_size_L, data = model [ceo_perf26 <0 & ind2==26,])
coeftest (lm_Ind26_up, vcov = sandwich)

ind28_median <- quantile (model [ind2==28, comp_performance],0.5)
model [, ceo_perf28 := comp_performance / ind28_median]
lm_Ind28_op <- lm (log(ceo_perf28) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [ceo_perf28 >0 & ind2==28,])
coeftest (lm_Ind28_op, vcov = sandwich)
lm_Ind28_up <- lm (log(-ceo_perf28) ~ D_dom_gender_Fem + D_CEO_y + 
                      D_size_M + D_size_L, data = model [ceo_perf28 <0 & ind2==28,])
coeftest (lm_Ind28_up, vcov = sandwich)

ind33_median <- quantile(model [ind2==33, comp_performance],0.5)
model [, ceo_perf33 := comp_performance / ind33_median]
lm_Ind33_op <- lm (log(ceo_perf33) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [ceo_perf33 >0 & ind2==33,])
coeftest (lm_Ind33_op, vcov = sandwich)
lm_Ind33_up <- lm (log(-ceo_perf33) ~ D_dom_gender_Fem + D_CEO_y + 
                      D_size_M + D_size_L, data = model [ceo_perf33 <0 & ind2==33,])
coeftest (lm_Ind33_up, vcov = sandwich)

ind55_median <- quantile (model [ind2==55, comp_performance],0.5)
model [, ceo_perf55 := comp_performance / ind55_median]
lm_Ind55_op <- lm (log(ceo_perf55) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [ceo_perf55 >0 & ind2==55,])
coeftest (lm_Ind55_op, vcov = sandwich)
lm_Ind55_up <- lm (log(-ceo_perf55) ~ D_dom_gender_Fem + D_CEO_y + 
                      D_size_M + D_size_L, data = model [ceo_perf55 <0 & ind2==55,])
coeftest (lm_Ind55_up, vcov = sandwich)

ind56_median <- quantile (model [ind2==56, comp_performance],0.5)
model [, ceo_perf56 := comp_performance / ind56_median]
lm_Ind56_op <- lm (log(ceo_perf56) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [ceo_perf56 >0 & ind2==56,])
coeftest (lm_Ind56_op, vcov = sandwich)
lm_Ind56_up <- lm (log(-ceo_perf56) ~ D_dom_gender_Fem + D_CEO_y + 
                      D_size_M + D_size_L, data = model [ceo_perf56 <0 & ind2==56,])
coeftest (lm_Ind56_up, vcov = sandwich)

### This is why we can find significant correlation in the overall population, but not in all industry segments.
### Too little number of young CEOs
model [ceo_perf26 <0 & ind2==26 & D_CEO_y == TRUE, .N]
model [ceo_perf28 <0 & ind2==28 & D_CEO_y == TRUE, .N]
model [ceo_perf33 <0 & ind2==33 & D_CEO_y == TRUE, .N]
model [ceo_perf55 <0 & ind2==55 & D_CEO_y == TRUE, .N]
model [ceo_perf56 <0 & ind2==56 & D_CEO_y == TRUE, .N]




################################# Model5B size and indusrty, size slice attempt #####################################
### slicing per size

model [, comp_performance := ceo_perfbase * median_Perf]

sizeS_median <- quantile (model [D_size_S == TRUE, comp_performance],0.5)
model [, ceo_perfS := comp_performance / sizeS_median]
lm_sizeS_op <- lm (log(ceo_perfS) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [ceo_perfS >0 & D_size_S == TRUE,])
coeftest (lm_sizeS_op, vcov = sandwich)

lm_sizeS_up <- lm (log(-ceo_perfS) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [ceo_perfS <0 & D_size_S == TRUE,])
coeftest (lm_sizeS_up, vcov = sandwich)

stargazer(list(lm_sizeS_op, lm_sizeS_up), digits=3, type="text", out="ROE-model_5b_sizeS.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

sizeM_median <- quantile (model [D_size_M == TRUE, comp_performance],0.5)
model [, ceo_perfM := comp_performance / sizeM_median]
lm_sizeM_op <- lm (log(ceo_perfM) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [ceo_perfM >0 & D_size_M == TRUE,])
coeftest (lm_sizeM_op, vcov = sandwich)

lm_sizeM_up <- lm (log(-ceo_perfM) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [ceo_perfM <0 & D_size_M == TRUE,])
coeftest (lm_sizeM_up, vcov = sandwich)

stargazer(list(lm_sizeM_op, lm_sizeM_up), digits=3, type="text", out="ROE-model_5b_sizeM.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

sizeL_median <- quantile (model [D_size_L == TRUE, comp_performance],0.5)
model [, ceo_perfL := comp_performance / sizeL_median]
lm_sizeL_op <- lm (log(ceo_perfL) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [ceo_perfL >0 & D_size_L == TRUE,])
coeftest (lm_sizeL_op, vcov = sandwich)

lm_sizeL_up <- lm (log(-ceo_perfL) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [ceo_perfL <0 & D_size_L == TRUE,])
coeftest (lm_sizeL_up, vcov = sandwich)

stargazer(list(lm_sizeL_op, lm_sizeL_up), digits=3, type="text", out="ROE-model_5b_sizeL.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

### Maybe this is why we can find significant correlation in the overall population, but not in all size segments.
### Too little number of young CEOs
model [ceo_perfS >0 & D_size_S == TRUE & D_CEO_y == TRUE, .N]
model [ceo_perfS <0 & D_size_S == TRUE & D_CEO_y == TRUE, .N]
model [ceo_perfM >0 & D_size_M == TRUE & D_CEO_y == TRUE, .N]
model [ceo_perfM <0 & D_size_M == TRUE & D_CEO_y == TRUE, .N]
model [ceo_perfL >0 & D_size_L == TRUE & D_CEO_y == TRUE, .N]
model [ceo_perfL <0 & D_size_L == TRUE & D_CEO_y == TRUE, .N]


################################# Model6 size interaction, industry incl. #####################################


lm_IndSizeX_op <- lm (log(ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                        D_size_M + D_size_L +
                        D_CEO_y * D_size_M + D_CEO_y * D_size_L,
                      data = model [ceo_perfbase >0,])
coeftest (lm_IndSizeX_op, vcov = sandwich)

BIC(lm_IndSizeX_op)

lm_IndSizeX_up <- lm (log(-ceo_perfbase) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                        D_size_M + D_size_L +
                        D_CEO_y * D_size_M + D_CEO_y * D_size_L,
                      data = model [ceo_perfbase <0,])
coeftest (lm_IndSizeX_up, vcov = sandwich)

BIC(lm_IndSizeX_up)

########################### SALESGROWTH_PERC MODELS #######################################
cor (model[,salesgrowth_perc], model[,ceo_perfbase])
## It looks that the two do not really correlate
## log: hard to check, as there are many 0-s

################################# Model2 disregarding industries but including size ##########################################
#less control variables (selection based on assumption of importance) region excluded based on above
#interaction where it makes sense
#control variables: ceo_performance, D_dom_gender_Fem, D_dom_gender_Mix, D_size_M, D_size_L, D_CEO_y

lm_IndAll_sop <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y +
                       comp_age +
                      D_size_M + D_size_L ,
                    #  comp_age * D_size_M + comp_age * D_size_L, 
                    data = model [salesgrowth_perc >0,])
coeftest(lm_IndAll_sop, vcov=sandwich)

stargazer(lm_IndAll_sop, digits=3, type="text", out="sales_growth-model_2-1.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

lm_IndAll_sup <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_dom_gender_Mix + D_CEO_y + comp_age +D_size_M + D_size_L, 
                    # + comp_age * D_size_M + comp_age * D_size_L, 
                    data = model [salesgrowth_perc <0,])
coeftest(lm_IndAll_sup, vcov=sandwich)

stargazer(lm_IndAll_sup, digits=3, type="text", out="sales_growth-model_2-2.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

BIC (lm_IndAll_sop)
BIC (lm_IndAll_sup)


################################# Model4 size and indusrty included ##########################################
#less control variables (selection based on assumption of importance) region excluded based on above
# no interaction
#control variables: see below
lm_IndSize_sop <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + comp_age + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+ 
                       D_size_M + D_size_L, data = model [salesgrowth_perc >0,])
coeftest (lm_IndSize_sop, vcov = sandwich)

stargazer(lm_IndSize_sop, digits=3, type="text", out="sales_growth-model_4-1.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

BIC(lm_IndSize_sop)

lm_IndSize_sup <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + comp_age + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                       D_size_M + D_size_L, data = model [salesgrowth_perc <0,])
coeftest (lm_IndSize_sup, vcov = sandwich)

stargazer(lm_IndSize_sup, digits=3, type="text", out="sales_growth-model_4-2.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

BIC(lm_IndSize_sup)

# Stargazer summing model 2 and 4

stargazer(list(lm_IndAll_sop, lm_IndSize_sop), digits=3, type="text", out="sales_growth-model_2-4-1.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

stargazer(list(lm_IndAll_sup, lm_IndSize_sup), digits=3, type="text", out="sales_growth-model_2-4-2.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

################################# Model5 size and indusrty slice attempt #####################################
### slicing per ind2
### here the baseline is 0, we do not need to find industry averages as in the ROE case:
### because salesgrowth_perc is already a relative growth, we don't need to baseline it to average

lm_Ind26_sop <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc > 0 & ind2==26,])
coeftest (lm_Ind26_sop, vcov = sandwich)
lm_Ind26_sup <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc <0 & ind2==26,])
coeftest (lm_Ind26_sup, vcov = sandwich)

lm_Ind28_sop <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc >0 & ind2==28,])
coeftest (lm_Ind28_sop, vcov = sandwich)
lm_Ind28_sup <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc <0 & ind2==28,])
coeftest (lm_Ind28_sup, vcov = sandwich)

lm_Ind33_sop <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc >0 & ind2==33,])
coeftest (lm_Ind33_sop, vcov = sandwich)
lm_Ind33_sup <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc <0 & ind2==33,])
coeftest (lm_Ind33_sup, vcov = sandwich)

lm_Ind55_sop <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc >0 & ind2==55,])
coeftest (lm_Ind55_sop, vcov = sandwich)
lm_Ind55_sup <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc <0 & ind2==55,])
coeftest (lm_Ind55_sup, vcov = sandwich)

lm_Ind56_op <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc >0 & ind2==56,])
coeftest (lm_Ind56_sop, vcov = sandwich)
lm_Ind56_sup <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_size_M + D_size_L, data = model [salesgrowth_perc <0 & ind2==56,])
coeftest (lm_Ind56_sup, vcov = sandwich)

### This is why we can find significant correlation in the overall population, but not in all industry segments.
### Too little number of young CEOs
model [salesgrowth_perc <0 & ind2==26 & D_CEO_y == TRUE, .N]
model [salesgrowth_perc <0 & ind2==28 & D_CEO_y == TRUE, .N]
model [salesgrowth_perc <0 & ind2==33 & D_CEO_y == TRUE, .N]
model [salesgrowth_perc <0 & ind2==55 & D_CEO_y == TRUE, .N]
model [salesgrowth_perc <0 & ind2==56 & D_CEO_y == TRUE, .N]


################################# Model5B size and indusrty, size slice attempt #####################################
### slicing per size

lm_sizeS_ops <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [salesgrowth_perc >0 & D_size_S == TRUE,])
coeftest (lm_sizeS_ops, vcov = sandwich)
lm_sizeS_ups <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [salesgrowth_perc <0 & D_size_S == TRUE,])
coeftest (lm_sizeS_ups, vcov = sandwich)
stargazer(list(lm_sizeS_ops, lm_sizeS_ups), digits=3, type="text", out="sales_growth-model_5b_sizeS.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

lm_sizeM_ops <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [salesgrowth_perc >0 & D_size_M == TRUE,])
coeftest (lm_sizeM_ops, vcov = sandwich)
lm_sizeM_ups <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [salesgrowth_perc <0 & D_size_M == TRUE,])
coeftest (lm_sizeM_ups, vcov = sandwich)
stargazer(list(lm_sizeM_ops, lm_sizeM_ups), digits=3, type="text", out="sales_growth-model_5b_sizeM.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

lm_sizeL_ops <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [salesgrowth_perc >0 & D_size_L == TRUE,])
coeftest (lm_sizeL_ops, vcov = sandwich)
lm_sizeL_ups <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + 
                     D_ind2_26 + D_ind2_28 + D_ind2_33 + D_ind2_55, data = model [salesgrowth_perc <0 & D_size_L == TRUE,])
coeftest (lm_sizeL_ups, vcov = sandwich)
stargazer(list(lm_sizeL_ops, lm_sizeL_ups), digits=3, type="text", out="sales_growth-model_5b_sizeL.doc",no.space = TRUE, omit.stat=c("LL","ser","f", "aic"))

### Maybe this is why we can find significant correlation in the overall population, but not in all size segments.
### Too little number of young CEOs
model [salesgrowth_perc >0 & D_size_S == TRUE & D_CEO_y == TRUE, .N]
model [salesgrowth_perc <0 & D_size_S == TRUE & D_CEO_y == TRUE, .N]
model [salesgrowth_perc >0 & D_size_M == TRUE & D_CEO_y == TRUE, .N]
model [salesgrowth_perc <0 & D_size_M == TRUE & D_CEO_y == TRUE, .N]
model [salesgrowth_perc >0 & D_size_L == TRUE & D_CEO_y == TRUE, .N]
model [salesgrowth_perc <0 & D_size_L == TRUE & D_CEO_y == TRUE, .N]


################################# Model6 size interaction, industry incl. #####################################


lm_IndSizeX_ops <- lm (log(salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                        D_size_M + D_size_L +
                        D_CEO_y * D_size_M + D_CEO_y * D_size_L,
                      data = model [salesgrowth_perc >0,])
coeftest (lm_IndSizeX_ops, vcov = sandwich)

BIC(lm_IndSizeX_ops)

lm_IndSizeX_ups <- lm (log(-salesgrowth_perc) ~ D_dom_gender_Fem + D_CEO_y + D_ind2_56 + D_ind2_28 + D_ind2_33+ D_ind2_55+
                        D_size_M + D_size_L +
                        D_CEO_y * D_size_M + D_CEO_y * D_size_L,
                      data = model [salesgrowth_perc <0,])
coeftest (lm_IndSizeX_ups, vcov = sandwich)

BIC(lm_IndSizeX_ups)

################################# Plots for technical report #####################################

# ====================================

# PLOTS

# Young CEO
freq(bisnode$young_CEO)

# CEO age
ggplot(model, aes(ceo_age)) + geom_histogram() +
  ggtitle(labs(title = "Histogram of CEO age", x = "Age of CEOs", y = "Count"))
pander(summary(bisnode$ceo_age))
CrossTable(bisnode$young_CEO, bisnode$ind2)
bisnode [,.N, by = list(ind2, size_cat)] # number of companies by industries

# ggplot(bisnode, aes(young_CEO)) + geom_histogram() + facet_wrap(~ ind2) +
#  ggtitle(labs(title = "Histogram of CEO binary variable accross industries", x = "CEO binary variable", y = "Count"))
CrossTable(bisnode$young_CEO, bisnode$ind2)
ggplot(bisnode, aes(ceo_age)) + geom_histogram() + facet_wrap(~ ind2) +
  ggtitle(labs(title = "Histogram of CEO age accross industries", x = "Age of CEOs", y = "Count"))

# inc_bef_tax
pander(summary(bisnode$inc_bef_tax))
ggplot(bisnode, aes(inc_bef_tax)) + geom_histogram() + facet_wrap(~ ind2)

#profit_loss_year
pander(summary(bisnode$profit_loss_year))
bisnode[,mean(profit_loss_year), by = ind2]
bisnode[,mean(profit_loss_year), by = size_cat]
#ggplot(bisnode, aes(profit_loss_year)) + geom_histogram() + facet_wrap(~ ind2)
#ggplot(bisnode, aes(profit_loss_year)) + geom_histogram() + facet_wrap(~ size_cat)
ggplot(bisnode, aes(log(profit_loss_year))) + geom_histogram() + facet_wrap(~ ind2) +
  ggtitle(labs(title = "Histogram of log profit_loss_year across industries", x = "Log of net profit (profit_loss_year)", y = "Count"))
ggplot(bisnode, aes(log(profit_loss_year))) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of log profit_loss_year across company sizes", x = "Log of net profit (profit_loss_year)", y = "Count"))

# share_eq
pander(summary(bisnode$share_eq))
ggplot(bisnode, aes(log(share_eq))) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of log profit_loss_year across company sizes", x = "Log of net profit (profit_loss_year)", y = "Count"))
ggplot(bisnode, aes(log(share_eq))) + geom_histogram() + facet_wrap(~ ind2) +
  ggtitle(labs(title = "Histogram of log profit_loss_year across industries", x = "Log of net profit (profit_loss_year)", y = "Count"))

# sales
pander(summary(bisnode$sales))
pander(summary(bisnode$sales_pastyear))
pander(summary(bisnode$salesgrowth_perc))
# ggplot(bisnode, aes(salesgrowth_perc)) + geom_histogram() + facet_wrap(~ ind2)

# companies
bisnode [sales == 0,.N, by = ind2]
bisnode [,.N, by = size_cat]
bisnode [,.N, by = list(ind2, size_cat)]
CrossTable(bisnode$ind2, bisnode$size_cat)
CrossTable(bisnode$young_CEO, bisnode$size_cat)
ggplot(bisnode, aes(ceo_age)) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of CEO age across company sizes", x = "Age of CEOs", y = "Count"))

# comp_age
pander(summary(model$comp_age))

# Gender
model[,.N, by = dom_gender]

# Origin
model[,.N, by = origin]

# comp_performance
summary(bisnode$comp_performance)
ggplot(bisnode, aes(comp_performance)) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of company performance across company sizes", x = "Company peformance", y = "Count"))
ggplot(bisnode, aes(log(comp_performance))) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of log company performance across company sizes", x = "Log of company performance", y = "Count"))

# ggplot(data = bisnode, aes(x=ceo_age, y=comp_performance)) +
#  geom_point(colour="orange") +
#  geom_smooth(method="lm", colour="navy") +
#  ggtitle(labs(title = "Scatterplot of comp_performance CEO age", x = "Age of CEOs", y = "Company performance"))
# ggplot(data = bisnode, aes(x=ceo_age, y=ceo_perfbase)) +
#  geom_point(colour="orange") +
#  geom_smooth(method="lm", colour="navy") +
#  ggtitle(labs(title = "Scatterplot of CEO performance on CEO age", x = "Age of CEOs", y = "CEO performance"))

# ggplot(data = bisnode, aes(x=ceo_age, y=comp_performance)) +
#  geom_point(colour="orange") +
#  geom_smooth(method="lm", colour="navy") + facet_wrap(~size_cat) +
#  ggtitle(labs(title = "Scatterplot of company performance on CEO age", x = "Age of CEOs", y = "CEO performance"))

ggplot(data = bisnode, aes(x=ceo_age, y=log(ceo_perfbase))) +
  geom_point(size=1.5, aes(colour=factor(size_cat))) + geom_smooth(method="lm", colour="navy") + facet_wrap(~ind2) +
  ggtitle(labs(title = "Scatterplot of CEO performance by form size", x = "Age of CEOs", y = "CEO performance")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1"))

# ggplot(data = bisnode, aes(x=comp_age, y=comp_performance)) +
#  geom_point(colour="orange") +
#  geom_smooth(method="lm", colour="navy") + facet_wrap(~size_cat) +
#  ggtitle(labs(title = "Scatterplot of company performance on company age", x = "Age of companies", y = "Company performance"))

# CEO performance
bisnode[, mean(ceo_perfbase), by = young_CEO]
bisnode[, mean(ceo_perfbase), by = list(size_cat, young_CEO)]
bisnode[, mean(ceo_perfbase), by = list(ind2, young_CEO)]
bisnode[, median(ceo_perfbase), by = young_CEO]
bisnode[, median(ceo_perfbase), by = list(size_cat, young_CEO)]
ggplot(bisnode, aes(ceo_perfbase)) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of ceo performance across company sizes", x = "CEO peformance", y = "Count"))
ggplot(bisnode, aes(log(ceo_perfbase))) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of log CEO performance across company sizes", x = "log of CEO peformance", y = "Count"))

# 26 Manufacture of computer, electronic and optical products
# 28 Manufacture of machinery and equipment n.e.c.
# 33 Repair and installation of machinery and equipment
# 55 Accommodation
# 56 Food and beverage service activities

ggplot(data = bisnode, aes(x=ceo_age, y=ceo_perfbase)) +
  geom_point(colour="orange") +
  geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot age and base performance of CEOs", x = "Age of CEOs", y = "Base performance of CEOs"))

# CEO performance - SALES and SALESGROWTH_PERC
summary(bisnode$sales_pastyear)
summary(bisnode$sales)
bisnode[, mean(sales), by = young_CEO]
bisnode[, mean(sales), by = list(size_cat, young_CEO)]
bisnode[, mean(sales), by = list(ind2, young_CEO)]
bisnode[, median(sales), by = young_CEO]
bisnode[, median(sales), by = list(size_cat, young_CEO)]

bisnode[, mean(salesgrowth_perc), by = young_CEO]
bisnode[, mean(salesgrowth_perc), by = list(size_cat, young_CEO)]
bisnode[, mean(salesgrowth_perc), by = list(ind2, young_CEO)]
bisnode[, median(salesgrowth_perc), by = young_CEO]
bisnode[, median(salesgrowth_perc), by = list(size_cat, young_CEO)]

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(size_cat))) + geom_smooth(method="lm", colour="navy") + facet_wrap(~ind2) +
  ggtitle(labs(title = "Scatterplot of sales growth on CEO age by firm size and industry", x = "Age of CEOs", y = "Sales growth")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1"))

ggplot(bisnode, aes(salesgrowth_perc)) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of sales growth across company sizes", x = "Sales growth", y = "Count"))
ggplot(bisnode, aes(log(salesgrowth_perc))) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of log sales growth across company sizes", x = "log of sales growth", y = "Count"))

ggplot(bisnode, aes(sales)) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of sales across company sizes in 2015", x = "Sales", y = "Count"))
ggplot(bisnode, aes(log(sales))) + geom_histogram() + facet_wrap(~ size_cat) +
  ggtitle(labs(title = "Histogram of log sales growth across company sizes in 2015", x = "log of sales", y = "Count"))

# Looking for patterns ...

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(size_cat))) + geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot of sales growth by form size", x = "Age of CEOs", y = "Sales growth")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1"))

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(ind2))) + geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot of sales growth by form size", x = "Age of CEOs", y = "Sales growth")) +
  facet_wrap(~ size_cat) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1", "black"))

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(ind2))) + geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot of sales growth by industry", x = "Age of CEOs", y = "Sales growth")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1", "navy"))

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(gender))) + geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot of sales growth by gender", x = "Age of CEOs", y = "Sales growth")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1", "navy"))

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(region_m))) + geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot of sales growth by region", x = "Age of CEOs", y = "Sales growth")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1", "navy"))

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(origin))) + geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot of sales growth by origin", x = "Age of CEOs", y = "Sales growth")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1", "navy"))

ggplot(data = bisnode, aes(x=ceo_age, y=log(salesgrowth_perc))) +
  geom_point(size=1.5, aes(colour=factor(ceo_count))) + geom_smooth(method="lm", colour="navy") +
  ggtitle(labs(title = "Scatterplot of sales growth by number of CEOs in management", x = "Age of CEOs", y = "Sales growth")) +
  scale_colour_manual(name="Company size",values=c("darkgreen","indian red","lightcyan 4", "lightpink 1", "navy", "black", "orange"))

# Colours for Legend
# "#66CC00","#FFFF33","#F8766D","#00BFC4"

bisnode[,median(comp_performance)]
