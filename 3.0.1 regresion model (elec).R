
# ORU PY2022 Behvaioural Billing analysis
## Prepare for regression
## Lia Tang, Aug 2023

rm(list = ls())

# 0.1 Import Library ----------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(fixest)

# 0.2 Directory ---------------------------------------------------------------
Drive <- "F:/AEG"
raw <- paste0(Drive, "/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/02 Raw Data/04 Behavioral Billing Data")
data_out <- paste0(Drive, "/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/03 Programs/Electric/Res_Electric Behavioral/R_output")

# 1. Infile clean PY2022 Billing Data  ---------------------------------------------
map_df <- readRDS(file.path(data_out, "PY2022_ORU_HER_mapping_AEG.RDS"))
elec <- fread(file.path(data_out, "Elec_calendarized_data_weather.csv"))
gas <- fread(file.path(data_out, "Gas_calendarized_data_weather.csv"))

# 2. checks prior to modelling ---------------------------------------------------
## 2.1 check if we need to model the waves separately ---------
waves <- elec[, unique(e_wave)]

dt <- elec[, .(avg_mon_kwh = mean(mon_kwh)), .(e_wave, moyr, treatment, post)]
ggplot(dt, aes(x = moyr, y = avg_mon_kwh, color = factor(treatment))) + geom_line() + facet_wrap(~e_wave)

# yes, the loads are quite different. we need to model each wave separately.
e <- elec[e_wave == "e_201907",]

# 3.1. fixed effects diff-in-diff model --------
# pdat <- pdata.frame(elec, index = c("opower_customer_id", "moyr"))

# feols(response ~ predictors | fixed_effects, data = your_data)

#### e_201907 ------
mod1 <- feols(mon_kwh ~ post + treatment + treat_post, e)
summary(mod1)

mod2 <- feols(mon_kwh ~ post  + treat_post | opower_customer_id, e)
summary(mod2)

mod3a <- feols(mon_kwh ~  treat_post | opower_customer_id + moyr, e)
summary(mod3a)
# above looks fine

# separate treatment effects by month year (by interacting with post months variables)
mod3 <- feols(mon_kwh ~ treat_post:factor(moyr) | opower_customer_id + moyr,
              data = e,
              cluster = ~opower_customer_id)
summary(mod3)
# this is it!!

# get baseline loads
e[, c("fixef_id", "fixef_moyr") := predict(mod3, fixef = TRUE)] # turn on SE also to get them around predictions

# rerun model to obtain the SEs by adding the FEs as standard factors
mod3b <- feols(mon_kwh ~ -1 + fixef_id + fixef_moyr + treat_post:factor(moyr), e)
summary(mod3b)

# std error
e[, se     := predict(mod3b, interval = "confidence")["se.fit"]]

#################


didmod201907 <- feols(mon_kwh ~ post + treat_post | opower_customer_id + moyr,
                 data = e_201907)
summary(didmod201907)

# get baseline loads
e_201907[, c("fixef_id", "fixef_month") := predict(didmod201907, fixef = TRUE)]

# rerun model to obtain the SEs by adding the FEs as standard factors
didmod201907_c <- feols(mon_kwh ~ fixef_id + fixef_month + post + treat_post ,
                      data = e_201907)
summary(didmod201907_c)

# std error
e_201907[, se     := predict(didmod201907_c, interval = "confidence")["se.fit"]]

# save coefficients
e_201907[, coef_post := coef(didmod201907_c)["post"]]
e_201907[, coef_treat_post := coef(didmod201907_c)["treat_post"]]
e_201907[, resid := residuals(didmod201907)]

# estimate usage from reg
e_201907[, yhat := predict(didmod201907)]

# counter-factual (reference usage, only changes if part,post are both 1)
e_201907[, cfyhat := predict(didmod201907, newdata = copy(e_201907)[, treat_post := 0])]

# sigma
e_201907[, model_sigma := sigma(didmod201907)]


# didmod1 <- feols(mon_kwh ~ post + treat_post | opower_customer_id + moyr + factor(month),
#                       data = elec)
# summary(didmod1)
# 
# didmod <- plm(mon_kwh ~ treatment + treatment * post + post , data = pdat, model = "within")
# summary(didmod)


# 3.2. FE DiD model w/ weather--------
curr_wave = "e_202201"
e <- elec[e_wave == curr_wave]


## 3.2.1 wave: e_201907 -------------

### 3.2.1.1 pre-final ------------------------
## this looks good for e_201907
# 
# didmod0 <- feols(mon_kwh ~ treat_post + post +
#                    HDD45 + CDD65 + factor(month) + factor(year)| 
#                    opower_customer_id,
#                  data = e)
curr_wave = "e_201907"
e <- elec[e_wave == curr_wave]

didmod0 <- feols(mon_kwh ~ treat_post + post +
                   HDD45 + CDD65 + factor(month) + factor(year)| 
                   opower_customer_id,
                 data = e, cluster = ~month+opower_customer_id+year)
print(paste0("wave: ", curr_wave))
summary(didmod0)

summary(didmod0, cluster = ~month+opower_customer_id+year)

se(didmod0, cluster = ~month+opower_customer_id+year)




## 3.2.2.NOT pre-final for e_202101 ----------------
e <- e[!(year == 2019)]
didmod0 <- feols(mon_kwh ~ treat_post + post +
                   HDD45 + CDD65 + factor(month) + factor(year)| 
                   opower_customer_id,
                 data = e)
print(paste0("wave: ", curr_wave))
summary(didmod0)


# only keeping 12 months pre and 12 months post
e <- e[moyr %between% c("2021-01-01", "2023-02-01")]

didmod0 <- feols(mon_kwh ~ treat_post + post +
                   HDD45 + factor(month) + factor(year)| 
                   opower_customer_id,
                 data = e)
print(paste0("wave: ", curr_wave))
summary(didmod0)


## 3.2.3 pre-final for e_202201 ------------------
# treat_post + treat_post:HDD45 
# e_202201
didmod0 <- feols(mon_kwh ~  treat_post + treat_post:HDD45 + treat_post:CDD65 
                   + CDD65 + HDD45 + factor(month) + factor(year) | 
                   opower_customer_id,
                 data = e)
print(paste0("wave: ", curr_wave))
summary(didmod0)




# get baseline loads
e[, c("fixef_id") := predict(didmod0, fixef = TRUE)]

# rerun model to obtain the SEs by adding the FEs as standard factors
didmod1 <- feols(mon_kwh ~ fixef_id + 
                   treat_post + treat_post:HDD45 + treat_post:CDD65 
                 + CDD65 + HDD45 + factor(month) + factor(year) ,
                 data = e)
summary(didmod1)

etable(didmod0, didmod1)



# std error
e[, se     := predict(didmod1, interval = "confidence")["se.fit"]]

# save coefficients
e[, coef_treat_post := coef(didmod1)["treat_post"]]
e[, coef_CDD65 := coef(didmod1)["CDD65"]]
e[, coef_HDD45 := coef(didmod1)["HDD45"]]
e[, coef_Feb := coef(didmod1)["factor(month)2"]]
e[, coef_Mar := coef(didmod1)["factor(month)3"]]
e[, coef_Apr := coef(didmod1)["factor(month)4"]]
e[, coef_May := coef(didmod1)["factor(month)5"]]
e[, coef_Jun := coef(didmod1)["factor(month)6"]]
e[, coef_Jul := coef(didmod1)["factor(month)7"]]
e[, coef_Aug := coef(didmod1)["factor(month)8"]]
e[, coef_Sep := coef(didmod1)["factor(month)9"]]
e[, coef_Oct := coef(didmod1)["factor(month)10"]]
e[, coef_Nov := coef(didmod1)["factor(month)11"]]
e[, coef_Dec := coef(didmod1)["factor(month)12"]]
e[, coef_2021 := coef(didmod1)["factor(year)2021"]]
e[, coef_2022 := coef(didmod1)["factor(year)2022"]]
e[, coef_2023 := coef(didmod1)["factor(year)2023"]]
e[, coef_didHDD45 := coef(didmod1)["treat_post:HDD45"]]
e[, coef_didCDD65 := coef(didmod1)["treat_post:CDD65"]]
e[, resid := residuals(didmod1)]

# estimate usage from reg
e[, yhat := predict(didmod1)]

# counter-factual (reference usage, only changes if part,post are both 1)
e[, cfyhat := predict(didmod1, newdata = copy(e)[, `:=` (treat_post = 0, 
                                                         coef_didHDD45 = 0, 
                                                         coef_didCDD65 = 0)])]

# sigma
e[, model_sigma := sigma(didmod1)]



# 3.3 collapse dataset first before doing the predictions  #####
# tested on e_202201:
# model: didmod0 <- feols(mon_kwh ~  treat_post +
#  treat_post:HDD45 + treat_post:CDD65 
# + CDD65 + HDD45 + factor(month) + factor(year) | 
#   opower_customer_id,
# data = e)
curr_wave = "e_202201"
e_mon <- elec[e_wave == curr_wave]

e_mon <- e_mon[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), .(moyr, treatment, post)]
# std error
e_mon[, se     := predict(didmod1, interval = "confidence")["se.fit"]]
e_mon[, se :=NULL]

# save coefficients
e_mon[, coef_treat_post := coef(didmod1)["treat_post"]]
e_mon[, coef_CDD65 := coef(didmod1)["CDD65"]]
e_mon[, coef_HDD45 := coef(didmod1)["HDD45"]]
e_mon[, coef_Feb := coef(didmod1)["factor(month)2"]]
e_mon[, coef_Mar := coef(didmod1)["factor(month)3"]]
e_mon[, coef_Apr := coef(didmod1)["factor(month)4"]]
e_mon[, coef_May := coef(didmod1)["factor(month)5"]]
e_mon[, coef_Jun := coef(didmod1)["factor(month)6"]]
e_mon[, coef_Jul := coef(didmod1)["factor(month)7"]]
e_mon[, coef_Aug := coef(didmod1)["factor(month)8"]]
e_mon[, coef_Sep := coef(didmod1)["factor(month)9"]]
e_mon[, coef_Oct := coef(didmod1)["factor(month)10"]]
e_mon[, coef_Nov := coef(didmod1)["factor(month)11"]]
e_mon[, coef_Dec := coef(didmod1)["factor(month)12"]]
e_mon[, coef_2021 := coef(didmod1)["factor(year)2021"]]
e_mon[, coef_2022 := coef(didmod1)["factor(year)2022"]]
e_mon[, coef_2023 := coef(didmod1)["factor(year)2023"]]
e_mon[, coef_didHDD45 := coef(didmod1)["treat_post:HDD45"]]
e_mon[, coef_didCDD65 := coef(didmod1)["treat_post:CDD65"]]
# e_mon[, resid := residuals(didmod1)]

# export this to get prediction by hand
e_mon[, yhat :=  (
    coef_treat_post * treatment * post +
    coef_CDD65 * CDD65 +
    coef_HDD45 * HDD45 +
    fcase(month == 1, 0,
          month == 2, coef_Feb,
          month == 3, coef_Mar, 
          month == 4, coef_Apr,
          month == 5, coef_May, 
          month == 6, coef_Jun,
          month == 7, coef_Jul, 
          month == 8, coef_Aug,
          month == 9, coef_Sep,
          month == 10, coef_Oct,
          month == 11, coef_Nov,
          month == 12, coef_Dec) +
    fcase(year == 2020, 0,
          year == 2021, coef_2021,
          year == 2022, coef_2022, 
          year == 2023, coef_2023) +
    coef_didHDD45 * treatment * post * HDD45 +
    coef_didCDD65 * treatment * post * CDD65
)]

fwrite(e_mon, 
       file.path(data_out, "e_202201_monthly_predict.csv"))

# estimate usage from reg
e_mon[, yhat := predict(didmod1)]

# counter-factual (reference usage, only changes if part,post are both 1)
e[, cfyhat := predict(didmod1, newdata = copy(e)[, `:=` (treat_post = 0, 
                                                         coef_didHDD45 = 0, 
                                                         coef_didCDD65 = 0)])]

# sigma
e[, model_sigma := sigma(didmod1)]

# 4. checks -------
## 4.1 lincom -----------
# run lincom in Stata to QC SE, it will be slightly differentuse xtreg
e[, lincom_check := yhat - (
    fixef_id +
    coef_treat_post * treatment * post +
    coef_CDD65 * CDD65 +
    coef_HDD45 * HDD45 +
    fcase(month == 1, 0,
          month == 2, coef_Feb,
          month == 3, coef_Mar, 
          month == 4, coef_Apr,
          month == 5, coef_May, 
          month == 6, coef_Jun,
          month == 7, coef_Jul, 
          month == 8, coef_Aug,
          month == 9, coef_Sep,
          month == 10, coef_Oct,
          month == 11, coef_Nov,
          month == 12, coef_Dec) +
    fcase(year == 2020, 0,
          year == 2021, coef_2021,
          year == 2022, coef_2022, 
          year == 2023, coef_2023) +
    coef_didHDD45 * treatment * post * HDD45 +
    coef_didCDD65 * treatment * post * CDD65
)]

fivenum(e$lincom_check) # not zero 



e[, impacts := cfyhat - yhat]
fivenum(e$impacts)

ggplot(e) + geom_line(aes(x = moyr, y = impacts, color = factor(treatment)))

# get SE for each coefficient (combination)
# 12 months coef
# 12 month SE

## 4.2 visuals ----------
# predicted vs actual
dat <- e[, .(moyr, post, treatment, mon_kwh, yhat, cfyhat)]
dat[treatment == 1 & post == 1]

# check residuals
### don't run this, too many points
# ggplot(e_201907, aes(yhat, resid)) + geom_point()

# make long for ggplot
dat <- pivot_longer(dat, cols = c("mon_kwh", "yhat", "cfyhat"), names_to = "pred", values_to = "kwh")
setDT(dat)

# plot load over moyr
# simplify dataset before plotting to avoid crash
dat <- dat[, .(avg_kwh = mean(kwh)), .(moyr, treat_post)]
ggplot(dat) + geom_line(aes(x = moyr, y = avg_kwh, color = factor(treat_post)))




