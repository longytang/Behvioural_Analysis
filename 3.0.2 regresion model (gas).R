
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
waves <- gas[, unique(g_wave)]

dt <- gas[, .(avg_mon_ccf = mean(mon_ccf)), .(g_wave, moyr, treatment, post)]
ggplot(dt, aes(x = moyr, y = avg_mon_ccf, color = factor(treatment))) + geom_line() + facet_wrap(~g_wave)
# yes, the loads are quite different. we need to model each wave separately.


# 3.1. fixed effects diff-in-diff model --------
curr_wave = "g_202201"
g <- gas[g_wave == curr_wave]
# pdat <- pdata.frame(elec, index = c("opower_customer_id", "moyr"))

# feols(response ~ predictors | fixed_effects, data = your_data)

didmod0 <- feols(mon_ccf ~ post + treat_post | opower_customer_id + moyr,
                 data = g)
print(paste0("wave: ", curr_wave))
summary(didmod0)

# get baseline loads
g[, c("fixef_id", "fixef_month") := predict(didmod0, fixef = TRUE)]

# rerun model to obtain the SEs by adding the FEs as standard factors
didmod1 <- feols(mon_ccf ~ fixef_id + fixef_month + post + treat_post ,
                      data = g)
summary(didmod1)

# std error
g[, se     := predict(didmod1, interval = "confidence")["se.fit"]]

# save coefficients
g[, coef_post := coef(didmod1)["post"]]
g[, coef_treat_post := coef(didmod1)["treat_post"]]
g[, resid := residuals(didmod1)]

# estimate usage from reg
g[, yhat := predict(didmod1)]

# counter-factual (reference usage, only changes if part,post are both 1)
g[, cfyhat := predict(didmod1, newdata = copy(g)[, treat_post := 0])]

# sigma
g[, model_sigma := sigma(didmod1)]


# didmod1 <- feols(mon_kwh ~ post + treat_post | opower_customer_id + moyr + factor(month),
#                       data = elec)
# summary(didmod1)
# 
# didmod <- plm(mon_kwh ~ treatment + treatment * post + post , data = pdat, model = "within")
# summary(didmod)


# 3.2. FE DiD model w/ weather--------
curr_wave = "g_202201"
g <- gas[g_wave == curr_wave]

## 3.2.1 full model --------------

####################################################
# g_201907
# didmod0 <- feols(mon_ccf ~  treat_post + 
#                    HDD45 + factor(month) + factor(year) | 
#                    opower_customer_id,
#                  data = g)
# 
didmod0 <- feols(mon_ccf ~  treat_post + 
                   HDD45 + factor(month) + factor(year) | 
                   opower_customer_id,
                 data = g)
print(paste0("wave: ", curr_wave))
summary(didmod0)
#####################################################

####################################################
didmod0 <- feols(mon_ccf ~  treat_post + treat_post:HDD45 + 
                   HDD45 + factor(month) + factor(year) | 
                   opower_customer_id,
                 data = g)
print(paste0("wave: ", curr_wave))
summary(didmod0)
#####################################################
didmod0 <- feols(mon_ccf ~  treat_post + 
                   HDD40 + HDD45 + HDD50 + HDD55 + CDD60 + CDD65 + CDD70 | 
                   opower_customer_id,
                 data = g)
print(paste0("wave: ", curr_wave))
summary(didmod0)

didmod0 <- feols(mon_ccf ~  treat_post + 
                   HDD40 + HDD45 + HDD50 + HDD55 + CDD60 + CDD65 + CDD70 | 
                   opower_customer_id + factor(month),
                 data = g)
print(paste0("wave: ", curr_wave))
summary(didmod0)

# get baseline loads
e[, c("fixef_id", "fixef_month") := predict(didmod0, fixef = TRUE)]

# rerun model to obtain the SEs by adding the FEs as standard factors
didmod1 <- feols(mon_kwh ~ fixef_id + fixef_month + post + treat_post ,
                        data = e)
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


# 4. checks -------

## 4.1 lincom -----------
g[, lincom_check := yhat - (
    fixef_id +
    fixef_month +
    coef_post * post +
    coef_treat_post * treatment * post
)]

fivenum(g$lincom_check) # not zero but very close
# [1] -4.972048e-04 -2.432131e-04 -2.432131e-04 -2.432131e-04  1.818989e-12

## 4.2 visuals ----------
# predicted vs actual
dat <- g[, .(moyr, treatment, post, mon_ccf, yhat, cfyhat)]

# check residuals
### don't run this, too many points
# ggplot(e_201907, aes(yhat, resid)) + geom_point()

# make long for ggplot
dat <- pivot_longer(dat, cols = c("mon_ccf", "yhat", "cfyhat"), names_to = "pred", values_to = "ccf")
setDT(dat)

# plot load over moyr
# simplify dataset before plotting to avoid crash
dat <- dat[, .(avg_ccf = mean(ccf)), .(moyr, treatment, post, pred)]
ggplot(dat) + geom_line(aes(x = moyr, y = avg_ccf, color = factor(pred)))




