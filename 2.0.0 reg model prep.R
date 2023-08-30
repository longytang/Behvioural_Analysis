
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
library(janitor)
library(ggplot2)

# 0.2 Directory ---------------------------------------------------------------
Drive <- "F:/AEG"
raw <- paste0(Drive, "/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/02 Raw Data/04 Behavioral Billing Data")
data_out <- paste0(Drive, "/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/03 Programs/Electric/Res_Electric Behavioral/R_output")

# 1. Infile clean PY2022 Billing Data  ---------------------------------------------
map_df <- readRDS(file.path(data_out, "PY2022_ORU_HER_mapping_AEG.RDS"))
elec <- fread(file.path(data_out, "Elec_calendarized_data.csv"))
gas <- fread(file.path(data_out, "Gas_calendarized_data.csv"))
wea <- fread(file.path(data_out, "Poughkeepsie_wea.csv"))

# 2. prepare for regression ---------------------------------------------------
# create DiD indicator
elec[, treat_post := treatment * post]
gas[, treat_post := treatment * post]

## 2.1 weather data -------
# downloaded from NOAA
ggplot(wea) + geom_point((aes(x = mon, y = t_mean, colour = factor(year))))

# use daily version of 202101 wave
# e_202101 <- fread(file.path(data_out, "e_202101_daily.csv"))

# merge weather data into daily data
# e_202101 <- merge(e_202101, wea, by = "date")

# get unique customer list
cust_list <- elec[, unique(opower_customer_id)]
length(cust_list)

# create numbers for chunking
n_chunks = 100

group_ass <- cut(1:length(cust_list), breaks = n_chunks, labels = FALSE)
cust_list_split <- split(cust_list, group_ass)

dat <- data.table()
dat[, opower_customer_id := cust_list]
dat[, group_ass := group_ass]
elec[, group_ass := NULL]
elec <- merge(elec, dat, by = "opower_customer_id")

grp_num = c(44, 88)
# plot daily usage against t
# minimise usage by taking avg usage

wea[, `:=` (mon = month(date), year = year(date))]
wea[, moyr := as.Date(paste0(year, "-", mon, "-01"))]
wea_mon <- wea[, .(t_mean = mean(t_mean, na.rm = TRUE)), .(moyr)]

elec <- merge(wea_mon, elec, by = "moyr")
gas <- merge(wea_mon, gas, by = "moyr")


elec[t_mean > 100]



ggplot(elec[group_ass %in% grp_num]) + 
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  geom_point(aes(x = t_mean, y = mon_kwh, color = factor(year)),  alpha = 0.2) +
  geom_smooth(aes(x = t_mean, y = mon_kwh, color = factor(year)), 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 7))


ggplot(elec[group_ass %in% grp_num & year == 2021]) + 
  scale_x_continuous(breaks = seq(0, 100, 5)) +
  geom_point(aes(x = t_mean, y = mon_kwh, color = factor(year)),  alpha = 0.2) +
  geom_smooth(aes(x = t_mean, y = mon_kwh, color = factor(year)), 
              method = "gam", formula = y ~ s(x, bs = "cs", k = 7))




### 2.1.1 check high daily averages -------
e_202101[daily_kwh > 60]

# merge weather monthly to elec dataset
# done

# daily HDD and CDD
# a set of billing cycle (by customer)

### 2.1.2. create weather variables --------------


# HDD
temp <- seq(40, 65, 5)

for (t in temp){
  elec[, paste0("HDD", t) := fcase(t_mean >= t, 0,
                                  t_mean < t, t - t_mean)]
  gas[, paste0("HDD", t) := fcase(t_mean >= t, 0,
                                   t_mean < t, t - t_mean)]
}

head(elec)
head(gas)

# CDD
temp <- seq(60, 70, 5)

for (t in temp){
  elec[, paste0("CDD", t) := fcase(t_mean <= t, 0,
                                   t_mean > t, t_mean - t)]
  gas[, paste0("CDD", t) := fcase(t_mean <= t, 0,
                                   t_mean > t, t_mean - t)]
}

head(elec)
head(gas)


### 2.1.3. export calenderised dataset with weather --------
fwrite(elec, file.path(data_out, "Elec_calendarized_data_weather.csv"))
fwrite(gas, file.path(data_out, "Gas_calendarized_data_weather.csv"))

## 2.2 check control vs treatment group -------
### 2.2.1 baseline load visual -------
# boxplots to compare 


### 2.2.3 t-test -------
#### 2.2.3.1 elect ---------
# overall
t <- t.test(mon_kwh ~ treatment, data = elec, alternative = "two.sided", var.equal = FALSE)
t
# Welch Two Sample t-test
# 
# data:  mon_kwh by treatment
# t = 7.9966, df = 4286881, p-value = 1.28e-15
# alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
# 95 percent confidence interval:
#   2.644948 4.362468
# sample estimates:
#   mean in group 0 mean in group 1 
# 715.7555        712.2518 

# check diff by wave (more than 2 waves, needs to use ANOVA instead)
t <-  t.test(mon_kwh ~ e_wave, data = elec, alternative = "two.sided", var.equal = FALSE)
t

t <-  t.test(mon_ccf ~ g_wave, data = gas, alternative = "two.sided", var.equal = FALSE)
t

# check date range
elec[, range(bill_date)] # "2018-05-07" "2023-08-10"
gas[, range(bill_date)] # "2018-05-07" "2023-08-10"
# earliest date is over a year before beginning of first wave, ok

# check if there are control / treatment
map_df[, unique(rct_start_date)] # doesn't seem like it

gas[, .N, .(is_estimate)]
108781/(nrow(gas)) * 100


# 3. expand to daily values before modelling -------
# do it by wave
curr_wave = "e_201907"

e <- elec[e_wave == curr_wave]

## 3.1 elec expand ------
# get unique customer list
cust_list <- e[, unique(opower_customer_id)]
length(cust_list)

# create an empty dataset to store expanded daily data
e_daily <- data.table()

# create numbers for chunking
n_chunks = 1000

group_ass <- cut(1:length(cust_list), breaks = n_chunks, labels = FALSE)
cust_list_split <- split(cust_list, group_ass)

total.time = 0
# expand to daily
for (i in 1:n_chunks) {
  
  start.time <- Sys.time()
  
  # subset customer
  cust_dt <- e[opower_customer_id %in% unlist(cust_list_split[i])]
  
  # expand to daily
  cust_dt_daily <- cust_dt[,
                           .(date = seq(floor_date(moyr, "month"),
                                        ceiling_date(moyr, "month") - days(1), 
                                        by = "day")), 
                           .(moyr, opower_customer_id, mon_kwh, treatment, post, treat_post)]
  
  # append to the wave data set
  e_daily <- rbind(e_daily, cust_dt_daily)
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  total.time <- total.time + time.taken
  cat("time taken: ", round(total.time, 3), " secs \n", 
      "progres: ", round(i/n_chunks*100, 2), "% \n")
  
}


e_daily[, daily_kwh := mon_kwh/days_in_month(date)]
e_daily[, mon_kwh := NULL]


# 4. export daily data -------
## 4.1 elec -------
fwrite(e_daily, file.path(data_out, paste0(curr_wave, "_daily.csv")))
