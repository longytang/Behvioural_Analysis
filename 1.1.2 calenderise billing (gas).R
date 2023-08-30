
# ORU PY2022 Behvaioural Billing analysis
## Prepare for calenderise
## calenderise data
## Lia Tang, Aug 2023

rm(list = ls())

# 0.1 Import Library ----------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)

# 0.2 Directory ---------------------------------------------------------------
Drive <- "F:/AEG"
raw <- paste0(Drive, "/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/02 Raw Data/04 Behavioral Billing Data")
data_out <- paste0(Drive, "/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/03 Programs/Electric/Res_Electric Behavioral/R_output")

# 1. Infile clean PY2022 Billing Data  ---------------------------------------------
map_df <- readRDS(file.path(data_out, "PY2022_ORU_HER_mapping_AEG.RDS"))
elec <- readRDS(file.path(data_out, "PY2022_ORU_HER_elec_CLEANED_AEG.RDS"))
gas <- readRDS(file.path(data_out, "PY2022_ORU_HER_gas_CLEANED_AEG.RDS"))

setDT(elec)
setDT(gas)
# 2. prepare for calenderise ---------------------------------------------------
# check date range
elec[, range(bill_date)] # 
gas[, range(bill_date)] # 
# earliest date is over a year before beginning of first wave, ok

# check control / treatment
gas[, uniqueN(opower_customer_id), .(treatment)]   
# treatment    V1
# 1:         1 86425
# 2:         0 28584

gas[, .N, .(is_estimate)]
108781/(nrow(gas)) * 100

# 3. calenderise (gas) --------------------------------------------------------------

# get unique identifier
IDs <- unique(gas$opower_customer_id)
ID_count <- length(unique(gas$opower_customer_id)) 
# set chunk size
chunk_size <- 10000

for (i in seq(0, length(IDs), chunk_size)) {
  # get current chunk of IDs
  chunk_ids <- IDs[i:(i + chunk_size - 1)]
  
  # get the rows for the current chunk of IDs
  gas_c <- gas[exclude == 0 & outlier == 0 &  # drop these bills
               opower_customer_id %in% chunk_ids, ]
  
  # calenderise in chunks
  # Start the timer
  start_time <- proc.time()
  
  # format to Date
  gas_c[, bill_date := as.Date(bill_date)]
  
  # create date from and to by the bill date
  gas_c[, dt_from := bill_date]
  gas_c[, dt_to := bill_date]
  
  # create the "end" of the billing period by subtracting one from the "start" of the next bill
  gas_c[, dt_to_2 := (lead(dt_to, n = 1)) - 1, .(opower_customer_id)] # 10,127,812
  
  # drop the last bills
  gas_c <- gas_c[!(is.na(dt_to_2)), ] # 9,920,562
  gas_c[, uniqueN(opower_customer_id)] # 207,231 customers
  gas_c[, range(bill_date)] # "2018-05-07" "2023-08-09"
  
  # drop duration <= 0
  
  ## get duration
  gas_c[duration <= 0] # all duration > 0
  ## calculate duration column again to validate
  gas_c[, duration_2 := as.numeric(dt_to_2 - dt_to)]
  ## this doesn't match with the duration given, use this one instead
  gas_c[duration_2 <= 0] # 798
  
  gas_c[is.na(duration_2)] # empty
  
  gas_c <- gas_c[duration_2 > 0] #
  gas_c[, uniqueN(opower_customer_id)] #  customers
  gas_c[, range(bill_date)] # 
  
  # get average usage
  gas_c[, avg_usage := usage_value/duration_2]
  
  # expand to daily values to prepare for roll up
  gas_c <- gas_c[rep(1:.N, duration_2)] # this is the most resource demanding line
  
  # prepare for roll up by fixing date from to the "start" of the billing period
  gas_c[, Count := 1:.N,by = .(dt_from, dt_to_2, opower_customer_id, treatment, g_wave)]
  gas_c[, dt_from := dt_from + (Count - 1)]
  
  # roll up to opower customer id level
  gas_c[, `:=` (month = month(dt_from), year = year(dt_from))]
  gas_c <- gas_c[, .(mon_ccf = mean(avg_usage, na.rm = T), ndays = .N),
                   by = .(opower_customer_id, treatment, month, year, g_wave)] 
  gas_c[, uniqueN(opower_customer_id)] 
  table(gas_c$ndays)
  
  # drop bills that are less than 23 days
  gas_c <- gas_c[!ndays < 23] #
  gas_c[, uniqueN(opower_customer_id)] # customers
  
  gas_c[, date := paste(year, month, "01", sep = "-")]
  gas_c[, date := as.Date(date, format = "%Y-%m-%d")]
  gas_c[, n_days_mon := days_in_month(date)]
  gas_c[, mon_ccf := mon_ccf * n_days_mon]
  
  gas_c <- gas_c[, .(mon_ccf = sum(mon_ccf)),
                   by = .(opower_customer_id, treatment, month, year, g_wave)] #
  gas_c[, uniqueN(opower_customer_id)] #  customers
  
  # Stop the timer
  end_time <- proc.time()
  
  # Print the process and time taken
  print(paste("Chunk", i, "took", end_time - start_time))
  
  if(i == 0){
    data_cal = gas_c
  }
  else {
    data_cal = rbind(data_cal, gas_c)
  }
  
}

# add post pre indicator according to wave
data_cal[, post := fcase(g_wave == "g_201907" & year > 2019, 1,
                         g_wave == "g_201907" & year == 2019 & month >= 7, 1,
                         g_wave == "g_202201" & year >= 2022, 1)]
                         #g_wave == "g_201907" & year == 2022 & month >= 1, 1)]
data_cal[, post := ifelse(is.na(post), 0, post)]

data_cal[, uniqueN(opower_customer_id)]
data_cal[, uniqueN(opower_customer_id), .(treatment)]

# add moyr variable for easier visualisation
data_cal[, moyr := as.Date(paste0(year, "-", month, "-01"))]

# 4. export calenderised data -----------
fwrite(data_cal, file.path(data_out, "Gas_calendarized_data.csv"))

# 5. checks ---------
# two waves.
gas[, unique(rct_start_date), .(treatment)] # "2019-07-07" "2022-01-16"

gas[, uniqueN(opower_customer_id), .(treatment)] # 115,009
data_cal[, uniqueN(opower_customer_id), .(treatment)] # 114,972, dropped 37 customers, 0.03%


# check load

dat <- data_cal[, .(avg_mon_ccf = mean(mon_ccf)), .(year, month, moyr, g_wave, treatment, post)]
ld <- ggplot(dat, aes(x = moyr, y = avg_mon_ccf, color = factor(treatment))) +
  geom_line() + facet_grid(.~g_wave)

