
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
elec[, range(bill_date)] # "2018-05-07" "2023-08-10"
gas[, range(bill_date)] # "2018-05-07" "2023-08-10"
# earliest date is over a year before beginning of first wave, ok

elec[, unique(rct_start_date)]
# check control / treatment
elec[, uniqueN(opower_customer_id), .(treatment)]
# treatment     V1
# 1:         1 154693
# 2:         0  52557


# 3. calenderise (elec) --------------------------------------------------------------

# get unique identifier
IDs <- unique(elec$opower_customer_id)
ID_count <- length(unique(elec$opower_customer_id)) 
# set chunk size
chunk_size <- 10000

for (i in seq(0, length(IDs), chunk_size)) {
  # get current chunk of IDs
  chunk_ids <- IDs[i:(i + chunk_size - 1)]
  
  # get the rows for the current chunk of IDs
  elec_c <- elec[exclude == 0 & outlier == 0 &  # drop these bills
                 opower_customer_id %in% chunk_ids, ]
  
  # calenderise in chunks
  # Start the timer
  start_time <- proc.time()
  
  # format to Date
  elec_c[, bill_date := as.Date(bill_date)]
  
  # create date from and to by the bill date
  elec_c[, dt_from := bill_date]
  elec_c[, dt_to := bill_date]
  
  # create the "end" of the billing period by subtracting one from the "start" of the next bill
  elec_c[, dt_to_2 := (lead(dt_to, n = 1)) - 1, .(opower_customer_id, treatment)] # 10,127,812
  
  # drop the last bills
  elec_c <- elec_c[!(is.na(dt_to_2)), ] # 9,920,562
  elec_c[, uniqueN(opower_customer_id)] # 207,231 customers
  elec_c[, range(bill_date)] # "2018-05-07" "2023-08-09"
  
  # drop duration <= 0
  
  ## get duration
  elec_c[duration <= 0] # all duration > 0
  ## calculate duration column again to validate
  elec_c[, duration_2 := as.numeric(dt_to_2 - dt_to)]
  ## this doesn't match with the duration given, use this one instead
  elec_c[duration_2 <= 0] # 798
  
  elec_c[is.na(duration_2)] # empty
  
  elec_c <- elec_c[duration_2 > 0] # 9,919,764
  elec_c[, uniqueN(opower_customer_id)] # 207,231 customers
  elec_c[, range(bill_date)] # "2018-05-07" "2023-08-07"
  
  # get average usage
  elec_c[, avg_usage := usage_value/duration_2]
  
  # expand to daily values to prepare for roll up
  elec_c <- elec_c[rep(1:.N, duration_2)] # cannot brute force this, dataset too big
  
  # prepare for roll up by fixing date from to the "start" of the billing period
  elec_c[, Count := 1:.N,by = .(dt_from, dt_to_2, opower_customer_id)]
  elec_c[, dt_from := dt_from + (Count - 1)]
  
  # roll up to opower customer id level
  elec_c[, `:=` (month = month(dt_from), year = year(dt_from))]
  elec_c <- elec_c[, .(mon_kwh = mean(avg_usage, na.rm = T), ndays = .N),
                   by = .(opower_customer_id, treatment, month, year, e_wave)] 
  elec_c[, uniqueN(opower_customer_id)] 
  table(elec_c$ndays)
  
  # drop bills that are less than 23 days
  elec_c <- elec_c[!ndays < 23] #
  elec_c[, uniqueN(opower_customer_id)] # customers
  
  elec_c[, date := paste(year, month, "01", sep = "-")]
  elec_c[, date := as.Date(date, format = "%Y-%m-%d")]
  elec_c[, n_days_mon := days_in_month(date)]
  elec_c[, mon_kwh := mon_kwh * n_days_mon]
  
  elec_c <- elec_c[, .(mon_kwh = sum(mon_kwh)),
                   by = .(opower_customer_id, treatment, month, year, e_wave)] #
  elec_c[, uniqueN(elec_c)] #  customers
  
  # Stop the timer
  end_time <- proc.time()
  
  # Print the process and time taken
  print(paste("Chunk", i, "took", end_time - start_time))
  
  if(i == 0){
    data_cal = elec_c
  }
  else {
    data_cal = rbind(data_cal, elec_c)
  }
  
}

# add post pre indicator according to wave
data_cal[, post := fcase(e_wave == "e_201907" & year > 2019, 1,
                         e_wave == "e_201907" & year == 2019 & month >= 7, 1,
                         e_wave == "e_202101" & year >= 2021, 1,
                         e_wave == "e_202201" & year >= 2022, 1)]

data_cal[, post := ifelse(is.na(post), 0, post)]
data_cal[, uniqueN(opower_customer_id)]
data_cal[, uniqueN(opower_customer_id), .(treatment)]

# 4. checks  -------
# add moyr variable for easier visualisation
data_cal[, moyr := as.Date(paste0(year, "-", month, "-01"))]

plt2 <- data_cal[, .(avg_usage = mean(mon_kwh)), .(e_wave, moyr, treatment, post)]

# basic inspection
plt2[e_wave == "e_201907", .(mean(avg_usage)), .(treatment, post)] # see excel 1.9 cleaning log

ld <- ggplot(plt2, aes(x = moyr, y = avg_usage, color = factor(treatment))) +
  geom_line() + facet_grid(.~e_wave)

# 5. export calenderised data --------
fwrite(data_cal, file.path(data_out, "Elec_calendarized_data.csv"))
