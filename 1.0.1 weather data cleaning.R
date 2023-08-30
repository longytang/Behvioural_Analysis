
# ORU PY2022 Behvaioural Billing analysis
## Data cleaning and Validation
## Lia Tang, Aug 2023

rm(list = ls())

# 0.1 Import Library ----------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)

# 0.2 Directory ---------------------------------------------------------------
Drive <- "F:"
raw <- paste0(Drive, "/aeg/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/02 Raw Data/04 Behavioral Billing Data")
data_out <- paste0(Drive, "/aeg/Clients/ORU/PY2022_ORU_Portfolio_Evaluation/02 Raw Data/04 Behavioral Billing Data/Processed Data")

# 1. Infile 2019-2023 NOAA weather data ---------------------------------------------
setwd(file.path(getwd(), "/NOAA"))
all.files <- list.files(pattern = "NOAA*")
wea <- lapply(all.files, fread)
wea <- rbindlist(wea, fill = TRUE)

# 2. clean weather files ----------------------------------------------------
keepcols <- c("DATE", "HourlyDryBulbTemperature")
wea <- wea[, .SD, .SDcols = keepcols] 
setnames(wea, "HourlyDryBulbTemperature", "t")
str(wea) # DATE alrady in POSIXct

# get date from the datetime col
wea[, Dt := date(DATE)]

# check why temperature is character
wea[, unique(t)]

# check blanks
wea[t == "", unique(Dt)]
# drop these
wea <- wea[!(t == "")]

# check with "s"
wea[grepl("s", t)] # 12
# spot check some of these dates
wea[Dt == as.Date("2023-06-07")]
wea[Dt == as.Date("2020-02-24")]
# the "s" looks like a typo, remove from string

# keep numbers only
wea[, t := as.integer(gsub("[^0-9]", "", t))]

# keep range included in the analysis
wea <- wea[Dt %between% elec[, range(moyr)]] 

# 3. prep data validation -----------------------------------------------------------

# get t_mean, t_min, t_max per day
wea <- wea[, .(t_mean = mean(t),
               t_min = min(t),
               t_max = max(t)), 
           .(Dt)]

wea[, `:=` (mon = month(date), year = year(date))]
wea[, moyr := as.Date(paste0(year, "-", mon, "-01"))]
wea_mon <- wea[, .(t_mean = mean(t_mean, na.rm = TRUE)), .(moyr)]
# 4. save processed weather data --------
setnames(wea, "Dt", "date")

fwrite(wea, file.path(data_out, "Poughkeepsie_wea.csv"))
fwrite(wea_mon, file.path(data_out, "Poughkeepsie_wea_Monthly.csv"))
