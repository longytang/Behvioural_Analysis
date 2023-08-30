
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

# 1. Infile PY2022 Billing Data  ---------------------------------------------
map_old <- fread(file.path(raw, "opwr_oru_mv_mapping_file_20230815.tsv"))

elec_201906 <- fread(file.path(raw, "oru_her_201906_de_trial_elec_raw.tsv.gz"))
elec_202201 <- fread(file.path(raw, "oru_her_202201_de_trial_elec_raw.tsv.gz"))
elec_202101 <- fread(file.path(raw, "oru_her_202101_e_trial_raw.tsv.gz"))

gas_201906  <- fread(file.path(raw, "oru_her_201906_de_trial_gas_raw.tsv.gz"))
gas_202201  <- fread(file.path(raw, "oru_her_202201_de_trial_gas_raw.tsv.gz"))

# 2. inspect mapping file ----------------------------------------------------
map_df[duplicated(map_df)] # no full dups
map_df[, uniqueN(opower_customer_id)] # 262,859
map_df[, uniqueN(utility_customer_id)] # 262,859
map_df[, unique(customer_type)] # "RESIDENTIAL" "SMB"

map_df[customer_type == "SMB"] # 15

# 3.data validation -----------------------------------------------------------
# check number of elec customers
elec_201906[, uniqueN(opower_customer_id)] # 162,045
elec_202201[, uniqueN(opower_customer_id)] # 24,231
elec_202101[, uniqueN(opower_customer_id)] # 20,974

# check for elec customers overlap
intersect(elec_201906[, unique(opower_customer_id)], elec_202201[, uniqueN(opower_customer_id)]) # 0, ok
intersect(elec_202101[, unique(opower_customer_id)], elec_202201[, uniqueN(opower_customer_id)]) # 0, ok
intersect(elec_202101[, unique(opower_customer_id)], elec_201906[, uniqueN(opower_customer_id)]) # 0, ok

# check number of gas customers
gas_201906[, uniqueN(opower_customer_id)] # 99,227
gas_202201[, uniqueN(opower_customer_id)] # 15,782

# check for gas customers overlap
intersect(gas_201906[, unique(opower_customer_id)], gas_202201[, uniqueN(opower_customer_id)]) # 15,782? double check
gas_202201[opower_customer_id %in% gas_201906[, unique(opower_customer_id)], .N] # 0
gas_201906[opower_customer_id %in% gas_202201[, unique(opower_customer_id)], .N] # 0

## 3.1. cleaning up ------------

# append dfs and add wave indicators
elec <- rbind(elec_201906, elec_202101, elec_202201)
elec[, uniqueN(opower_customer_id)] # 207,250
elec[, uniqueN(opower_customer_id), .(treatment)]

elec[, unique(rct_start_date)] # "2019-07-07" "2021-01-03" "2022-01-16"
elec[, e_wave := fcase(rct_start_date == as.IDate("2019-07-07"), "e_201907",
                       rct_start_date == as.IDate("2021-01-03"), "e_202101",
                       rct_start_date == as.IDate("2022-01-16"), "e_202201")]

gas <- rbind(gas_201906, gas_202201)
gas[, uniqueN(opower_customer_id)] # 115,009
gas[, unique(rct_start_date)] # "2019-07-07" "2022-01-16"
gas[, g_wave := fcase(rct_start_date == as.IDate("2019-07-07"), "g_201907",
                      rct_start_date == as.IDate("2022-01-16"), "g_202201")]

# remove unused files
rm(elec_201906, elec_202101, elec_202201)
rm(gas_201906, gas_202201)

# get customer counts by wave
elec[, uniqueN(opower_customer_id), .(e_wave)]
gas[, uniqueN(opower_customer_id), .(g_wave)]

# check cols formats
str(elec)
str(gas)

# overlap between gas and elec
elec[opower_customer_id %in% gas[, unique(opower_customer_id)], 
     uniqueN(opower_customer_id)] # 115,008
gas[opower_customer_id %in% elec[, unique(opower_customer_id)], 
     uniqueN(opower_customer_id)] # 115,008
# there are 115,008 customers participate in both elec and gas
# 1 customer only in gas, not elec
# check the wave distribution of those customers
elec[opower_customer_id %in% gas[, unique(opower_customer_id)], 
     uniqueN(opower_customer_id),
     .(rct_start_date)] # 115,008
#     rct_start_date    V1
# 1:     2019-07-07 99227
# 2:     2022-01-16 15781

# check is_estimate
elec[, .N, .(is_estimate)] # approx 1.750%
# is_estimate       N
# 1:           0 9950554
# 2:           1  177258
gas[, .N, .(is_estimate)] # approx 1.851%
# is_estimate       N
# 1:           0 5766086
# 2:           1  108781

# 4. data cleansing steps ----------
## 4.1. raw data -------
nrow(elec) # 10127812
elec[, range(bill_date)] # "2018-05-07" "2023-08-10"
elec[, uniqueN(opower_customer_id)] # 207,250
elec[, uniqueN(opower_customer_id), .(treatment)]
# treatment     V1
# 1:         1 154693
# 2:         0  52557

nrow(gas) # 5874867
gas[, range(bill_date)] # "2018-05-07" "2023-08-10"
gas[, uniqueN(opower_customer_id)] # 115009
gas[, uniqueN(opower_customer_id), .(treatment)]
# treatment    V1
# 1:         0 28584
# 2:         1 86425

## 4.2. zeros and negative reads ------
elec[usage_value < 0] # 0

gas[usage_value < 0] # 0

## 4.3. billing duration (off-cycle reads) ----------
elec[duration < 25] # 59842
elec[, exclude := ifelse(duration < 25, 1, 0)] 
elec[exclude == 0] %>% nrow() # 10067970
elec[exclude == 0, range(bill_date)] # "2018-05-07" "2023-08-09"
elec[exclude == 0, uniqueN(opower_customer_id)] # 207244
elec[exclude == 0, uniqueN(opower_customer_id), .(treatment)]
# treatment     V1
# 1:         1 154687
# 2:         0  52557

gas[duration < 25] # 89127
gas[, exclude := ifelse(duration < 25, 1, 0)]
gas[exclude == 0] %>% nrow() # 5785740
gas[exclude == 0, range(bill_date)] # "2018-05-07" "2023-08-09"
gas[exclude == 0, uniqueN(opower_customer_id)] # 115000
gas[exclude == 0, uniqueN(opower_customer_id), .(treatment)]

## 4.4. outliers / extreme data -----------
elec_temp <- elec[, .(mean = mean(usage_value), sd = sd(usage_value)), .(opower_customer_id)]
elec <- left_join(elec, elec_temp)
# mark outliers +/- 3 s.d.
elec[, outlier := ifelse(usage_value < mean - 3*sd | usage_value > mean + 3*sd, 1, 0)]
elec[outlier == 1] # 64521
elec[exclude == 0 & outlier == 0] %>% nrow() # 10005629
elec[exclude == 0 & outlier == 0, range(bill_date)] # "2018-05-07" "2023-08-09"
elec[exclude == 0 & outlier == 0, uniqueN(opower_customer_id)] #  207230
elec[exclude == 0 & outlier == 0, uniqueN(opower_customer_id), .(treatment)]

gas_temp <- gas[, .(mean = mean(usage_value), sd = sd(usage_value)), .(opower_customer_id)]
gas <- left_join(gas, gas_temp)
# mark outliers +/- 3 s.d.
gas[, outlier := ifelse(usage_value < mean - 3*sd | usage_value > mean + 3*sd, 1, 0)]
gas[outlier == 1] # 7287
gas[exclude == 0 & outlier == 0] %>% nrow() # 5778730
gas[exclude == 0 & outlier == 0, range(bill_date)] # "2018-05-07" "2023-08-09"
gas[exclude == 0 & outlier == 0, uniqueN(opower_customer_id)] # 
gas[exclude == 0 & outlier == 0, uniqueN(opower_customer_id), .(treatment)]
4.
## 5. check is_estimate bills ---------
elec[exclude == 0 & outlier == 0 & is_estimate == 1] # 173346

gas[exclude == 0 & outlier == 0 & is_estimate == 1] # 105578

# 5. append rxt start date onto mapping data -------------
map_elec <- elec[, .(e_no_of_bills = .N), .(opower_customer_id, rct_start_date, e_wave, treatment)] 
                                # 207,250 rows, same as total no. of elec customers
map_gas <- gas[, .(g_no_of_bills = .N), .(opower_customer_id, rct_start_date, g_wave, treatment)] 
                                # 115,009 rows, same as total no. of gas customers
map_new <- full_join(map_elec, map_gas) # 207,251, fewer customers than in mapping file

map_new <- left_join(map_df, map_new[, .(opower_customer_id, treatment)], by = "opower_customer_id")
map_new[, uniqueN(opower_customer_id)]

# check if the SMB accounts are participants
map_new[customer_type == "SMB"]

# total number of participants
map_new[!(is.na(rct_start_date))]

# 6. save processed data --------
setkey(elec, "opower_customer_id")
setkey(gas, "opower_customer_id")
setkey(map_new, "opower_customer_id")
saveRDS(elec, file.path(data_out, "PY2022_ORU_HER_elec_CLEANED_AEG.RDS"))
saveRDS(gas , file.path(data_out, "PY2022_ORU_HER_gas_CLEANED_AEG.RDS"))
saveRDS(map_new, file.path(data_out, "PY2022_ORU_HER_mapping_AEG.RDS"))

