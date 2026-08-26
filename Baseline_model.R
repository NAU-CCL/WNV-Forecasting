library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(checkmate)
library(cli)
library(pak)
pak::pkg_install("cmu-delphi/epiprocess@main")
library(epiprocess)
pak::pkg_install("cmu-delphi/epipredict@main") #install this and it will install epiprocess
library(epipredict)
library(ggplot2)
library(lubridate)
library(distributional)
library(tidyverse)
library(gridExtra)
library(grid)
#Year <- 2017
#Year <- 2021
years_to_run <- c(2006:2019, 2021)
#years_to_run <- c(2006:2019)
#years_to_run <- c(2021)
for (Year in years_to_run) {
  
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("RUNNING YEAR:", Year, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n\n")
# ==================================================================== 
# YEAR-SPECIFIC DATA LOADING
# ==================================================================== 
if (Year == 2021) {
  cat("Loading 2021 data...\n")
  
  # Climate data for 2021
  T_temp <- read.csv("./county_temp_2020-2024.csv")
  column_name <- "rowMeans.combined_temp..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  inputTem_i <- inputTem_i[367:731]  # 2021 subset (365 days)
  
  P_temp <- read.csv("./county_prcp_2021-2024.csv")
  column_name <- "rowMeans.combined_p..na.rm...TRUE."
  inputP_i <- as.numeric(P_temp[[column_name]])
  inputP_i <- inputP_i[1:365]
  
  # Observation data for 2021
  mosq_pools_agg <- read.csv("./mosq_pools_agg_2021.csv")
  X_obs <- as.numeric(mosq_pools_agg$Tot_Mosq_Abund)
  
  mosq_pools_data <- read.csv("./mosq_pools_data_2021.csv")
  X2_obs <- as.numeric(mosq_pools_data$Inf_Mosq_Per_1000)
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2021)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2021
  training_start_date <- as.Date("2021-01-01")
  fitting_start_date <- as.Date("2021-02-01")
  date_gam <- seq(as.Date("2021-01-01"), as.Date("2021-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
} else if (Year == 2020) {
  cat("Loading 2020 data...\n")
  
  T_temp <- read.csv("./county_temp_2020-2024.csv")
  column_name <- "rowMeans.combined_temp..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  inputTem_i <- inputTem_i[1:366]  # 2020 subset (365 days)
  P_temp <-read.csv("./county_prcp_2020.csv")
  column_name <- "rowMeans.combined_p..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  mosq_pools_agg <- read.csv("./mosq_pools_agg_2020.csv")
  X_obs <- as.numeric(mosq_pools_agg$Tot_Mosq_Abund)
  
  mosq_pools_data <- read.csv("./mosq_pools_data_2020.csv")
  X2_obs <- as.numeric(mosq_pools_data$Inf_Mosq_Per_1000)
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2020)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2020
  training_start_date <- as.Date("2020-01-01")
  fitting_start_date <- as.Date("2020-02-01")
  date_gam <- seq(as.Date("2020-01-01"), as.Date("2020-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
  
}else if (Year == 2019) {
  cat("Loading 2019 data...\n")
  
  T_temp <-read.csv("./county_temp_2018-2019.csv")
  column_name <- "rowMeans.combined_temp..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  inputTem_i <- inputTem_i[366:730]
  P_temp <-read.csv("./county_prcp_2018-2019.csv")
  column_name <- "rowMeans.combined_p..na.rm...TRUE."
  inputP_i <- as.numeric(P_temp[[column_name]])
  inputP_i <- inputP_i[366:730]
  
  
  mosq_pools_agg <- read.csv("./mosq_pools_data_2019.csv")
  X_obs <- as.numeric(c(mosq_pools_agg$Tot_Mosq_Abund)) 
  X_obs <- as.numeric(c(0, X_obs, 0))
  mosq_pools_data <- read.csv("./mosq_pools_data_2019.csv")
  X2_obs <- as.numeric(c(mosq_pools_data$Inf_Mosq_Per_1000))  
  X2_obs <- as.numeric(c(0, X2_obs, 0))
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2018-2019.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2019)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X3_obs[which(X3_obs == 1)[1]] <- 0
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2019
  training_start_date <- as.Date("2019-01-01")
  fitting_start_date <- as.Date("2019-02-01")
  date_gam <- seq(as.Date("2019-01-01"), as.Date("2019-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2018) {
  cat("Loading 2018 data...\n")
  
  T_temp <-read.csv("./county_temp_2018-2019.csv")
  column_name <- "rowMeans.combined_temp..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  inputTem_i <- inputTem_i[1:365]
  
  P_temp <-read.csv("./county_prcp_2018-2019.csv")
  column_name <- "rowMeans.combined_p..na.rm...TRUE."
  inputP_i <- as.numeric(P_temp[[column_name]])
  inputP_i <- inputP_i[1:365]
  
  mosq_pools_agg <- read.csv("./mosq_pools_data_2018-2019.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2018))
  X_obs <- as.numeric(c(X_obs$Tot_Mosq_Abund)) 
  mosq_pools_agg0 <- read.csv("./temp_csv_2018.csv")
  X_obs0 <- as.numeric(c(mosq_pools_agg0$Tot_Mosq_Abund)) 
  X_obs <- as.numeric(c(0, X_obs0 , X_obs, 0))
  mosq_pools_data <- read.csv("./mosq_pools_data_2018-2019.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2018))
  X2_obs <- as.numeric(c(X2_obs$Inf_Mosq_Per_1000))  
  X2_obs <- as.numeric(c(rep(0, 22), X2_obs, 0))
  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2018-2019.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2018)
  X3_obs <- X3_obs[1:52,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2018
  training_start_date <- as.Date("2018-01-01")
  fitting_start_date <- as.Date("2018-02-01")
  date_gam <- seq(as.Date("2018-01-01"), as.Date("2018-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2017) {
  cat("Loading 2017 data...\n")
  
  T_temp <-read.csv("./County_TEMP_2017.csv")
  T_temp <- as.data.frame(T_temp)
  inputTem_i <- as.numeric(T_temp$row_means[1:365])
  P_temp <-read.csv("./County_PRCP_2017.csv")
  P_temp <- as.data.frame(P_temp)
  inputP_i <- as.numeric(P_temp$row_means[1:365])
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2017))
  X_obs <- as.numeric(c(X_obs$Tot_Mosq_Abund, 0,0)) 
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2017))
  X2_obs <- as.numeric(c(X2_obs$Inf_Mosq_Per_1000,0,0))  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2017)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2017
  training_start_date <- as.Date("2017-01-01")
  fitting_start_date <- as.Date("2017-02-01")
  date_gam <- seq(as.Date("2017-01-01"), as.Date("2017-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2016) {
  cat("Loading 2016 data...\n")
  
  # Climate data for 2016 (leap year: 366 days)
  T_temp <- read.csv("./county_temp.csv")
  column_name <- "rowMeans.combined_temp..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  inputTem_i <- inputTem_i[731:1096]  # Days 731-1096 (366 days for leap year)
  
  P_temp <- read.csv("./county_prcp.csv")
  column_name <- "rowMeans.combined_p..na.rm...TRUE."
  inputP_i <- as.numeric(P_temp[[column_name]])
  inputP_i <- inputP_i[731:1096]  # Days 731-1096 (366 days for leap year)
  
  # Observation data for 2016 (52 weeks - NO padding)
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2016))
  X_obs <- as.numeric(X_obs$Tot_Mosq_Abund)  # No padding for 2016
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2016))
  X2_obs <- as.numeric(X2_obs$Inf_Mosq_Per_1000)  # No padding for 2016
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2016)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2016
  training_start_date <- as.Date("2016-01-01")
  fitting_start_date <- as.Date("2016-02-01")
  date_gam <- seq(as.Date("2016-01-01"), as.Date("2016-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2015) {
  cat("Loading 2015 data...\n")
  
  # Climate data for 2015
  T_temp <- read.csv("./county_temp.csv")
  column_name <- "rowMeans.combined_temp..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  inputTem_i <- inputTem_i[366:730]  # Days 366-730
  
  P_temp <- read.csv("./county_prcp.csv")
  column_name <- "rowMeans.combined_p..na.rm...TRUE."
  inputP_i <- as.numeric(P_temp[[column_name]])
  inputP_i <- inputP_i[366:730]  # Days 366-730
  
  # Observation data for 2015
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2015))
  X_obs <- as.numeric(c(X_obs$Tot_Mosq_Abund, 0))  # Pad with 0 (51 weeks)
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2015))
  X2_obs <- as.numeric(c(X2_obs$Inf_Mosq_Per_1000, 0))  # Pad with 0 (51 weeks)
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2015)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2015
  training_start_date <- as.Date("2015-01-01")
  fitting_start_date <- as.Date("2015-02-01")
  date_gam <- seq(as.Date("2015-01-01"), as.Date("2015-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2014) {
  cat("Loading 2014 data...\n")
  
  # Climate data for 2014
  T_temp <- read.csv("./county_temp.csv")
  column_name <- "rowMeans.combined_temp..na.rm...TRUE."
  inputTem_i <- as.numeric(T_temp[[column_name]])
  inputTem_i <- inputTem_i[1:365]  # Days 1-365
  
  P_temp <- read.csv("./county_prcp.csv")
  column_name <- "rowMeans.combined_p..na.rm...TRUE."
  inputP_i <- as.numeric(P_temp[[column_name]])
  inputP_i <- inputP_i[1:365]  # Days 1-365
  
  # Observation data for 2014
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2014)) 
  X_obs <- as.numeric(c(X_obs$Tot_Mosq_Abund, 0))  # Pad with 0 (51 weeks)
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2014))
  X2_obs <- as.numeric(c(X2_obs$Inf_Mosq_Per_1000, 0))  # Pad with 0 (51 weeks)
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2014)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2014
  training_start_date <- as.Date("2014-01-01")
  fitting_start_date <- as.Date("2014-02-01")
  date_gam <- seq(as.Date("2014-01-01"), as.Date("2014-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2013) {
  cat("Loading 2013 data...\n")
  
  T_temp <-read.csv("./County_TEMP_2013.csv")
  T_temp <- as.data.frame(T_temp)
  inputTem_i <- as.numeric(T_temp$row_means[1:365])
  P_temp <-read.csv("./County_PRCP_2013.csv")
  P_temp <- as.data.frame(P_temp)
  inputP_i <- as.numeric(P_temp$row_means[1:365])
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2013))
  X_obs <- as.numeric(c(X_obs$Tot_Mosq_Abund, 0)) 
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2013))
  X2_obs <- as.numeric(c(X2_obs$Inf_Mosq_Per_1000,0))  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2013)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2013
  training_start_date <- as.Date("2013-01-01")
  fitting_start_date <- as.Date("2013-02-01")
  date_gam <- seq(as.Date("2013-01-01"), as.Date("2013-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2012) {
  cat("Loading 2012 data...\n")
  
  # Climate data for 2012
  T_temp <- read.csv("./county_TEMP_2006-2012.csv")
  T_temp <- T_temp %>% filter(Year == 2012)
  inputTem_i <- as.numeric(T_temp$rowMeans.combined_temp..na.rm...TRUE.)
  
  
  P_temp <- read.csv("./county_PRCP_2006-2012.csv")
  P_temp <- P_temp %>% filter(Year == 2012)
  inputP_i <- as.numeric(P_temp$rowMeans.combined_p..na.rm...TRUE.)
  # Observation data for 2012
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2012)) 
  X_obs <- as.numeric(X_obs$Tot_Mosq_Abund)  # Pad with 0 (51 weeks)
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2012))
  X2_obs <- as.numeric(X2_obs$Inf_Mosq_Per_1000)  # Pad with 0 (51 weeks)
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2012)
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2012
  training_start_date <- as.Date("2012-01-01")
  fitting_start_date <- as.Date("2012-02-01")
  date_gam <- seq(as.Date("2012-01-01"), as.Date("2012-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
} else if (Year == 2011) {
  cat("Loading 2011 data...\n")
  
  # Climate data for 2011
  T_temp <- read.csv("./county_TEMP_2006-2012.csv")
  T_temp <- T_temp %>% filter(Year == 2011)
  inputTem_i <- as.numeric(T_temp$rowMeans.combined_temp..na.rm...TRUE.)
  
  
  P_temp <- read.csv("./county_PRCP_2006-2012.csv")
  P_temp <- P_temp %>% filter(Year == 2011)
  inputP_i <- as.numeric(P_temp$rowMeans.combined_p..na.rm...TRUE.)
  
  # Observation data for 2011
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2011))
  X_obs <- as.numeric(c(X_obs$Tot_Mosq_Abund, 0))  # Pad with 0 (51 weeks)
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2011))
  X2_obs <- as.numeric(c(X2_obs$Inf_Mosq_Per_1000, 0))  # Pad with 0 (51 weeks)
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2011)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2011
  training_start_date <- as.Date("2011-01-01")
  fitting_start_date <- as.Date("2011-02-01")
  date_gam <- seq(as.Date("2011-01-01"), as.Date("2011-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
} else if (Year == 2010) {
  cat("Loading 2010 data...\n")
  
  # Climate data for 2010
  T_temp <- read.csv("./county_TEMP_2006-2012.csv")
  T_temp <- T_temp %>% filter(Year == 2010)
  inputTem_i <- as.numeric(T_temp$rowMeans.combined_temp..na.rm...TRUE.)
  
  
  P_temp <- read.csv("./county_PRCP_2006-2012.csv")
  P_temp <- P_temp %>% filter(Year == 2010)
  inputP_i <- as.numeric(P_temp$rowMeans.combined_p..na.rm...TRUE.)
  
  # Observation data for 2010 (52 weeks - NO padding)
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2010))
  X_obs <- as.numeric(X_obs$Tot_Mosq_Abund)  
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2010))
  X2_obs <- as.numeric(X2_obs$Inf_Mosq_Per_1000)  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2010)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2010
  training_start_date <- as.Date("2010-01-01")
  fitting_start_date <- as.Date("2010-02-01")
  date_gam <- seq(as.Date("2010-01-01"), as.Date("2010-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
} else if (Year == 2009) {
  cat("Loading 2009 data...\n")
  
  T_temp <- read.csv("./county_TEMP_2006-2012.csv")
  T_temp <- T_temp %>% filter(Year == 2009)
  inputTem_i <- as.numeric(T_temp$rowMeans.combined_temp..na.rm...TRUE.)
  
  
  P_temp <- read.csv("./county_PRCP_2006-2012.csv")
  P_temp <- P_temp %>% filter(Year == 2009)
  inputP_i <- as.numeric(P_temp$rowMeans.combined_p..na.rm...TRUE.)
  
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2009))
  X_obs <- as.numeric(X_obs$Tot_Mosq_Abund) 
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2009))
  X2_obs <- as.numeric(X2_obs$Inf_Mosq_Per_1000)  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2009)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2009
  training_start_date <- as.Date("2009-01-01")
  fitting_start_date <- as.Date("2009-02-01")
  date_gam <- seq(as.Date("2009-01-01"), as.Date("2009-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2008) {
  cat("Loading 2008 data...\n")
  
  T_temp <- read.csv("./county_TEMP_2006-2012.csv")
  T_temp <- T_temp %>% filter(Year == 2008)
  inputTem_i <- as.numeric(T_temp$rowMeans.combined_temp..na.rm...TRUE.)
  
  
  P_temp <- read.csv("./county_PRCP_2006-2012.csv")
  P_temp <- P_temp %>% filter(Year == 2008)
  inputP_i <- as.numeric(P_temp$rowMeans.combined_p..na.rm...TRUE.)
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2008))
  X_obs <- as.numeric(X_obs$Tot_Mosq_Abund) 
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2008))
  X2_obs <- as.numeric(X2_obs$Inf_Mosq_Per_1000)  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2008)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2008
  training_start_date <- as.Date("2008-01-01")
  fitting_start_date <- as.Date("2008-02-01")
  date_gam <- seq(as.Date("2008-01-01"), as.Date("2008-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2007) {
  cat("Loading 2007 data...\n")
  
  T_temp <- read.csv("./county_TEMP_2006-2012.csv")
  T_temp <- T_temp %>% filter(Year == 2007)
  inputTem_i <- as.numeric(T_temp$rowMeans.combined_temp..na.rm...TRUE.)
  
  
  P_temp <- read.csv("./county_PRCP_2006-2012.csv")
  P_temp <- P_temp %>% filter(Year == 2007)
  inputP_i <- as.numeric(P_temp$rowMeans.combined_p..na.rm...TRUE.)
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2007))
  X_obs <- as.numeric(c(X_obs$Tot_Mosq_Abund,0,0)) 
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2007))
  X2_obs <- as.numeric(c(X2_obs$Inf_Mosq_Per_1000,0,0))  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2007)
  X3_obs <- X3_obs[-1,]
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2007
  training_start_date <- as.Date("2007-01-01")
  fitting_start_date <- as.Date("2007-02-01")
  date_gam <- seq(as.Date("2007-01-01"), as.Date("2007-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else if (Year == 2006) {
  cat("Loading 2006 data...\n")
  T_temp <- read.csv("./county_TEMP_2006-2012.csv")
  T_temp <- T_temp %>% filter(Year == 2006)
  inputTem_i <- as.numeric(T_temp$rowMeans.combined_temp..na.rm...TRUE.)
  
  
  P_temp <- read.csv("./county_PRCP_2006-2012.csv")
  P_temp <- P_temp %>% filter(Year == 2006)
  inputP_i <- as.numeric(P_temp$rowMeans.combined_p..na.rm...TRUE.)
  mosq_pools_agg <- read.csv("./mosq_pools_data.csv")
  X_obs <- as.data.frame(mosq_pools_agg %>% filter(Year == 2006))
  X_obs <- as.numeric(X_obs$Tot_Mosq_Abund) 
  
  mosq_pools_data <- read.csv("./mosq_pools_data.csv")
  X2_obs <- as.data.frame(mosq_pools_data %>% filter(Year == 2006))
  X2_obs <- as.numeric(X2_obs$Inf_Mosq_Per_1000)  
  
  WNV_humans_summary3 <- read.csv("./WNV_humans_summary3_2006-2017.csv")
  X3_obs <- WNV_humans_summary3 %>% filter(YEAR == 2006)
  X3_obs <- as.numeric(X3_obs$cases)
  X4_obs <- cumsum(X3_obs)
  
  # Universal dates for 2006
  training_start_date <- as.Date("2006-01-01")
  fitting_start_date <- as.Date("2006-02-01")
  date_gam <- seq(as.Date("2006-01-01"), as.Date("2006-12-28"), by = "week")
  all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else {
  stop("Year must be one of: 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 or 2021")
}

# Common observation setup
X0_obs <- X3_obs #use this for baseline forecast
X_obs1 <- X_obs
X_obs2 <- X2_obs

# Forecast date setup (derived from all_weeks)
observed_dates <- all_weeks
forecast_1week_dates <- all_weeks[6:51]
forecast_2week_dates <- all_weeks[7:52]

#X0_obs <- X4_obs #use this for cummulative

##############################
## Configuration parameters ##
##############################

# Output directory for forecasts
output_dirpath <- "wnv-forecasts/"

quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                     0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                     0.95, 0.975, 0.99)

# Minimum training window size (weeks)
min_train_weeks <- 5

# Forecast horizons (weeks ahead)
forecast_horizons <- c(1, 2)

# Number of trajectory samples
n_output_trajectories <- 100L

####################################
## Convert vector data to epi_df ##
####################################

prepare_vector_to_edf <- function(value_vector, date_vector, location = "AZ") {
  
  # Check that vectors are same length
  if (length(value_vector) != length(date_vector)) {
    stop("value_vector and date_vector must have the same length")
  }
  
  # Create tibble
  tibble(
    geo_value = location,
    time_value = date_vector,
    weekly_count = value_vector
  ) %>%
    as_epi_df()
}

prepare_wnv_edf <- function(data, value_col = "value") {
  data %>%
    transmute(
      geo_value = as.character(geo_value),
      time_value = as.Date(time_value),
      weekly_count = .data[[value_col]]
    ) %>%
    as_epi_df()
}

######################
## Helper functions ##
######################

curr_else_next_date_with_ltwday <- function(date, ltwday) {
  assert_class(date, "Date")
  assert_integerish(ltwday, lower = 0L, upper = 7L)
  date + (ltwday - as.POSIXlt(date)$wday) %% 7L
}

#######################################################
## Rolling forecast function for a single target    ##
#######################################################

run_rolling_forecast <- function(target_edf, 
                                 target_name,
                                 min_train_weeks = 5,
                                 forecast_horizons = c(1, 2),
                                 quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                     0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                     0.95, 0.975, 0.99),
                                 nsims = 1e5) {
  
  # Get unique time values and sort
  all_weeks <- sort(unique(target_edf$time_value))
  n_weeks <- length(all_weeks)
  
  # Determine forecast origins (reference dates)
  forecast_origins <- all_weeks[(min_train_weeks):(n_weeks - max(forecast_horizons))]
  
  cli_alert_info("Running {length(forecast_origins)} rolling forecasts for {target_name}")
  cli_alert_info("Training on {min_train_weeks} weeks initially, forecasting {max(forecast_horizons)} weeks ahead")
  cli_alert_info("Using {length(quantile_levels)} quantile levels")
  
  all_forecasts <- list()
  all_samples <- list()
  
  for (i in seq_along(forecast_origins)) {
    reference_date <- forecast_origins[i]
    
    # Training data includes everything up to and including reference_date
    train_data <- target_edf %>%
      filter(time_value <= reference_date)
    
    # Check if we have enough training data
    n_train_weeks <- length(unique(train_data$time_value))
    if (n_train_weeks < min_train_weeks) {
      cli_alert_warning("Skipping {reference_date}: insufficient training data ({n_train_weeks} weeks)")
      next
    }
    
    # Set seed for reproducibility
    rng_seed <- as.integer((59460707 + as.numeric(reference_date)) %% 2e9)
    
    # Initialize global variable for capturing samples
    assign("subsamples_by_geo", list(), envir = .GlobalEnv)
    
    # Run forecast
    withr::with_rng_version("4.0.0", withr::with_seed(rng_seed, {
      
      # Capture trajectory samples
      trace(epipredict:::propagate_samples, exit = quote({
        n <- 1L
        e <- rlang::caller_env(n)
        while(! ".data" %in% names(e)) {
          n <- n + 1L
          e <- rlang::caller_env(n)
          if (identical(e, globalenv()) || identical(e, emptyenv())) {
            return()
          }
        }
        target_geo <- e$.data$geo_value
        sample_by_horizon <- res
        
        if (length(sample_by_horizon) > 0 && length(sample_by_horizon[[1L]]) >= n_output_trajectories) {
          selected_trajectory_inds <- sample.int(
            length(sample_by_horizon[[1L]]), 
            n_output_trajectories
          )
          subsample_by_horizon <- lapply(sample_by_horizon, `[`, selected_trajectory_inds)
          subsample_by_horizon <- lapply(subsample_by_horizon, pmax, 0L)
          subsample_ids_every_horizon <- paste0(target_geo, "_s", seq_len(n_output_trajectories))
          subsample <- tibble(
            geo_value = target_geo,
            horizon = seq_along(subsample_by_horizon),
            output_type_id = rep(list(subsample_ids_every_horizon), length(horizon)),
            value = subsample_by_horizon
          ) %>%
            unchop(c(output_type_id, value))
          
          current_samples <- get("subsamples_by_geo", envir = .GlobalEnv)
          assign("subsamples_by_geo", c(current_samples, list(subsample)), envir = .GlobalEnv)
        }
      }), print = FALSE)
      
      # Generate forecast with extended quantile levels
      fcst <- tryCatch({
        cdc_baseline_forecaster(
          train_data,
          "weekly_count",
          cdc_baseline_args_list(
            aheads = forecast_horizons,
            nsims = nsims,
            quantile_levels = quantile_levels  # Use extended quantile levels
          )
        )
      }, error = function(e) {
        cli_alert_warning("Forecast failed for {reference_date}: {e$message}")
        return(NULL)
      })
      
      untrace(epipredict:::propagate_samples)
      
      if (!is.null(fcst)) {
        # Extract predictions
        preds <- fcst$predictions %>%
          mutate(
            reference_date = .env$reference_date,
            forecast_date = .env$reference_date,
            ahead = as.integer(.data$target_date - .env$reference_date) %/% 7L
          )
        
        all_forecasts[[i]] <- preds
        
        # Store samples
        current_samples <- get("subsamples_by_geo", envir = .GlobalEnv)
        if (length(current_samples) > 0) {
          samples <- current_samples %>%
            bind_rows() %>%
            mutate(
              reference_date = .env$reference_date,
              target_date = .env$reference_date + 7L * horizon,
              value = round(value)
            )
          all_samples[[i]] <- samples
        }
      }
    }))
    
    # Clean up global variable
    if (exists("subsamples_by_geo", envir = .GlobalEnv)) {
      rm("subsamples_by_geo", envir = .GlobalEnv)
    }
    
    if (i %% 10 == 0) {
      cli_alert_success("Completed {i}/{length(forecast_origins)} forecasts (training on {n_train_weeks} weeks)")
    }
  }
  
  # Combine all forecasts
  combined_forecasts <- bind_rows(all_forecasts)
  combined_samples <- bind_rows(all_samples)
  
  # Summary statistics
  cli_alert_success("Complete! Generated {nrow(combined_forecasts)} forecast rows")
  if (nrow(combined_samples) > 0) {
    cli_alert_success("Captured {nrow(combined_samples)} trajectory samples")
  } else {
    cli_alert_warning("No trajectory samples captured")
  }
  
  list(
    forecasts = combined_forecasts,
    samples = combined_samples,
    target_name = target_name,
    quantile_levels = quantile_levels
  )
}

#####################################
## Verify forecast setup             ##
#####################################

verify_forecast_setup <- function(forecast_results, actual_data, n_examples = 3) {
  
  cat("\n=== FORECAST VERIFICATION ===\n\n")
  
  # Get unique reference dates
  ref_dates <- unique(forecast_results$forecasts$reference_date)
  ref_dates <- ref_dates[!is.na(ref_dates)]
  ref_dates <- sort(ref_dates)[1:min(n_examples, length(ref_dates))]
  
  for (ref_date in ref_dates) {
    cat(sprintf("\n--- Reference Date: %s ---\n", ref_date))
    
    # Get training data
    train_weeks <- actual_data %>%
      filter(time_value <= ref_date) %>%
      arrange(time_value) %>%
      pull(time_value)
    
    cat(sprintf("Training data: %d weeks\n", length(train_weeks)))
    cat(sprintf("  First week: %s\n", min(train_weeks)))
    cat(sprintf("  Last week:  %s (reference date)\n", max(train_weeks)))
    
    # Get forecasts
    forecasts <- forecast_results$forecasts %>%
      filter(reference_date == ref_date) %>%
      arrange(ahead)
    
    cat(sprintf("\nForecasts made:\n"))
    for (j in 1:nrow(forecasts)) {
      cat(sprintf("  Horizon %d: Target week %s (%.1f weeks after reference)\n",
                  forecasts$ahead[j],
                  forecasts$target_date[j],
                  as.numeric(forecasts$target_date[j] - ref_date) / 7))
    }
    
    cat("\n")
  }
  
  cat("\n=== SUMMARY ===\n")
  cat(sprintf("Total forecasts: %d rows\n", nrow(forecast_results$forecasts)))
  cat(sprintf("Unique reference dates: %d\n", 
              length(unique(forecast_results$forecasts$reference_date[!is.na(forecast_results$forecasts$reference_date)]))))
  cat(sprintf("Horizons: %s\n", paste(sort(unique(forecast_results$forecasts$ahead)), collapse = ", ")))
  cat(sprintf("Quantile levels: %d levels\n", length(forecast_results$quantile_levels)))
  cat(sprintf("Samples captured: %d rows\n\n", nrow(forecast_results$samples)))
}

#####################################
## Evaluate forecast performance   ##
#####################################

evaluate_forecasts <- function(forecast_results, actual_data, verbose = TRUE) {
  
  forecasts <- forecast_results$forecasts
  
  # Remove any rows with NA reference_date
  forecasts <- forecasts %>%
    filter(!is.na(reference_date))
  
  if (verbose) {
    cat("\n=== FORECAST EVALUATION ===\n")
    cat(sprintf("Total forecast rows: %d\n", nrow(forecasts)))
    cat(sprintf("Unique reference dates: %d\n", n_distinct(forecasts$reference_date)))
    cat(sprintf("Horizons: %s\n\n", paste(sort(unique(forecasts$ahead)), collapse = ", ")))
  }
  
  # Extract quantile forecasts (now with 23 quantiles)
  quantile_forecasts <- tryCatch({
    forecasts %>%
      pivot_quantiles_wider(.pred_distn) %>%
      select(reference_date, target_date, ahead, geo_value, .pred, 
             starts_with("0."))
  }, error = function(e) {
    # Manual extraction if pivot fails
    q_levels <- forecast_results$quantile_levels
    forecasts %>%
      rowwise() %>%
      mutate(
        quantiles = list(if(!is.null(.pred_distn)) 
          distributional::quantile(.pred_distn, q_levels) 
          else rep(NA, length(q_levels)))
      ) %>%
      ungroup() %>%
      bind_cols(
        map_dfc(seq_along(q_levels), function(i) {
          tibble(!!paste0(q_levels[i]) := map_dbl(.$quantiles, ~.[i]))
        })
      ) %>%
      select(reference_date, target_date, ahead, geo_value, .pred, 
             starts_with("0."))
  })
  
  # Join with actual data
  evaluation <- quantile_forecasts %>%
    left_join(
      actual_data %>% 
        select(time_value, geo_value, actual = weekly_count),
      by = c("target_date" = "time_value", "geo_value")
    )
  
  if (verbose) {
    n_missing <- sum(is.na(evaluation$actual))
    if (n_missing > 0) {
      cat(sprintf("Warning: %d forecasts do not have matching actual values\n", n_missing))
    }
    cat(sprintf("Evaluating %d forecasts with actual values\n\n", sum(!is.na(evaluation$actual))))
  }
  
  # Filter to only forecasts with actual values
  evaluation <- evaluation %>%
    filter(!is.na(actual))
  
  # Calculate metrics
  metrics <- evaluation %>%
    mutate(
      horizon = ahead,
      error = actual - .pred,
      abs_error = abs(error),
      squared_error = error^2,
      in_50_PI = actual >= `0.25` & actual <= `0.75`,
      in_80_PI = actual >= `0.1` & actual <= `0.9`,
      in_90_PI = actual >= `0.05` & actual <= `0.95`,
      in_95_PI = actual >= `0.025` & actual <= `0.975`,
      in_98_PI = actual >= `0.01` & actual <= `0.99`,
      median_error = actual - `0.5`,
      abs_median_error = abs(median_error)
    ) %>%
    group_by(geo_value, horizon) %>%
    summarize(
      n_forecasts = n(),
      MAE = mean(abs_error, na.rm = TRUE),
      RMSE = sqrt(mean(squared_error, na.rm = TRUE)),
      MdAE = median(abs_error, na.rm = TRUE),
      MAPE = mean(abs_error / (actual + 1e-6) * 100, na.rm = TRUE),
      coverage_50 = mean(in_50_PI, na.rm = TRUE),
      coverage_80 = mean(in_80_PI, na.rm = TRUE),
      coverage_90 = mean(in_90_PI, na.rm = TRUE),
      coverage_95 = mean(in_95_PI, na.rm = TRUE),
      coverage_98 = mean(in_98_PI, na.rm = TRUE),
      mean_actual = mean(actual, na.rm = TRUE),
      mean_forecast = mean(.pred, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(geo_value, horizon)
  
  if (verbose) {
    cat("=== METRICS SUMMARY ===\n")
    print(metrics)
    cat("\n")
  }
  
  return(metrics)
}

get_forecast_errors <- function(forecast_results, actual_data) {
  
  forecasts <- forecast_results$forecasts %>%
    filter(!is.na(reference_date))
  
  # Extract quantiles
  quantile_forecasts <- tryCatch({
    forecasts %>%
      pivot_quantiles_wider(.pred_distn)
  }, error = function(e) {
    q_levels <- forecast_results$quantile_levels
    forecasts %>%
      rowwise() %>%
      mutate(
        quantiles = list(if(!is.null(.pred_distn)) 
          distributional::quantile(.pred_distn, q_levels) 
          else rep(NA, length(q_levels)))
      ) %>%
      ungroup() %>%
      bind_cols(
        map_dfc(seq_along(q_levels), function(i) {
          tibble(!!paste0(q_levels[i]) := map_dbl(.$quantiles, ~.[i]))
        })
      ) %>%
      select(-quantiles)
  })
  
  # Join with actual data
  errors <- quantile_forecasts %>%
    left_join(
      actual_data %>% 
        select(time_value, geo_value, actual = weekly_count),
      by = c("target_date" = "time_value", "geo_value")
    ) %>%
    filter(!is.na(actual)) %>%
    mutate(
      horizon = ahead,
      error = actual - .pred,
      abs_error = abs(error),
      pct_error = (error / (actual + 1e-6)) * 100,
      in_50_PI = actual >= `0.25` & actual <= `0.75`,
      in_80_PI = actual >= `0.1` & actual <= `0.9`,
      in_95_PI = actual >= `0.025` & actual <= `0.975`
    )
  
  return(errors)
}

#####################################
## Visualization                   ##
#####################################

plot_forecast_example <- function(forecast_results, 
                                  actual_data,
                                  selected_geo = NULL,
                                  selected_reference_date = NULL) {
  
  # Select data to plot
  if (is.null(selected_geo)) {
    selected_geo <- first(unique(forecast_results$forecasts$geo_value))
  }
  
  if (is.null(selected_reference_date)) {
    selected_reference_date <- max(forecast_results$forecasts$reference_date, na.rm = TRUE)
  }
  
  # Get forecast data
  forecast_data_raw <- forecast_results$forecasts %>%
    filter(
      geo_value == selected_geo,
      reference_date == selected_reference_date
    )
  
  # Check if .pred_distn exists and extract quantiles
  if (".pred_distn" %in% names(forecast_data_raw)) {
    forecast_plot_data <- tryCatch({
      forecast_data_raw %>%
        pivot_quantiles_wider(.pred_distn)
    }, error = function(e) {
      # Manual extraction
      q_levels <- forecast_results$quantile_levels
      forecast_data_raw %>%
        rowwise() %>%
        mutate(
          quantiles = list(if(!is.null(.pred_distn)) 
            distributional::quantile(.pred_distn, q_levels) 
            else rep(NA, length(q_levels)))
        ) %>%
        ungroup() %>%
        bind_cols(
          map_dfc(seq_along(q_levels), function(i) {
            tibble(!!paste0(q_levels[i]) := map_dbl(.$quantiles, ~.[i]))
          })
        ) %>%
        select(-quantiles)
    })
  } else {
    forecast_plot_data <- forecast_data_raw
  }
  
  # Get samples if available
  if (!is.null(forecast_results$samples) && nrow(forecast_results$samples) > 0) {
    sample_plot_data <- forecast_results$samples %>%
      filter(
        geo_value == selected_geo,
        reference_date == selected_reference_date
      ) %>%
      slice_head(n = 10, by = c(geo_value, horizon))
  } else {
    sample_plot_data <- NULL
  }
  
  # Create base plot
  p <- ggplot(forecast_plot_data, aes(target_date))
  
  # Add uncertainty bands if quantiles exist (using extended quantiles)
  if ("0.1" %in% names(forecast_plot_data) && "0.9" %in% names(forecast_plot_data)) {
    p <- p + 
      geom_ribbon(aes(ymin = `0.05`, ymax = `0.95`), fill = "lightblue", alpha = 0.3) +
      geom_ribbon(aes(ymin = `0.1`, ymax = `0.9`), fill = "lightblue", alpha = 0.4) +
      geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), fill = "steelblue", alpha = 0.5)
  }
  
  # Add point forecast
  p <- p + geom_line(aes(y = .pred), color = "orange", linewidth = 1)
  
  # Add actual data
  p <- p + geom_line(
    data = actual_data %>% filter(geo_value == selected_geo),
    aes(x = time_value, y = weekly_count),
    color = "black",
    linewidth = 0.8
  )
  
  # Add sample trajectories if available
  if (!is.null(sample_plot_data) && nrow(sample_plot_data) > 0) {
    p <- p + geom_line(
      data = sample_plot_data,
      aes(y = value, group = output_type_id),
      alpha = 0.2,
      color = "gray40"
    )
  }
  
  # Add reference date line
  p <- p + geom_vline(xintercept = selected_reference_date, linetype = "dashed", color = "red")
  
  # Add labels and theme
  p <- p + 
    labs(
      title = paste0(forecast_results$target_name, " - ", selected_geo),
      subtitle = paste("Reference date:", selected_reference_date, "| 23 quantile levels"),
      x = "Date",
      y = "Count"
    ) +
    theme_bw() +
    theme(text = element_text(size = 12), axis.title = element_text(size = 17),  # cex.lab equivalent
          axis.text = element_text(size = 17),   # cex.axis equivalent
          legend.text = element_text(size = 15), # cex.names equivalent for legend
          plot.title = element_text(size = 17))# Use a clean theme)
  
  return(p)
}

plot_all_forecasts <- function(forecast_results, 
                               actual_data,
                               selected_geo = NULL,
                               n_forecasts = 5,
                               show_uncertainty = TRUE) {
  
  if (is.null(selected_geo)) {
    selected_geo <- first(unique(forecast_results$forecasts$geo_value))
  }
  
  # Get the most recent n reference dates
  recent_dates <- forecast_results$forecasts %>%
    filter(geo_value == selected_geo) %>%
    distinct(reference_date) %>%
    arrange(desc(reference_date)) %>%
    slice_head(n = n_forecasts) %>%
    pull(reference_date)
  
  # Get forecast data for these dates
  forecast_data_raw <- forecast_results$forecasts %>%
    filter(
      geo_value == selected_geo,
      reference_date %in% recent_dates
    )
  
  # Extract quantiles if needed
  if (show_uncertainty && ".pred_distn" %in% names(forecast_data_raw)) {
    forecast_plot_data <- tryCatch({
      forecast_data_raw %>%
        pivot_quantiles_wider(.pred_distn) %>%
        mutate(ref_date_label = format(reference_date, "%Y-%m-%d"))
    }, error = function(e) {
      # Manual extraction
      q_levels <- forecast_results$quantile_levels
      forecast_data_raw %>%
        rowwise() %>%
        mutate(
          quantiles = list(if(!is.null(.pred_distn)) 
            distributional::quantile(.pred_distn, q_levels) 
            else rep(NA, length(q_levels)))
        ) %>%
        ungroup() %>%
        bind_cols(
          map_dfc(seq_along(q_levels), function(i) {
            tibble(!!paste0(q_levels[i]) := map_dbl(.$quantiles, ~.[i]))
          })
        ) %>%
        mutate(ref_date_label = format(reference_date, "%Y-%m-%d")) %>%
        select(-quantiles)
    })
  } else {
    forecast_plot_data <- forecast_data_raw %>%
      mutate(ref_date_label = format(reference_date, "%Y-%m-%d"))
  }
  
  # Create plot
  p <- ggplot() +
    # Add actual data
    geom_line(
      data = actual_data %>% filter(geo_value == selected_geo),
      aes(x = time_value, y = weekly_count),
      color = "black",
      linewidth = 1.2,
      alpha = 0.8
    )
  
  # Add uncertainty bands if available (using extended quantiles)
  if (show_uncertainty && all(c("0.1", "0.9") %in% names(forecast_plot_data))) {
    p <- p +
      # 98% prediction interval
      geom_ribbon(
        data = forecast_plot_data,
        aes(x = target_date, ymin = `0.01`, ymax = `0.99`, 
            fill = ref_date_label, group = reference_date),
        alpha = 0.1
      ) +
      # 90% prediction interval
      geom_ribbon(
        data = forecast_plot_data,
        aes(x = target_date, ymin = `0.05`, ymax = `0.95`, 
            fill = ref_date_label, group = reference_date),
        alpha = 0.15
      ) +
      # 80% prediction interval
      geom_ribbon(
        data = forecast_plot_data,
        aes(x = target_date, ymin = `0.1`, ymax = `0.9`, 
            fill = ref_date_label, group = reference_date),
        alpha = 0.2
      ) +
      # 50% prediction interval
      geom_ribbon(
        data = forecast_plot_data,
        aes(x = target_date, ymin = `0.25`, ymax = `0.75`, 
            fill = ref_date_label, group = reference_date),
        alpha = 0.3
      )
  }
  
  # Add point forecasts and markers
  p <- p +
    geom_line(
      data = forecast_plot_data,
      aes(x = target_date, y = .pred, color = ref_date_label, group = reference_date),
      linewidth = 1
    ) +
    geom_point(
      data = forecast_plot_data,
      aes(x = target_date, y = .pred, color = ref_date_label),
      size = 2.5
    ) +
    labs(
      title = paste0(forecast_results$target_name, " - Rolling Forecasts - ", selected_geo),
      subtitle = if(show_uncertainty) "Shaded regions show 50%, 80%, 90%, and 98% prediction intervals" else NULL,
      x = "Date",
      y = "Count",
      color = "Reference Date",
      fill = "Reference Date"
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 12),
      legend.position = "bottom",
      axis.title = element_text(size = 17),  # cex.lab equivalent
          axis.text = element_text(size = 17),   # cex.axis equivalent
          legend.text = element_text(size = 15), # cex.names equivalent for legend
          plot.title = element_text(size = 17),# Use a clean theme
      legend.box = "vertical"
    )
  
  return(p)
}



#####################################
## Main execution                  ##
#####################################

# EXAMPLE 1: Single year data
# --------------------------------------------


# Convert to epi_df format
X1_edf <- prepare_vector_to_edf(X_obs1[1:52], date_gam, "AZ")

# Do the same for other datasets
X0_edf <- prepare_vector_to_edf(X0_obs[1:52], date_gam, "AZ")
X2_edf <- prepare_vector_to_edf(X_obs2[1:52], date_gam, "AZ")



# --------------------------------------------

# STEP 2: Run rolling forecasts for each target
# --------------------------------------------
results_abundance <- run_rolling_forecast(
  X1_edf,
  "Total Mosquito Abundance",
  min_train_weeks = 5,
  forecast_horizons = c(1, 2)
)

results_infected <- run_rolling_forecast(
  X2_edf,
  "Infected Mosquitoes per 1000",
  min_train_weeks = 5,
  forecast_horizons = c(1, 2)
)

results_cases <- run_rolling_forecast(
  X0_edf,
  "Human Cases",
  min_train_weeks = 5,
  forecast_horizons = c(1, 2)
)

# STEP 3: Evaluate forecasts
# --------------------------------------------
metrics_abundance <- evaluate_forecasts(results_abundance, X1_edf)
metrics_infected <- evaluate_forecasts(results_infected, X2_edf)
metrics_cases <- evaluate_forecasts(results_cases, X0_edf)

verify_forecast_setup(results_abundance, X1_edf, n_examples = 3)

errors_abundance <- get_forecast_errors(results_abundance, X1_edf)
#head(errors_abundance)
# STEP 4: Plot examples
# --------------------------------------------
plot_forecast_example(results_abundance, X1_edf)
plot_forecast_example(results_infected, X2_edf)
plot_forecast_example(results_cases, X0_edf)
plot_all_forecasts(results_infected, X2_edf, n_forecasts = 15)
plot_all_forecasts(results_cases, X0_edf, n_forecasts = 15)
plot_all_forecasts(results_abundance, X1_edf, n_forecasts = 15)
# Without uncertainty bands
plot_all_forecasts(results_abundance, X1_edf, n_forecasts = 5, show_uncertainty = FALSE)
# STEP 5: Save results
# --------------------------------------------
if (!dir.exists(output_dirpath)) {
  dir.create(output_dirpath, recursive = TRUE)
}

#change prediction of human cases to cummulative
results_cases$forecasts <- results_cases$forecasts %>%
  group_by(ahead) %>%
  arrange(ahead) %>%
  mutate(
    .pred = cumsum(.pred),
    # Rebuild the distribution object with cumulative values
    .pred_distn = {
      cum_vals <- cumsum(.pred)
      # Get the quantiles from original distribution
      orig_quantiles <- lapply(.pred_distn, function(d) {
        quantile(d, probs = quantile_levels)
      })
      # Calculate cumulative quantiles
      cum_quantiles <- list()
      for (i in seq_along(orig_quantiles)) {
        if (i == 1) {
          cum_quantiles[[i]] <- orig_quantiles[[i]]
        } else {
          cum_quantiles[[i]] <- cum_quantiles[[i-1]] + orig_quantiles[[i]]
        }
      }
      # Rebuild dist_quantiles objects
      lapply(cum_quantiles, function(q) {
        dist_quantiles(list(q), quantile_levels)
      })
    }
  ) %>%
  ungroup()
#####################################
## Convert Baseline to Custom Format ##
#####################################

convert_baseline_to_custom_format <- function(results_abundance, 
                                              results_infected, 
                                              results_cases,
                                              quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                                  0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                                  0.95, 0.975, 0.99)) {
  
  cat("Converting baseline results to custom format...\n")
  
  # Extract forecasts from each result
  abundance_forecasts <- results_abundance$forecasts %>%
    filter(!is.na(reference_date))
  
  infected_forecasts <- results_infected$forecasts %>%
    filter(!is.na(reference_date))
  
  cases_forecasts <- results_cases$forecasts %>%
    filter(!is.na(reference_date))
  
  # Get unique reference dates (should be the same across all targets)
  reference_dates <- unique(abundance_forecasts$reference_date)
  n_iterations <- length(reference_dates)
  
  cat(sprintf("Processing %d iterations...\n", n_iterations))
  
  # Initialize results list
  results <- list()
  
  # Process each iteration
  for (i in seq_along(reference_dates)) {
    ref_date <- reference_dates[i]
    
    # Extract quantiles for this reference date
    
    # Total Abundance
    abundance_data <- abundance_forecasts %>%
      filter(reference_date == ref_date)
    
    total_abundance_q_1 <- extract_quantiles_for_horizon(abundance_data, horizon = 1, quantile_levels)
    total_abundance_q_2 <- extract_quantiles_for_horizon(abundance_data, horizon = 2, quantile_levels)
    
    # Infected per 1000
    infected_data <- infected_forecasts %>%
      filter(reference_date == ref_date)
    
    infectious_per_1000_q_1 <- extract_quantiles_for_horizon(infected_data, horizon = 1, quantile_levels)
    infectious_per_1000_q_2 <- extract_quantiles_for_horizon(infected_data, horizon = 2, quantile_levels)
    
    # Human Cases
    cases_data <- cases_forecasts %>%
      filter(reference_date == ref_date)
    
    human_cases_q_1 <- extract_quantiles_for_horizon(cases_data, horizon = 1, quantile_levels)
    human_cases_q_2 <- extract_quantiles_for_horizon(cases_data, horizon = 2, quantile_levels)
    
    # Store in results list
    results[[i]] <- list(
      reference_date = ref_date,
      total_abundance_q_1 = total_abundance_q_1,
      total_abundance_q_2 = total_abundance_q_2,
      infectious_per_1000_q_1 = infectious_per_1000_q_1,
      infectious_per_1000_q_2 = infectious_per_1000_q_2,
      human_cases_q_1 = human_cases_q_1,
      human_cases_q_2 = human_cases_q_2
    )
    
    if (i %% 10 == 0) {
      cat(sprintf("  Processed %d/%d iterations\n", i, n_iterations))
    }
  }
  
  cat("Conversion complete!\n\n")
  
  return(results)
}

extract_quantiles_for_horizon <- function(forecast_data, horizon, quantile_levels) {
  
  # Filter to the specific horizon
  horizon_data <- forecast_data %>%
    filter(ahead == horizon)
  
  if (nrow(horizon_data) == 0) {
    # Return NA vector if no data
    quantiles <- rep(NA, length(quantile_levels))
    names(quantiles) <- paste0(quantile_levels * 100, "%")
    return(quantiles)
  }
  
  # Extract the distribution
  pred_distn <- horizon_data$.pred_distn[[1]]
  
  # Get quantiles
  if (!is.null(pred_distn)) {
    quantile_values <- quantile(pred_distn, quantile_levels)
    names(quantile_values) <- paste0(quantile_levels * 100, "%")
  } else {
    # If distribution is NULL, use point prediction for all quantiles
    point_pred <- horizon_data$.pred[1]
    quantile_values <- rep(point_pred, length(quantile_levels))
    names(quantile_values) <- paste0(quantile_levels * 100, "%")
  }
  
  return(quantile_values)
}

#####################################
## Verify Conversion               ##
#####################################

verify_custom_format <- function(results, n_examples = 3) {
  
  cat("\n=== CUSTOM FORMAT VERIFICATION ===\n\n")
  cat(sprintf("Total iterations: %d\n", length(results)))
  
  # Check first iteration structure
  cat("\nStructure of first iteration:\n")
  cat(sprintf("  Names: %s\n", paste(names(results[[1]]), collapse = ", ")))
  
  # Show examples
  for (i in 1:min(n_examples, length(results))) {
    cat(sprintf("\n--- Iteration %d (Reference Date: %s) ---\n", 
                i, results[[i]]$reference_date))
    
    # Show total_abundance_q_1
    cat("total_abundance_q_1 (first 5 quantiles):\n")
    print(head(results[[i]]$total_abundance_q_1, 5))
    
    # Show total_abundance_q_2
    cat("\ntotal_abundance_q_2 (first 5 quantiles):\n")
    print(head(results[[i]]$total_abundance_q_2, 5))
    
    # Check for NAs
    has_na <- any(is.na(results[[i]]$total_abundance_q_1)) || 
      any(is.na(results[[i]]$total_abundance_q_2))
    if (has_na) {
      cat("  WARNING: Contains NA values\n")
    }
  }
  
  cat("\n")
}

#####################################
## Save and Load Results           ##
#####################################

save_custom_results <- function(results, year, output_dir = "results") {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  filename <- file.path(output_dir, sprintf("results_baseline_%d.rds", year))
  saveRDS(results, file = filename)
  
  cat(sprintf("Results saved to: %s\n", filename))
}

load_custom_results <- function(year, output_dir = "results") {
  
  filename <- file.path(output_dir, sprintf("results_baseline_%d.rds", year))
  
  if (!file.exists(filename)) {
    stop(sprintf("File not found: %s", filename))
  }
  
  results <- readRDS(filename)
  cat(sprintf("Results loaded from: %s\n", filename))
  cat(sprintf("Number of iterations: %d\n", length(results)))
  
  return(results)
}

#####################################
## Comparison Helper               ##
#####################################

compare_results_structure <- function(baseline_results, other_results) {
  
  cat("\n=== STRUCTURE COMPARISON ===\n\n")
  
  cat("Baseline results:\n")
  cat(sprintf("  Number of iterations: %d\n", length(baseline_results)))
  cat(sprintf("  Fields per iteration: %s\n", paste(names(baseline_results[[1]]), collapse = ", ")))
  
  cat("\nOther model results:\n")
  cat(sprintf("  Number of iterations: %d\n", length(other_results)))
  cat(sprintf("  Fields per iteration: %s\n", paste(names(other_results[[1]]), collapse = ", ")))
  
  # Check if structures match
  if (length(baseline_results) != length(other_results)) {
    cat("\nWARNING: Different number of iterations!\n")
  }
  
  baseline_fields <- names(baseline_results[[1]])
  other_fields <- names(other_results[[1]])
  
  missing_in_baseline <- setdiff(other_fields, baseline_fields)
  missing_in_other <- setdiff(baseline_fields, other_fields)
  
  if (length(missing_in_baseline) > 0) {
    cat(sprintf("\nFields in other model but not in baseline: %s\n", 
                paste(missing_in_baseline, collapse = ", ")))
  }
  
  if (length(missing_in_other) > 0) {
    cat(sprintf("\nFields in baseline but not in other model: %s\n", 
                paste(missing_in_other, collapse = ", ")))
  }
  
  if (length(missing_in_baseline) == 0 && length(missing_in_other) == 0) {
    cat("\n✓ Structures are compatible!\n")
  }
  
  cat("\n")
}

results_baseline <- convert_baseline_to_custom_format(
  results_abundance = results_abundance,
  results_infected = results_infected,
  results_cases = results_cases,
  quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                      0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                      0.95, 0.975, 0.99)
)

# 3. Verify it worked
verify_custom_format(results_baseline)

saveRDS(results_baseline, file = paste0("results_BaselineModel_", Year, ".rds"))


results <- readRDS(paste0("results_BaselineModel_", Year, ".rds")) #ALWAYS RUN THIS 
num_iterations <- length(results)   
iteration      <- num_iterations    
# 1-WEEK AHEAD - Human Cases
plot_data3 <- map_dfr(1:num_iterations, ~ {
  forecast_data <- results[[.x]]$human_cases_q_1
  data.frame(
    Iteration = .x,
    Week = .x + 5,  # Actual week number being forecasted
    Date = forecast_1week_dates[.x],  # Use universal dates
    #50% CI 
    Lower_50 = forecast_data[["25%"]],
    Upper_50 = forecast_data[["75%"]],
    # 90% CI
    Lower_90 = forecast_data[["5%"]],
    Upper_90 = forecast_data[["95%"]],
    # 80% CI 
    Lower_80 = forecast_data[["10%"]],
    Upper_80 = forecast_data[["90%"]],
    
    # Median
    Median = forecast_data[["50%"]]
  )
})
X0_obs <- X4_obs
# Observed data for 1-week forecasts
df_observed_1week <- data.frame(
  Date = forecast_1week_dates,
  Week = 6:51,
  X_real = X0_obs[6:51],  # Observations at weeks 6-51
  Type = "Observed"
)

Q1 = ggplot() +
  # 50% CI - outermost, thickest, most transparent
  geom_linerange(data = plot_data3,
                 aes(x = as.Date(Date), ymin = Lower_50, ymax = Upper_50, color = "50% CI"),
                 linewidth = 3, alpha = 0.3) +
  # 50% CI - innermost, thinnest, least transparent
  geom_linerange(data = plot_data3,
                 aes(x = as.Date(Date), ymin = Lower_90, ymax = Upper_90, color = "90% CI"),
                 linewidth = 1.5, alpha = 0.7) +
  # 80% CI - middle layer
  geom_linerange(data = plot_data3,
                 aes(x = as.Date(Date), ymin = Lower_80, ymax = Upper_80, color = "80% CI"),
                 linewidth = 2, alpha = 0.5) +
  # Median point
  geom_point(data = plot_data3,
             aes(x = as.Date(Date), y = Median, color = "Forecast"),
             size = 2) +
  # Observed data
  geom_line(data = df_observed_1week,
            aes(x = as.Date(Date), y = X_real, color = "Observed"),
            linewidth = 1) +
  labs(x = "Date", y = "Human Cases",
       title = "1-week ahead forecasts with uncertainty intervals") +
  scale_color_manual(
    name = "Uncertainty",
    values = c(
      "90% CI" = "#6baed6",    # Medium blue
      "80% CI" = "#08519c" ,    # Dark blue
      "50% CI" = "blue",    # Very light blue
      "Forecast" = "blue",    # Dark blue (matches 50% CI)
      "Observed" = "red"     # Red
    ),
    breaks = c("90% CI", "80% CI", "50% CI", "Forecast", "Observed")  # Control legend order
  ) +
  scale_y_continuous(limits = c(0, 260)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")

# 2-WEEK AHEAD - Human Cases
plot_data4 <- map_dfr(1:num_iterations, ~ {
  forecast_data <- results[[.x]]$human_cases_q_2
  data.frame(
    Iteration = .x,
    Week = .x + 6,  # Actual week number being forecasted
    Date = forecast_2week_dates[.x],  # Use universal dates
    #50% CI 
    Lower_50 = forecast_data[["25%"]],
    Upper_50 = forecast_data[["75%"]],
    # 90% CI
    Lower_90 = forecast_data[["5%"]],
    Upper_90 = forecast_data[["95%"]],
    # 80% CI 
    Lower_80 = forecast_data[["10%"]],
    Upper_80 = forecast_data[["90%"]],
    
    # Median
    Median = forecast_data[["50%"]]
  )
})

# Observed data for 2-week forecasts
df_observed_2week <- data.frame(
  Date = forecast_2week_dates,
  Week = 7:52,
  X_real = X0_obs[7:52],  # Observations at weeks 7-52
  Type = "Observed"
)

Q2 = ggplot() +
  # 50% CI - lightest gray, thickest
  geom_linerange(data = plot_data4,
                 aes(x = as.Date(Date), ymin = Lower_50, ymax = Upper_50, color = "50% CI"),
                 linewidth = 4, alpha = 0.3) +
  # 90% CI - medium gray
  geom_linerange(data = plot_data4,
                 aes(x = as.Date(Date), ymin = Lower_90, ymax = Upper_90, color = "90% CI"),
                 linewidth = 3, alpha = 0.5) +
  # 80% CI - dark gray
  geom_linerange(data = plot_data4,
                 aes(x = as.Date(Date), ymin = Lower_80, ymax = Upper_80, color = "80% CI"),
                 linewidth = 2, alpha = 0.7) +
  # Median forecast points
  geom_point(data = plot_data4,
             aes(x = as.Date(Date), y = Median, color = "Forecast"),
             size = 3) +
  # Observed data
  geom_line(data = df_observed_2week,
            aes(x = as.Date(Date), y = X_real, color = "Observed"),
            linewidth = 1.5) +
  labs(x = "Date", y = "Human Cases",
       title = "2-week ahead forecasts with uncertainty intervals") +
  scale_color_manual(
    name = "Uncertainty",
    values = c(
      "90% CI" = "#636363",    # Medium gray
      "80% CI" = "#252525",    # Very dark gray
      "50% CI" = "black",    # Light gray
      "Forecast" = "black",  # Black
      "Observed" = "red"   # Red
    ),
    breaks = c( "90% CI", "80% CI", "50% CI", "Forecast", "Observed")
  ) +
  scale_y_continuous(limits = c(0, 260)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")



# 1-WEEK AHEAD - Infectious_Mosq_1000
# Prepare data with multiple confidence intervals
plot_data7 <- map_dfr(1:num_iterations, ~ {
  forecast_data <- results[[.x]]$infectious_per_1000_q_1
  data.frame(
    Iteration = .x,
    Date = forecast_1week_dates[.x],
    #50% CI 
    Lower_50 = forecast_data[["25%"]],
    Upper_50 = forecast_data[["75%"]],
    # 90% CI
    Lower_90 = forecast_data[["5%"]],
    Upper_90 = forecast_data[["95%"]],
    # 80% CI 
    Lower_80 = forecast_data[["10%"]],
    Upper_80 = forecast_data[["90%"]],
    
    # Median
    Median = forecast_data[["50%"]]
  )
})

df_observed_Infectious_Mosq_1000_1week <- data.frame(
  Date = forecast_1week_dates,
  X_real = X_obs2[6:51],
  Type = "Observed"
)

Q3 <- ggplot() +
  # 50% CI - outermost, thickest, most transparent
  geom_linerange(data = plot_data7,
                 aes(x = as.Date(Date), ymin = Lower_50, ymax = Upper_50, color = "50% CI"),
                 linewidth = 3, alpha = 0.3) +
  # 90% CI - innermost, thinnest, least transparent
  geom_linerange(data = plot_data7,
                 aes(x = as.Date(Date), ymin = Lower_90, ymax = Upper_90, color = "90% CI"),
                 linewidth = 1.5, alpha = 0.7) +
  # 80% CI - middle layer
  geom_linerange(data = plot_data7,
                 aes(x = as.Date(Date), ymin = Lower_80, ymax = Upper_80, color = "80% CI"),
                 linewidth = 2, alpha = 0.5) +
  # Median point
  geom_point(data = plot_data7,
             aes(x = as.Date(Date), y = Median, color = "Forecast"),
             size = 2) +
  # Observed data
  geom_line(data = df_observed_Infectious_Mosq_1000_1week,
            aes(x = as.Date(Date), y = X_real, color = "Observed"),
            linewidth = 1) +
  labs(x = "Date", y = "Infectious Mosq per 1000",
       title = "1-week ahead forecasts with uncertainty intervals") +
  scale_color_manual(
    name = "Uncertainty",
    values = c(
      "90% CI" = "#6baed6",    # Medium blue
      "80% CI" = "#08519c" ,    # Dark blue
      "50% CI" = "blue",    # Very light blue
      "Forecast" = "blue",    # Dark blue (matches 50% CI)
      "Observed" = "red"     # Red
    ),
    breaks = c("90% CI", "80% CI", "50% CI", "Forecast", "Observed")  # Control legend order
  ) +
  scale_y_continuous(limits = c(0, 400)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")

#print(Q3)

# 2-WEEK AHEAD - Infectious_Mosq_1000
plot_data8 <- map_dfr(1:num_iterations, ~ {
  forecast_data <- results[[.x]]$infectious_per_1000_q_2
  data.frame(
    Iteration = .x,
    Date = forecast_2week_dates[.x],
    #50% CI 
    Lower_50 = forecast_data[["25%"]],
    Upper_50 = forecast_data[["75%"]],
    # 90% CI
    Lower_90 = forecast_data[["5%"]],
    Upper_90 = forecast_data[["95%"]],
    # 80% CI 
    Lower_80 = forecast_data[["10%"]],
    Upper_80 = forecast_data[["90%"]],
    
    # Median
    Median = forecast_data[["50%"]]
  )
})

df_observed_Infectious_Mosq_1000_2week <- data.frame(
  Date = forecast_2week_dates,
  X_real = X_obs2[7:52],
  Type = "Observed"
)

Q4 <- ggplot() +
  # 50% CI - lightest gray, thickest
  geom_linerange(data = plot_data8,
                 aes(x = as.Date(Date), ymin = Lower_50, ymax = Upper_50, color = "50% CI"),
                 linewidth = 4, alpha = 0.3) +
  # 90% CI - medium gray
  geom_linerange(data = plot_data8,
                 aes(x = as.Date(Date), ymin = Lower_90, ymax = Upper_90, color = "90% CI"),
                 linewidth = 3, alpha = 0.5) +
  # 80% CI - dark gray
  geom_linerange(data = plot_data8,
                 aes(x = as.Date(Date), ymin = Lower_80, ymax = Upper_80, color = "80% CI"),
                 linewidth = 2, alpha = 0.7) +
  # Median forecast points
  geom_point(data = plot_data8,
             aes(x = as.Date(Date), y = Median, color = "Forecast"),
             size = 3) +
  # Observed data
  geom_line(data = df_observed_Infectious_Mosq_1000_2week,
            aes(x = as.Date(Date), y = X_real, color = "Observed"),
            linewidth = 1.5) +
  labs(x = "Date", y = "Infectious Mosq per 1000",
       title = "2-week ahead forecasts with uncertainty intervals") +
  scale_color_manual(
    name = "Uncertainty",
    values = c(
      "90% CI" = "#636363",    # Medium gray
      "80% CI" = "#252525",    # Very dark gray
      "50% CI" = "black",    # Light gray
      "Forecast" = "black",  # Black
      "Observed" = "red"   # Red
    ),
    breaks = c( "90% CI", "80% CI", "50% CI", "Forecast", "Observed")
  ) +
  scale_y_continuous(limits = c(0, 400)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")

#print(Q4)
# 1-WEEK AHEAD - Total Abundance
plot_data5 <- map_dfr(1:num_iterations, ~ {
  forecast_data <- results[[.x]]$total_abundance_q_1
  data.frame(
    Iteration = .x,
    Date = forecast_1week_dates[.x],
    #50% CI 
    Lower_50 = forecast_data[["25%"]],
    Upper_50 = forecast_data[["75%"]],
    # 90% CI
    Lower_90 = forecast_data[["5%"]],
    Upper_90 = forecast_data[["95%"]],
    # 80% CI 
    Lower_80 = forecast_data[["10%"]],
    Upper_80 = forecast_data[["90%"]],
    
    # Median
    Median = forecast_data[["50%"]]
  )
})

df_observed_abundance_1week <- data.frame(
  Date = forecast_1week_dates,
  X_real = X_obs1[6:51],
  Type = "Observed"
)

Q5 = ggplot() +
  # 50% CI - outermost, thickest, most transparent
  geom_linerange(data = plot_data5,
                 aes(x = as.Date(Date), ymin = Lower_50, ymax = Upper_50, color = "50% CI"),
                 linewidth = 3, alpha = 0.3) +
  # 90% CI - innermost, thinnest, least transparent
  geom_linerange(data = plot_data5,
                 aes(x = as.Date(Date), ymin = Lower_90, ymax = Upper_90, color = "90% CI"),
                 linewidth = 1.5, alpha = 0.7) +
  # 80% CI - middle layer
  geom_linerange(data = plot_data5,
                 aes(x = as.Date(Date), ymin = Lower_80, ymax = Upper_80, color = "80% CI"),
                 linewidth = 2, alpha = 0.5) +
  # Median point
  geom_point(data = plot_data5,
             aes(x = as.Date(Date), y = Median, color = "Forecast"),
             size = 2) +
  # Observed data
  geom_line(data = df_observed_abundance_1week,
            aes(x = as.Date(Date), y = X_real, color = "Observed"),
            linewidth = 1) +
  labs(x = "Date", y = "Total Abundance",
       title = "1-week ahead forecasts with uncertainty intervals") +
  scale_color_manual(
    name = "Uncertainty",
    values = c(
      "90% CI" = "#6baed6",    # Medium blue
      "80% CI" = "#08519c" ,    # Dark blue
      "50% CI" = "blue",    # Very light blue
      "Forecast" = "blue",    # Dark blue (matches 50% CI)
      "Observed" = "red"     # Red
    ),
    breaks = c("90% CI", "80% CI", "50% CI",  "Forecast", "Observed")  # Control legend order
  ) +
  scale_y_continuous(limits = c(0, 6500)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")

# 2-WEEK AHEAD - Total Abundance
plot_data6 <- map_dfr(1:num_iterations, ~ {
  forecast_data <- results[[.x]]$total_abundance_q_2
  data.frame(
    Iteration = .x,
    Date = forecast_2week_dates[.x],
    #50% CI 
    Lower_50 = forecast_data[["25%"]],
    Upper_50 = forecast_data[["75%"]],
    # 90% CI
    Lower_90 = forecast_data[["5%"]],
    Upper_90 = forecast_data[["95%"]],
    # 80% CI 
    Lower_80 = forecast_data[["10%"]],
    Upper_80 = forecast_data[["90%"]],
    
    # Median
    Median = forecast_data[["50%"]]
  )
})

df_observed_abundance_2week <- data.frame(
  Date = forecast_2week_dates,
  X_real = X_obs1[7:52],
  Type = "Observed"
)

Q6 =  ggplot() +
  # 50% CI - lightest gray, thickest
  geom_linerange(data = plot_data6,
                 aes(x = as.Date(Date), ymin = Lower_50, ymax = Upper_50, color = "50% CI"),
                 linewidth = 4, alpha = 0.3) +
  # 90% CI - medium gray
  geom_linerange(data = plot_data6,
                 aes(x = as.Date(Date), ymin = Lower_90, ymax = Upper_90, color = "90% CI"),
                 linewidth = 3, alpha = 0.5) +
  # 80% CI - dark gray
  geom_linerange(data = plot_data6,
                 aes(x = as.Date(Date), ymin = Lower_80, ymax = Upper_80, color = "80% CI"),
                 linewidth = 2, alpha = 0.7) +
  # Median forecast points
  geom_point(data = plot_data6,
             aes(x = as.Date(Date), y = Median, color = "Forecast"),
             size = 3) +
  # Observed data
  geom_line(data = df_observed_abundance_2week,
            aes(x = as.Date(Date), y = X_real, color = "Observed"),
            linewidth = 1.5) +
  labs(x = "Date", y = "Total Abundance",
       title = "2-week ahead forecasts with uncertainty intervals") +
  scale_color_manual(
    name = "Uncertainty",
    values = c(
      "90% CI" = "#636363",    # Medium gray
      "80% CI" = "#252525",    # Very dark gray
      "50% CI" = "black",    # Light gray
      "Forecast" = "black",  # Black
      "Observed" = "red"   # Red
    ),
    breaks = c("90% CI", "80% CI", "50% CI", "Forecast", "Observed")
  ) +
  scale_y_continuous(limits = c(0, 6500)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")



ggsave(paste0("Q1_BASELINE_", iteration, "_", Year, ".png"), plot = Q1, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q2_BASELINE_", iteration, "_", Year, ".png"), plot = Q2, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q3_BASELINE_", iteration, "_", Year, ".png"), plot = Q3, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q4_BASELINE_", iteration, "_", Year, ".png"), plot = Q4, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q5_BASELINE_", iteration, "_", Year, ".png"), plot = Q5, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q6_BASELINE_", iteration, "_", Year, ".png"), plot = Q6, width = 10, height = 8, dpi = 300)


#####################################
## Convert Custom Results to       ##
## epipredict Format for WIS       ##
#####################################

convert_to_quantile_pred <- function(quantiles_vector, quantile_levels) {
  
  # If quantiles_vector is already a named vector, use it directly
  # Otherwise, assume it's in the same order as quantile_levels
  if (is.null(names(quantiles_vector))) {
    names(quantiles_vector) <- as.character(quantile_levels)
  }
  
  # Convert to dist_quantiles
  dist_quantiles(
    list(as.numeric(quantiles_vector)),
    list(quantile_levels)
  )
}

calculate_wis_from_custom_results <- function(results, 
                                              actual_data,
                                              target_variable,
                                              horizon,
                                              quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                                  0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                                  0.95, 0.975, 0.99)) {
  
  n_forecasts <- length(results)
  
  wis_results <- map_dfr(1:n_forecasts, function(i) {
    # Get the forecast quantiles for this iteration
    forecast_key <- paste0(target_variable, "_q_", horizon)
    forecast_quantiles <- results[[i]][[forecast_key]]
    
    # Get the actual value for this forecast
    # The actual value is at position i + horizon (since we're forecasting ahead)
    actual_idx <- i + 5 + horizon - 1  # Assuming you start forecasting after 5 training weeks
    
    if (actual_idx > length(actual_data)) {
      return(NULL)  # Skip if we don't have actual data
    }
    
    actual_value <- actual_data[actual_idx]
    
    # Skip if actual value is NA
    if (is.na(actual_value)) {
      return(NULL)
    }
    
    # Convert to quantile_pred format (matrix with 1 row)
    # The quantile values should be in a matrix format
    quantile_matrix <- matrix(as.numeric(forecast_quantiles), nrow = 1)
    
    # Create quantile_pred object
    pred_dist <- quantile_pred(quantile_matrix, quantile_levels)
    
    # Calculate WIS using epipredict function
    wis_score <- weighted_interval_score(
      x = pred_dist,
      actual = actual_value,
      quantile_levels = quantile_levels,
      na_handling = "impute"
    )
    
    # Return results
    tibble(
      iteration = i,
      horizon = horizon,
      target = target_variable,
      actual = actual_value,
      median_forecast = forecast_quantiles[["50%"]] %||% forecast_quantiles[[12]],  # 0.5 quantile
      WIS = as.numeric(wis_score)
    )
  })
  
  return(wis_results)
}

calculate_wis_all_targets <- function(results,
                                      X_obs1,  #  Total abundance
                                      X_obs2,  #  Infected per 1000
                                      X0_obs,  # Human cases
                                      quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                          0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                          0.95, 0.975, 0.99)) {
  
  cat("Calculating WIS for all targets...\n")
  
  # Total abundance
  wis_abundance_1wk <- calculate_wis_from_custom_results(
    results, X_obs1, "total_abundance", 1, quantile_levels
  )
  wis_abundance_2wk <- calculate_wis_from_custom_results(
    results, X_obs1, "total_abundance", 2, quantile_levels
  )
  
  # Infected per 1000
  wis_infected_1wk <- calculate_wis_from_custom_results(
    results, X_obs2, "infectious_per_1000", 1, quantile_levels
  )
  wis_infected_2wk <- calculate_wis_from_custom_results(
    results, X_obs2, "infectious_per_1000", 2, quantile_levels
  )
  
  # Human cases
  wis_cases_1wk <- calculate_wis_from_custom_results(
    results, X0_obs, "human_cases", 1, quantile_levels
  )
  wis_cases_2wk <- calculate_wis_from_custom_results(
    results, X0_obs, "human_cases", 2, quantile_levels
  )
  
  cat("WIS calculation complete!\n")
  
  return(list(
    abundance_1wk = wis_abundance_1wk,
    abundance_2wk = wis_abundance_2wk,
    infected_1wk = wis_infected_1wk,
    infected_2wk = wis_infected_2wk,
    cases_1wk = wis_cases_1wk,
    cases_2wk = wis_cases_2wk
  ))
}

summarize_wis_results <- function(wis_results) {
  summary_list <- map_dfr(names(wis_results), function(name) {
    data <- wis_results[[name]]
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    tibble(
      Variable = sub("_.*", "", name),
      Horizon = sub(".*_", "", name),
      n_forecasts = nrow(data),
      Mean_WIS = mean(data$WIS, na.rm = TRUE),
      Median_WIS = median(data$WIS, na.rm = TRUE),
      SD_WIS = sd(data$WIS, na.rm = TRUE),
      Min_WIS = min(data$WIS, na.rm = TRUE),
      Max_WIS = max(data$WIS, na.rm = TRUE)
    )
  })
  
  return(summary_list)
}

calculate_normalized_wis_custom <- function(wis_results) {
  
  normalized_list <- map_dfr(names(wis_results), function(name) {
    data <- wis_results[[name]]
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    Y_total <- sum(data$actual, na.rm = TRUE)
    sum_wis <- sum(data$WIS, na.rm = TRUE)
    
    if (Y_total == 0) {
      wis_norm <- NA
    } else {
      wis_norm <- (1 / Y_total) * sum_wis
    }
    
    tibble(
      Variable = sub("_.*", "", name),
      Horizon = sub(".*_", "", name),
      WIS_normalized = wis_norm,
      n_forecasts = nrow(data)
    )
  })
  
  return(normalized_list)
}

#####################################
## Visualization Functions         ##
#####################################

plot_wis_custom_results <- function(wis_results, add_dates = TRUE, start_date = as.Date("2014-02-15")) {
  
  # Combine all results
  combined <- bind_rows(wis_results, .id = "forecast_type") %>%
    separate(forecast_type, into = c("Variable", "Horizon"), sep = "_(?=[12]wk)")
  
  # Add dates if requested
  if (add_dates) {
    combined <- combined %>%
      mutate(Date = start_date + 7 * (iteration - 1))
    x_var <- "Date"
  } else {
    x_var <- "iteration"
  }
  
  facet_labels <- c(
    "abundance" = "Total abundance",
    "cases" = "Human cases",
    "infected" = "Infectious mosq per 1000"
  )
  
  # Create plot
  p <- ggplot(combined, aes(x = .data[[x_var]], y = WIS, color = Horizon, group = Horizon)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5, alpha = 0.6) +
    # REMOVED the duplicate facet_wrap - keep only this one:
    facet_wrap(~Variable, scales = "free_y", ncol = 1, 
               labeller = as_labeller(facet_labels)) +
    scale_color_manual(
      values = c("1wk" = "#2E86AB", "2wk" = "#A23B72"),
      labels = c("1wk" = "1-week ahead", "2wk" = "2-week ahead")
    ) +
    labs(
      title = "Weighted Interval Score (WIS) Over Time",
      subtitle = "Lower scores indicate better forecast performance",
      x = if (add_dates) "Date" else "Iteration",
      y = "WIS",
      color = "Forecast Horizon"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "bold", size = 11),
      axis.title = element_text(size = 17),
      axis.text = element_text(size = 17),
      legend.text = element_text(size = 15),
      plot.title = element_text(face = "bold", size = 17)
    ) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")
  
  return(p)
}

plot_wis_summary_custom <- function(wis_summary) {
  
  p <- ggplot(wis_summary, aes(x = Variable, y = Mean_WIS, fill = Horizon)) +
    geom_col(position = "dodge", alpha = 0.8) +
    geom_errorbar(
      aes(ymin = Mean_WIS - SD_WIS, ymax = Mean_WIS + SD_WIS),
      position = position_dodge(width = 0.9),
      width = 0.2
    ) +
    scale_fill_manual(
      values = c("1wk" = "#2E86AB", "2wk" = "#A23B72"),
      labels = c("1wk" = "1-week ahead", "2wk" = "2-week ahead")
    ) +
    labs(
      title = "Mean WIS with Standard Deviation",
      subtitle = "Error bars show ±1 SD",
      x = "Variable",
      y = "Mean WIS",
      fill = "Horizon"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 17),  # cex.lab equivalent
          axis.text = element_text(size = 17),   # cex.axis equivalent
          legend.text = element_text(size = 15), # cex.names equivalent for legend
      plot.title = element_text(face = "bold", size = 17)
    )
  
  return(p)
}

#####################################
## Usage                   ##
#####################################

# # After running your forecasts and having results list:
# 
# # 1. Calculate WIS for all targets
wis_all <- calculate_wis_all_targets(
  results = results,
  X_obs1 = X_obs1,
  X_obs2 = X_obs2,
  X0_obs = X4_obs[1:52], #X0_obs,
  quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                      0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                      0.95, 0.975, 0.99)
)

saveRDS(wis_all, file = paste0("wis_all_BaselineModel_", Year, ".rds"))
}
# ============================================================
# Baseline WIS heatmap
# Y-axis: target x horizon combinations
# X-axis: year (one column per year — single panel)
# Fill:   median raw WIS across weeks in that year
# ============================================================

# ── Load baseline WIS data ─────────────────────────────────────────────────────
years <- c(2006:2019, 2021)

# Correct way to read and combine baseline WIS data
baseline_all <- map_df(years, function(year) {
  
  # Read the RDS file (it's a list)
  lis <- readRDS(paste0("wis_all_BaselineModel_", year, ".rds"))
  
  # Convert the list into one tidy data frame
  df <- bind_rows(
    lis$abundance_1wk %>% mutate(horizon = 1, target = "Total abundance"),
    lis$abundance_2wk %>% mutate(horizon = 2, target = "Total abundance"),
    
    lis$infected_1wk %>% mutate(horizon = 1, target = "Infectious mosq per 1000"),
    lis$infected_2wk %>% mutate(horizon = 2, target = "Infectious mosq per 1000"),
    
    lis$cases_1wk    %>% mutate(horizon = 1, target = "Human cases"),
    lis$cases_2wk    %>% mutate(horizon = 2, target = "Human cases")
  )
  
  # Add year and clean column names
  df %>%
    mutate(year = year) %>%
    select(year, horizon, target, iteration, actual, median_forecast, WIS)
})

# Check the result
glimpse(baseline_all)

# ========================================
# Add forecast dates to baseline_all
# ========================================

baseline_all <- baseline_all %>%
  group_by(year, horizon, target) %>%   # Important: group by all three
  mutate(
    # Create the 52-week sequence for this year
    all_weeks  = list(seq(as.Date(paste0(year, "-01-01")), 
                          by = "weeks", length.out = 52)),
    
    # Reset week_index to 1, 2, ..., 138 within each target/horizon/year
    week_index = row_number()
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    forecast_date = {
      weeks <- all_weeks
      idx   <- week_index
      
      if (horizon == 1) {
        if (idx + 5 <= 52) weeks[idx + 5] else NA
      } else {
        if (idx + 6 <= 52) weeks[idx + 6] else NA
      }
    }
  ) %>%
  ungroup() %>%
  select(-all_weeks, -week_index)

# ── Build year-level summary ───────────────────────────────────────────────────

baseline_summary  <- baseline_all %>%
  filter(is.finite(WIS)) %>%
  mutate(
    month     = month(forecast_date, label = TRUE, abbr = TRUE),
    month_num = month(forecast_date)
  ) %>%
  group_by(year, month, month_num, target, horizon) %>%
  summarise(
    median_WIS = median(WIS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month = factor(month, levels = month.abb),
  )

# ========================================
# 4. Create nice y-axis labels
# ========================================
baseline_summary <- baseline_summary %>%
  mutate(
    target_short = recode(target,
                          "Total abundance"          = "Total Abundance",
                          "Infectious mosq per 1000" = "Infectious Mosq per 1000",
                          "Human cases"              = "Human Cases"
    ),
    horizon_label = paste0(horizon, "-wk"),
    y_label = paste0(target_short, " (", horizon_label, ")")
  )

# Desired order for y-axis
y_order <- c(
  "Total Abundance (1-wk)",
  "Total Abundance (2-wk)",
  "Infectious Mosq per 1000 (1-wk)",
  "Infectious Mosq per 1000 (2-wk)",
  "Human Cases (1-wk)",
  "Human Cases (2-wk)"
)

baseline_summary <- baseline_summary %>%
  mutate(y_label = factor(y_label, levels = y_order))

# ========================================
# 5. Compute colour scale limits (YOUR preferred method)
# ========================================
lo <- quantile(baseline_summary$median_WIS, 0.05, na.rm = TRUE)
hi <- quantile(baseline_summary$median_WIS, 0.95, na.rm = TRUE)

break_vals   <- c(lo, (lo + hi) / 2, hi)
break_labels <- round(break_vals, 1)

# ========================================
# 6. Build the plot
# ========================================
p_baseline <- ggplot(baseline_summary,
                     aes(x = month, y = y_label, fill = pmax(pmin(median_WIS, hi), lo))) +
  
  geom_tile(colour = "white", linewidth = 0.3) +
  
  facet_wrap(~ year, ncol = 5, nrow = 3) +   # 15 panels: 3 rows x 5 columns
  
  # Your preferred diverging-style sequential gradient with outlier control
  scale_fill_gradient(
    low    = "#fff5f0",      # very light peach/white
    high   = "#99000d",      # dark red
    limits = c(lo, hi),
    oob    = scales::squish,   # squish outliers to the limits
    name   = "Median WIS",
    breaks = break_vals,
    labels = break_labels
  ) +
  
  labs(
    title    = "Baseline Model: Median WIS by Month, Target and Horizon",
    subtitle = paste0(
      "Light colours = better performance (lower WIS)  |  ",
      "Dark red = worse performance (higher WIS)\n",
      "Colour scale capped at 5th–95th percentiles to reduce outlier influence"
    ),
    x = "Month",
    y = NULL
  ) +
  
  theme_minimal(base_size = 19) +
  theme(
    plot.title      = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle   = element_text(size = 19, colour = "grey40"),
    strip.text      = element_text(face = "bold", size = 19),
    axis.text.x     = element_text(angle = 90, hjust = 1, size = 19, face = "bold"),
    axis.text.y     = element_text(size = 19, face = "bold"),
    legend.position = "right",
    legend.text     = element_text(size = 19, face = "bold"),
    legend.title    = element_text(size = 19, face = "bold"),
    panel.grid      = element_blank(),
    panel.spacing   = unit(0.5, "lines")
  )

# ========================================
# 7. Save
# ========================================
ggsave(
  filename = "heatmap_baseline_WIS_by_month.pdf",
  plot     = p_baseline,
  width    = 20,
  height   = 14,
  dpi      = 300,
  bg       = "white"
)

message("Plot saved successfully as heatmap_baseline_WIS_by_month.pdf")

#Boxplot for year
# ── Load baseline WIS data ─────────────────────────────────────────────────────
years <- c(2006:2019, 2021)

# Correct way to read and combine baseline WIS data
baseline_all <- map_df(years, function(year) {
  
  # Read the RDS file (it's a list)
  lis <- readRDS(paste0("wis_all_BaselineModel_", year, ".rds"))
  
  # Convert the list into one tidy data frame
  df <- bind_rows(
    lis$abundance_1wk %>% mutate(horizon = 1, target = "Total abundance"),
    lis$abundance_2wk %>% mutate(horizon = 2, target = "Total abundance"),
    
    lis$infected_1wk %>% mutate(horizon = 1, target = "Infectious mosq per 1000"),
    lis$infected_2wk %>% mutate(horizon = 2, target = "Infectious mosq per 1000"),
    
    lis$cases_1wk    %>% mutate(horizon = 1, target = "Human cases"),
    lis$cases_2wk    %>% mutate(horizon = 2, target = "Human cases")
  )
  
  # Add year and clean column names
  df %>%
    mutate(year = year) %>%
    select(year, horizon, target, iteration, actual, median_forecast, WIS)
})

# Check the result
glimpse(baseline_all)

# ========================================
# Add forecast dates to baseline_all
# ========================================

baseline_all <- baseline_all %>%
  group_by(year, horizon, target) %>%   # Important: group by all three
  mutate(
    # Create the 52-week sequence for this year
    all_weeks  = list(seq(as.Date(paste0(year, "-01-01")), 
                          by = "weeks", length.out = 52)),
    
    # Reset week_index to 1, 2, ..., 138 within each target/horizon/year
    week_index = row_number()
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    forecast_date = {
      weeks <- all_weeks
      idx   <- week_index
      
      if (horizon == 1) {
        if (idx + 5 <= 52) weeks[idx + 5] else NA
      } else {
        if (idx + 6 <= 52) weeks[idx + 6] else NA
      }
    }
  ) %>%
  ungroup() %>%
  select(-all_weeks, -week_index)
baseline_all_summary_stats <- baseline_all %>%
  group_by(year, target, horizon) %>%
  summarise(
    median_WIS = median(WIS[is.finite(WIS)], na.rm = TRUE),
    mean_WIS   = mean(WIS[is.finite(WIS)],   na.rm = TRUE),
    .groups = "drop"
  )
baseline_all_summary_stats <- baseline_all_summary_stats %>%
  mutate(
    target_short = recode(target,
                          "Total abundance"          = "Total Abundance",
                          "Infectious mosq per 1000" = "Infectious Mosq per 1000",
                          "Human cases"              = "Human Cases"
    ),
    horizon_label = paste0(horizon, "-wk"),
    y_label = paste0(target_short, " (", horizon_label, ")")
  )

# ============================================================
# Baseline WIS Boxplot
# X-axis: target x horizon combinations (6 boxes)
# Y-axis: WIS score
# Each point = one year
# ============================================================

# ── Colorblind-safe palette for 6 target-horizon groups ───────────────────────
# Using Okabe-Ito palette — pairs of shades per target (dark/light)
group_colors <- c(
  "Total Abundance (1-wk)"          = "#009E73",   # bluish green (dark)
  "Total Abundance (2-wk)"          = "#85D4BA",   # bluish green (light)
  "Infectious Mosq per 1000 (1-wk)" = "#E69F00",   # orange (dark)
  "Infectious Mosq per 1000 (2-wk)" = "#F5CF7A",   # orange (light)
  "Human Cases (1-wk)"              = "#0072B2",   # blue (dark)
  "Human Cases (2-wk)"              = "#6DB8E0"    # blue (light)
)

# ── Set consistent factor order ────────────────────────────────────────────────
y_order <- c(
  "Total Abundance (1-wk)",
  "Total Abundance (2-wk)",
  "Infectious Mosq per 1000 (1-wk)",
  "Infectious Mosq per 1000 (2-wk)",
  "Human Cases (1-wk)",
  "Human Cases (2-wk)"
)

baseline_all_summary_stats <- baseline_all_summary_stats %>%
  mutate(y_label = factor(y_label, levels = y_order))

# ── Plotting function ──────────────────────────────────────────────────────────
plot_baseline_boxplots_free <- function(data, stat_type = "median") {
  
  if (stat_type == "median") {
    plot_data <- data %>% rename(value = median_WIS) %>% select(-mean_WIS)
    y_label_axis <- "Median WIS"
  } else {
    plot_data <- data %>% rename(value = mean_WIS) %>% select(-median_WIS)
    y_label_axis <- "Mean WIS"
  }
  
  plot_data <- plot_data %>%
    filter(is.finite(value)) %>%
    # Ensure facet order: Abundance → Infectious → Human Cases
    mutate(
      target_group = factor(target_group,
                            levels = c("Total Abundance",
                                       "Infectious Mosq per 1000",
                                       "Human Cases")),
      horizon_label_x = factor(horizon_label_x,
                               levels = c("1-wk", "2-wk"))
    )
  
  ggplot(plot_data,
         aes(x = horizon_label_x, y = value, fill = y_label)) +
    
    geom_boxplot(
      alpha         = 0.75,
      outlier.shape = NA,
      linewidth     = 0.7,
      width         = 0.5
    ) +
    
    geom_point(
      aes(color = y_label),
      position = position_jitter(width = 0.12),
      size     = 3,
      alpha    = 0.85
    ) +
    
    geom_text(
      aes(label = year, color = y_label),
      position    = position_jitter(width = 0.12),
      size        = 3,
      vjust       = -0.8,
      show.legend = FALSE
    ) +
    
    # Free y-axis so each target panel has its own scale
    facet_wrap(
      ~ target_group,
      scales = "free_y",
      ncol   = 3
    ) +
    
    scale_fill_manual(values  = group_colors) +
    scale_color_manual(values = group_colors) +
    
    labs(
      title    = paste0(y_label_axis,
                        " by Target and Forecast Horizon — Baseline Model"),
      subtitle = paste0(
        "Each point = one year's value  |  ",
        "Low score = better calibration  |  ",
        "Y-axis is free per target"
      ),
      x = "Forecast Horizon",
      y = y_label_axis
    ) +
    
    theme_minimal(base_size = 19) +
    theme(
      legend.position  = "none",
      axis.text.x      = element_text(size = 19, face = "bold"),
      axis.ticks.x     = element_line(),
      axis.title.x     = element_text(size = 19, face = "bold"),
      axis.title.y     = element_text(size = 19, face = "bold"),
      axis.text.y      = element_text(size = 19, face = "bold"),
      plot.title       = element_text(face = "bold", size = 19),
      plot.subtitle    = element_text(size = 19, color = "grey40"),
      strip.text       = element_text(face = "bold", size = 19),
      panel.grid.minor = element_blank()
    )
}

# ── Add missing columns before calling the function ───────────────────────────
baseline_all_summary_stats <- baseline_all_summary_stats %>%
  mutate(
    target_group = recode(target,
                          "Total abundance"          = "Total Abundance",
                          "Infectious mosq per 1000" = "Infectious Mosq per 1000",
                          "Human cases"              = "Human Cases"
    ),
    horizon_label_x = paste0(horizon, "-wk")
  )

# ── Now call the function ──────────────────────────────────────────────────────
baseline_median_plot2 <- plot_baseline_boxplots_free(
  baseline_all_summary_stats, stat_type = "median"
)
baseline_mean_plot2 <- plot_baseline_boxplots_free(
  baseline_all_summary_stats, stat_type = "mean"
)

print(baseline_median_plot2)

ggsave("baseline_median_WIS_boxplot.png",
       baseline_median_plot2, width = 14, height = 9, dpi = 300)
ggsave("baseline_median_WIS_boxplot.pdf",
       baseline_median_plot2, width = 14, height = 9, dpi = 300)
ggsave("baseline_mean_WIS_boxplot.png",
       baseline_mean_plot,   width = 14, height = 9, dpi = 300)
ggsave("baseline_mean_WIS_boxplot.pdf",
       baseline_mean_plot,   width = 14, height = 9, dpi = 300)

#boxplot by month
# ── Load baseline WIS data ─────────────────────────────────────────────────────
years <- c(2006:2019, 2021)

# Correct way to read and combine baseline WIS data
baseline_all <- map_df(years, function(year) {
  
  # Read the RDS file (it's a list)
  lis <- readRDS(paste0("wis_all_BaselineModel_", year, ".rds"))
  
  # Convert the list into one tidy data frame
  df <- bind_rows(
    lis$abundance_1wk %>% mutate(horizon = 1, target = "Total abundance"),
    lis$abundance_2wk %>% mutate(horizon = 2, target = "Total abundance"),
    
    lis$infected_1wk %>% mutate(horizon = 1, target = "Infectious mosq per 1000"),
    lis$infected_2wk %>% mutate(horizon = 2, target = "Infectious mosq per 1000"),
    
    lis$cases_1wk    %>% mutate(horizon = 1, target = "Human cases"),
    lis$cases_2wk    %>% mutate(horizon = 2, target = "Human cases")
  )
  
  # Add year and clean column names
  df %>%
    mutate(year = year) %>%
    select(year, horizon, target, iteration, actual, median_forecast, WIS)
})

# Check the result
glimpse(baseline_all)

# ========================================
# Add forecast dates to baseline_all
# ========================================

baseline_all <- baseline_all %>%
  group_by(year, horizon, target) %>%   # Important: group by all three
  mutate(
    # Create the 52-week sequence for this year
    all_weeks  = list(seq(as.Date(paste0(year, "-01-01")), 
                          by = "weeks", length.out = 52)),
    
    # Reset week_index to 1, 2, ..., 138 within each target/horizon/year
    week_index = row_number()
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    forecast_date = {
      weeks <- all_weeks
      idx   <- week_index
      
      if (horizon == 1) {
        if (idx + 5 <= 52) weeks[idx + 5] else NA
      } else {
        if (idx + 6 <= 52) weeks[idx + 6] else NA
      }
    }
  ) %>%
  ungroup() %>%
  select(-all_weeks, -week_index)

# ── Prepare data ───────────────────────────────────────────────────────────────
baseline_summary_month <- baseline_all %>%
  filter(is.finite(WIS)) %>%
  mutate(
    month     = month(forecast_date, label = TRUE, abbr = TRUE),
    month_num = month(forecast_date)
  ) %>%
  group_by(year, month, month_num, target, horizon) %>%
  summarise(
    median_WIS = median(WIS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month = factor(month, levels = month.abb),
    
    # Target labels for facet headers
    target_group = recode(target,
                          "Total abundance"          = "Total Abundance",
                          "Infectious mosq per 1000" = "Infectious Mosq per 1000",
                          "Human cases"              = "Human Cases"
    ),
    target_group = factor(target_group,
                          levels = c("Total Abundance",
                                     "Infectious Mosq per 1000",
                                     "Human Cases")),
    
    # Horizon labels for legend and fill/colour
    horizon_label = factor(paste0(horizon, "-wk"),
                           levels = c("1-wk", "2-wk"))
  )
# ── Add y_label to match group_colors keys ────────────────────────────────────
baseline_summary_month <- baseline_summary_month %>%
  mutate(
    y_label = paste0(target_group, " (", horizon_label, ")")
  )

# ── Keep the same group_colors as before ──────────────────────────────────────
group_colors <- c(
  "Total Abundance (1-wk)"          = "#009E73",
  "Total Abundance (2-wk)"          = "#85D4BA",
  "Infectious Mosq per 1000 (1-wk)" = "#E69F00",
  "Infectious Mosq per 1000 (2-wk)" = "#F5CF7A",
  "Human Cases (1-wk)"              = "#0072B2",
  "Human Cases (2-wk)"              = "#6DB8E0"
)

# ── Updated plotting function ──────────────────────────────────────────────────
plot_baseline_by_month <- function(data) {
  
  ggplot(data,
         aes(x     = month,
             y     = median_WIS,
             fill  = y_label,
             color = y_label)) +
    
    geom_boxplot(
      alpha         = 0.70,
      outlier.shape = NA,
      linewidth     = 0.6,
      width         = 0.65,
      position      = position_dodge(width = 0.75)
    ) +
    
    geom_point(
      position = position_jitterdodge(
        jitter.width = 0.12,
        dodge.width  = 0.75
      ),
      size  = 2,
      alpha = 0.70
    ) +
    
    geom_text(
      aes(label = year),
      position = position_jitterdodge(
        jitter.width = 0.12,
        dodge.width  = 0.75
      ),
      size        = 2.2,
      vjust       = -0.7,
      show.legend = FALSE
    ) +
    
    facet_wrap(
      ~ target_group,
      scales = "free_y",
      ncol   = 1
    ) +
    
    scale_fill_manual(
      name   = "Target & Horizon",
      values = group_colors,
      # Show only the 2 entries relevant to each facet in the legend
      # by keeping all 6 — reader can match colour to facet title
      breaks = names(group_colors)
    ) +
    scale_color_manual(
      name   = "Target & Horizon",
      values = group_colors,
      breaks = names(group_colors)
    ) +
    
    labs(
      title    = "Baseline Model: Median WIS by Month and Forecast Horizon",
      subtitle = paste0(
        "Each box aggregates across all years for that month  |  ",
        "Each point = one year  |  ",
        "Y-axis is free per target"
      ),
      x = "Month",
      y = "Median WIS"
    ) +
    
    theme_minimal(base_size = 19) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 19, face = "bold"),
      legend.title     = element_text(size = 19, face = "bold"),
      legend.key.size  = unit(0.6, "cm"),
      axis.text.x      = element_text(angle = 45, hjust = 1,
                                      size = 19, face = "bold"),
      axis.ticks.x     = element_line(),
      axis.title.x     = element_text(size = 19, face = "bold"),
      axis.title.y     = element_text(size = 19, face = "bold"),
      axis.text.y      = element_text(size = 19, face = "bold"),
      plot.title       = element_text(face = "bold", size = 19),
      plot.subtitle    = element_text(size = 19, color = "grey40"),
      strip.text       = element_text(face = "bold", size = 19),
      panel.grid.minor = element_blank()
    ) +
    guides(
      fill  = guide_legend(nrow = 2),
      color = guide_legend(nrow = 2)
    )
}

# ── Generate and save ──────────────────────────────────────────────────────────
baseline_month_plot <- plot_baseline_by_month(baseline_summary_month)

print(baseline_month_plot)

ggsave("baseline_WIS_by_month.png",
       baseline_month_plot,
       width  = 18,
       height = 16,
       dpi    = 300,
       bg     = "white")

ggsave("baseline_WIS_by_month.pdf",
       baseline_month_plot,
       width  = 18,
       height = 16,
       dpi    = 300,
       bg     = "white")

# ============================================================
# Multi-Year Panel Plotting Script
# Baseline Model - Forecast + Fan Charts
# Targets: Total Abundance, Infectious Mosq per 1000, Human Cases
# Years: 2006-2019, 2021 (15 years total)
#
# NOTE: This script only loads observation data and RDS results.
#       It does NOT re-run the EnKF or forecasting model.
#       All RDS files must already exist before running this script.
# ============================================================
# ============================================================
# CONFIGURATION
# ============================================================

years      <- c(2006:2019, 2021)
model_name <- "BaselineModel"
#N_ens      <- 8000

y_limits_forecast <- list(
  total_abundance     = c(0, 6500),
  infectious_per_1000 = c(0, 40),    
  human_cases         = c(0, 400)
)

# ============================================================
# DATA LOADING  
# Returns: list(X_obs1, X_obs2, X0_obs, all_weeks,
#               forecast_1week_dates, forecast_2week_dates)
# ============================================================

load_year_data <- function(Year) {
  
  if (Year == 2021) {
    mosq_pools_agg  <- read.csv("./mosq_pools_agg_2021.csv")
    X_obs           <- as.numeric(mosq_pools_agg$Tot_Mosq_Abund)
    mosq_pools_data <- read.csv("./mosq_pools_data_2021.csv")
    X2_obs          <- as.numeric(mosq_pools_data$Inf_Mosq_Per_1000)
    WNV             <- read.csv("./WNV_humans_summary3.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2021)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2021-01-01")
    
  } else if (Year == 2019) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data_2019.csv")
    X_obs           <- as.numeric(c(0, mosq_pools_agg$Tot_Mosq_Abund, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data_2019.csv")
    X2_obs          <- as.numeric(c(0, mosq_pools_data$Inf_Mosq_Per_1000, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2018-2019.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2019)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- as.numeric(X3_obs$cases)
    X3_obs[which(X3_obs == 1)[1]] <- 0
    X3_obs          <- cumsum(X3_obs)
    training_start_date <- as.Date("2019-01-01")
    
  } else if (Year == 2018) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data_2018-2019.csv")
    X_obs_raw       <- as.numeric(as.data.frame(mosq_pools_agg %>% filter(Year == 2018))$Tot_Mosq_Abund)
    mosq_agg0       <- read.csv("./temp_csv_2018.csv")
    X_obs0          <- as.numeric(mosq_agg0$Tot_Mosq_Abund)
    X_obs           <- as.numeric(c(0, X_obs0, X_obs_raw, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data_2018-2019.csv")
    X2_obs_raw      <- as.numeric(as.data.frame(mosq_pools_data %>% filter(Year == 2018))$Inf_Mosq_Per_1000)
    X2_obs          <- as.numeric(c(rep(0, 22), X2_obs_raw, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2018-2019.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2018)
    X3_obs          <- X3_obs[1:52, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2018-01-01")
    
  } else if (Year == 2017) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(c(as.data.frame(mosq_pools_agg %>% filter(Year == 2017))$Tot_Mosq_Abund, 0, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(c(as.data.frame(mosq_pools_data %>% filter(Year == 2017))$Inf_Mosq_Per_1000, 0, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2017)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2017-01-01")
    
  } else if (Year == 2016) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(as.data.frame(mosq_pools_agg %>% filter(Year == 2016))$Tot_Mosq_Abund)
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(as.data.frame(mosq_pools_data %>% filter(Year == 2016))$Inf_Mosq_Per_1000)
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2016)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2016-01-01")
    
  } else if (Year == 2015) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(c(as.data.frame(mosq_pools_agg %>% filter(Year == 2015))$Tot_Mosq_Abund, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(c(as.data.frame(mosq_pools_data %>% filter(Year == 2015))$Inf_Mosq_Per_1000, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2015)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2015-01-01")
    
  } else if (Year == 2014) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(c(as.data.frame(mosq_pools_agg %>% filter(Year == 2014))$Tot_Mosq_Abund, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(c(as.data.frame(mosq_pools_data %>% filter(Year == 2014))$Inf_Mosq_Per_1000, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2014)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2014-01-01")
    
  } else if (Year == 2013) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(c(as.data.frame(mosq_pools_agg %>% filter(Year == 2013))$Tot_Mosq_Abund, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(c(as.data.frame(mosq_pools_data %>% filter(Year == 2013))$Inf_Mosq_Per_1000, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2013)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2013-01-01")
    
  } else if (Year == 2012) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(as.data.frame(mosq_pools_agg %>% filter(Year == 2012))$Tot_Mosq_Abund)
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(as.data.frame(mosq_pools_data %>% filter(Year == 2012))$Inf_Mosq_Per_1000)
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2012)
    # NOTE: 2012 does NOT drop first row (matches your original code)
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2012-01-01")
    
  } else if (Year == 2011) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(c(as.data.frame(mosq_pools_agg %>% filter(Year == 2011))$Tot_Mosq_Abund, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(c(as.data.frame(mosq_pools_data %>% filter(Year == 2011))$Inf_Mosq_Per_1000, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2011)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2011-01-01")
    
  } else if (Year == 2010) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(as.data.frame(mosq_pools_agg %>% filter(Year == 2010))$Tot_Mosq_Abund)
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(as.data.frame(mosq_pools_data %>% filter(Year == 2010))$Inf_Mosq_Per_1000)
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2010)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2010-01-01")
    
  } else if (Year == 2009) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(as.data.frame(mosq_pools_agg %>% filter(Year == 2009))$Tot_Mosq_Abund)
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(as.data.frame(mosq_pools_data %>% filter(Year == 2009))$Inf_Mosq_Per_1000)
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2009)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2009-01-01")
    
  } else if (Year == 2008) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(as.data.frame(mosq_pools_agg %>% filter(Year == 2008))$Tot_Mosq_Abund)
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(as.data.frame(mosq_pools_data %>% filter(Year == 2008))$Inf_Mosq_Per_1000)
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2008)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2008-01-01")
    
  } else if (Year == 2007) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(c(as.data.frame(mosq_pools_agg %>% filter(Year == 2007))$Tot_Mosq_Abund, 0, 0))
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(c(as.data.frame(mosq_pools_data %>% filter(Year == 2007))$Inf_Mosq_Per_1000, 0, 0))
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2007)
    X3_obs          <- X3_obs[-1, ]
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2007-01-01")
    
  } else if (Year == 2006) {
    mosq_pools_agg  <- read.csv("./mosq_pools_data.csv")
    X_obs           <- as.numeric(as.data.frame(mosq_pools_agg %>% filter(Year == 2006))$Tot_Mosq_Abund)
    mosq_pools_data <- read.csv("./mosq_pools_data.csv")
    X2_obs          <- as.numeric(as.data.frame(mosq_pools_data %>% filter(Year == 2006))$Inf_Mosq_Per_1000)
    WNV             <- read.csv("./WNV_humans_summary3_2006-2017.csv")
    X3_obs          <- WNV %>% filter(YEAR == 2006)
    # NOTE: 2006 does NOT drop first row (matches your original code)
    X3_obs          <- cumsum(as.numeric(X3_obs$cases))
    training_start_date <- as.Date("2006-01-01")
    
  } else {
    stop("Year must be one of: 2006-2019 or 2021")
  }
  
  # ---- Pad / trim all obs vectors to exactly 52 weeks ----
  pad52 <- function(x) {
    if (length(x) < 52) x <- c(x, rep(0, 52 - length(x)))
    x[1:52]
  }
  X_obs1 <- pad52(X_obs)
  X_obs2 <- pad52(X2_obs)
  X0_obs <- pad52(X3_obs)
  
  # ---- Date vectors (identical to your original code) ----
  all_weeks            <- seq(training_start_date, by = "weeks", length.out = 52)
  forecast_1week_dates <- all_weeks[6:51]
  forecast_2week_dates <- all_weeks[7:52]
  
  list(
    X_obs1               = X_obs1,
    X_obs2               = X_obs2,
    X0_obs               = X0_obs,
    all_weeks            = all_weeks,
    forecast_1week_dates = forecast_1week_dates,
    forecast_2week_dates = forecast_2week_dates
  )
}

# ============================================================
# PLOT BUILDERS  (return single ggplot, no legend)
# ============================================================

# ---- FORECAST plot (1-wk or 2-wk ahead) --------------------

make_forecast_plot <- function(Year, target, horizon, d, apply_ylim) {
  
  rds_file <- paste0("results_", model_name, "_", Year, ".rds")
  if (!file.exists(rds_file)) { message("Missing: ", rds_file); return(NULL) }
  results  <- readRDS(rds_file)
  
  q_key   <- paste0(target, "_q_", horizon)
  fdates  <- if (horizon == 1) d$forecast_1week_dates else d$forecast_2week_dates
  obs_idx <- if (horizon == 1) 6:51                   else 7:52
  obs_vec <- switch(target,
                    total_abundance     = d$X_obs1,
                    infectious_per_1000 = d$X_obs2,
                    human_cases         = d$X0_obs)
  
  plot_df <- map_dfr(seq_along(results), function(i) {
    qs <- results[[i]][[q_key]]
    if (is.null(qs)) return(NULL)
    data.frame(
      Date     = fdates[i],
      Lower_90 = qs[["5%"]],  Upper_90 = qs[["95%"]],
      Lower_80 = qs[["10%"]], Upper_80 = qs[["90%"]],
      Lower_50 = qs[["25%"]], Upper_50 = qs[["75%"]],
      Median   = qs[["50%"]]
    )
  })
  
  df_obs <- data.frame(Date     = fdates,
                       Observed = obs_vec[obs_idx])
  
  ylab <- c(total_abundance     = "Total Abundance",
            infectious_per_1000 = "Inf. Mosq. per 1000",
            human_cases         = "Human Cases")[target]
  
  p <- ggplot(plot_df, aes(x = as.Date(Date))) +
    geom_linerange(aes(ymin = Lower_90, ymax = Upper_90, color = "90% CI"),
                   linewidth = 1.0, alpha = 0.45) +
    geom_linerange(aes(ymin = Lower_80, ymax = Upper_80, color = "80% CI"),
                   linewidth = 1.6, alpha = 0.55) +
    geom_linerange(aes(ymin = Lower_50, ymax = Upper_50, color = "50% CI"),
                   linewidth = 2.2, alpha = 0.70) +
    geom_point(aes(y = Median, color = "Median forecast"), size = 1.0) +
    geom_line(data = df_obs,
              aes(x = as.Date(Date), y = Observed, color = "Observed"),
              linewidth = 0.7) +
    scale_color_manual(
      name   = NULL,
      values = c("90% CI"         = "#BDD7EE",
                 "80% CI"         = "#5B9BD5",
                 "50% CI"         = "#1F4E79",
                 "Median forecast"= "#08306b",
                 "Observed"       = "red"),
      guide  = guide_legend(
        override.aes = list(
          linetype  = c("solid","solid","solid","solid","solid"),
          linewidth = c(1.0, 1.6, 2.2, 0.5, 0.7),
          shape     = c(NA, NA, NA, 16, NA)
        )
      )
    ) +
    labs(x = NULL, y = ylab, title = as.character(Year)) +
    theme_minimal(base_size = 9) +
    theme(plot.title       = element_text(face = "bold", size = 17, hjust = 0.5),
          axis.text.x      = element_text(angle = 45, hjust = 1, size = 17),
          axis.text.y      = element_text(size = 17),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.position  = "none",
          panel.grid.minor = element_blank())
  
  if (apply_ylim) p <- p + scale_y_continuous(limits = y_limits_forecast[[target]])
  p
}

# ---- fan chart plot --------------------------------

make_fansight_plot <- function(Year, target, d, apply_ylim,
                               plot_every_n = 4) {
  
  rds_file <- paste0("results_", model_name, "_", Year, ".rds")
  if (!file.exists(rds_file)) { message("Missing: ", rds_file); return(NULL) }
  results  <- readRDS(rds_file)
  
  f1 <- d$forecast_1week_dates
  f2 <- d$forecast_2week_dates
  obs_vec <- switch(target,
                    total_abundance     = d$X_obs1,
                    infectious_per_1000 = d$X_obs2,
                    human_cases         = d$X0_obs)
  
  q_levels <- c(0.01,0.025,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,
                0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,
                0.95,0.975,0.99)
  
  iter_plot <- seq(1, length(results), by = plot_every_n)
  fan_rows  <- list()
  
  for (i in iter_plot) {
    for (h in c(1, 2)) {
      qs <- results[[i]][[paste0(target, "_q_", h)]]
      if (is.null(qs) || all(is.na(qs))) next
      get_q <- function(p) { idx <- which(abs(q_levels - p) < 1e-9); if (!length(idx)) NA else as.numeric(qs[idx]) }
      fan_rows[[length(fan_rows) + 1]] <- data.frame(
        origin  = f1[i] - 7, iter = i, horizon = h,
        tdate   = if (h == 1) f1[i] else f2[i],
        lo90 = get_q(0.05), hi90 = get_q(0.95),
        lo80 = get_q(0.10), hi80 = get_q(0.90),
        lo50 = get_q(0.25), hi50 = get_q(0.75),
        med  = get_q(0.50)
      )
    }
  }
  if (!length(fan_rows)) return(NULL)
  fan_df <- bind_rows(fan_rows)
  
  fan_wide <- fan_df %>%
    group_by(origin, iter) %>%
    filter(n() == 2) %>%
    summarise(
      date_h1 = tdate[horizon==1],  date_h2 = tdate[horizon==2],
      lo90_h1 = lo90[horizon==1],   hi90_h1 = hi90[horizon==1],
      lo90_h2 = lo90[horizon==2],   hi90_h2 = hi90[horizon==2],
      lo80_h1 = lo80[horizon==1],   hi80_h1 = hi80[horizon==1],
      lo80_h2 = lo80[horizon==2],   hi80_h2 = hi80[horizon==2],
      lo50_h1 = lo50[horizon==1],   hi50_h1 = hi50[horizon==1],
      lo50_h2 = lo50[horizon==2],   hi50_h2 = hi50[horizon==2],
      med_h1  = med[horizon==1],    med_h2  = med[horizon==2],
      .groups = "drop"
    ) %>% filter(!is.na(date_h2))
  
  make_rib <- function(df, lo1, hi1, lo2, hi2, lv)
    bind_rows(
      df %>% transmute(origin, iter, date = date_h1,
                       ymin = !!sym(lo1), ymax = !!sym(hi1), level = lv),
      df %>% transmute(origin, iter, date = date_h2,
                       ymin = !!sym(lo2), ymax = !!sym(hi2), level = lv)
    ) %>% arrange(origin, iter, date)
  
  r90 <- make_rib(fan_wide,"lo90_h1","hi90_h1","lo90_h2","hi90_h2","90% PI")
  r80 <- make_rib(fan_wide,"lo80_h1","hi80_h1","lo80_h2","hi80_h2","80% PI")
  r50 <- make_rib(fan_wide,"lo50_h1","hi50_h1","lo50_h2","hi50_h2","50% PI")
  med_long <- bind_rows(
    fan_wide %>% transmute(origin, iter, date = date_h1, med = med_h1),
    fan_wide %>% transmute(origin, iter, date = date_h2, med = med_h2)
  ) %>% arrange(origin, iter, date)
  
  obs_df <- data.frame(date   = d$all_weeks[1:length(obs_vec)],
                       actual = obs_vec)
  ylab <- c(total_abundance     = "Total Abundance",
            infectious_per_1000 = "Inf. Mosq. per 1000",
            human_cases         = "Human Cases")[target]
  
  p <- ggplot() +
    geom_ribbon(data = r90,
                aes(x = date, ymin = ymin, ymax = ymax,
                    group = interaction(origin, iter), fill = "90% CI"), alpha = 0.40) +
    geom_ribbon(data = r80,
                aes(x = date, ymin = ymin, ymax = ymax,
                    group = interaction(origin, iter), fill = "80% CI"), alpha = 0.55) +
    geom_ribbon(data = r50,
                aes(x = date, ymin = ymin, ymax = ymax,
                    group = interaction(origin, iter), fill = "50% CI"), alpha = 0.70) +
    geom_line(data = med_long,
              aes(x = date, y = med, group = interaction(origin, iter),
                  color = "Median forecast"),
              linewidth = 0.5, alpha = 0.85) +
    geom_point(
      data = med_long,
      aes(x = date, y = med, group = interaction(origin, iter)),
      color = "black", size = 1.5, alpha = 0.9)+
    geom_line(data = obs_df, aes(x = date, y = actual, color = "Observed"),
              linewidth = 0.7) +
    geom_point(data = obs_df,
               aes(x = date, y = actual),
               color = "red", size = 1.5)+
    scale_fill_manual(name   = NULL,
                      values = c("90% CI" = "#BDD7EE",
                                 "80% CI" = "#5B9BD5",
                                 "50% CI" = "#1F4E79")) +
    scale_color_manual(name  = NULL,
                       values = c("Observed"        = "red",
                                  "Median forecast" = "black")) +
    labs(x = NULL, y = ylab, title = as.character(Year)) +
    theme_minimal(base_size = 9) +
    theme(plot.title       = element_text(face = "bold", size = 17, hjust = 0.5),
          axis.text.x      = element_text(angle = 45, hjust = 1, size = 17),
          axis.text.y      = element_text(size = 17),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.position  = "none",
          panel.grid.minor = element_blank())
  
  if (apply_ylim) p <- p + scale_y_continuous(limits = y_limits_forecast[[target]])
  p
}

# ============================================================
# LEGEND EXTRACTOR
# Builds one "reference" plot with legend visible, extracts grob
# ============================================================

extract_shared_legend <- function(plot_type, target) {
  
  # Dummy data for a minimal reference plot
  df_dummy <- data.frame(
    Date     = seq(as.Date("2014-01-01"), by = "week", length.out = 10),
    q05 = 1, q10 = 2, q25 = 3, q50 = 5, q75 = 7, q90 = 8, q95 = 9,
    Observed = 5,
    Lower_90 = 1, Upper_90 = 9,
    Lower_80 = 2, Upper_80 = 8,
    Lower_50 = 3, Upper_50 = 7,
    Median   = 5
  )
  
  if (plot_type %in% c("forecast_1wk", "forecast_2wk")) {
    ref_p <- ggplot(df_dummy, aes(x = Date)) +
      geom_linerange(aes(ymin = Lower_90, ymax = Upper_90, color = "90% CI"), linewidth = 1.0) +
      geom_linerange(aes(ymin = Lower_80, ymax = Upper_80, color = "80% CI"), linewidth = 1.6) +
      geom_linerange(aes(ymin = Lower_50, ymax = Upper_50, color = "50% CI"), linewidth = 2.2) +
      geom_point(aes(y = Median,   color = "Median forecast"), size = 2) +
      geom_line(aes(y = Observed,  color = "Observed"),        linewidth = 0.7) +
      scale_color_manual(
        name   = NULL,
        values = c("90% CI"          = "#BDD7EE",
                   "80% CI"          = "#5B9BD5",
                   "50% CI"          = "#1F4E79",
                   "Median forecast" = "#08306b",
                   "Observed"        = "red")
      ) +
      theme_minimal() + theme(legend.position = "bottom",
                              legend.text = element_text(size = 17))
    
  } else {  # fansight
    ref_p <- ggplot(df_dummy, aes(x = Date)) +
      geom_ribbon(aes(ymin = Lower_90, ymax = Upper_90, fill = "90% CI"), alpha = 0.40) +
      geom_ribbon(aes(ymin = Lower_80, ymax = Upper_80, fill = "80% CI"), alpha = 0.55) +
      geom_ribbon(aes(ymin = Lower_50, ymax = Upper_50, fill = "50% CI"), alpha = 0.70) +
      geom_line(aes(y = Observed,  color = "Observed"),         linewidth = 0.7) +
      geom_line(aes(y = Median,    color = "Median forecast"),  linewidth = 0.7) +
      scale_fill_manual(name   = NULL,
                        values = c("90% CI" = "#BDD7EE",
                                   "80% CI" = "#5B9BD5",
                                   "50% CI" = "#1F4E79")) +
      scale_color_manual(name  = NULL,
                         values = c("Observed"        = "red",
                                    "Median forecast" = "black")) +
      theme_minimal() + theme(legend.position = "bottom",
                              legend.text = element_text(size = 17))
  }
  
  tmp  <- ggplot_gtable(ggplot_build(ref_p))
  idx  <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  tmp$grobs[[idx]]
}

# ============================================================
# MASTER PANEL BUILDER
# plot_type:  "forecast_1wk" | "forecast_2wk" | "fansight"
# ============================================================

build_panel <- function(target, plot_type, save_dir = ".") {
  
  all_plots <- list()
  
  for (yr in years) {
    message("  Year ", yr)
    apply_ylim <- (yr != 2021)          # 2021 has no y-axis limit
    
    # --- Load observation data using your exact per-year logic ---
    d <- tryCatch(
      load_year_data(yr),
      error = function(e) { message("  Data load failed for ", yr, ": ", e$message); NULL }
    )
    if (is.null(d)) { all_plots[[as.character(yr)]] <- NULL; next }
    
    # --- Build the appropriate plot ---
    p <- switch(plot_type,
                forecast_1wk = make_forecast_plot(yr, target, 1, d, apply_ylim),
                forecast_2wk = make_forecast_plot(yr, target, 2, d, apply_ylim),
                fansight     = make_fansight_plot(yr, target, d, apply_ylim, plot_every_n = 4)
    )
    all_plots[[as.character(yr)]] <- p
  }
  
  valid_plots <- Filter(Negate(is.null), all_plots)
  if (!length(valid_plots)) { message("No plots for ", target, " ", plot_type); return(invisible(NULL)) }
  
  # ---- Shared legend (built from dummy data, not from real plots) ----
  shared_legend <- extract_shared_legend(plot_type, target)
  
  # ---- Figure title ----
  tgt_label <- c(total_abundance     = "Total Mosquito Abundance",
                 infectious_per_1000 = "Infectious Mosquitoes per 1000",
                 human_cases         = "Human WNV Cases")[target]
  pt_label  <- c(forecast_1wk = "1-Week-Ahead Forecast",
                 forecast_2wk = "2-Week-Ahead Forecast",
                 fansight     = "2-week ahead forecasts at each forecast origin")[plot_type]
  
  panel_grid <- gridExtra::arrangeGrob(
    grobs = valid_plots,
    ncol  = 5,
    nrow  = 3,
    top   = grid::textGrob(
      label = paste0(tgt_label, "  —  ", pt_label, "  |  ", model_name),
      gp    = grid::gpar(fontsize = 17, fontface = "bold")
    )
  )
  
  final_fig <- gridExtra::arrangeGrob(
    panel_grid,
    shared_legend,
    ncol    = 1,
    heights = grid::unit(c(20, 1.2), c("null", "cm"))
  )
  
  fname <- file.path(save_dir,
                     paste0("panel_", model_name, "_", target, "_", plot_type, ".pdf"))
  ggsave(fname, plot = final_fig,
         width = 32, height = 24, dpi = 300, bg = "white")
  message("  Saved: ", fname)
  invisible(final_fig)
}

# ============================================================
# RUN ALL 12 COMBINATIONS  (3 targets x 4 plot types)
# ============================================================

targets    <- c("total_abundance", "infectious_per_1000", "human_cases")
plot_types <- c("fit", "forecast_1wk", "forecast_2wk", "fansight")

save_dir <- "./figures"          # change to your preferred output folder
dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

for (tgt in targets) {
  for (pt in plot_types) {
    message("\n=== Building panel: ", tgt, "  |  ", pt, " ===")
    tryCatch(
      build_panel(target = tgt, plot_type = pt, save_dir = save_dir),
      error = function(e) message("  ERROR: ", e$message)
    )
  }
}

message("\nAll 12 panels complete.")
years <- c(2006:2019, 2021)
model_files <- list(
  "FullModel"            = "wis_all_FullModel_",
  "FullModel_NoWeather"   = "wis_all_FullModel_NoClimate_",
  "Mosq+Human+Weather"    = "wis_all_Mosq+Human+Climate_",
  "Mosq+Human+NoWeather"  = "wis_all_Mosq+Human+NoClimate_"
)

# ── Build per-iteration model WIS with forecast date ───────────────────────────
model_wis_monthly <- map_dfr(names(model_files), function(model_name) {
  map_dfr(years, function(yr) {
    fpath <- paste0(model_files[[model_name]], yr, ".rds")
    if (!file.exists(fpath)) return(NULL)
    lis <- readRDS(fpath)
    
    bind_rows(
      lis$abundance_1wk %>% mutate(horizon = 1, target = "Total abundance"),
      lis$abundance_2wk %>% mutate(horizon = 2, target = "Total abundance"),
      lis$infected_1wk  %>% mutate(horizon = 1, target = "Infectious mosq per 1000"),
      lis$infected_2wk  %>% mutate(horizon = 2, target = "Infectious mosq per 1000"),
      lis$cases_1wk     %>% mutate(horizon = 1, target = "Human cases"),
      lis$cases_2wk     %>% mutate(horizon = 2, target = "Human cases")
    ) %>%
      mutate(
        year          = yr,
        model         = model_name,
        forecast_week = if_else(horizon == 1, iteration + 5, iteration + 6),
        forecast_date = as.Date(paste0(yr, "-01-01")) + (forecast_week - 1) * 7
      ) %>%
      select(year, model, horizon, target, iteration, WIS, forecast_date)
  })
})

# ── Join with baseline WIS per same iteration (reuse your existing baseline_all) ──
baseline_join <- baseline_all %>%
  select(year, horizon, target, iteration, WIS_baseline = WIS)
saveRDS(baseline_join, "baseline_joinWIS.rds")   
relwis_monthly <- model_wis_monthly %>%
  left_join(baseline_join, by = c("year", "horizon", "target", "iteration")) %>%
  filter(is.finite(WIS), is.finite(WIS_baseline), WIS_baseline > 0) %>%
  mutate(
    rel_wis = log(WIS / WIS_baseline),
    month   = month(forecast_date, label = TRUE, abbr = TRUE),
    season  = case_when(
      month %in% c("Jan","Feb","Mar") ~ "Winter",
      month %in% c("Apr","May","Jun") ~ "Spring",
      month %in% c("Jul","Aug","Sep") ~ "Summer",
      month %in% c("Oct","Nov","Dec") ~ "Fall"
    )
  )

# ── Median relative WIS by model x target x season ──────────────────────────────
season_summary <- relwis_monthly %>%
  group_by(model, target, season) %>%
  summarise(median_relWIS = median(rel_wis, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(target, model, season)
print(season_summary, n = 200)

# ── Median relative WIS by model x target x month (finer detail if needed) ─────
monthly_summary <- relwis_monthly %>%
  group_by(model, target, month) %>%
  summarise(median_relWIS = median(rel_wis, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(target, model, month)
print(monthly_summary, n = 200)
