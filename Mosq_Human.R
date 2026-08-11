library(tidyr)
library(scales)
library(tidyverse)
library(readr)
library(readxl)
library(mgcv)
library(ciTools)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(MASS)
library(purrr)
library(dplyr)
library(tidyr)
library(purrr)
library(epipredict)
library(distributional)
library(grid)
library(tibble)
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
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
    X3_obs <- cumsum(X3_obs)
    
    # Universal dates for 2006
    training_start_date <- as.Date("2006-01-01")
    fitting_start_date <- as.Date("2006-02-01")
    date_gam <- seq(as.Date("2006-01-01"), as.Date("2006-12-28"), by = "week")
    all_weeks <- seq(training_start_date, by = "weeks", length.out = 52)
  
}else {
  stop("Year must be one of: 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 or 2021")
}

# Common observation setup
X0_obs <- X3_obs
X_obs1 <- X_obs
X_obs2 <- X2_obs

# Forecast date setup (derived from all_weeks)
observed_dates <- all_weeks
forecast_1week_dates <- all_weeks[6:51]
forecast_2week_dates <- all_weeks[7:52]
# Model Parameters
time_initial <- 1
time_final <- 52
dt <- 1
time_vector <- seq(time_initial, time_final, by = 1)
N <- 8000  # Number of ensemble
mu1 <- 1/120 
mu <- 0.05 
i_year <- c(
  "2006" = 100,
  "2007" = 100,
  "2008" = 100,
  "2009" = 100,
  "2010" = 100,
  "2011" = 100,
  "2012" = 25,
  "2013" = 25,
  "2014" = 100,
  "2015" = 50,
  "2016" = 50,
  "2017" = 50,
  "2018" = 100,
  "2019" = 50,
  "2020" = 50,
  "2021" = 50
)


brim <- vector("numeric", length = 365)
for(i in 1:365){
  if(i >= as.numeric(i_year[as.character(Year)]) & i <= 180) brim[i] = 0.01
}


# OU Process func

OUproc_func = function(this_prev, mu, lambda, sigma, dt=1){
  # Random variate
  temp_rand = rnorm(length(this_prev))
  
  # Calc next value
  this_next = exp(
    log(this_prev) * exp(-lambda * dt) -
      mu * (exp(-lambda * dt) - 1) + 
      sigma * sqrt(1 - exp(-2 * lambda * dt)) * temp_rand
  )
  
  return(this_next)
}


# OU params
ou_mu_vm = log(1.0) # log scale
ou_lambda_vm = 1 / 14

sigma_vm_year <- c(
  "2006" = 1.10,
  "2007" = 1.50,
  "2008" = 1.10,
  "2009" = 1.50,
  "2010" = 1.10,
  "2011" = 1.50,
  "2012" = 1.50,
  "2013" = 1.10,
  "2014" = 1.10,
  "2015" = 1.10,
  "2016" = 1.10,
  "2017" = 1.10,
  "2018" = 1.50,
  "2019" = 1.50,
  "2020" = 3.10,
  "2021" = 2.00
)
ou_sigma_vm = as.numeric(sigma_vm_year[as.character(Year)]) 


ou_lambda_r = 1 / 14;

# Function to adjust temperature based on parameters
inputTem <- function(time_points, par, inputT) {
  temp <- inputT[time_points]
  if (is.na(temp) || temp < par[1] || temp > par[2]) {
    return(par[1])
  } else {
    return(temp)
  }
}

# Function to return precipitation
inputP <- function(time_points, inputP) {
  return(inputP[time_points])
}



WNV_model <- function(sirWNV_ensemble, Vm_t, r_t, f_t, T_temp, P_temp, par_input, week) {
  # Extract state variables
  
  #if(week %in% c(26)){yes_debug = 1}else{yes_debug = 0}
  yes_debug = 0
  
  Sm <- sirWNV_ensemble[1, ]
  Im <- sirWNV_ensemble[2, ]
  Sh <- sirWNV_ensemble[3, ]
  Eh <- sirWNV_ensemble[4, ]
  Ih <- sirWNV_ensemble[5, ]
  
  if(yes_debug){
    cat("Im_inside", Im, "\n")
  }
  # Parameters
  par <- c(Tmi = par_input[1], Tma = par_input[2], alpha = par_input[3], phi = par_input[4],
           Tmh = par_input[5], psi = par_input[6])
  
  # Get week indices
  week_correct <- week - 1
  indx1 <- 7 * week_correct + 1
  indx2 <- indx1 + 6
  indx_range <- c(indx1:indx2)
  
  # Check if T_temp and P_temp are already 7-element vectors or full vectors
  if (length(T_temp) == 7 && length(P_temp) == 7) {
    # FORECASTING MODE: Already have the 7 values we need
    T_temp_week <- T_temp
    P_temp_week <- P_temp
    #cat("Using pre-computed 7-day T_temp:", T_temp_week, "\n")
  } else {
    # FITTING MODE: Extract the 7 days from full vectors
    T_temp_week <- as.numeric(sapply(indx_range, function(t) {
      if (t > length(T_temp)) {
        return(par["Tmi"])  # Default if out of bounds
      }
      inputTem(t, par, T_temp)
    }))
    P_temp_week <- as.numeric(sapply(indx_range, function(t) {
      if (t > length(P_temp)) {
        return(0)  # Default if out of bounds
      }
      inputP(t, P_temp)
    }))
  }
  
  # Calculate t_temp and brim_temp for this week
  t_temp <- ((indx_range - 1) %% 365) + 1
  
  # Handle brim_temp safely
  if (indx1 <= 365 && indx2 <= 365) {
    brim_temp <- brim[indx1:indx2]
  } else {
    brim_temp <- rep(0, 7)  # Default if out of bounds
  }
  
  if(yes_debug){
    cat("brim_inside:", brim_temp[1], "\n")
  }
  
  # Daily iteration within the week
  for (i in 1:7) {
    if(yes_debug){
      cat("day:", i, "\n")
    }
   
    Nh <- ifelse((Sh + Eh + Ih < 0), 0, (Sh + Eh + Ih))
    
    # Handle NA values
    
    Nh <- ifelse(is.na(Nh), 0, Nh)
    
    temp_growth1 <- r_t[i] * Sm * f_t[i]
    
    
    if(yes_debug){
      cat("growth1_inside", temp_growth1, "\n")
      cat("rt_inside", r_t[i], "\n")
    }
    
    
    dSm <- (Vm_t[i] * (-(T_temp_week[i] - par["Tmi"]) * (T_temp_week[i] - par["Tma"])) *
              1 / (1 + exp(par["alpha"] - par["phi"] * P_temp_week[i])) -
              temp_growth1 - mu * Sm) * dt
    
    dIm <- (temp_growth1 - mu * Im + brim_temp[i]) * dt
    if(yes_debug){
      cat("dIm_inside", dIm, "\n")
    }
    
    # Human dynamics
    if (is.na(Nh) || Nh == 0) {
      dSh <- 0
      dEh <- 0
    } else {
      dSh <- (-r_t[i] * par["Tmh"] * Im * Sh / Nh) * dt
      dEh <- (r_t[i] * par["Tmh"] * Im * Sh / Nh - par["psi"] * Eh) * dt
    }
    dIh <- (par["psi"] * Eh) * dt
    
    # Update state variables (ensure non-negative)
    Sm <- pmax(Sm + dSm, 0)
    Im <- pmax(Im + dIm, 0)
    Sh <- pmax(Sh + dSh, 0)
    Eh <- pmax(Eh + dEh, 0)
    Ih <- pmax(Ih + dIh, 0)
    
    if(yes_debug){
      cat("Im_inside_after", Im, "\n")
    }
    
    # Save to ensemble
    sirWNV_ensemble[1, ] <- Sm
    sirWNV_ensemble[2, ] <- Im
    sirWNV_ensemble[3, ] <- Sh
    sirWNV_ensemble[4, ] <- Eh
    sirWNV_ensemble[5, ] <- Ih
  }
  
  return(sirWNV_ensemble)
}

pop <- read.csv("./maricopa_population_2006-2021.csv")

results <- list()

num_iterations <- 46 
# Updated observation noise covariance 
R <- diag(c(5.0, 0.001, 0.05))

# ====================================================================
# Initialize storage OUTSIDE the iteration loop (GLOBAL)
# ====================================================================
total_time_points <- num_iterations + 4

# Storage for ensemble outputs
save_vm_ensemble_global <- matrix(0, nrow = N, ncol = total_time_points)
save_rt_ensemble_global <- matrix(0, nrow = N, ncol = total_time_points)
save_ft_ensemble_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted2_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted3_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted_Sm_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted_Im_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted_Sh_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted_Eh_global <- matrix(0, nrow = N, ncol = total_time_points)
predicted_Ih_global <- matrix(0, nrow = N, ncol = total_time_points)

save_log_vm_ensemble_global <- matrix(0, nrow = N, ncol = total_time_points)
save_log_rt_ensemble_global <- matrix(0, nrow = N, ncol = total_time_points)
save_log_ft_ensemble_global <- matrix(0, nrow = N, ncol = total_time_points)
# Full ensemble storage (22 state variables + parameters)
save_ensemble_full_global <- array(0, dim = c(14, N, total_time_points))
static2_global <- array(0, dim = c(6, N, total_time_points))
static_global <- matrix(0, nrow = 6, ncol = N)
# Diagnostics
scale_factor_history <- numeric(total_time_points)
sigma_t_history <- numeric(total_time_points)

# ====================================================================
# Initialize ensemble ONCE before the loop
# ====================================================================
Sm_init <- 7
Im_init <- 0
Ib_init <- 0
Rb_init <- 0
Sb_init <- 0
Nh <- as.numeric(pop %>% 
                   filter(year == Year) %>% 
                   pull(population))
Eh_init <- 0
Ih_init <- 0
Sh_init <- Nh - Eh_init - Ih_init

# State variables
Sm_init_particles <- rep(Sm_init, N)
Im_init_particles <- rep(Im_init, N)

Sh_init_particles <- rep(Sh_init, N)
Eh_init_particles <- rep(Eh_init, N)
Ih_init_particles <- rep(Ih_init, N)

# Time-varying parameters (log-transformed)
Vm_t_particles <- runif(N, min = 0.0001, max = 7)
#Vm_t_particles <- runif(N, min = 27.0001, max = 37)
log_Vm_t_particles <- log(Vm_t_particles)
r_t_particles <- runif(N, min = 0.0001, max = 0.001)
log_r_t_particles <- log(r_t_particles)
f_t_particles <-runif(N, min = 0, max = 0.001)
log_f_t_particles <- log(f_t_particles)
# Static parameters
Tmi_particles <- runif(N, min = 17, max = 20)
Tma_particles <- runif(N, min = 42, max = 48)
alpha_particles <- runif(N, min = 0.7, max = 1.8)
phi_particles <- runif(N, min = 0.95, max = 1.8)


Tmh_particles <- runif(N, min = 0.0140, max = 0.055)
psi_particles <- runif(N, min = 0.05, max = 0.14)

# Build ensemble matrix 
ensemble <- rbind(Sm_init_particles, Im_init_particles,
                  Sh_init_particles, Eh_init_particles, Ih_init_particles, 
                  log_Vm_t_particles,log_r_t_particles, log_f_t_particles, Tmi_particles,Tma_particles, alpha_particles, phi_particles,
                  Tmh_particles, psi_particles)

param_names <- c("Tmi", "Tma", "alpha", "phi", "Tmh", "psi")


obs_year <- c(
  "2006" = 18,
  "2007" = 18,
  "2008" = 18,
  "2009" = 18,
  "2010" = 18,
  "2011" = 18,
  "2012" = 5,
  "2013" = 5,
  "2014" = 18,
  "2015" = 10,
  "2016" = 10,
  "2017" = 10,
  "2018" = 18,
  "2019" = 10,
  "2020" = 10,
  "2021" = 10
)
# Daily OU storage: 350 days = 50 weeks x 7 days
total_days <- (num_iterations + 4) * 7   # 50 x 7 = 350

save_vm_daily_global <- matrix(0, nrow = N, ncol = total_days)
save_rt_daily_global <- matrix(0, nrow = N, ncol = total_days)
save_ft_daily_global <- matrix(0, nrow = N, ncol = total_days)
for (iteration in 1:num_iterations) {
  print(paste("Iteration:", iteration))
  print(paste("In the fit section"))
  # Determine which observations to process
  if (iteration == 1) {
    obs_indices <- 1:5
  } else {
    obs_indices <- (iteration + 4)
  }
  
  # Get training data up to this point
  X_obs1_train <- X_obs1[1:(iteration + 4)]
  X_obs2_train <- X_obs2[1:(iteration + 4)]
  X0_obs_train <- X0_obs[1:(iteration + 4)]
  
  # Fit EnKF for the current training window
  for (obs_index in obs_indices) {
    #t_index =1
    week <- ceiling(obs_index)  # Calculate week number
    # === Generate daily Vm_t and r_t for the current week (7 days) ===
    current_Vm <- exp(ensemble[6, ])      # N ensemble members
    current_rt <- exp(ensemble[7, ])
    current_ft <- exp(ensemble[8, ])
    Vm_daily <- matrix(NA, nrow = N, ncol = 7)   # rows = ensemble, cols = days
    rt_daily  <- matrix(NA, nrow = N, ncol = 7)
    ft_daily  <- matrix(NA, nrow = N, ncol = 7)
    for (day in 1:7) {
      # Daily OU step for Vm
      current_Vm <- OUproc_func(current_Vm,
                                mu = ou_mu_vm,
                                lambda = ou_lambda_vm,
                                sigma = ou_sigma_vm,
                                dt = 1)
      Vm_daily[, day] <- current_Vm
      
      # Daily OU step for r_t (adaptive sigma)
      if (obs_index < as.numeric(obs_year[as.character(Year)])) {
        ou_sigma_r_day <- 0.002
        ou_mu_r_day    <- log(0.001)
      } else {
        ou_sigma_r_day <- 1.50
        ou_mu_r_day    <- log(0.05)
      }
      sigma_t_history[obs_index] <- ou_sigma_r_day
      current_rt <- OUproc_func(current_rt,
                                mu = ou_mu_r_day,
                                lambda = ou_lambda_r,
                                sigma = ou_sigma_r_day,
                                dt = 1)
      rt_daily[, day] <- current_rt
      current_ft <- OUproc_func(current_ft,
                                mu = ou_mu_r_day,
                                lambda = ou_lambda_r,
                                sigma = ou_sigma_r_day,
                                dt = 1)
      ft_daily[, day] <- current_ft
      # ── Store daily values into global matrices ──────────────────
      # Global day index: each obs_index covers 7 days
      day_global <- (obs_index - 1) * 7 + day
      save_vm_daily_global[, day_global] <- current_Vm
      save_rt_daily_global[, day_global] <- current_rt
      save_ft_daily_global[, day_global] <- current_ft
    }
    
    # Put the **last** daily value back into the ensemble for storage/Kalman update
    ensemble[6, ]  <- log(current_Vm)
    ensemble[7, ] <- log(current_rt)
    ensemble[8, ] <- log(current_ft)
    # Forecast step
    for (i in 1:N) {
      
     # par <- as.numeric(ensemble[9:14, i])
      Vm_vec <- Vm_daily[i, ]      # length 7
      rt_vec <- rt_daily[i, ]      # length 7
      ft_vec <- ft_daily[i, ]      # length 7
      # Run the WNV model for each ensemble member
      sirWNV_output <- WNV_model(ensemble[1:5, i, drop = FALSE], 
                                 Vm_t = Vm_vec, r_t = rt_vec, f_t = ft_vec, T_temp = inputTem_i, P_temp = inputP_i, par_input = as.numeric(ensemble[9:14, i]), week = week)
      # Update ensemble states
      ensemble[1:5, i] <- sirWNV_output
    }
   
      # Compute ensemble predictions for observed quantities
      tmp_Sm = ensemble[1, ]
      tmp_Im = ensemble[2, ]
      tmp_Ih = ensemble[5, ]
      tmp_Im = ifelse(tmp_Im < 1, 0, tmp_Im)
      tmp_Ih = ifelse(tmp_Ih < 1, 0, tmp_Ih)
      ensemble[1, ] = tmp_Sm
      ensemble[2, ] = tmp_Im
      ensemble[5, ] = tmp_Ih
      scalar_value <- 10
      Sm <- tmp_Sm / scalar_value
      Im <- tmp_Im / scalar_value
      Ih <- tmp_Ih
      # Observations
      Sm_observed <- as.numeric((1 - X_obs2_train[obs_index] / 1000) * 
                                  (X_obs1_train[obs_index] / scalar_value))
      Im_observed <- as.numeric((X_obs2_train[obs_index] / 1000) * 
                                  (X_obs1_train[obs_index] / scalar_value))
      Ih_observed <- as.numeric(X0_obs_train[obs_index])
      observation <- matrix(c(Sm_observed, Im_observed, Ih_observed), ncol = 1)
      
      #print(observation)
      # Forecasted observation mean
      obs_ensemble <- rbind(Sm, Im, Ih)
      obs_mean <- rowMeans(obs_ensemble)
      
      # Covariances
      Pf <- cov(t(ensemble))
      Py <- cov(t(obs_ensemble))
      Pxy <- cov(t(ensemble), t(obs_ensemble))
    
      # Kalman Gain
      R_temp = matrix(0, nrow=3,ncol=3)
      R_temp[1,1] = observation[1,1] * R[1,1]
      R_temp[2,2] = max((observation[2,1] * R[2,2]), R[2,2])
      R_temp[3,3] = max((observation[3,1] * R[3,3]), R[3,3])
      
      K <- Pxy %*% ginv(Py + R_temp)
  
      # Innovation with noise
  
      for (i in 1:N) {
        obs_noise <- mvrnorm(1, mu = c(0, 0, 0), Sigma = R_temp)  # Multivariate normal noise
        ensemble[, i] <- ensemble[, i] + K %*% (observation - obs_ensemble[, i] + obs_noise)
      }
      
      for(k in 1:5){ensemble[k, ] = pmax(ensemble[k, ], 1e-6)}
      for(k in 9:14){ensemble[k, ] = pmax(ensemble[k, ], 1e-6)}
    # Save results
    tmp_Sm = ensemble[1, ]
    tmp_Im = ensemble[2, ]
    tmp_Ih = ensemble[5, ]
    tmp_Im = ifelse(tmp_Im < 1, 0, tmp_Im)
    tmp_Ih = ifelse(tmp_Ih < 1, 0, tmp_Ih)
    ensemble[1, ] = tmp_Sm
    ensemble[2, ] = tmp_Im
    ensemble[5, ] = tmp_Ih
    predicted_global[, obs_index] <- (tmp_Sm + tmp_Im)  # Total abundance
    predicted2_global[, obs_index] <- tmp_Im / (tmp_Sm + tmp_Im) * 1000
    predicted3_global[, obs_index] <- tmp_Ih
    predicted_Sm_global[, obs_index] <- tmp_Sm
    predicted_Sh_global[, obs_index] <- ensemble[3, ]
    predicted_Im_global[, obs_index] <- tmp_Im 
    predicted_Ih_global[, obs_index] <- tmp_Ih
    predicted_Eh_global[, obs_index] <- ensemble[4, ]
    save_vm_ensemble_global[, obs_index] <- exp(ensemble[6, ])        # Vm estimates
    save_rt_ensemble_global[, obs_index] <- exp(ensemble[7, ]) # r estimates
    save_ft_ensemble_global[, obs_index] <- exp(ensemble[8, ]) # f_t estimates
    save_log_vm_ensemble_global[, obs_index] <- ensemble[6, ]        # Vm estimates
    save_log_rt_ensemble_global[, obs_index] <- ensemble[7, ] # r estimates
    save_log_ft_ensemble_global[, obs_index] <- ensemble[8, ] # f_t estimates
    static_global[,] <- ensemble[9:14, ]
    save_ensemble_full_global[, , obs_index] <- ensemble
    static2_global[, , obs_index] <- ensemble[9:14, ]
  }
  if (iteration %in% c(46)) {
    saveRDS(save_ensemble_full_global, file = paste0("save_ensemble_full_global_Mosq+Human+Climate_", Year, ".rds"))
    
  }
  # ============================================================
  # Daily OU parameter plots
  # ============================================================
  
  # Build daily date vector: 350 days starting Jan 1 of Year
  # (matches the 50 weeks x 7 days = 350 days structure)
  date_daily <- seq(
    from = as.Date(paste0(Year, "-01-01")),
    by   = "day",
    length.out = total_days   # 350
  )
  
  # Compute quantiles across ensemble for each day
  vm_daily_q <- apply(
    save_vm_daily_global[, 1:total_days], 2,
    quantile, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
    na.rm = TRUE
  )
  
  rt_daily_q <- apply(
    save_rt_daily_global[, 1:total_days], 2,
    quantile, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
    na.rm = TRUE
  )
  ft_daily_q <- apply(
    save_ft_daily_global[, 1:total_days], 2,
    quantile, probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
    na.rm = TRUE
  )
  df_vm_daily <- data.frame(
    Date = date_daily,
    q05  = vm_daily_q[1, ], q10 = vm_daily_q[2, ], q25 = vm_daily_q[3, ],
    q50  = vm_daily_q[4, ],
    q75  = vm_daily_q[5, ], q90 = vm_daily_q[6, ], q95 = vm_daily_q[7, ]
  )
  
  df_rt_daily <- data.frame(
    Date = date_daily,
    q05  = rt_daily_q[1, ], q10 = rt_daily_q[2, ], q25 = rt_daily_q[3, ],
    q50  = rt_daily_q[4, ],
    q75  = rt_daily_q[5, ], q90 = rt_daily_q[6, ], q95 = rt_daily_q[7, ]
  )
  df_ft_daily <- data.frame(
    Date = date_daily,
    q05  = ft_daily_q[1, ], q10 = ft_daily_q[2, ], q25 = ft_daily_q[3, ],
    q50  = ft_daily_q[4, ],
    q75  = ft_daily_q[5, ], q90 = ft_daily_q[6, ], q95 = ft_daily_q[7, ]
  )
  # ---- Plot Vm_t daily ----
  p_vm_daily <- ggplot(df_vm_daily, aes(x = Date)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#c6dbef", alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90), fill = "#6baed6", alpha = 0.5) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#2171b5", alpha = 0.5) +
    geom_line(aes(y = q50), colour = "#08306b", linewidth = 0.7) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(
      x     = "Date",
      y     = expression(nu[M](t)),
      title = paste0("Daily OU trajectory: Vm_t  |  Year ", Year)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title       = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 18),
      axis.text.y      = element_text(size = 18),
      panel.grid.minor = element_blank()
    )
  
  # ---- Plot r_t daily ----
  p_rt_daily <- ggplot(df_rt_daily, aes(x = Date)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#c6dbef", alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90), fill = "#6baed6", alpha = 0.5) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#2171b5", alpha = 0.5) +
    geom_line(aes(y = q50), colour = "#08306b", linewidth = 0.7) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(
      x     = "Date",
      y     = expression(r(t)),
      title = paste0("Daily OU trajectory: r_t  |  Year ", Year)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title       = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 18),
      axis.text.y      = element_text(size = 18),
      panel.grid.minor = element_blank()
    )
  # ---- Plot f_t daily ----
  p_ft_daily <- ggplot(df_ft_daily, aes(x = Date)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "#c6dbef", alpha = 0.5) +
    geom_ribbon(aes(ymin = q10, ymax = q90), fill = "#6baed6", alpha = 0.5) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#2171b5", alpha = 0.5) +
    geom_line(aes(y = q50), colour = "#08306b", linewidth = 0.7) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(
      x     = "Date",
      y     = expression(f(t)),
      title = paste0("Daily OU trajectory: f_t  |  Year ", Year)
    ) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title       = element_text(face = "bold", size = 18, hjust = 0.5),
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 18),
      axis.text.y      = element_text(size = 18),
      panel.grid.minor = element_blank()
    )
  # ---- Save both as one stacked figure ----
  if (iteration %in% c(46)) {
    ggsave(
      paste0("vm_daily_OU_Mosq+Human+Weather_", Year, ".png"),
      plot   = p_vm_daily,
      width  = 10, height = 5, dpi = 300
    )
    ggsave(
      paste0("rt_daily_OU_Mosq+Human+Weather_", Year, ".png"),
      plot   = p_rt_daily,
      width  = 10, height = 5, dpi = 300
    )
    ggsave(
      paste0("ft_daily_OU_Mosq+Human+Weather_", Year, ".png"),
      plot   = p_ft_daily,
      width  = 10, height = 5, dpi = 300
    )
    # Or save as a single stacked 3-panel figure
    p_combined_daily <- gridExtra::arrangeGrob(
      p_vm_daily, p_rt_daily, p_ft_daily, ncol = 1
    )
    ggsave(
      paste0("vm_rt_ft_daily_OU_Mosq+Human+Weather_", Year, ".png"),
      plot   = p_combined_daily,
      width  = 10, height = 19, dpi = 300
    )
    message("Daily OU plots saved for year ", Year)
  }
  # ====================================================================
  # Post-processing for this iteration
  # ====================================================================
  final_obs_count <- iteration + 4
  dates <- 1:final_obs_count
  date <- date_gam[dates]
  
  # Extract data for this iteration
  Ih_obs <- X0_obs[1:final_obs_count]
  
  # Calculate quantiles
  save_quantiles <- apply(save_vm_ensemble_global[, 1:final_obs_count], 2, quantile, 
                          probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  save_rt_quantiles <- apply(save_rt_ensemble_global[, 1:final_obs_count], 2, quantile, 
                             probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  save_ft_quantiles <- apply(save_ft_ensemble_global[, 1:final_obs_count], 2, quantile, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm=TRUE)
  save_quantiles2 <- apply(predicted_global[, 1:final_obs_count], 2, quantile, 
                           probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  save_quantiles3 <- apply(predicted2_global[, 1:final_obs_count], 2, quantile, 
                           probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  save_quantiles_human <- apply(predicted3_global[, 1:final_obs_count], 2, quantile, 
                                probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  # Diagnostics data frame
  df_diagnostics <- data.frame(
    date = date,
    #scale_factor = scale_factor_history[1:final_obs_count],
    sigma_t = sigma_t_history[1:final_obs_count]
  )
  
  # ====================================================================
  # Plotting
  # ====================================================================
  
  # Plot sigma_t
  p2 <- ggplot(df_diagnostics, aes(x = as.Date(date), y = sigma_t)) +
    geom_line(color = "red", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(#title = paste("Sigma_t over Time - Iteration", iteration),
         x = "Date", y = "Sigma_t") +
    theme_minimal() +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 16))
  
  A1 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles[1, ], 
                    ymax = save_quantiles[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles[2, ], 
                    ymax = save_quantiles[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles[3, ], 
                    ymax = save_quantiles[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_quantiles[4, ]), color = "red") +
    labs(x = "Date", y = "Vmt") + #, title = paste("Iteration", iteration)) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  # rt plot
  A2 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_rt_quantiles[1, ], 
                    ymax = save_rt_quantiles[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_rt_quantiles[2, ], 
                    ymax = save_rt_quantiles[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_rt_quantiles[3, ], 
                    ymax = save_rt_quantiles[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_rt_quantiles[4, ]), color = "red") +
    labs(x = "Date", y = "rt") + #, title = paste("Iteration", iteration)) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  # ft plot
  A3=ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_ft_quantiles[1, ], 
                    ymax = save_ft_quantiles[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_ft_quantiles[2, ], 
                    ymax = save_ft_quantiles[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_ft_quantiles[3, ], 
                    ymax = save_ft_quantiles[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_ft_quantiles[4, ]), color = "red") +
    labs(x = "Date", y = "ft") + #, title = paste("Iteration", iteration)) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  
  
  # Total Abundance
  A4 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles2[1, ], 
                    ymax = save_quantiles2[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles2[2, ], 
                    ymax = save_quantiles2[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles2[3, ], 
                    ymax = save_quantiles2[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_quantiles2[4, ]), color = "black") +
    geom_point(aes(x = as.Date(date), y = X_obs1_train[1:final_obs_count]), 
               color = "red", size = 3) +
    labs(x = "Date", y = "Total Abundance") + #, title = paste("Iteration", iteration)) +
    #scale_y_continuous(limits = c(0,6500)) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  # Infectious per 1000
  A5 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles3[1, ], 
                    ymax = save_quantiles3[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles3[2, ], 
                    ymax = save_quantiles3[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles3[3, ], 
                    ymax = save_quantiles3[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_quantiles3[4, ]), color = "black") +
    geom_point(aes(x = as.Date(date), y = X_obs2_train[1:final_obs_count]), 
               color = "red", size = 3) +
    labs(x = "Date", y = "Inf_Mosq_Per_1000") + #, title = paste("Iteration", iteration)) +
    scale_y_continuous(limits = c(0,40)) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  # Human cases
  A6 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles_human[1, ], 
                    ymax = save_quantiles_human[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles_human[2, ], 
                    ymax = save_quantiles_human[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles_human[3, ], 
                    ymax = save_quantiles_human[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_quantiles_human[4, ]), color = "black") +
    geom_point(aes(x = as.Date(date), y = Ih_obs), color = "red",size = 3) +
    labs(x = "Date", y = "Inf_human") + #, title = paste("Iteration", iteration)) +
    #scale_y_continuous(limits = c(0,260)) +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  # Additional state variable plots (Sm, Im, Eh)
  A7 <- save_ensemble_full_global[1, 1:N, 1:final_obs_count]
  save_quantiles4 <- apply(A7, 2, quantile, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  S7 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles4[1, ], 
                    ymax = save_quantiles4[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles4[2, ], 
                    ymax = save_quantiles4[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles4[3, ], 
                    ymax = save_quantiles4[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_quantiles4[4, ]), color = "red") +
    labs(x = "Date", y = "Sm") + #, title = "Ensemble Kalman Filter: No. of Ensemble Member = 10000") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  A8 <- save_ensemble_full_global[2, 1:N, 1:final_obs_count]
  save_quantiles5 <- apply(A8, 2, quantile, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  S8 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles5[1, ], 
                    ymax = save_quantiles5[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles5[2, ], 
                    ymax = save_quantiles5[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles5[3, ], 
                    ymax = save_quantiles5[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_quantiles5[4, ]), color = "red") +
    labs(x = "Date", y = "Im") + #, title = "Ensemble Kalman Filter: No. of Ensemble Member = 10000") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  A9 <- save_ensemble_full_global[7, 1:N, 1:final_obs_count]
  save_quantiles6 <- apply(A9, 2, quantile, probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)
  S9 <- ggplot() +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles6[1, ], 
                    ymax = save_quantiles6[7, ]), fill = "lightblue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles6[2, ], 
                    ymax = save_quantiles6[6, ]), fill = "blue", alpha = 0.5) +
    geom_ribbon(aes(x = as.Date(date), ymin = save_quantiles6[3, ], 
                    ymax = save_quantiles6[5, ]), fill = "#c6dbef", alpha = 0.5) +
    geom_line(aes(x = as.Date(date), y = save_quantiles6[4, ]), color = "red") +
    labs(x = "Date", y = "Eh") + #, title = "Ensemble Kalman Filter: No. of Ensemble Member = 10000") +
    theme_minimal() +
    theme(legend.position = "right",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(size = 17))
  
  if (iteration %in% c(46)) {
    ggsave(paste0("A1_iteration_", iteration, "_", Year, ".png"), plot = A1, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A2_iteration_", iteration, "_", Year, ".png"), plot = A2, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A3_iteration_", iteration, "_", Year, ".png"), plot = A3, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A4_iteration_", iteration, "_", Year, ".png"), plot = A4, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A5_iteration_", iteration, "_", Year, ".png"), plot = A5, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A6_iteration_", iteration, "_", Year, ".png"), plot = A6, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A7_iteration_", iteration, "_", Year, ".png"), plot = S7, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A8_iteration_", iteration, "_", Year, ".png"), plot = S8, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A9_iteration_", iteration, "_", Year, ".png"), plot = S9, width = 10, height = 8, dpi = 300)
    #ggsave(paste0("A35_iteration_", iteration, ".png"), plot = p1, width = 10, height = 8, dpi = 300)
    ggsave(paste0("A36_iteration_", iteration, "_", Year, ".png"), plot = p2, width = 10, height = 8, dpi = 300)
  }
  print(paste("In the forecast section"))
  
  vm_test <- save_vm_ensemble_global[, 1:final_obs_count]
  rt_test = save_rt_ensemble_global[, 1:final_obs_count]
  ft_test = save_ft_ensemble_global[, 1:final_obs_count]

  combined_values <- array(NA, dim = c(N, final_obs_count, 3))
  # Fill each slice
  combined_values[, , 1] <- vm_test
  combined_values[, , 2] <- rt_test
  combined_values[, , 3] <- ft_test
  n_forecast_sample = 1000
  random_indices <- sample(1:N, n_forecast_sample)
  selected_values <- combined_values[random_indices,, ]
  loop_all_forecast <- vector("list", n_forecast_sample)
  all_vm_forecast <- vector("list", n_forecast_sample)
  all_rt_forecast <- vector("list", n_forecast_sample)
  all_ft_forecast <- vector("list", n_forecast_sample)
  all_fits_vm <- vector("list", n_forecast_sample) #added
  all_forecasts_vm <- vector("list", n_forecast_sample) #added
  all_fits_rt <- vector("list", n_forecast_sample) #added
  all_forecasts_rt <- vector("list", n_forecast_sample) #added
  all_fits_ft <- vector("list", n_forecast_sample) #added
  all_forecasts_ft <- vector("list", n_forecast_sample) #added
  selected_values2 <- save_ensemble_full_global[1:5, random_indices, 1:final_obs_count]
  selected_values3 <- save_ensemble_full_global[9:14, random_indices, 1:final_obs_count]
  all_vm_daily_forecast <- vector("list", n_forecast_sample)  
  all_rt_daily_forecast <- vector("list", n_forecast_sample)
  all_ft_daily_forecast <- vector("list", n_forecast_sample)
  for (i in 1:n_forecast_sample) {
    Vm_t_median <- selected_values[i,,1 ]
    rt_median<- selected_values[i,,2 ]
    ft_median<- selected_values[i,,3 ]
    # Fit GAM to forecast Vm_t for 1 week
    Weeks <- as.numeric(1:length(Vm_t_median))
    
    df_train_vm <- data.frame(Weeks = Weeks, Vm = as.numeric(Vm_t_median))
    df_train_rt <- data.frame(Weeks = Weeks, rt = as.numeric(rt_median))
    df_train_ft <- data.frame(Weeks = Weeks, ft = as.numeric(ft_median))
    fitted_vals_vm <- tail(Vm_t_median, 1) #added
    fitted_vals_rt <- tail(rt_median, 1) #added
    fitted_vals_ft <- tail(ft_median, 1) #added
    # Forecast for two weeks at a time
    forecast_weeks <- max(df_train_vm$Weeks) + seq(1, 2, by = 1)  # Every two weeks
    df_forecast <- data.frame(Weeks = forecast_weeks)  # Forecasting for two-week intervals
    
    
    
    # === DAILY CHAINED OU for 2-week forecast  ===
    # Start from the very last fitted value
    current_Vm <- tail(fitted_vals_vm, 1)
    current_rt <- tail(fitted_vals_rt, 1)
    current_ft <- tail(fitted_vals_ft, 1)
    Vm_daily_forecast <- numeric(14)   # full 14 daily values (for the model)
    rt_daily_forecast <- numeric(14)
    ft_daily_forecast <- numeric(14)
    # ---- Week 1: 7 daily steps (days 1-7) ----
    for (d in 1:7) {
      current_Vm <- OUproc_func(current_Vm,
                                mu = ou_mu_vm,
                                lambda = ou_lambda_vm,
                                sigma = ou_sigma_vm,
                                dt = 1)
      Vm_daily_forecast[d] <- current_Vm
      forecast_obs_index <- iteration + 4
      # adaptive sigma for r_t
      ou_sigma_r_day <- if (forecast_obs_index < as.numeric(obs_year[as.character(Year)])) 0.002 else 1.50
      ou_mu_r_day    <- if (forecast_obs_index < as.numeric(obs_year[as.character(Year)])) log(0.001) else log(0.05)
      
      current_rt <- OUproc_func(current_rt,
                                mu = ou_mu_r_day,
                                lambda = ou_lambda_r,
                                sigma = ou_sigma_r_day,
                                dt = 1)
      rt_daily_forecast[d] <- current_rt
      
      current_ft <- OUproc_func(current_ft,
                                mu = ou_mu_r_day,
                                lambda = ou_lambda_r,
                                sigma = ou_sigma_r_day,
                                dt = 1)
      ft_daily_forecast[d] <- current_ft
    }
    
    # Save the *last* value of week 1 as the weekly value (for storage/quantiles)
    Vm_pred <- numeric(2)
    rt_pred <- numeric(2)
    ft_pred <- numeric(2)
    Vm_pred[1] <- current_Vm
    rt_pred[1] <- current_rt
    ft_pred[1] <- current_ft
    # ---- Week 2: 7 daily steps (days 8-14), starting from the end of week 1 ----
    for (d in 8:14) {
      current_Vm <- OUproc_func(current_Vm,
                                mu = ou_mu_vm,
                                lambda = ou_lambda_vm,
                                sigma = ou_sigma_vm,
                                dt = 1)
      Vm_daily_forecast[d] <- current_Vm
      
      ou_sigma_r_day <- if (forecast_obs_index < as.numeric(obs_year[as.character(Year)])) 0.002 else 1.50
      ou_mu_r_day    <- if (forecast_obs_index < as.numeric(obs_year[as.character(Year)])) log(0.001) else log(0.05)
      
      current_rt <- OUproc_func(current_rt,
                                mu = ou_mu_r_day,
                                lambda = ou_lambda_r,
                                sigma = ou_sigma_r_day,
                                dt = 1)
      rt_daily_forecast[d] <- current_rt
      current_ft <- OUproc_func(current_ft,
                                mu = ou_mu_r_day,
                                lambda = ou_lambda_r,
                                sigma = ou_sigma_r_day,
                                dt = 1)
      ft_daily_forecast[d] <- current_ft
    }
    
    # Save the *last* value of week 2 as the weekly value
    Vm_pred[2] <- current_Vm
    rt_pred[2] <- current_rt
    ft_pred[2] <- current_ft
    all_vm_forecast[[i]] <- Vm_pred
    all_rt_forecast[[i]] <- rt_pred
    all_ft_forecast[[i]] <- ft_pred
    all_vm_daily_forecast[[i]] <- Vm_daily_forecast   
    all_rt_daily_forecast[[i]] <- rt_daily_forecast  
    all_ft_daily_forecast[[i]] <- ft_daily_forecast 
    all_fits_vm[[i]] <- data.frame(Weeks = df_train_vm$Weeks, Fit = fitted_vals_vm) #added
    all_forecasts_vm[[i]] <- data.frame(Weeks = forecast_weeks, Fit = Vm_pred)#added
    all_fits_rt[[i]] <- data.frame(Weeks = df_train_rt$Weeks, Fit = fitted_vals_rt) #added
    all_forecasts_rt[[i]] <- data.frame(Weeks = forecast_weeks, Fit = rt_pred)#added
    all_fits_ft[[i]] <- data.frame(Weeks = df_train_ft$Weeks, Fit = fitted_vals_ft) #added
    all_forecasts_ft[[i]] <- data.frame(Weeks = forecast_weeks, Fit = ft_pred)#added
    
    # Update WNV model
    
    Sm_next_week <- as.numeric(selected_values2[1,i, final_obs_count])
    Sm_next = Sm_next_week
    Im_next_week <- as.numeric(selected_values2[2,i,final_obs_count])
    Im_next = Im_next_week
    Sh_next_week <- as.numeric(selected_values2[3,i, final_obs_count])
    Sh_next = Sh_next_week
    Eh_next_week <- as.numeric(selected_values2[4,i, final_obs_count])
    Eh_next = Eh_next_week
    Ih_next_week <- as.numeric(selected_values2[5,i, final_obs_count])
    Ih_next = Ih_next_week
    
    static_medians <- as.numeric(selected_values3[,i,final_obs_count])  # Static parameters for each ensemble member
    forecast_week = forecast_weeks[1]
    indx1 <- (forecast_week - 1) * 7 + 1  
    indx2 <- indx1 + 13  
    indx_range <- indx1:indx2
    
    T_temp_next_week <- as.numeric(inputTem_i[indx_range])
    P_temp_next_week <- as.numeric(inputP_i[indx_range])
    # Initialize ensemble matrix
    sirWNV_ensemble <- matrix(
      c(Sm_next, Im_next, Sh_next, Eh_next, Ih_next),
      nrow = 5, ncol = 1
    )
    # To store Sm, Im, Ih etc for each forecasted week
    all_forecast <- matrix(NA, nrow = 5, ncol = 2)  # rows: Sm, Im, Ih etc; cols: week1, week2
    for (temp_week in 1:2) {
      day_idx <- ((temp_week - 1) * 7 + 1):(temp_week * 7)
      sirWNV_ensemble <- WNV_model(
        sirWNV_ensemble,
        Vm_t = Vm_daily_forecast[day_idx],
        r_t = rt_daily_forecast[day_idx],
        f_t = ft_daily_forecast[day_idx],
        T_temp = T_temp_next_week[day_idx],
        P_temp = P_temp_next_week[day_idx],
        par_input = static_medians,
        week = forecast_weeks[temp_week]
      )
      # Save  after each week
      all_forecast[, temp_week] <- sirWNV_ensemble[c(1:5), 1]
      
    }
    
    
    loop_all_forecast[[i]] <- all_forecast
  }
  vm_matrix <- do.call(rbind, all_vm_forecast)  # Convert to matrix: 1000 rows × 2 columns (weeks)
  
  vm_forecast_q <- apply(vm_matrix, 2, function(x) quantile(x, c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                                 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                                 0.95, 0.975, 0.99)))
  rt_matrix <- do.call(rbind, all_rt_forecast)  # Convert to matrix: 1000 rows × 2 columns (weeks)
  
  rt_forecast_q <- apply(rt_matrix, 2, function(x) quantile(x, c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                                 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                                 0.95, 0.975, 0.99)))
  
  WNV_forecast_array <- simplify2array(loop_all_forecast)  # dimensions: 5 x 2 x 1000
  
  
  
  # For week 1
  total_abundance_1 <- WNV_forecast_array[1,1, ] + WNV_forecast_array[2,1, ]
  abundance_1 <- WNV_forecast_array[1,1, ]
  infectious_mosq_1 <- WNV_forecast_array[2,1, ]
  human_cases_1 <- WNV_forecast_array[5,1, ]
  # Filter: keep only ensemble members where total abundance > 0 and infection rate < 100%
  valid_idx_1 <- which(total_abundance_1 > 0 & 
                         (infectious_mosq_1 / total_abundance_1) < 1.0)
  
  if(length(valid_idx_1) > 0) {
    # Calculate infectious per 1000 only for valid ensemble members
    infectious_per_1000_1 <- (infectious_mosq_1[valid_idx_1] / total_abundance_1[valid_idx_1]) * 1000
    
    # Get quantiles
    total_abundance_q_1 <- quantile(total_abundance_1[valid_idx_1], 
                                    probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                              0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                              0.95, 0.975, 0.99))
    infectious_per_1000_q_1 <- quantile(infectious_per_1000_1, 
                                        probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                  0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                  0.95, 0.975, 0.99))
    human_cases_q_1 <- quantile(human_cases_1[valid_idx_1], 
                                probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                          0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                          0.95, 0.975, 0.99))
    abundance_q_1 <- quantile(abundance_1[valid_idx_1], 
                              probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                        0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                        0.95, 0.975, 0.99))
    infectious_mosq_q_1 <- quantile(infectious_mosq_1[valid_idx_1], 
                                    probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                              0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                              0.95, 0.975, 0.99))
  } else {
    # Handle case where no valid ensemble members exist
    warning(paste("Iteration", iteration, "Week 1: No valid ensemble members"))
    
  }
  
  # For week 2 
  total_abundance_2 <- WNV_forecast_array[1,2, ] + WNV_forecast_array[2,2, ]
  abundance_2 <- WNV_forecast_array[1,2, ]
  infectious_mosq_2 <- WNV_forecast_array[2,2, ]
  human_cases_2 <- WNV_forecast_array[5,2, ]
  
  valid_idx_2 <- which(total_abundance_2 > 0 & 
                         (infectious_mosq_2 / total_abundance_2) < 1.0)
  
  if(length(valid_idx_2) > 0) {
    infectious_per_1000_2 <- (infectious_mosq_2[valid_idx_2] / total_abundance_2[valid_idx_2]) * 1000
    
    total_abundance_q_2 <- quantile(total_abundance_2[valid_idx_2], 
                                    probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                              0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                              0.95, 0.975, 0.99))
    infectious_per_1000_q_2 <- quantile(infectious_per_1000_2, 
                                        probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                                  0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                                  0.95, 0.975, 0.99))
    human_cases_q_2 <- quantile(human_cases_2[valid_idx_2], 
                                probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                          0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                          0.95, 0.975, 0.99))
    abundance_q_2 <- quantile(abundance_2[valid_idx_2], 
                              probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                        0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                        0.95, 0.975, 0.99))
    infectious_mosq_q_2 <- quantile(infectious_mosq_2[valid_idx_2], 
                                    probs = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                                              0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                                              0.95, 0.975, 0.99))
  } else {
    warning(paste("Iteration", iteration, "Week 2: No valid ensemble members"))
  }
  results[[iteration]] <- list(
    vm_forecast_q = vm_forecast_q,
    rt_forecast_q = rt_forecast_q,
    total_abundance_q_1 = total_abundance_q_1,
    total_abundance_q_2 = total_abundance_q_2,
    infectious_per_1000_q_2 = infectious_per_1000_q_2,
    infectious_per_1000_q_1 = infectious_per_1000_q_1,
    human_cases_q_1 = human_cases_q_1,
    human_cases_q_2 = human_cases_q_2,
    infectious_mosq_q_2 = infectious_mosq_q_2,
    infectious_mosq_q_1 = infectious_mosq_q_1,
    abundance_q_1 = abundance_q_1,
    abundance_q_2 = abundance_q_2
  )
}



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
       axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
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
  X0_obs = X0_obs,
  quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 
                      0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 
                      0.95, 0.975, 0.99)
)

saveRDS(wis_all, file = paste0("wis_all_Mosq+Human+Climate_", Year, ".rds"))



#######################
#New plot

# ====================================================================
# STATE FORECASTS (Human cases, Total abundance, Infectious_Mosq_1000)
# ====================================================================

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
  #scale_y_continuous(limits = c(0, 260)) +
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
  #scale_y_continuous(limits = c(0, 260)) +
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
  #scale_y_continuous(limits = c(0, 400)) +
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
  #scale_y_continuous(limits = c(0, 400)) +
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
  #scale_y_continuous(limits = c(0, 6500)) +
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
  #scale_y_continuous(limits = c(0, 6500)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")



ggsave(paste0("Q1_iteration_", iteration, "_", Year, ".png"), plot = Q1, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q2_iteration_", iteration, "_", Year, ".png"), plot = Q2, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q3_iteration_", iteration, "_", Year, ".png"), plot = Q3, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q4_iteration_", iteration, "_", Year, ".png"), plot = Q4, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q5_iteration_", iteration, "_", Year, ".png"), plot = Q5, width = 10, height = 8, dpi = 300)
ggsave(paste0("Q6_iteration_", iteration, "_", Year, ".png"), plot = Q6, width = 10, height = 8, dpi = 300)
saveRDS(results, file = paste0("results_Mosq+Human+Climate_", Year, ".rds"))
cat("\nYear", Year, "complete. Results saved.\n")
}

cat("\nAll years complete.\n")
#####################################
##  Fan Chart Plot   ##
## Two-week forward forecast fans  ##
#####################################

plot_flusight_style <- function(results,
                                forecast_1week_dates,
                                forecast_2week_dates,
                                actual_data,
                                target = "total_abundance",
                                quantile_levels = c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3,
                                                    0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7,
                                                    0.75, 0.8, 0.85, 0.9, 0.95, 0.975, 0.99),
                                model_name = "SEIR Model",
                                # Which forecast origins to plot fans for (NULL = all)
                                # e.g. plot_every_n = 4 plots every 4th forecast to avoid clutter
                                plot_every_n = 1) {
  
  n_iterations <- length(results)
  
  # Select which iterations to plot fans for
  iter_to_plot <- seq(1, n_iterations, by = plot_every_n)
  
  # Build a long data frame of fan segments
  # Each "fan" = one forecast origin, with horizon 1 and horizon 2 as two x-positions
  fan_rows <- list()
  
  for (i in iter_to_plot) {
    for (horizon in c(1, 2)) {
      key  <- paste0(target, "_q_", horizon)
      qs   <- results[[i]][[key]]
      
      if (is.null(qs) || all(is.na(qs))) next
      
      target_date <- if (horizon == 1) forecast_1week_dates[i] else forecast_2week_dates[i]
      origin_date <- forecast_1week_dates[i] - 7   # one week before the 1-week-ahead date
      
      fan_rows[[length(fan_rows) + 1]] <- data.frame(
        origin_date  = origin_date,
        target_date  = target_date,
        horizon      = horizon,
        iteration    = i,
        q_0.025      = qs[which(abs(quantile_levels - 0.025) < 1e-9)],
        q_0.05       = qs[which(abs(quantile_levels - 0.05)  < 1e-9)],
        q_0.10       = qs[which(abs(quantile_levels - 0.10)  < 1e-9)],
        q_0.25       = qs[which(abs(quantile_levels - 0.25)  < 1e-9)],
        q_0.50       = qs[which(abs(quantile_levels - 0.50)  < 1e-9)],
        q_0.75       = qs[which(abs(quantile_levels - 0.75)  < 1e-9)],
        q_0.90       = qs[which(abs(quantile_levels - 0.90)  < 1e-9)],
        q_0.95       = qs[which(abs(quantile_levels - 0.95)  < 1e-9)],
        q_0.975      = qs[which(abs(quantile_levels - 0.975) < 1e-9)],
        stringsAsFactors = FALSE
      )
    }
  }
  
  fan_df <- bind_rows(fan_rows)
  
  # For each forecast origin, build the fan polygon:
  # The fan connects origin_date (at the observed value or median) through horizon 1 and horizon 2
  # We'll draw ribbons at each horizon (stacked shading per PI level)
  
  # Prepare actual data
  if (is.vector(actual_data)) {
    actual_df <- tibble(
      date   = c(forecast_1week_dates - 7, forecast_1week_dates),  # rough alignment
      actual = actual_data
    )
  } else {
    actual_df <- actual_data  # expects columns: date, actual
  }
  
  # Target label
  target_name <- dplyr::case_when(
    target == "total_abundance"    ~ "Total Mosquito Abundance",
    target == "infectious_per_1000" ~ "Infected Mosquitoes per 1000",
    target == "human_cases"        ~ "Human Cases",
    TRUE ~ target
  )
  
  # ---- Build fan polygon data ----
  # For each origin, we create ribbon segments connecting origin->h1->h2
  # We need to build "wide" segments: for each forecast group (origin), 
  # create a ribbon from h1 to h2 at each PI level.
  
  fan_wide <- fan_df %>%
    dplyr::group_by(origin_date, iteration) %>%
    dplyr::arrange(horizon) %>%
    dplyr::summarise(
      date_h1    = target_date[horizon == 1],
      date_h2    = target_date[horizon == 2],
      # 90% PI
      lo90_h1 = q_0.05[horizon == 1], hi90_h1 = q_0.95[horizon == 1],
      lo90_h2 = q_0.05[horizon == 2], hi90_h2 = q_0.95[horizon == 2],
      # 80% PI
      lo80_h1 = q_0.10[horizon == 1],  hi80_h1 = q_0.90[horizon == 1],
      lo80_h2 = q_0.10[horizon == 2],  hi80_h2 = q_0.90[horizon == 2],
      # 50% PI
      lo50_h1 = q_0.25[horizon == 1],  hi50_h1 = q_0.75[horizon == 1],
      lo50_h2 = q_0.25[horizon == 2],  hi50_h2 = q_0.75[horizon == 2],
      # medians
      med_h1  = q_0.50[horizon == 1],
      med_h2  = q_0.50[horizon == 2],
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(date_h2))  # keep only origins with both horizons
  
  # Melt to long for ribbons: each row = (origin, date, lo, hi, level)
  make_ribbon_long <- function(df, lo_h1, hi_h1, lo_h2, hi_h2, level) {
    bind_rows(
      df %>% transmute(origin_date, iteration, date = date_h1,
                       ymin = !!sym(lo_h1), ymax = !!sym(hi_h1), level = level),
      df %>% transmute(origin_date, iteration, date = date_h2,
                       ymin = !!sym(lo_h2), ymax = !!sym(hi_h2), level = level)
    ) %>% arrange(origin_date, iteration, date)
  }
  
  ribbon_90 <- make_ribbon_long(fan_wide, "lo90_h1","hi90_h1","lo90_h2","hi90_h2", "90%")
  ribbon_80 <- make_ribbon_long(fan_wide, "lo80_h1","hi80_h1","lo80_h2","hi80_h2", "80%")
  ribbon_50 <- make_ribbon_long(fan_wide, "lo50_h1","hi50_h1","lo50_h2","hi50_h2", "50%")
  
  median_long <- bind_rows(
    fan_wide %>% transmute(origin_date, iteration, date = date_h1, median = med_h1),
    fan_wide %>% transmute(origin_date, iteration, date = date_h2, median = med_h2)
  ) %>% arrange(origin_date, iteration, date)
  
  # PI colours (blue shades, lightest = widest, like FluSight)
  pi_colors <- c("90%" = "#BDD7EE", "80%" = "#9DC3E6", "50%" = "#2E75B6")
  pi_alphas <- c("90%" = 0.5,       "80%" = 0.6,       "50%" = 0.8)
  
  # ---- Plot ----
  p <- ggplot() 
  # Draw fan ribbons per origin (group by iteration so each fan is separate)
  for (lvl in c("90%", "80%", "50%")) {
    rdf <- switch(lvl,
                  "90%" = ribbon_90,
                  "80%" = ribbon_80,
                  "50%" = ribbon_50)
    p <- p + geom_ribbon(
      data = rdf,
      aes(x = date, ymin = ymin, ymax = ymax, group = interaction(origin_date, iteration)),
      fill  = pi_colors[lvl],
      alpha = pi_alphas[lvl]
    )
  }
  
  # Median forecast line per origin
  p <- p + geom_line(
    data = median_long,
    aes(x = date, y = median, group = interaction(origin_date, iteration)),
    color = "#1F4E79", linewidth = 0.8, alpha = 0.8
  ) +
    geom_point(
      data = median_long,
      aes(x = date, y = median, group = interaction(origin_date, iteration)),
      color = "#1F4E79", size = 1.5, alpha = 0.9
    )
  #Observed data on top so it's never obscured
  p <- p +
    geom_line(data = actual_df,
              aes(x = date, y = actual),
              color = "black", linewidth = 1, alpha = 0.9) +
    geom_point(data = actual_df,
               aes(x = date, y = actual),
               color = "black", size = 1.5)
  # Manual legend for PI levels (mimics FluSight style)
  # Use dummy data for the legend
  legend_df <- data.frame(
    x    = as.Date(NA), xend = as.Date(NA),
    y    = NA_real_,    yend = NA_real_,
    fill = factor(c("90%", "80%", "50%"), levels = c("90%", "80%", "50%"))
  )
  
  p <- p +
    # Invisible ribbons just to get legend entries
    geom_rect(data = data.frame(level = factor(c("90%","80%","50%"),
                                               levels = c("90%","80%","50%"))),
              aes(xmin = as.Date(-Inf), xmax = as.Date(-Inf),
                  ymin = -Inf, ymax = -Inf, fill = level)) +
    scale_fill_manual(
      name   = "Uncertainty",
      values = pi_colors,
      guide  = guide_legend(override.aes = list(alpha = 1))
    ) +
    labs(
      title    = paste0(target_name, " - ", model_name),
      subtitle = "2-week ahead forecasts at each forecast origin",
      x = "Date",
      y = "Count"
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position   = "right",
      axis.title = element_text(size = 17),
      axis.text = element_text(size = 17),
      legend.text = element_text(size = 15),
      legend.title      = element_text(size = 17, face = "bold"),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(face = "bold",  size = 17),
      plot.subtitle     = element_text(color = "grey40", size = 14)
    )
  
  return(p)
}


#####################################
## Usage                           ##
#####################################

# Build actual_df from your variables
actual_df <- tibble(
  date   = observed_dates,   # all_weeks
  actual = X_obs1             # X_obs1 = X_obs
)

# --- Total Abundance ---
p_abundance <- plot_flusight_style(
  results              = results,
  forecast_1week_dates = forecast_1week_dates,
  forecast_2week_dates = forecast_2week_dates,
  actual_data          = actual_df,
  target               = "total_abundance",
  model_name           = "Mosq+Human+Climate",
  plot_every_n         = 4   # increase (e.g. 4) if fans overlap too much
)
print(p_abundance)
# Build actual_df from your variables
actual_df <- tibble(
  date   = observed_dates,   # all_weeks
  actual = X_obs2             # X_obs1 = X_obs
)
# --- Infectious per 1000 ---
# (swap actual_df$actual to your infectious observed vector if different)
p_infected <- plot_flusight_style(
  results              = results,
  forecast_1week_dates = forecast_1week_dates,
  forecast_2week_dates = forecast_2week_dates,
  actual_data          = actual_df,
  target               = "infectious_per_1000",
  model_name           = "Mosq+Human+Climate",
  plot_every_n         = 1
)
print(p_infected)
# Build actual_df from your variables
actual_df <- tibble(
  date   = observed_dates,   # all_weeks
  actual = X0_obs[1:52]             # X_obs1 = X_obs
)
# --- Human Cases ---
p_cases <- plot_flusight_style(
  results              = results,
  forecast_1week_dates = forecast_1week_dates,
  forecast_2week_dates = forecast_2week_dates,
  actual_data          = actual_df,
  target               = "human_cases",
  model_name           = "Mosq+Human+Climate",
  plot_every_n         = 4
)
print(p_cases)

# Save plot 
ggsave("f_abundance_Mosq+Human+Climate.png", p_abundance, width = 11, height = 6, dpi = 300)
ggsave("f_infectious_per_1000_Mosq+Human+Climate.png", p_infected, width = 11, height = 6, dpi = 300)
ggsave("f_Humancases_Mosq+Human+Climate.png", p_cases, width = 11, height = 6, dpi = 300)

# ============================================================
# Multi-Year Panel Plotting Script
# Mosq+Human+Climate - Fit + Forecast + Fan Charts
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
model_name <- "Mosq+Human+Climate"
N_ens      <- 8000

# Y-axis limits applied to 2006-2019 only; 2021 gets no limit
y_limits_fit <- list(
  total_abundance     = c(0, 6500),
  infectious_per_1000 = c(0, 40),     # <-- tighter for fit
  human_cases         = c(0, 260)
)

y_limits_forecast <- list(
  total_abundance     = c(0, 6500),
  infectious_per_1000 = c(0, 400),    # <-- wider for forecast/fansight
  human_cases         = c(0, 260)
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

# ---- FIT plot (from save_ensemble_full_global RDS) ----------

make_fit_plot <- function(Year, target, d, apply_ylim) {
  
  rds_file <- paste0("save_ensemble_full_global_", model_name, "_", Year, ".rds")
  if (!file.exists(rds_file)) { message("Missing: ", rds_file); return(NULL) }
  ens     <- readRDS(rds_file)            # [18, 8000, 50]
  n_weeks <- dim(ens)[3]
  dates   <- d$all_weeks[1:n_weeks]
  
  obs_vec <- switch(target,
                    total_abundance     = d$X_obs1[1:n_weeks],
                    infectious_per_1000 = d$X_obs2[1:n_weeks],
                    human_cases         = d$X0_obs[1:n_weeks]
  )
  
  # Extract fitted quantiles week-by-week
  pred_mat <- switch(target,
                     total_abundance = {
                       ens[1, 1:N_ens, ] + ens[2, 1:N_ens, ]
                     },
                     infectious_per_1000 = {
                       sm  <- ens[1, 1:N_ens, ]; im <- ens[2, 1:N_ens, ]
                       tot <- sm + im; tot[tot == 0] <- NA
                       im / tot * 1000
                     },
                     human_cases = ens[8, 1:N_ens, ]
  )
  
  qmat <- apply(pred_mat, 2, quantile,
                probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95),
                na.rm = TRUE)
  
  df_rib <- data.frame(Date = dates,
                       q05  = qmat[1,], q10 = qmat[2,], q25 = qmat[3,],
                       q50  = qmat[4,],
                       q75  = qmat[5,], q90 = qmat[6,], q95 = qmat[7,])
  
  df_obs <- data.frame(Date     = d$all_weeks[1:length(obs_vec)],
                       Observed = obs_vec)
  
  ylab <- c(total_abundance     = "Total Abundance",
            infectious_per_1000 = "Inf. Mosq. per 1000",
            human_cases         = "Human Cases")[target]
  
  p <- ggplot(df_rib, aes(x = Date)) +
    geom_ribbon(aes(ymin = q05, ymax = q95, fill = "90% CI"), alpha = 0.40) +
    geom_ribbon(aes(ymin = q10, ymax = q90, fill = "80% CI"), alpha = 0.50) +
    geom_ribbon(aes(ymin = q25, ymax = q75, fill = "50% CI"), alpha = 0.65) +
    geom_line(aes(y = q50,         color = "Median fit"),  linewidth = 0.7) +
    geom_point(data = df_obs,
               aes(x = Date, y = Observed, color = "Observed"), size = 3) +
    scale_fill_manual(name   = NULL,
                      values = c("90% CI" = "#BDD7EE",
                                 "80% CI" = "#5B9BD5",
                                 "50% CI" = "#1F4E79")) +
    scale_color_manual(name  = NULL,
                       values = c("Median fit" = "#08306b",
                                  "Observed"   = "red")) +
    labs(x = NULL, y = ylab, title = as.character(Year)) +
    theme_minimal(base_size = 9) +
    theme(plot.title       = element_text(face = "bold", size = 17, hjust = 0.5),
          axis.text.x      = element_text(angle = 45, hjust = 1, size = 17),
          axis.text.y      = element_text(size = 17),
          axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.position  = "none",
          panel.grid.minor = element_blank())
  
  if (apply_ylim) p <- p + scale_y_continuous(limits = y_limits_fit[[target]])
  p
}
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
  
  if (plot_type == "fit") {
    ref_p <- ggplot(df_dummy, aes(x = Date)) +
      geom_ribbon(aes(ymin = q05, ymax = q95, fill = "90% CI"), alpha = 0.40) +
      geom_ribbon(aes(ymin = q10, ymax = q90, fill = "80% CI"), alpha = 0.50) +
      geom_ribbon(aes(ymin = q25, ymax = q75, fill = "50% CI"), alpha = 0.65) +
      geom_line(aes(y = q50,      color = "Median fit"),  linewidth = 0.7) +
      geom_point(aes(y = Observed, color = "Observed"), size = 3) +
      scale_fill_manual(name   = NULL,
                        values = c("90% CI" = "#BDD7EE",
                                   "80% CI" = "#5B9BD5",
                                   "50% CI" = "#1F4E79")) +
      scale_color_manual(name  = NULL,
                         values = c("Median fit" = "#08306b", "Observed" = "red")) +
      theme_minimal() + theme(legend.position = "bottom",
                              legend.text = element_text(size = 17))
    
  } else if (plot_type %in% c("forecast_1wk", "forecast_2wk")) {
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
# plot_type: "fit" | "forecast_1wk" | "forecast_2wk" | "fansight"
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
                fit          = make_fit_plot(yr, target, d, apply_ylim),
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
  pt_label  <- c(fit          = "Model Fit",
                 forecast_1wk = "1-Week-Ahead Forecast",
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

#calculatin Tmin and Tmax for weather models



years <- c(2006:2019, 2021)

# ── Row indices for T_min and T_max, per model ────────────────────────────────
model_indices <- list(
  "FullModel" = list(
    file_prefix = "save_ensemble_full_global_FullModel_",
    tmin_row    = 11,
    tmax_row    = 12
  ),
  "Mosq+Human+Climate" = list(
    file_prefix = "save_ensemble_full_global_Mosq+Human+Climate_",
    tmin_row    = 9,
    tmax_row    = 10
  )
)

# ── Extract T_min/T_max posterior for one model, one year ─────────────────────
extract_tmin_tmax <- function(model_name, year, final_week = NULL) {
  
  idx   <- model_indices[[model_name]]
  fpath <- paste0(idx$file_prefix, year, ".rds")
  
  if (!file.exists(fpath)) {
    warning(paste("File not found:", fpath))
    return(NULL)
  }
  
  arr <- readRDS(fpath)  # dims: (variables, N, total_time_points)
  
  # If final_week not specified, use the last time point with non-zero data
  # (array was pre-allocated with zeros, so trailing unused weeks would be 0)
  if (is.null(final_week)) {
    tmin_series  <- arr[idx$tmin_row, 1, ]  # check first ensemble member's trace
    nonzero_weeks <- which(tmin_series != 0)
    if (length(nonzero_weeks) == 0) {
      warning(paste("No non-zero T_min values found for", model_name, year))
      return(NULL)
    }
    final_week <- max(nonzero_weeks)
  }
  
  T_min_draws <- arr[idx$tmin_row, , final_week]
  T_max_draws <- arr[idx$tmax_row, , final_week]
  
  tibble(
    model         = model_name,
    year          = year,
    final_week    = final_week,
    T_min_median  = median(T_min_draws, na.rm = TRUE),
    T_max_median  = median(T_max_draws, na.rm = TRUE),
    T_min_lo      = quantile(T_min_draws, 0.025, na.rm = TRUE),
    T_min_hi      = quantile(T_min_draws, 0.975, na.rm = TRUE),
    T_max_lo      = quantile(T_max_draws, 0.025, na.rm = TRUE),
    T_max_hi      = quantile(T_max_draws, 0.975, na.rm = TRUE),
    T_peak_median = median((T_min_draws + T_max_draws) / 2, na.rm = TRUE)
  )
}

# ── Run across all years and both weather-inclusive models ────────────────────
tmin_tmax_by_year <- map_dfr(names(model_indices), function(model_name) {
  map_dfr(years, function(yr) {
    extract_tmin_tmax(model_name, yr)
  })
})

print(tmin_tmax_by_year, n = 50)

# ── Summary across years, per model ────────────────────────────────────────────
summary_by_model <- tmin_tmax_by_year %>%
  group_by(model) %>%
  summarise(
    median_T_peak = median(T_peak_median, na.rm = TRUE),
    min_T_peak    = min(T_peak_median, na.rm = TRUE),
    max_T_peak    = max(T_peak_median, na.rm = TRUE),
    median_T_min  = median(T_min_median, na.rm = TRUE),
    median_T_max  = median(T_max_median, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_by_model)

# ── Combined summary across both models and all years ─────────────────────────
cat("\nOverall peak growth temperature (median across all years, both models):",
    round(median(tmin_tmax_by_year$T_peak_median, na.rm = TRUE), 1), "°C\n")
cat("Range across years/models:",
    round(min(tmin_tmax_by_year$T_peak_median, na.rm = TRUE), 1), "–",
    round(max(tmin_tmax_by_year$T_peak_median, na.rm = TRUE), 1), "°C\n")

#precipitation patters in Mosq+Human+Climate, Mosq+Human+NoClimate

library(dplyr)

# ── Years where Model 3 or Model 4 had relative WIS > 0 (worse than baseline) for IM1000 ──
underperform_years <- summary_stats_log %>%
  filter(
    target == "Infectious mosq per 1000",
    model %in% c("Mosq+Human+Weather", "Mosq+Human+NoWeather")
  ) %>%
  group_by(year, model) %>%
  summarise(median_relWIS = median(median_rel_wis, na.rm = TRUE), .groups = "drop") %>%
  filter(median_relWIS > 0) %>%
  arrange(model, desc(median_relWIS))

print(underperform_years, n = 30)

# ── Cross-reference against the precipitation ranking from earlier ────────────
precip_ranking <- tibble::tribble(
  ~year, ~monsoon_precip, ~precip_rank,
  2021, 104.63, 1,
  2014, 87.01,  2,
  2008, 76.06,  3,
  2012, 58.12,  4,
  2006, 54.91,  5,
  2017, 51.02,  6,
  2018, 49.47,  7,
  2016, 39.60,  8,
  2015, 38.70,  9,
  2013, 38.26,  10,
  2010, 37.52,  11,
  2009, 28.30,  12,
  2007, 24.87,  13,
  2019, 24.84,  14,
  2011, 23.86,  15
)

underperform_years %>%
  left_join(precip_ranking, by = "year") %>%
  arrange(model, precip_rank)
print(underperform_years %>% left_join(precip_ranking, by = "year") %>% arrange(model, precip_rank), n = 30)
# ── Correlation between monsoon precipitation and IM1000 relative WIS ─────────
# for each of the two weather-free-avian models

cor_check <- underperform_years %>%
  left_join(precip_ranking, by = "year") %>%
  group_by(model) %>%
  summarise(
    spearman_rho = cor(monsoon_precip, median_relWIS, method = "spearman"),
    spearman_p   = cor.test(monsoon_precip, median_relWIS, method = "spearman")$p.value,
    .groups = "drop"
  )

print(cor_check)
