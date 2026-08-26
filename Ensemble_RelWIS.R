##############################
# ENSEMBLE MODEL

library(tidyverse)
library(nloptr)
library(epipredict)  # for quantile_pred and weighted_interval_score
library(RColorBrewer)

years_to_run <- c(2006:2019, 2021)
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
# ============================================================
# ENSEMBLE: ALL DATA, NO TRAIN/EVAL SPLIT
# Weekly WIS output with dates
# ============================================================

QUANTILE_LEVELS <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                     0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
                     0.95, 0.975, 0.99)
N_Q     <- length(QUANTILE_LEVELS)
Q_NAMES <- c("1%", "2.5%", "5%", "10%", "15%", "20%", "25%", "30%", "35%",
             "40%", "45%", "50%", "55%", "60%", "65%", "70%", "75%", "80%",
             "85%", "90%", "95%", "97.5%", "99%")

TARGET_CONFIG <- list(
  list(key = "total_abundance",     horizon = 1, wis_name = "abundance_1wk",  obs = "X_obs1"),
  list(key = "total_abundance",     horizon = 2, wis_name = "abundance_2wk",  obs = "X_obs1"),
  list(key = "infectious_per_1000", horizon = 1, wis_name = "infected_1wk",   obs = "X_obs2"),
  list(key = "infectious_per_1000", horizon = 2, wis_name = "infected_2wk",   obs = "X_obs2"),
  list(key = "human_cases",         horizon = 1, wis_name = "cases_1wk",      obs = "X0_obs"),
  list(key = "human_cases",         horizon = 2, wis_name = "cases_2wk",      obs = "X0_obs")
)

# ============================================================
# LOAD ALL MODEL QUANTILES (same as before)
# ============================================================

load_all_model_quantiles <- function(year, obs_list) {
  model_names <- c("FullModel", "MHClimate", "MHNoClimate", "FullNoClimate")
  
  result_files <- c(
    FullModel     = paste0("results_FullModel_", year, ".rds"),
    MHClimate     = paste0("results_Mosq+Human+Climate_", year, ".rds"),
    MHNoClimate   = paste0("results_Mosq+Human+NoClimate_", year, ".rds"),
    FullNoClimate = paste0("results_FullModel_NoClimate_", year, ".rds")
  )
  
  all_results <- map(result_files, readRDS)
  n_iter      <- length(all_results[[1]])
  n_models    <- length(model_names)
  
  target_data <- map(TARGET_CONFIG, function(cfg) {
    forecast_key <- paste0(cfg$key, "_q_", cfg$horizon)
    actual_vec   <- obs_list[[cfg$obs]]
    
    actuals <- map_dbl(seq_len(n_iter), function(i) {
      actual_idx <- i + 5 + cfg$horizon - 1
      if (actual_idx > length(actual_vec)) return(NA_real_)
      actual_vec[actual_idx]
    })
    
    q_array <- array(NA_real_, dim = c(n_iter, n_models, N_Q),
                     dimnames = list(NULL, model_names, Q_NAMES))
    
    for (k in seq_along(model_names)) {
      for (i in seq_len(n_iter)) {
        q_vec <- all_results[[k]][[i]][[forecast_key]]
        if (!is.null(q_vec)) {
          q_array[i, k, ] <- as.numeric(q_vec[Q_NAMES])
        }
      }
    }
    
    list(
      key      = paste0(cfg$key, "_", cfg$horizon, "wk"),
      wis_name = cfg$wis_name,
      horizon  = cfg$horizon,
      tgt_name = cfg$key,
      actuals  = actuals,
      q_array  = q_array
    )
  })
  
  names(target_data) <- map_chr(TARGET_CONFIG,
                                ~ paste0(.$key, "_", .$horizon, "wk"))
  target_data
}

# ============================================================
# WIS HELPER (your epipredict function)
# ============================================================

compute_wis_single_epipredict <- function(q_vec, actual) {
  if (is.na(actual) || any(is.na(q_vec))) return(NA_real_)
  qmat      <- matrix(as.numeric(q_vec), nrow = 1)
  pred_dist <- quantile_pred(qmat, QUANTILE_LEVELS)
  as.numeric(weighted_interval_score(
    x               = pred_dist,
    actual          = actual,
    quantile_levels = QUANTILE_LEVELS,
    na_handling     = "impute"
  ))
}

compute_wis_vec <- function(q_matrix, actuals) {
  map2_dbl(asplit(q_matrix, 1), actuals, compute_wis_single_epipredict)
}

# ============================================================
#BUILD ENSEMBLE QUANTILE MATRICES (all iterations)
# ============================================================

build_ens_avg <- function(q_array) {
  apply(q_array, c(1, 3), mean, na.rm = TRUE)   # [n_iter x n_q]
}

build_ens_median <- function(q_array) {
  apply(q_array, c(1, 3), median, na.rm = TRUE)  # [n_iter x n_q]
}

# EnsReg: OLS on ALL data (no split), applied to all quantile levels
build_ens_reg <- function(q_array, actuals) {
  n_iter      <- dim(q_array)[1]
  model_names <- dimnames(q_array)[[2]]
  
  # Use median (50th pctile) forecasts as OLS covariates
  medians_mat <- q_array[, , which(Q_NAMES == "50%"), drop = TRUE]
  colnames(medians_mat) <- model_names
  
  valid    <- !is.na(actuals) & complete.cases(medians_mat)
  df_train <- as.data.frame(medians_mat[valid, , drop = FALSE])
  df_train$actual <- actuals[valid]
  
  lm_fit <- lm(actual ~ ., data = df_train)
  coefs  <- coef(lm_fit)
  w0     <- coefs["(Intercept)"]
  wk     <- coefs[setdiff(names(coefs), "(Intercept)")]
  wk     <- wk[model_names]  # ensure order
  
  cat("  EnsReg intercept:", round(w0, 4),
      "| weights:", paste(round(wk, 4), collapse = ", "), "\n")
  
  # Apply weights across ALL quantile levels -> [n_iter x n_q]
  ens_q <- matrix(NA_real_, nrow = n_iter, ncol = N_Q)
  for (i in seq_len(n_iter)) {
    for (q in seq_len(N_Q)) {
      ens_q[i, q] <- w0 + sum(wk * q_array[i, , q])
    }
  }
  pmax(ens_q, 0)  # clip negatives
}

# EnsWIS: constrained weights on ALL data
build_ens_wis <- function(q_array, actuals) {
  n_models    <- dim(q_array)[2]
  model_names <- dimnames(q_array)[[2]]
  
  valid   <- !is.na(actuals)
  q_valid <- q_array[valid, , , drop = FALSE]
  act_v   <- actuals[valid]
  n_valid <- length(act_v)
  
  obj_wis <- function(w) {
    ens_q <- matrix(NA_real_, nrow = n_valid, ncol = N_Q)
    for (i in seq_len(n_valid)) {
      for (q in seq_len(N_Q)) {
        ens_q[i, q] <- sum(w * q_valid[i, , q])
      }
    }
    sum(compute_wis_vec(ens_q, act_v), na.rm = TRUE)
  }
  
  result <- nloptr(
    x0        = rep(1 / n_models, n_models),
    eval_f    = obj_wis,
    lb        = rep(0, n_models),
    ub        = rep(1, n_models),
    eval_g_eq = function(w) sum(w) - 1,
    opts      = list(algorithm   = "NLOPT_LN_COBYLA",
                     xtol_rel    = 1e-8,
                     ftol_rel    = 1e-8,
                     maxeval     = 5000,
                     print_level = 0)
  )
  
  w_opt <- pmax(result$solution, 0)
  w_opt <- w_opt / sum(w_opt)
  cat("  EnsWIS weights:", paste(round(w_opt, 4), collapse = ", "), "\n")
  
  # Apply to ALL iterations -> [n_iter x n_q]
  n_iter <- dim(q_array)[1]
  ens_q  <- matrix(NA_real_, nrow = n_iter, ncol = N_Q)
  for (i in seq_len(n_iter)) {
    for (q in seq_len(N_Q)) {
      ens_q[i, q] <- sum(w_opt * q_array[i, , q])
    }
  }
  
  list(q_matrix = ens_q, weights = setNames(w_opt, model_names))
}

# ============================================================
#CONVERT TO WEEKLY WIS TIBBLE WITH DATES
# ============================================================

ensemble_to_weekly_wis <- function(ens_q_matrix, actuals, target_key,
                                   horizon, observed_dates) {
  n_iter <- nrow(ens_q_matrix)
  
  map_dfr(seq_len(n_iter), function(i) {
    actual  <- actuals[i]
    q_vec   <- ens_q_matrix[i, ]
    
    # Date of the forecast TARGET (i.e. the week being predicted)
    # iteration i forecasts week i + 5 + horizon - 1 in observed_dates
    target_date_idx <- i + 5 + horizon - 1
    forecast_date   <- if (target_date_idx <= length(observed_dates))
      as.Date(observed_dates[target_date_idx]) else NA
    
    wis_score <- compute_wis_single_epipredict(q_vec, actual)
    
    tibble(
      iteration       = i,
      forecast_date   = forecast_date,   # date of the predicted week
      horizon         = horizon,
      target          = target_key,
      actual          = actual,
      median_forecast = q_vec[which(Q_NAMES == "50%")],
      WIS             = wis_score
    )
  })
}

# ============================================================
#MAIN FUNCTION — ALL DATA, NO SPLIT
# ============================================================

run_ensembles_all_data <- function(year, obs_list, observed_dates) {
  cat("\n============ Year:", year, "============\n")
  
  cat("Loading model quantiles...\n")
  target_data <- load_all_model_quantiles(year, obs_list)
  
  results_by_target <- map(target_data, function(td) {
    cat("\n  Target:", td$key, "\n")
    
    q_array <- td$q_array
    actuals <- td$actuals
    
    # Build all 4 ensembles using ALL data points
    cat("  Building EnsAvg and EnsMedian...\n")
    ens_avg_q    <- build_ens_avg(q_array)
    ens_median_q <- build_ens_median(q_array)
    
    cat("  Fitting EnsReg...\n")
    ens_reg_q    <- build_ens_reg(q_array, actuals)
    
    cat("  Fitting EnsWIS...\n")
    ens_wis_res  <- build_ens_wis(q_array, actuals)
    
    # Convert each to weekly WIS tibble with dates
    make_weekly <- function(q_mat) {
      ensemble_to_weekly_wis(q_mat, actuals, td$tgt_name,
                             td$horizon, observed_dates)
    }
    
    list(
      wis_name       = td$wis_name,
      wis_weights    = ens_wis_res$weights,
      weekly_EnsAvg    = make_weekly(ens_avg_q),
      weekly_EnsMedian = make_weekly(ens_median_q),
      weekly_EnsReg    = make_weekly(ens_reg_q),
      weekly_EnsWIS    = make_weekly(ens_wis_res$q_matrix)
    )
  })
  
  # Collect into wis_all-style lists per ensemble
  extract_wis_list <- function(slot) {
    out <- map(results_by_target, ~ .[[slot]])
    names(out) <- map_chr(results_by_target, ~ .$wis_name)
    out
  }
  
  list(
    year             = year,
    wis_EnsAvg       = extract_wis_list("weekly_EnsAvg"),
    wis_EnsMedian    = extract_wis_list("weekly_EnsMedian"),
    wis_EnsReg       = extract_wis_list("weekly_EnsReg"),
    wis_EnsWIS       = extract_wis_list("weekly_EnsWIS"),
    weights_by_target = map(results_by_target, ~ .$wis_weights)
  )
}

# ============================================================
# RUN FOR A SINGLE YEAR
# ============================================================

#Year <- 2006

results_ <- run_ensembles_all_data(
  year           = Year,
  obs_list       = list(X_obs1 = X_obs1, X_obs2 = X_obs2, X0_obs = X0_obs),
  observed_dates = observed_dates   # your vector of 52 weekly dates
)

# Save
saveRDS(results_, paste0("ensemble_results_alldata_", Year, ".rds"))
cat("Saved:", paste0("ensemble_results_alldata_", Year, ".rds"), "\n")

# ============================================================
# LINEAR POOL ENSEMBLE
# ============================================================

QUANTILE_LEVELS <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                     0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
                     0.95, 0.975, 0.99)
N_Q     <- length(QUANTILE_LEVELS)
Q_NAMES <- c("1%", "2.5%", "5%", "10%", "15%", "20%", "25%", "30%", "35%",
             "40%", "45%", "50%", "55%", "60%", "65%", "70%", "75%", "80%",
             "85%", "90%", "95%", "97.5%", "99%")

TARGET_CONFIG <- list(
  list(key = "total_abundance",     horizon = 1, wis_name = "abundance_1wk",  obs = "X_obs1"),
  list(key = "total_abundance",     horizon = 2, wis_name = "abundance_2wk",  obs = "X_obs1"),
  list(key = "infectious_per_1000", horizon = 1, wis_name = "infected_1wk",   obs = "X_obs2"),
  list(key = "infectious_per_1000", horizon = 2, wis_name = "infected_2wk",   obs = "X_obs2"),
  list(key = "human_cases",         horizon = 1, wis_name = "cases_1wk",      obs = "X0_obs"),
  list(key = "human_cases",         horizon = 2, wis_name = "cases_2wk",      obs = "X0_obs")
)



# ============================================================
# LINEAR POOL 
# ============================================================

build_linear_pool <- function(q_array,
                              quantile_levels = QUANTILE_LEVELS,
                              n_grid = 500,
                              weights = NULL) {
  
  # q_array: [n_iter x n_models x n_quantiles]
  n_iter   <- dim(q_array)[1]
  n_models <- dim(q_array)[2]
  
  if (is.null(weights)) weights <- rep(1 / n_models, n_models)
  
  ens_q_matrix <- matrix(NA_real_, nrow = n_iter, ncol = length(quantile_levels))
  
  for (i in seq_len(n_iter)) {
    
    # Correct 2D extraction: [n_models x n_quantiles]
    model_quantiles <- q_array[i, , ]  
    
    # === EARLY EXIT: all NA ===
    if (all(is.na(model_quantiles))) {
      next
    }
    
    # Get all valid values for grid construction
    valid_all <- as.numeric(model_quantiles[!is.na(model_quantiles)])
    if (length(valid_all) == 0) {
      next
    }
    
    x_min <- min(valid_all)
    x_max <- max(valid_all)
    
    # If all predictions are identical (constant forecast)
    if (abs(x_max - x_min) < 1e-10) {
      constant_val <- x_min
      ens_q_matrix[i, ] <- rep(constant_val, length(quantile_levels))
      next
    }
    
    # Safe grid with small buffer
    delta   <- max(1e-8, (x_max - x_min) * 0.001)
    x_grid  <- seq(x_min - delta, x_max + delta, length.out = n_grid)
    
    # === Build CDF matrix for each model ===
    cdf_matrix <- matrix(NA_real_, nrow = n_models, ncol = n_grid)
    
    for (k in seq_len(n_models)) {
      q_vals <- as.numeric(model_quantiles[k, ])
      q_levs <- quantile_levels
      
      # Remove NAs
      valid_idx <- !is.na(q_vals)
      q_vals <- q_vals[valid_idx]
      q_levs <- q_levs[valid_idx]
      
      if (length(q_vals) == 0) {
        next
      }
      
      # Ensure monotonicity and remove duplicates
      ord    <- order(q_vals)
      q_vals <- q_vals[ord]
      q_levs <- q_levs[ord]
      keep   <- !duplicated(q_vals)
      q_vals <- q_vals[keep]
      q_levs <- q_levs[keep]
      
      if (length(q_vals) == 1) {
        # Step function for constant forecast
        cdf_matrix[k, ] <- ifelse(x_grid < q_vals[1], 0, 1)
        next
      }
      
      # Standard interpolation
      cdf_matrix[k, ] <- approx(
        x    = q_vals,
        y    = q_levs,
        xout = x_grid,
        rule = 2,
        ties = "ordered"
      )$y
    }
    
    # === Linear Pool: average the CDFs ===
    lp_cdf <- as.numeric(weights %*% cdf_matrix)
    
    # === Invert to get ensemble quantiles ===
    if (length(unique(na.omit(lp_cdf))) < 2) {
      # Fallback when pooled CDF is flat
      medians <- model_quantiles[, which.min(abs(quantile_levels - 0.5))]
      fallback_val <- median(medians, na.rm = TRUE)
      if (is.na(fallback_val)) fallback_val <- mean(valid_all, na.rm = TRUE)
      ens_q_matrix[i, ] <- rep(fallback_val, length(quantile_levels))
    } else {
      ens_q_matrix[i, ] <- approx(
        x    = lp_cdf,
        y    = x_grid,
        xout = quantile_levels,
        rule = 2,
        ties = "ordered"
      )$y
    }
  }
  
  ens_q_matrix
}

# ============================================================
# WIS HELPER (epipredict function)
# ============================================================

compute_wis_single_epipredict <- function(q_vec, actual) {
  if (is.na(actual) || any(is.na(q_vec))) return(NA_real_)
  qmat      <- matrix(as.numeric(q_vec), nrow = 1)
  pred_dist <- quantile_pred(qmat, QUANTILE_LEVELS)
  as.numeric(weighted_interval_score(
    x               = pred_dist,
    actual          = actual,
    quantile_levels = QUANTILE_LEVELS,
    na_handling     = "impute"
  ))
}

# ============================================================
# CONVERT TO WEEKLY WIS TIBBLE WITH DATES
# ============================================================

ensemble_to_weekly_wis <- function(ens_q_matrix, actuals, target_key,
                                   horizon, observed_dates) {
  n_iter <- nrow(ens_q_matrix)
  
  map_dfr(seq_len(n_iter), function(i) {
    actual        <- actuals[i]
    q_vec         <- ens_q_matrix[i, ]
    target_idx    <- i + 5 + horizon - 1
    forecast_date <- if (target_idx <= length(observed_dates))
      as.Date(observed_dates[target_idx]) else NA
    
    tibble(
      iteration       = i,
      forecast_date   = forecast_date,
      horizon         = horizon,
      target          = target_key,
      actual          = actual,
      median_forecast = q_vec[which(Q_NAMES == "50%")],
      WIS             = compute_wis_single_epipredict(q_vec, actual)
    )
  })
}

# ============================================================
# LOAD MODEL QUANTILES
# ============================================================

load_all_model_quantiles <- function(year, obs_list) {
  model_names <- c("FullModel", "MHClimate", "MHNoClimate", "FullNoClimate")
  
  result_files <- c(
    FullModel     = paste0("results_FullModel_", year, ".rds"),
    MHClimate     = paste0("results_Mosq+Human+Climate_", year, ".rds"),
    MHNoClimate   = paste0("results_Mosq+Human+NoClimate_", year, ".rds"),
    FullNoClimate = paste0("results_FullModel_NoClimate_", year, ".rds")
  )
  
  all_results <- map(result_files, readRDS)
  n_iter      <- length(all_results[[1]])
  n_models    <- length(model_names)
  
  target_data <- map(TARGET_CONFIG, function(cfg) {
    forecast_key <- paste0(cfg$key, "_q_", cfg$horizon)
    actual_vec   <- obs_list[[cfg$obs]]
    
    actuals <- map_dbl(seq_len(n_iter), function(i) {
      actual_idx <- i + 5 + cfg$horizon - 1
      if (actual_idx > length(actual_vec)) return(NA_real_)
      actual_vec[actual_idx]
    })
    
    q_array <- array(NA_real_, dim = c(n_iter, n_models, N_Q),
                     dimnames = list(NULL, model_names, Q_NAMES))
    
    for (k in seq_along(model_names)) {
      for (i in seq_len(n_iter)) {
        q_vec <- all_results[[k]][[i]][[forecast_key]]
        if (!is.null(q_vec)) {
          q_array[i, k, ] <- as.numeric(q_vec[Q_NAMES])
        }
      }
    }
    
    list(
      key      = paste0(cfg$key, "_", cfg$horizon, "wk"),
      wis_name = cfg$wis_name,
      horizon  = cfg$horizon,
      tgt_name = cfg$key,
      actuals  = actuals,
      q_array  = q_array
    )
  })
  
  names(target_data) <- map_chr(TARGET_CONFIG,
                                ~ paste0(.$key, "_", .$horizon, "wk"))
  target_data
}

# ============================================================
# MAIN FUNCTION
# ============================================================

run_linear_pool <- function(year, obs_list, observed_dates, weights = NULL) {
  cat("\n============ Year:", year, "(Linear Pool) ============\n")
  
  target_data <- load_all_model_quantiles(year, obs_list)
  
  results_by_target <- map(target_data, function(td) {
    cat("  Target:", td$key, "\n")
    
    lp_q <- build_linear_pool(td$q_array,
                              weights = weights)  # NULL = equal weights
    
    list(
      wis_name    = td$wis_name,
      weekly_wis  = ensemble_to_weekly_wis(lp_q, td$actuals, td$tgt_name,
                                           td$horizon, observed_dates)
    )
  })
  
  wis_list       <- map(results_by_target, ~ .$weekly_wis)
  names(wis_list) <- map_chr(results_by_target, ~ .$wis_name)
  
  cat("\n--- Linear Pool Summary ---\n")
  print(summarize_wis_results(wis_list))
  
  cat("\n--- Normalised WIS ---\n")
  print(calculate_normalized_wis_custom(wis_list))
  
  list(year = year, wis_LinearPool = wis_list)
}

# ============================================================
# RUN FOR A SINGLE YEAR
# ============================================================

#Year <- 2006

lp_results <- run_linear_pool(
  year           = Year,
  obs_list       = list(X_obs1 = X_obs1, X_obs2 = X_obs2, X0_obs = X0_obs),
  observed_dates = observed_dates
)

saveRDS(lp_results, paste0("linear_pool_results_", Year, ".rds"))

# View weekly WIS for one target
# lp_results$wis_LinearPool$abundance_1wk %>%
#   select(forecast_date, actual, median_forecast, WIS) %>%
#   print(n = 50)
# 
# # Plot using your existing function
# plot_wis_custom_results(lp_results$wis_LinearPool,
#                         add_dates  = TRUE,
#                         start_date = as.Date(observed_dates[7]))


#Calculation of relative wis with ensemble models

wis_all1 = readRDS(paste0("wis_all_FullModel_", Year, ".rds"))
wis_all2 = readRDS(paste0("wis_all_Mosq+Human+Climate_", Year, ".rds")) 
wis_all3 = readRDS(paste0("wis_all_Mosq+Human+NoClimate_", Year, ".rds")) 
wis_all4 = readRDS(paste0("wis_all_FullModel_NoClimate_", Year, ".rds"))  
wis_allR = readRDS(paste0("wis_all_BaselineModel_", Year, ".rds")) 
wis_all5 = readRDS(paste0("ensemble_results_alldata_", Year, ".rds"))
wis_all6 <- readRDS(paste0("linear_pool_results_", Year, ".rds"))
# ── Original extraction function (unchanged) ──────────────────────────────────
extract_wis <- function(model_list, model_name) {
  bind_rows(
    model_list[["abundance_1wk"]] %>% mutate(time = forecast_1week_dates, model = model_name, target = "Total abundance",           horizon = 1L),
    model_list[["infected_1wk"]]  %>% mutate(time = forecast_1week_dates, model = model_name, target = "Infectious mosq per 1000",  horizon = 1L),
    model_list[["cases_1wk"]]     %>% mutate(time = forecast_1week_dates, model = model_name, target = "Human cases",               horizon = 1L),
    model_list[["abundance_2wk"]] %>% mutate(time = forecast_2week_dates, model = model_name, target = "Total abundance",           horizon = 2L),
    model_list[["infected_2wk"]]  %>% mutate(time = forecast_2week_dates, model = model_name, target = "Infectious mosq per 1000",  horizon = 2L),
    model_list[["cases_2wk"]]     %>% mutate(time = forecast_2week_dates, model = model_name, target = "Human cases",               horizon = 2L)
  ) %>%
    dplyr::select(time, model, target, horizon, WIS) %>%
    arrange(target, horizon, time)
}

# ── New extraction function for wis_all5 (multiple models in one file) ─────────
# Maps internal names to display names
ensemble_name_map <- c(
  wis_EnsAvg    = "Ensemble_model_1",
  wis_EnsMedian = "Ensemble_model_2",
  wis_EnsReg    = "Ensemble_model_3",
  wis_EnsWIS    = "Ensemble_model_4",
  wis_LinearPool = "Ensemble_model_5" 
)

extract_wis_ensemble <- function(results_file, name_map) {
  map_dfr(names(name_map), function(internal_name) {
    model_list <- results_file[[internal_name]]   # e.g. results_2017$wis_EnsAvg
    # Skip if this key doesn't exist in this file
    if (is.null(model_list)) return(NULL)
    display_name <- name_map[[internal_name]]
    extract_wis(model_list, display_name)
  })
}
lp_as_ensemble <- list(
  wis_LinearPool = map(lp_results$wis_LinearPool, function(tbl) {
    tbl %>% rename(time = forecast_date)   # align column name
  })
)
# ── Combine all models ─────────────────────────────────────────────────────────
all_wis_long <- bind_rows(
  extract_wis(wis_all1, "Full Model"),
  extract_wis(wis_all2, "Mosq+Human+Climate"),
  extract_wis(wis_all3, "Mosq+Human+NoClimate"),
  extract_wis(wis_all4, "FullModel_NoClimate"),
  extract_wis(wis_allR, "Baseline"),
  extract_wis_ensemble(wis_all5, ensemble_name_map),   # adds all 4 ensemble models
  extract_wis_ensemble(lp_as_ensemble, ensemble_name_map)
)

#glimpse(all_wis_long)

plot_wis <- function(data, target_name, horizon_val, title_prefix = "") {
  data %>%
    filter(target == target_name, horizon == horizon_val) %>%
    ggplot(aes(x = time, y = WIS, color = model, group = model)) +
    geom_line(linewidth = 0.9, alpha = 0.9) +
    labs(
      title    = paste(title_prefix, target_name, "–", horizon_val, "week ahead WIS"),
      x        = "Target date",
      y        = "WIS score (lower = better)",
      color    = "Model"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom",
          axis.title = element_text(size = 17),
          axis.text = element_text(size = 17),
          legend.text = element_text(size = 15),
          plot.title = element_text(face = "bold", size = 17))+
    scale_x_date(date_breaks = "2 month", date_labels = "%b %Y")
}

# Generate the six plots
# p_ab1 <- plot_wis(all_wis_long, "Total abundance", 1)
# p_ab2 <- plot_wis(all_wis_long, "Total abundance", 2)
# p_in1 <- plot_wis(all_wis_long, "Infectious mosq per 1000",  1)
# p_in2 <- plot_wis(all_wis_long, "Infectious mosq per 1000",  2)
# p_ca1 <- plot_wis(all_wis_long, "Human cases",     1)
# p_ca2 <- plot_wis(all_wis_long, "Human cases",     2)

# Show one example
#print(p_ab1)

# Save if you want
# ggsave("abundance_1wk_wis.png", p_ab1, width = 10, height = 6, dpi = 140)

# Baseline as reference
baseline_wide <- all_wis_long %>%
  filter(model == "Baseline") %>%
  dplyr::select(time, target, horizon, wis_baseline = WIS)

all_rel_long <- all_wis_long %>%
  filter(model != "Baseline") %>%
  left_join(baseline_wide, by = c("time", "target", "horizon")) %>%
  mutate(rel_wis = WIS / wis_baseline)


all_rel_long <- all_rel_long %>%
  filter(is.finite(rel_wis))
#glimpse(all_rel_long)
plot_rel <- function(data, target_name, horizon_val) {
  plot_wis(data, target_name, horizon_val, title_prefix = "Relative") +
    aes(y = rel_wis) +
    labs(y = "Relative WIS (model / baseline, < 1 = better than baseline)") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.7)
}

# Example
# print(plot_rel(all_rel_long, "Total abundance", 1))
# print(plot_rel(all_rel_long, "Total abundance", 2))
# print(plot_rel(all_rel_long, "Human cases", 1))
# print(plot_rel(all_rel_long, "Human cases", 2))
# print(plot_rel(all_rel_long, "Infectious mosq per 1000", 1))
# print(plot_rel(all_rel_long, "Infectious mosq per 1000", 2))
saveRDS(all_wis_long, file = paste0("all_wis_longE_", Year, ".rds"))
saveRDS(all_rel_long, file = paste0("all_rel_longE_", Year, ".rds"))
cat("\nYear", Year, "complete. Results saved.\n")
}

cat("\nAll years complete.\n")


years <- c(2006:2019, 2021)

all_data <- map_df(years, function(year) {
  readRDS(paste0("all_rel_longE_", year, ".rds")) %>%
    mutate(year = year)
})

# Calculate mean and median, handling Inf values
summary_stats <- all_data %>%
  group_by(year, model, target, horizon) %>%
  summarise(
    mean_rel_wis = mean(rel_wis[is.finite(rel_wis)], na.rm = TRUE),
    median_rel_wis = median(rel_wis[is.finite(rel_wis)], na.rm = TRUE),
    .groups = "drop"
  )


#print(summary_stats)

#  Export summary statistics
#write.csv(summary_stats, "rel_wis_summary_stats.csv", row.names = FALSE)
#log scale
summary_stats_log <- summary_stats %>%
  mutate(mean_rel_wis = log(mean_rel_wis),
         median_rel_wis = log(median_rel_wis))
summary_stats_log <- summary_stats_log %>%
  mutate(model = recode(model,
                        "Full Model" = "FullModel",
                        "FullModel_NoClimate" = "FullModel_NoWeather",
                        "Mosq+Human+Climate"           = "Mosq+Human+Weather",
                        "Mosq+Human+NoClimate"           = "Mosq+Human+NoWeather",
                        .default = model   # keeps all other model names unchanged
  ))

#to rank models calculation
rank = summary_stats_log %>%
  filter(target == "Infectious mosq per 1000") %>%
  group_by(model) %>%
  summarise(
    median_relWIS = median(median_rel_wis, na.rm = TRUE),
    mean_relWIS   = mean(median_rel_wis, na.rm = TRUE),
    n             = n(),
    .groups = "drop"
  ) %>%
  arrange(median_relWIS)
rank2 = summary_stats_log %>%
  filter(target == "Infectious mosq per 1000") %>%
  group_by(model, horizon) %>%
  summarise(median_relWIS = median(median_rel_wis, na.rm = TRUE), .groups = "drop") %>%
  arrange(horizon, median_relWIS)


individual_models <- c("Full Model", "FullModel_NoClimate",
                       "Mosq+Human+Climate", "Mosq+Human+NoClimate")

ensemble_models <- c("Ensemble_model_1", "Ensemble_model_2",
                     "Ensemble_model_3", "Ensemble_model_4",
                     "Ensemble_model_5")

# ════════════════════════════════════════════════════════════════
# 1. "No individual model ranked in top half of all models for
#    more than 80% of year-target-horizon combinations"
# ════════════════════════════════════════════════════════════════

# ── (a) Rank among the 4 individual models only ──────────────────
rank_among_4 <- summary_stats_log %>%
  filter(model %in% individual_models) %>%
  group_by(year, target, horizon) %>%
  mutate(
    rank_4     = rank(median_rel_wis, ties.method = "average"),
    n_models_4 = n()
  ) %>%
  ungroup()

top_half_4 <- rank_among_4 %>%
  group_by(model) %>%
  summarise(
    n_combinations = n(),                          
    pct_top_half   = mean(rank_4 <= n_models_4 / 2) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_top_half))

print(top_half_4)

# ── (b) Rank among all 9 models (4 individual + 5 ensemble) ──────
rank_among_9 <- summary_stats_log %>%
  filter(model %in% c(individual_models, ensemble_models)) %>%
  group_by(year, target, horizon) %>%
  mutate(
    rank_9     = rank(median_rel_wis, ties.method = "average"),
    n_models_9 = n()
  ) %>%
  ungroup()

top_half_9 <- rank_among_9 %>%
  filter(model %in% individual_models) %>%          # only report the 4 individual models' standing
  group_by(model) %>%
  summarise(
    n_combinations = n(),                           
    pct_top_half   = mean(rank_9 <= n_models_9 / 2) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_top_half))

print(top_half_9)

cat("Max % top-half, 4-model comparison:", round(max(top_half_4$pct_top_half), 1), "%\n")
cat("Max % top-half, 9-model comparison:", round(max(top_half_9$pct_top_half), 1), "%\n")


# ════════════════════════════════════════════════════════════════
# 2. Human cases — "[X] of four models... more than two-thirds
#    of year-horizon combinations" (resolves Option A vs B)
# ════════════════════════════════════════════════════════════════

human_cases_individual <- summary_stats_log %>%
  filter(target == "Human cases", model %in% individual_models)

# ── Option A: per-model event rate ────────────────────────────────
option_A <- human_cases_individual %>%
  group_by(model) %>%
  summarise(
    n_combinations        = n(),                    
    n_better_than_base    = sum(median_rel_wis < 0),
    pct_better_than_base  = mean(median_rel_wis < 0) * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(pct_better_than_base))

print(option_A)

n_models_clearing_bar <- sum(option_A$pct_better_than_base > (2/3) * 100)
cat("Models with >2/3 of combinations better than baseline:",
    n_models_clearing_bar, "of 4\n")

# ── Option B: pooled statistic across all 4 models combined ──────
option_B <- human_cases_individual %>%
  summarise(
    n_total               = n(),                     
    n_better_than_base    = sum(median_rel_wis < 0),
    pct_better_than_base  = mean(median_rel_wis < 0) * 100
  )

print(option_B)
model_colors <- c(
  # 4 component models
  "FullModel"               = "#009E73",  # bluish green
  "FullModel_NoWeather"     = "#D55E00",  # vermillion
  "Mosq+Human+Weather"      = "#E69F00",  # orange
  "Mosq+Human+NoWeather"    = "#0072B2",  # blue
  
  
  # 5 ensemble models
  "Ensemble_model_1"        = "#56B4E9",  # sky blue
  "Ensemble_model_2"        = "#CC79A7",  # reddish purple
  "Ensemble_model_3"        = "#F0E442",  # yellow
  "Ensemble_model_4"        = "#999999",  # grey
  "Ensemble_model_5"        = "#000000"   # black
)

#This is for more than 8 models
plot_boxplots <- function(data, stat_type = "mean", horizon_num = 1, color_map = model_colors) {
  
  # Select the statistic
  if (stat_type == "mean") {
    data <- data %>% rename(value = mean_rel_wis)
    y_label <- "Mean Relative WIS (log scale)"
  } else {
    data <- data %>% rename(value = median_rel_wis)
    y_label <- "Median Relative WIS (log scale)"
  }
  
  # Filter for specific horizon
  plot_data <- data %>% 
    filter(horizon == horizon_num) %>%
    # Handle any remaining Inf/NaN values
    filter(is.finite(value))
  
  # Only keep colors for models present in this plot
  models_present <- unique(plot_data$model)
  colors_used    <- color_map[models_present]
  
  # Create plot with facets for free y-axis
  p <- ggplot(plot_data, aes(x = model, y = value, fill = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_point(aes(color = model), 
               position = position_jitter(width = 0.2),
               size = 2.5, alpha = 0.8) +
    geom_text(aes(label = year, color = model),
              position = position_jitter(width = 0.2), fontface = "bold", 
              size = 2.5, vjust = -0.8, show.legend = FALSE) +
    facet_wrap(~target, scales = "free_y", ncol = 3) +
    scale_fill_manual(values  = colors_used) +   # <-- changed
    scale_color_manual(values = colors_used) +
    labs(
      title = paste0(y_label, " by Model and Target (", horizon_num, "-week ahead forecast)"),
      subtitle = "Dashed line at 0.0 indicates relative WIS (model / baseline, < 0 = better than baseline)", 
      x = "",
      y = y_label,
      fill = "Model",
      color = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle  = 90,
                                  hjust  = 1,                       # right-align at the tick mark
                                  vjust  = 0.5,                     # vertically centred on tick
                                  size   = 19,
                                  face   = "bold"),  
      axis.ticks.x = element_blank(),
      axis.title = element_text(size = 19, face   = "bold"),
      plot.title = element_text(face = "bold", size = 19),
      plot.subtitle = element_text(size = 16, face   = "bold"),  # Added subtitle size
      legend.text = element_text(size = 19, color = "grey40"),  # Reduced from 17 to fit better
      legend.key.size = unit(0.6, "cm"),  # Added to control legend size
      strip.text = element_text(face = "bold", size = 19)
    ) +
    guides(
      fill = guide_legend(nrow = 3, byrow = TRUE),  # Changed to 3 rows
      color = guide_legend(nrow = 3, byrow = TRUE)
    )
  
  return(p)
}
#if i want to filter
summary_stats_log <- summary_stats_log %>%
  filter(model %in% c("Ensemble_model_4",
                      "FullModel",
                      "FullModel_NoWeather",
                      "Mosq+Human+Weather",
                      "Mosq+Human+NoWeather"
                      ))
# Generate plots
mean_h1 <- plot_boxplots(summary_stats_log, stat_type = "mean", horizon_num = 1)
mean_h2 <- plot_boxplots(summary_stats_log, stat_type = "mean", horizon_num = 2)
median_h1 <- plot_boxplots(summary_stats_log, stat_type = "median", horizon_num = 1)
median_h2 <- plot_boxplots(summary_stats_log, stat_type = "median", horizon_num = 2)

# Display plots
print(mean_h1)
print(mean_h2)
print(median_h1)
print(median_h2)

# Save with extra height for legend
ggsave("mean_h1.png", mean_h1, width = 16, height = 10, dpi = 300)
ggsave("mean_h2.png", mean_h2, width = 16, height = 10, dpi = 300)
ggsave("median_h1.pdf", median_h1, width = 16, height = 10, dpi = 300)
ggsave("median_h2.pdf", median_h2, width = 16, height = 10, dpi = 300)

#saving when i dont filter
ggsave("median_h1_full.pdf", median_h1, width = 16, height = 10, dpi = 300)
ggsave("median_h2_full.pdf", median_h2, width = 16, height = 10, dpi = 300)
library(patchwork)
combined <- (median_h2/final_combined) 

ggsave(
  filename = "median_plot_final_forecast_all_targets_2014.png",
  plot     = combined,
  width    = 28,
  height   = 25,
  dpi      = 300,
  bg       = "white"
)
ggsave(
  filename = "median_plot_final_forecast_all_targets_2014.pdf",
  plot     = combined,
  width    = 28,
  height   = 25,
  dpi      = 300,
  bg       = "white"
)

install.packages("patchwork")
library(patchwork)
median_plot / median_h2

#box plots by months

# Step 1: Load and combine all years with month extraction
years <- c(2006:2019, 2021)

all_data <- map_df(years, function(year) {
  readRDS(paste0("all_rel_longE_", year, ".rds")) %>%
    mutate(year = year,
           month = month(time, label = TRUE, abbr = TRUE),  # Extract month (Jan, Feb, etc.)
           month_num = month(time))  # For sorting
})

#  Calculate mean and median by month (across all years)
summary_stats_by_month <- all_data %>%
  group_by(month, month_num, model, target, horizon) %>%
  summarise(
    mean_rel_wis = mean(rel_wis[is.finite(rel_wis)], na.rm = TRUE),
    median_rel_wis = median(rel_wis[is.finite(rel_wis)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month_num)  # Ensure months are in order

#  Keep individual year data for points
individual_points <- all_data %>%
  filter(is.finite(rel_wis)) %>%
  mutate(month = month(time, label = TRUE, abbr = TRUE),
         month_num = month(time)) %>%
  group_by(year, month, month_num, model, target, horizon) %>%
  summarise(
    mean_rel_wis = mean(rel_wis, na.rm = TRUE),
    median_rel_wis = median(rel_wis, na.rm = TRUE),
    .groups = "drop"
  )


plot_boxplots_by_month <- function(individual_data, stat_type = "median", horizon_num = 1, color_map = model_colors) {
  
  if (stat_type == "mean") {
    individual_data <- individual_data %>% rename(value = mean_rel_wis)
    y_label <- "Mean Relative WIS (log scale)"
  } else {
    individual_data <- individual_data %>% rename(value = median_rel_wis)
    y_label <- "Median Relative WIS (log scale)"
  }
  
  plot_data <- individual_data %>% 
    filter(horizon == horizon_num) %>%
    filter(is.finite(value)) %>%
    arrange(month_num) %>%
    mutate(month = factor(month, levels = month.abb))
  # Only keep colors for models present in this plot
  models_present <- unique(plot_data$model)
  colors_used    <- color_map[models_present]
  p <- ggplot(plot_data, aes(x = month, y = value, fill = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8, alpha = 0.7) +
    # Points behind, smaller and more transparent
    geom_point(aes(color = model), 
               position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
               size = 1.5, alpha = 0.4) +
    geom_text(aes(label = year, color = model),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),fontface = "bold", 
              size = 1.8, vjust = -0.8, alpha = 0.7, show.legend = FALSE) +
    # Boxplot on top with thicker lines
    geom_boxplot(alpha = 0.8, outlier.shape = NA, linewidth = 0.7) +
    facet_wrap(~target, scales = "free_y", ncol = 1) +
    scale_fill_manual(values  = colors_used) +   # <-- changed
    scale_color_manual(values = colors_used) +
    labs(
      title = paste0(y_label, " by Model and Month (", horizon_num, "-week ahead forecast)"),
      subtitle = "Each box aggregates across all years for that month. Dashed line at 0.0 = baseline",
      x = "Month",
      y = y_label,
      fill = "Model",
      color = "Model"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 17, face = "bold"),
      axis.title = element_text(size = 17, face = "bold"),
      plot.title = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 17, face = "bold"),
      strip.text = element_text(face = "bold", size = 17)
    ) +
    guides(fill = guide_legend(nrow = 2))
  
  return(p)
}

individual_points_log <- individual_points %>%
  mutate(mean_rel_wis = log(mean_rel_wis),
         median_rel_wis = log(median_rel_wis))
individual_points_log <- individual_points_log %>%
  mutate(model = recode(model,
                        "Full Model" = "FullModel",
                        "FullModel_NoClimate" = "FullModel_NoWeather",
                        "Mosq+Human+Climate"           = "Mosq+Human+Weather",
                        "Mosq+Human+NoClimate"           = "Mosq+Human+NoWeather",
                        .default = model   # keeps all other model names unchanged
  ))
#if i want to filter
individual_points_log <- individual_points_log %>%
  filter(model %in% c("Ensemble_model_4",
                      "FullModel",
                      "Mosq+Human+Weather",
                      "Mosq+Human+NoWeather",
                      "FullModel_NoWeather"))

median_h1_month <- plot_boxplots_by_month(individual_points_log, stat_type = "median", horizon_num = 1)
median_h2_month <- plot_boxplots_by_month(individual_points_log, stat_type = "median", horizon_num = 2)
mean_h1_month <- plot_boxplots_by_month(individual_points_log, stat_type = "mean", horizon_num = 1)
mean_h2_month <- plot_boxplots_by_month(individual_points_log, stat_type = "mean", horizon_num = 2)

# Display
print(median_h1_month)
print(median_h2_month)
print(mean_h1_month)
print(mean_h2_month)
# Save
ggsave("log_median_rel_wis_by_month_horizon1.pdf", median_h1_month, width = 14, height = 16, dpi = 300)
ggsave("log_median_rel_wis_by_month_horizon2.pdf", median_h2_month, width = 14, height = 16, dpi = 300)
# Save when I dont filter
ggsave("log_median_rel_wis_by_month_horizon1_full.pdf", median_h1_month, width = 16, height = 16, dpi = 300)
ggsave("log_median_rel_wis_by_month_horizon2_full.pdf", median_h2_month, width = 16, height = 16, dpi = 300)


ggsave("log_mean_rel_wis_by_month_horizon1.pdf", mean_h1_month, width = 14, height = 16, dpi = 300)
ggsave("log_mean_rel_wis_by_month_horizon2.pdf", mean_h2_month, width = 14, height = 16, dpi = 300)

# ============================================================
# Heatmap: Median Relative WIS by Model x Month, faceted by Year
# Y-axis: model names
# X-axis: calendar month (Jan-Dec)
# Fill:   median relative WIS (blue = better, red = worse)
# Facet:  one panel per year (15 years)
# ============================================================



# ================================================
# Load and combine all yearly data
# ================================================
years <- c(2006:2019, 2021)

all_data <- map_df(years, function(year) {
  readRDS(paste0("all_rel_longE_", year, ".rds")) %>%
    mutate(year = year)
})

# ================================================
# Clean model names
# ================================================
all_data <- all_data %>%
  mutate(model = recode(model,
                        "FullModel_NoClimate"  = "FullModel_WithoutWeather",
                        "Mosq+Human+Climate"   = "Mosq+Human+Weather",
                        "Mosq+Human+NoClimate" = "Mosq+Human+WithoutWeather",
                        .default = model
  ))

# ================================================
# Prepare heatmap data
# ================================================
heatmap_data <- all_data %>%
  filter(is.finite(rel_wis)) %>%
  mutate(
    month     = month(time, label = TRUE, abbr = TRUE),
    month_num = month(time)
  ) %>%
  group_by(year, month, month_num, model, target, horizon) %>%
  summarise(
    median_relWIS = log(median(rel_wis, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    month = factor(month, levels = month.abb),
    relWIS_capped = pmax(pmin(median_relWIS, 7), -7)
  )

# ================================================
# ORDER MODELS CONSISTENTLY (using rev(sort(unique)))
# ================================================
model_order <- rev(sort(unique(heatmap_data$model)))

heatmap_data <- heatmap_data %>%
  mutate(model = factor(model, levels = model_order))

# ================================================
# Define targets
# ================================================
targets_list <- c(
  "Total abundance"          = "Total abundance",
  "Infectious mosq per 1000" = "Infectious mosq per 1000",
  "Human cases"              = "Human cases"
)

# ================================================
# Plotting function
# ================================================
plot_heatmap <- function(target_name, target_value, horizon_value) {
  
  horizon_label <- ifelse(horizon_value == 1, "1-Week Ahead", "2-Week Ahead")
  
  p <- ggplot(
    heatmap_data %>% 
      filter(target == target_value, horizon == horizon_value),
    aes(x = month, y = model, fill = relWIS_capped)
  ) +
    geom_tile(colour = "white", linewidth = 0.3) +
    
    geom_vline(xintercept = seq(0.5, 12.5, by = 1),
               colour = "white", linewidth = 0.25) +
    
    facet_wrap(~ year, ncol = 5, nrow = 3) +
    
    scale_fill_gradient2(
      low      = "#2166ac",
      mid      = "white",
      high     = "#d6604d",
      midpoint = 0,
      limits   = c(-7, 7),
      oob      = scales::squish,
      name     = "Median\nRelative WIS",
      breaks   = c(-7, 0, 7),
      labels   = c("≥ -7", "0\n(baseline)", "≤ 7")
    ) +
    
    labs(
      title    = paste0("Median Relative WIS — ", target_name, " | ", horizon_label),
      subtitle = "Blue = better than baseline  |  Red = worse than baseline\nEach cell = median log(relative WIS) across weeks in the month",
      x = "Month",
      y = "Model"
    ) +
    
    theme_minimal(base_size = 18) +
    theme(
      plot.title      = element_text(face = "bold", size = 20, hjust = 0),
      plot.subtitle   = element_text(size = 19, colour = "grey40"),
      strip.text      = element_text(face = "bold", size = 11),
      axis.text.x     = element_text(angle = 90, hjust = 1, size = 19, face = "bold"),
      axis.text.y     = element_text(size = 19, face = "bold"),
      axis.title      = element_text(size = 19, face = "bold"),
      legend.position = "right",
      legend.text     = element_text(size = 19),
      legend.title    = element_text(size = 19, face = "bold"),
      panel.grid      = element_blank(),
      panel.spacing   = unit(0.5, "lines")
    )
  
  return(p)
}

# ================================================
#  Generate all 6 plots
# ================================================
for (tgt in names(targets_list)) {
  for (h in c(1, 2)) {
    
    target_clean <- gsub(" ", "_", gsub(" per 1000", "", tolower(tgt)))
    horizon_clean <- ifelse(h == 1, "h1", "h2")
    
    p <- plot_heatmap(
      target_name   = tgt,
      target_value  = targets_list[[tgt]],
      horizon_value = h
    )
    
    filename <- paste0("heatmap_relWIS_", target_clean, "_", horizon_clean, ".pdf")
    
    ggsave(
      filename = filename,
      plot     = p,
      width    = 20,
      height   = 14,
      dpi      = 300,
      bg       = "white"
    )
    
    message("Saved: ", filename)
  }
}

message("All 6 heatmaps have been generated successfully!")

# ============================================================
# Heatmap: Median Relative WIS by Model x Month — Year 2014
# Layout: 1 row x 3 columns
# Col 1: Human Cases
# Col 2: Infectious Mosq per 1000
# Col 3: Total Abundance
# ============================================================


years <- c(2006:2019, 2021)

all_data <- map_df(years, function(year) {
  readRDS(paste0("all_rel_longE_", year, ".rds")) %>%
    mutate(year = year)
})
# ── Build month x model summary for 2014 only ─────────────────────────────────
heatmap_2014 <- all_data %>%
  filter(is.finite(rel_wis), year == 2014) %>%
  mutate(
    month     = month(time, label = TRUE, abbr = TRUE),
    month_num = month(time)
  ) %>%
  group_by(month, month_num, model, target, horizon) %>%
  summarise(
    median_relWIS = median(log(rel_wis), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month         = factor(month, levels = month.abb),
    relWIS_capped = pmax(pmin(median_relWIS, 4.5), -4.5)  # cap at ±3
  )
heatmap_2014 <- heatmap_2014 %>%
  mutate(model = recode(model,
                        "Full Model" = "FullModel",
                        "FullModel_NoClimate" = "FullModel_NoWeather",
                        "Mosq+Human+Climate"           = "Mosq+Human+Weather",
                        "Mosq+Human+NoClimate"           = "Mosq+Human+NoWeather",
                        .default = model   # keeps all other model names unchanged
  ))
heatmap_2014 <- heatmap_2014 %>%
  filter(model %in% c("Ensemble_model_4",
                      "FullModel",
                      "FullModel_NoWeather",
                      "Mosq+Human+Weather",
                      "Mosq+Human+NoWeather"
  ))
# ── Set consistent model order ─────────────────────────────────────────────────
model_order <- c(
  "Ensemble_model_4",
  "FullModel",
  "FullModel_NoWeather",
  "Mosq+Human+Weather",
  "Mosq+Human+NoWeather"
)

# If you have ensemble models too, add them here:
# model_order <- c(model_order,
#   "Ensemble_model_1", "Ensemble_model_2", "Ensemble_model_3",
#   "Ensemble_model_4", "Ensemble_model_5")

heatmap_2014 <- heatmap_2014 %>%
  mutate(model = factor(model, levels = rev(model_order)))
# rev() so first model appears at top of y-axis

# ── Target order and labels ────────────────────────────────────────────────────
target_order <- c("Human cases", "Infectious mosq per 1000", "Total abundance")

target_label_map <- c(
  `Human cases`         = "Human cases",
  `Infectious mosq per 1000` = "Infectious mosq per 1000",
  `Total abundance`     = "Total abundance"
)

heatmap_2014 <- heatmap_2014 %>%
  mutate(
    target = factor(target,
                    levels  = target_order,
                    labels  = target_label_map[target_order])
  )

# ── Build plot ─────────────────────────────────────────────────────────────────
for (h in c(1, 2)) {
  
  horizon_label <- ifelse(h == 1, "1-Week Ahead", "2-Week Ahead")
  p <- ggplot(
  heatmap_2014 %>% filter(horizon == h),
  aes(x = month, y = model, fill = relWIS_capped)
) +
  
  geom_tile(colour = "white", linewidth = 0.4) +
  
  # Facet: one column per target, fixed order
  facet_wrap(
    ~ target,
    ncol     = 1,
    nrow     = 3
  ) +
  
  # Diverging colour scale: blue = better, white = baseline, red = worse
  scale_fill_gradient2(
    low      = "#2166ac",
    mid      = "white",
    high     = "#d6604d",
    midpoint = 0,
    limits   = c(-4.5, 4.5),
    oob      = scales::squish,
    name     = "Median\nRelative WIS\n(log scale)",
    breaks   = c(-4, 0, 4),
    labels   = c("≥ -4",  "0\n(baseline)",  "≤ 4")
  ) +
  
  labs(
    title    = paste0("Median Relative WIS — 2014  |  ",
                      horizon_label),
    subtitle = paste0(
      "Blue = better than baseline  |  ",
      "Red = worse than baseline  |  ",
      "Each cell = median relative WIS across weeks in that month"
    ),
    x = "Month",
    y = "Model"
  ) +
  
  theme_minimal(base_size = 19) +
  theme(
    plot.title       = element_text(face = "bold", size = 19, hjust = 0),
    plot.subtitle    = element_text(size = 19, colour = "grey40"),
    strip.text       = element_text(face = "bold", size = 19),
    axis.text.x      = element_text(angle = 90, hjust = 1, size = 19, face = "bold"),
    axis.text.y      = element_text(size = 19, face = "bold"),
    axis.title       = element_text(size = 19, face = "bold"),
    legend.position  = "right",
    legend.text      = element_text(size = 19, face = "bold"),
    legend.title     = element_text(size = 11, face = "bold"),
    panel.grid       = element_blank(),
    panel.spacing    = unit(0.6, "lines")
  )

# ── Save ───────────────────────────────────────────────────────────────────────
ggsave(
  filename = paste0("heatmap_2014_relWIS_horizon", h, ".pdf"),
  plot     = p,
  width    = 18,
  height   = 15,
  dpi      = 300,
  bg       = "white"
)
message("Saved: heatmap_2014_relWIS_horizon", h, ".pdf")
}

#ensemble 4 exploration
#  saved files
ensemble4_files <- list("Ensemble_model_4" = "all_wis_longE_")

ensemble4_monthly <- map_dfr(names(ensemble4_files), function(model_name) {
  map_dfr(years, function(yr) {
    fpath <- paste0(ensemble4_files[[model_name]], yr, ".rds")
    if (!file.exists(fpath)) {
      message("File not found: ", fpath)
      return(NULL)
    }
    
    # Read the full data and filter for Ensemble_model_4
    all_wis <- readRDS(fpath)
    
    ens4_data <- all_wis %>%
      filter(model == "Ensemble_model_4") %>%
      mutate(year = yr) %>%
      # Create iteration within each horizon/target combination
      group_by(horizon, target) %>%
      arrange(time) %>%
      mutate(iteration = row_number()) %>%
      ungroup() %>%
      # Create forecast_week and forecast_date
      mutate(
        model = model_name,
        forecast_week = if_else(horizon == 1, iteration + 5, iteration + 6),
        forecast_date = as.Date(paste0(yr, "-01-01")) + (forecast_week - 1) * 7
      ) %>%
      select(year, model, horizon, target, iteration, WIS, forecast_date)
    
    return(ens4_data)
  })
})


print(ensemble4_monthly)


ensemble4_relwis_monthly <- ensemble4_monthly %>%
  left_join(baseline_join, by = c("year", "horizon", "target", "iteration")) %>%
  filter(is.finite(WIS), is.finite(WIS_baseline), WIS_baseline > 0) %>%
  mutate(
    rel_wis = log(WIS / WIS_baseline),
    month = month(forecast_date, label = TRUE, abbr = TRUE)
  )

ensemble4_monthly_summary <- ensemble4_relwis_monthly %>%
  group_by(model, target, month) %>%
  summarise(median_relWIS = median(rel_wis, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(target, month)

print(ensemble4_monthly_summary, n = 50)
