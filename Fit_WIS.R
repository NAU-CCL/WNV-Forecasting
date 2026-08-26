library(patchwork)
#Full Model
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

probs <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
           0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
           0.95, 0.975, 0.99)

save_ensemble_full_global <- readRDS(paste0("save_ensemble_full_global_FullModel_", Year, ".rds"))
results <- vector("list", 50)  # 50 weeks

for (week in 1:50) {
  
  total_abundance      <- save_ensemble_full_global[1, 1:8000, week] + 
    save_ensemble_full_global[2, 1:8000, week]
  
  infectious_per_1000  <- (save_ensemble_full_global[2, 1:8000, week] /
                             (save_ensemble_full_global[1, 1:8000, week] + 
                                save_ensemble_full_global[2, 1:8000, week])) * 1000
  human_cases          <- save_ensemble_full_global[8, 1:8000, week]
  
  results[[week]] <- list(
    total_abundance_q     = quantile(total_abundance,     probs = probs, na.rm = TRUE),
    infectious_per_1000_q = quantile(infectious_per_1000, probs = probs, na.rm = TRUE),
    human_cases_q         = quantile(human_cases,         probs = probs, na.rm = TRUE)
  )
}

saveRDS(results, file = paste0("fit_results_FullModel_", Year, ".rds"))
# You need actual observed weekly values for each year:
X_obs1 = X_obs1[1:50]
X_obs2 = X_obs2[1:50]
X0_obs = X0_obs[1:50]

quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                     0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
                     0.95, 0.975, 0.99)

calculate_wis_per_year <- function(results, X_obs1, X_obs2, X0_obs, quantile_levels) {
  # results is a list of 50 weeks, each with _q entries
  # X_obs1/2/X0_obs are vectors of length 50 (observed value per week)
  
  n_weeks <- length(results)
  
  targets <- list(
    total_abundance     = list(q_key = "total_abundance_q",     obs = X_obs1),
    infectious_per_1000 = list(q_key = "infectious_per_1000_q", obs = X_obs2),
    human_cases         = list(q_key = "human_cases_q",         obs = X0_obs)
  )
  
  wis_out <- map_dfr(names(targets), function(tgt) {
    
    q_key <- targets[[tgt]]$q_key
    obs   <- targets[[tgt]]$obs
    
    map_dfr(1:n_weeks, function(week) {
      
      q_vec  <- as.numeric(results[[week]][[q_key]])
      actual <- obs[week]
      
      if (is.na(actual)) return(NULL)
      
      pred_dist <- quantile_pred(
        matrix(q_vec, nrow = 1),
        quantile_levels
      )
      
      wis_val <- weighted_interval_score(
        x               = pred_dist,
        actual          = actual,
        quantile_levels = quantile_levels,
        na_handling     = "impute"
      )
      
      tibble(
        week            = week,
        target          = tgt,
        actual          = actual,
        median_forecast = q_vec[which(quantile_levels == 0.5)],
        WIS             = as.numeric(wis_val)
      )
    })
  })
  
  return(wis_out)
}

results  <- readRDS(paste0("fit_results_FullModel_", Year, ".rds"))

wis_all <- calculate_wis_per_year(results, X_obs1, X_obs2, X0_obs, quantile_levels)

saveRDS(wis_all, file = paste0("fit_wis_all_FullModel_", Year, ".rds"))
cat("\nYear", Year, "complete. Results saved.\n")
}

cat("\nAll years complete.\n")
#Mosq + Human + Climate
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
  
  probs <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
             0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
             0.95, 0.975, 0.99)
  
  save_ensemble_full_global <- readRDS(paste0("save_ensemble_full_global_Mosq+Human+Climate_", Year, ".rds"))
  
  results <- vector("list", 50)  # 50 weeks
  
  for (week in 1:50) {
    
    total_abundance      <- save_ensemble_full_global[1, 1:8000, week] + 
      save_ensemble_full_global[2, 1:8000, week]
    
    infectious_per_1000  <- (save_ensemble_full_global[2, 1:8000, week] /
                               (save_ensemble_full_global[1, 1:8000, week] + 
                                  save_ensemble_full_global[2, 1:8000, week])) * 1000
    human_cases          <- save_ensemble_full_global[5, 1:8000, week] #it should be 5 for other models
    
    results[[week]] <- list(
      total_abundance_q     = quantile(total_abundance,     probs = probs, na.rm = TRUE),
      infectious_per_1000_q = quantile(infectious_per_1000, probs = probs, na.rm = TRUE),
      human_cases_q         = quantile(human_cases,         probs = probs, na.rm = TRUE)
    )
  }
  
  saveRDS(results, file = paste0("fit_results_Mosq+Human+Climate_", Year, ".rds"))
  # Load your observed data for this year (vectors of length 50)
  X_obs1 <- X_obs1[1:50]
  X_obs2 <- X_obs2[1:50]
  X0_obs <- X0_obs[1:50]
  
  quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                       0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
                       0.95, 0.975, 0.99)
  
  calculate_wis_per_year <- function(results, X_obs1, X_obs2, X0_obs, quantile_levels) {
    # results is a list of 50 weeks, each with _q entries
    # X_obs1/2/X0_obs are vectors of length 50 (observed value per week)
    
    n_weeks <- length(results)
    
    targets <- list(
      total_abundance     = list(q_key = "total_abundance_q",     obs = X_obs1),
      infectious_per_1000 = list(q_key = "infectious_per_1000_q", obs = X_obs2),
      human_cases         = list(q_key = "human_cases_q",         obs = X0_obs)
    )
    
    wis_out <- map_dfr(names(targets), function(tgt) {
      
      q_key <- targets[[tgt]]$q_key
      obs   <- targets[[tgt]]$obs
      
      map_dfr(1:n_weeks, function(week) {
        
        q_vec  <- as.numeric(results[[week]][[q_key]])
        actual <- obs[week]
        
        if (is.na(actual)) return(NULL)
        
        pred_dist <- quantile_pred(
          matrix(q_vec, nrow = 1),
          quantile_levels
        )
        
        wis_val <- weighted_interval_score(
          x               = pred_dist,
          actual          = actual,
          quantile_levels = quantile_levels,
          na_handling     = "impute"
        )
        
        tibble(
          week            = week,
          target          = tgt,
          actual          = actual,
          median_forecast = q_vec[which(quantile_levels == 0.5)],
          WIS             = as.numeric(wis_val)
        )
      })
    })
    
    return(wis_out)
  }
  
  results  <- readRDS(paste0("fit_results_Mosq+Human+Climate_", Year, ".rds"))

  
  wis_all <- calculate_wis_per_year(results, X_obs1, X_obs2, X0_obs, quantile_levels)
  
  saveRDS(wis_all, file = paste0("fit_wis_all_Mosq+Human+Climate_", Year, ".rds"))
  cat("\nYear", Year, "complete. Results saved.\n")
}

cat("\nAll years complete.\n")

#Mosq + Human + WithoutClimate
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
  
  probs <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
             0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
             0.95, 0.975, 0.99)
  
  save_ensemble_full_global <- readRDS(paste0("save_ensemble_full_global_Mosq+Human+NoClimate_", Year, ".rds"))
  
  results <- vector("list", 50)  # 50 weeks
  
  for (week in 1:50) {
    
    total_abundance      <- save_ensemble_full_global[1, 1:8000, week] + 
      save_ensemble_full_global[2, 1:8000, week]
    
    infectious_per_1000  <- (save_ensemble_full_global[2, 1:8000, week] /
                               (save_ensemble_full_global[1, 1:8000, week] + 
                                  save_ensemble_full_global[2, 1:8000, week])) * 1000
    human_cases          <- save_ensemble_full_global[5, 1:8000, week] #it should be 5 for other models
    
    results[[week]] <- list(
      total_abundance_q     = quantile(total_abundance,     probs = probs, na.rm = TRUE),
      infectious_per_1000_q = quantile(infectious_per_1000, probs = probs, na.rm = TRUE),
      human_cases_q         = quantile(human_cases,         probs = probs, na.rm = TRUE)
    )
  }
  
  saveRDS(results, file = paste0("fit_results_Mosq+Human+NoClimate_", Year, ".rds"))
  # Load your observed data for this year (vectors of length 50)
  X_obs1 <- X_obs1[1:50]
  X_obs2 <- X_obs2[1:50]
  X0_obs <- X0_obs[1:50]
  
  quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                       0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
                       0.95, 0.975, 0.99)
  
  calculate_wis_per_year <- function(results, X_obs1, X_obs2, X0_obs, quantile_levels) {
    # results is a list of 50 weeks, each with _q entries
    # X_obs1/2/X0_obs are vectors of length 50 (observed value per week)
    
    n_weeks <- length(results)
    
    targets <- list(
      total_abundance     = list(q_key = "total_abundance_q",     obs = X_obs1),
      infectious_per_1000 = list(q_key = "infectious_per_1000_q", obs = X_obs2),
      human_cases         = list(q_key = "human_cases_q",         obs = X0_obs)
    )
    
    wis_out <- map_dfr(names(targets), function(tgt) {
      
      q_key <- targets[[tgt]]$q_key
      obs   <- targets[[tgt]]$obs
      
      map_dfr(1:n_weeks, function(week) {
        
        q_vec  <- as.numeric(results[[week]][[q_key]])
        actual <- obs[week]
        
        if (is.na(actual)) return(NULL)
        
        pred_dist <- quantile_pred(
          matrix(q_vec, nrow = 1),
          quantile_levels
        )
        
        wis_val <- weighted_interval_score(
          x               = pred_dist,
          actual          = actual,
          quantile_levels = quantile_levels,
          na_handling     = "impute"
        )
        
        tibble(
          week            = week,
          target          = tgt,
          actual          = actual,
          median_forecast = q_vec[which(quantile_levels == 0.5)],
          WIS             = as.numeric(wis_val)
        )
      })
    })
    
    return(wis_out)
  }
  
  results  <- readRDS(paste0("fit_results_Mosq+Human+NoClimate_", Year, ".rds"))
  
  
  wis_all <- calculate_wis_per_year(results, X_obs1, X_obs2, X0_obs, quantile_levels)
  
  saveRDS(wis_all, file = paste0("fit_wis_all_Mosq+Human+NoClimate_", Year, ".rds"))
  cat("\nYear", Year, "complete. Results saved.\n")
}

cat("\nAll years complete.\n")


#Full Model without climate
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
  
  probs <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
             0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
             0.95, 0.975, 0.99)
  
  save_ensemble_full_global <- readRDS(paste0("save_ensemble_full_global_FullModel_NoClimate_", Year, ".rds"))
  results <- vector("list", 50)  # 50 weeks
  
  for (week in 1:50) {
    
    total_abundance      <- save_ensemble_full_global[1, 1:8000, week] + 
      save_ensemble_full_global[2, 1:8000, week]
    
    infectious_per_1000  <- (save_ensemble_full_global[2, 1:8000, week] /
                               (save_ensemble_full_global[1, 1:8000, week] + 
                                  save_ensemble_full_global[2, 1:8000, week])) * 1000
    human_cases          <- save_ensemble_full_global[8, 1:8000, week]
    
    results[[week]] <- list(
      total_abundance_q     = quantile(total_abundance,     probs = probs, na.rm = TRUE),
      infectious_per_1000_q = quantile(infectious_per_1000, probs = probs, na.rm = TRUE),
      human_cases_q         = quantile(human_cases,         probs = probs, na.rm = TRUE)
    )
  }
  
  saveRDS(results, file = paste0("fit_results_FullModel_NoClimate_", Year, ".rds"))
  # You need actual observed weekly values for each year:
  X_obs1 = X_obs1[1:50]
  X_obs2 = X_obs2[1:50]
  X0_obs = X0_obs[1:50]
  
  quantile_levels <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
                       0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9,
                       0.95, 0.975, 0.99)
  
  calculate_wis_per_year <- function(results, X_obs1, X_obs2, X0_obs, quantile_levels) {
    # results is a list of 50 weeks, each with _q entries
    # X_obs1/2/X0_obs are vectors of length 50 (observed value per week)
    
    n_weeks <- length(results)
    
    targets <- list(
      total_abundance     = list(q_key = "total_abundance_q",     obs = X_obs1),
      infectious_per_1000 = list(q_key = "infectious_per_1000_q", obs = X_obs2),
      human_cases         = list(q_key = "human_cases_q",         obs = X0_obs)
    )
    
    wis_out <- map_dfr(names(targets), function(tgt) {
      
      q_key <- targets[[tgt]]$q_key
      obs   <- targets[[tgt]]$obs
      
      map_dfr(1:n_weeks, function(week) {
        
        q_vec  <- as.numeric(results[[week]][[q_key]])
        actual <- obs[week]
        
        if (is.na(actual)) return(NULL)
        
        pred_dist <- quantile_pred(
          matrix(q_vec, nrow = 1),
          quantile_levels
        )
        
        wis_val <- weighted_interval_score(
          x               = pred_dist,
          actual          = actual,
          quantile_levels = quantile_levels,
          na_handling     = "impute"
        )
        
        tibble(
          week            = week,
          target          = tgt,
          actual          = actual,
          median_forecast = q_vec[which(quantile_levels == 0.5)],
          WIS             = as.numeric(wis_val)
        )
      })
    })
    
    return(wis_out)
  }
  
  results  <- readRDS(paste0("fit_results_FullModel_NoClimate_", Year, ".rds"))
  
  wis_all <- calculate_wis_per_year(results, X_obs1, X_obs2, X0_obs, quantile_levels)
  
  saveRDS(wis_all, file = paste0("fit_wis_all_FullModel_NoClimate_", Year, ".rds"))
  cat("\nYear", Year, "complete. Results saved.\n")
}

cat("\nAll years complete.\n")

#plots

years <- c(2006:2019, 2021)

model_files <- list(
  "FullModel"             = "fit_wis_all_FullModel_",
  "Mosq+Human+Climate"    = "fit_wis_all_Mosq+Human+Climate_",
  "Mosq+Human+NoClimate"  = "fit_wis_all_Mosq+Human+NoClimate_",
  "FullModel_NoClimate"   = "fit_wis_all_FullModel_NoClimate_"
)

# ── 1. Load all data across years and models, compute median WIS per year ──────
all_data <- map_dfr(years, function(yr) {
  map_dfr(names(model_files), function(model_name) {
    
    fpath <- paste0(model_files[[model_name]], yr, ".rds")
    
    if (!file.exists(fpath)) {
      warning(paste("File not found:", fpath))
      return(NULL)
    }
    
    readRDS(fpath) %>%
      mutate(year  = yr,
             model = model_name)
  })
})

# ── 2. Compute median WIS per year × model × target ───────────────────────────
# (each year has 50 weekly WIS values per target — we take the median)
summary_stats <- all_data %>%
  group_by(year, model, target) %>%
  summarise(
    median_WIS = median(WIS[is.finite(WIS)], na.rm = TRUE),
    mean_WIS   = mean(WIS[is.finite(WIS)],   na.rm = TRUE),
    .groups = "drop"
  )
summary_stats <- summary_stats %>%
  mutate(model = recode(model,
                        "FullModel_NoClimate" = "FullModel_NoWeather",
                        "Mosq+Human+Climate"           = "Mosq+Human+Weather",
                        "Mosq+Human+NoClimate"           = "Mosq+Human+NoWeather",
                        .default = model   # keeps all other model names unchanged
  ))
 #summary_stats <- summary_stats %>%
  # mutate(median_WIS = log1p(median_WIS),
 #        mean_WIS = log1p(mean_WIS))
# ── 3. Plotting function ───────────────────────────────────────────────────────
target_labels <- c(
  total_abundance     = "Total Abundance",
  infectious_per_1000 = "Infectious per 1000",
  human_cases         = "Human Cases"
)
model_colors <- c(
  # 4 component models
  "FullModel"               = "#009E73",  # bluish green
  "Mosq+Human+Weather"      = "#E69F00",  # orange
  "Mosq+Human+NoWeather"    = "#0072B2",  # blue
  "FullModel_NoWeather"     = "#D55E00",  # vermillion
  
  # 5 ensemble models
  "Ensemble_model_1"        = "#56B4E9",  # sky blue
  "Ensemble_model_2"        = "#CC79A7",  # reddish purple
  "Ensemble_model_3"        = "#F0E442",  # yellow
  "Ensemble_model_4"        = "#999999",  # grey
  "Ensemble_model_5"        = "#000000"   # black
)
plot_wis_boxplots <- function(data, stat_type = "median",
                              color_map = model_colors) {
  
  if (stat_type == "median") {
    plot_data <- data %>% rename(value = median_WIS) %>% select(-mean_WIS)
    y_label   <- "Median WIS"
  } else {
    plot_data <- data %>% rename(value = mean_WIS) %>% select(-median_WIS)
    y_label   <- "Mean WIS"
  }
  
  plot_data <- plot_data %>%
    filter(is.finite(value)) %>%
    mutate(target = recode(target, !!!target_labels))
  
  models_present <- unique(plot_data$model)
  colors_used    <- color_map[models_present]
  
  model_order <- names(colors_used)
  plot_data   <- plot_data %>%
    mutate(model = factor(model, levels = model_order))
  
  p <- ggplot(plot_data, aes(x = model, y = value, fill = model)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_point(aes(color = model),
               position = position_jitter(width = 0.2),
               size = 2.5, alpha = 0.8) +
    geom_text(aes(label = year, color = model),
              position = position_jitter(width = 0.2), fontface = "bold", 
              size = 3.5, vjust = -0.8, show.legend = FALSE) +
    facet_wrap(~ target, scales = "free_y", ncol = 3,
               labeller = as_labeller(setNames(
                 unique(plot_data$target), unique(plot_data$target)
               ))) +
    scale_fill_manual(values  = colors_used) +
    scale_color_manual(values = colors_used) +
    labs(
      title    = paste0(y_label, " by Model and Target (across years) for Fits"),
      subtitle = "Each point = one year's median WIS. Low score is better",
      x        = NULL,
      y        = y_label
    ) +
    theme_minimal() +
    theme(
      legend.position  = "none",          # remove legend entirely
      axis.text.x      = element_text(    # model names on x-axis, rotated
        angle  = 90,
        hjust  = 1,                       # right-align at the tick mark
        vjust  = 0.5,                     # vertically centred on tick
        size   = 19,
        face   = "bold"
      ),
      axis.ticks.x     = element_line(),  # keep tick marks visible
      axis.title.y     = element_text(size = 19, face   = "bold"),
      axis.text.y      = element_text(size = 19, face   = "bold"),
      plot.title       = element_text(face = "bold", size = 19),
      plot.subtitle    = element_text(size = 19, color = "grey40"),
      strip.text       = element_text(face = "bold", size = 19),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ── 4. Generate and print plots ────────────────────────────────────────────────
median_plot <- plot_wis_boxplots(summary_stats, stat_type = "median")
mean_plot   <- plot_wis_boxplots(summary_stats, stat_type = "mean")

print(median_plot)
print(mean_plot)
ggsave("median_h2_fit.png", median_plot, width = 16, height = 15, dpi = 300)
ggsave("median_h2_fit.pdf", median_plot, width = 16, height = 15, dpi = 300)

#combined <- (median_plot/final_fit) 

final_fit <- readRDS("final_fit_2014_FullModel.rds")   
combined  <- (median_plot / wrap_elements(final_fit))
ggsave(
  filename = "median_plot_final_fit_all_targets_2014.png",
  plot     = combined,
  width    = 28,
  height   = 16,
  dpi      = 300,
  bg       = "white"
)
ggsave(
  filename = "median_plot_final_fit_all_targets_2014.pdf",
  plot     = combined,
  width    = 28,
  height   = 16,
  dpi      = 300,
  bg       = "white"
)

#
years <- c(2006:2019, 2021)

model_files <- list(
  "FullModel"             = "fit_wis_all_FullModel_",
  "Mosq+Human+Climate"    = "fit_wis_all_Mosq+Human+Climate_",
  "Mosq+Human+NoClimate"  = "fit_wis_all_Mosq+Human+NoClimate_",
  "FullModel_NoClimate"   = "fit_wis_all_FullModel_NoClimate_"
)
all_data <- map_dfr(years, function(yr) {
  map_dfr(names(model_files), function(model_name) {
    
    fpath <- paste0(model_files[[model_name]], yr, ".rds")
    
    if (!file.exists(fpath)) {
      warning(paste("File not found:", fpath))
      return(NULL)
    }
    
    readRDS(fpath) %>%
      mutate(
        year  = yr,
        model = model_name,
        date  = as.Date(paste0(yr, "-01-01")) + (week - 1) * 7  # week 1 = Jan 1, week 2 = Jan 8, etc.
      )
  })
})

# ── Summary by month (across all years) ───────────────────────────────────────
summary_stats_by_month <- all_data %>%
  mutate(
    month     = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date)
  ) %>%
  group_by(month, month_num, model, target) %>%
  summarise(
    mean_WIS   = mean(WIS[is.finite(WIS)],   na.rm = TRUE),
    median_WIS = median(WIS[is.finite(WIS)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(month_num)

# ── Individual year-month points (for jittered points in plot) ─────────────────
individual_points <- all_data %>%
  filter(is.finite(WIS)) %>%
  mutate(
    month     = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date)
  ) %>%
  group_by(year, month, month_num, model, target) %>%
  summarise(
    mean_WIS   = mean(WIS,   na.rm = TRUE),
    median_WIS = median(WIS, na.rm = TRUE),
    .groups = "drop"
  )
individual_points <- individual_points %>%
  mutate(model = recode(model,
                        "FullModel_NoClimate" = "FullModel_NoWeather",
                        "Mosq+Human+Climate"           = "Mosq+Human+Weather",
                        "Mosq+Human+NoClimate"           = "Mosq+Human+NoWeather",
                        .default = model   # keeps all other model names unchanged
  ))
# ── Plotting function ──────────────────────────────────────────────────────────
target_labels <- c(
  total_abundance     = "Total Abundance",
  infectious_per_1000 = "Infectious per 1000",
  human_cases         = "Human Cases"
)

plot_wis_by_month <- function(individual_data, stat_type = "median",
                              color_map = model_colors) {
  
  if (stat_type == "median") {
    individual_data <- individual_data %>% rename(value = median_WIS) %>% select(-mean_WIS)
    y_label <- "Median WIS"
  } else {
    individual_data <- individual_data %>% rename(value = mean_WIS) %>% select(-median_WIS)
    y_label <- "Mean WIS"
  }
  
  plot_data <- individual_data %>%
    filter(is.finite(value)) %>%
    arrange(month_num) %>%
    mutate(
      month  = factor(month, levels = month.abb),
      target = recode(target, !!!target_labels)
    )
  
  models_present <- unique(plot_data$model)
  colors_used    <- color_map[models_present]
  
  model_order <- names(colors_used)
  plot_data   <- plot_data %>%
    mutate(model = factor(model, levels = model_order))
  
  p <- ggplot(plot_data, aes(x = month, y = value, fill = model)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA, linewidth = 0.7) +
    geom_point(aes(color = model),
               position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
               size = 2.5, alpha = 0.8) +
    geom_text(aes(label = year, color = model),
              position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75),
              size = 3.5, vjust = -0.8, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(~ target, scales = "free_y", ncol = 1) +
    scale_fill_manual(values  = colors_used) +
    scale_color_manual(values = colors_used) +
    labs(
      title    = paste0(y_label, " by Model and Month"),
      subtitle = "Each box aggregates across all years for that month",
      x        = "Month",
      y        = y_label
    ) +
    theme_minimal() +
    theme(
      legend.position  = "bottom",
      axis.text.x      = element_text(angle = 45, hjust = 1,
                                      size = 19, face = "bold"),
      axis.ticks.x     = element_line(),
      axis.title       = element_text(size = 19, face = "bold"),
      axis.text.y      = element_text(size = 19, face = "bold"),
      plot.title       = element_text(face = "bold", size = 19),
      plot.subtitle    = element_text(size = 19, color = "grey40"),
      strip.text       = element_text(face = "bold", size = 19),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ── Generate plots ─────────────────────────────────────────────────────────────
median_month <- plot_wis_by_month(individual_points, stat_type = "median")
mean_month   <- plot_wis_by_month(individual_points, stat_type = "mean")

print(median_month)
print(mean_month)
ggsave("median_month_fit.png", median_month , width = 16, height = 15, dpi = 300)
ggsave("median_month_fit.pdf", median_month , width = 16, height = 15, dpi = 300)


# ============================================================
# Heatmap: Median  WIS by Model x Month, faceted by Year
# Y-axis: model names
# X-axis: calendar month (Jan-Dec)
# Fill:   median  WIS (lighter = better, darker = worse)
# Facet:  one panel per year (15 years)
# ============================================================
years <- c(2006:2019, 2021)

model_files <- list(
  "FullModel"             = "fit_wis_all_FullModel_",
  "Mosq+Human+Climate"    = "fit_wis_all_Mosq+Human+Climate_",
  "Mosq+Human+NoClimate"  = "fit_wis_all_Mosq+Human+NoClimate_",
  "FullModel_NoClimate"   = "fit_wis_all_FullModel_NoClimate_"
)
all_data <- map_dfr(years, function(yr) {
  map_dfr(names(model_files), function(model_name) {
    
    fpath <- paste0(model_files[[model_name]], yr, ".rds")
    
    if (!file.exists(fpath)) {
      warning(paste("File not found:", fpath))
      return(NULL)
    }
    
    readRDS(fpath) %>%
      mutate(
        year  = yr,
        model = model_name,
        date  = as.Date(paste0(yr, "-01-01")) + (week - 1) * 7  # week 1 = Jan 1, week 2 = Jan 8, etc.
      )
  })
})


# ── Build month-level summary per year ────────────────────────────────────────
all_data <- all_data %>%
  mutate(model = recode(model,
                        "FullModel_NoClimate" = "FullModel_WithoutWeather",
                        "Mosq+Human+Climate"           = "Mosq+Human+Weather",
                        "Mosq+Human+NoClimate"           = "Mosq+Human+WithoutWeather",
                        .default = model   # keeps all other model names unchanged
  ))
heatmap_data <- all_data %>%
  filter(is.finite(WIS)) %>%
  mutate(
    month     = month(date, label = TRUE, abbr = TRUE),
    month_num = month(date)
  ) %>%
  group_by(year, month, month_num, model, target) %>%
  summarise(
    median_WIS = median(WIS, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    month = factor(month, levels = month.abb),
    # Cap extreme values for colour scale readability
    WIS_capped = pmax(pmin(median_WIS, 500), 0)
  )
# ── Compute per-target limits ─────────────────────────────────────
target_scales <- heatmap_data %>%
  mutate(WIS = median_WIS) %>%  
  group_by(target) %>%
  summarise(
    lo  = quantile(WIS, 0.05, na.rm = TRUE),  # 5th percentile — ignore low outliers
    hi  = quantile(WIS, 0.95, na.rm = TRUE),  # 95th percentile — ignore high outliers
    .groups = "drop"
  )
heatmap_data <- heatmap_data %>%
  left_join(target_scales, by = "target") %>%
  mutate(
    WIS_capped = pmax(pmin(median_WIS, hi), lo)
  )
# ── Updated plot function ──────────────────────────────────────────────────────
plot_heatmap_by_year <- function(target_name, target_label) {
  
  plot_df <- heatmap_data %>%
    filter(target == target_name) %>%
    mutate(WIS = median_WIS)
  
  scale_params <- target_scales %>% filter(target == target_name)
  lo <- scale_params$lo
  hi <- scale_params$hi
  
  # Cap to [lo, hi] so outliers don't dominate
  plot_df <- plot_df %>%
    mutate(fill_val =WIS_capped)
  
  model_order <- rev(sort(unique(plot_df$model)))
  plot_df <- plot_df %>%
    mutate(model = factor(model, levels = model_order))
  
  
  break_vals   <- c(lo, (lo + hi) / 2, hi)
  break_labels <- round(break_vals, 1)   
  
  ggplot(plot_df, aes(x = month, y = model, fill = fill_val)) +
    
    geom_tile(colour = "white", linewidth = 0.3) +
    
    geom_vline(xintercept = seq(0.5, 12.5, by = 1),
               colour = "white", linewidth = 0.2) +
    
    facet_wrap(~ year, ncol = 5, nrow = 3) +
    
    # Single sequential colour: white (low WIS = good) → dark red (high WIS = bad)
    scale_fill_gradient(
      low    = "#fff5f0",   # near-white
      high   = "#99000d",   # dark red
      limits = c(lo, hi),
      oob    = scales::squish,
      name   = "Median WIS",
      breaks = break_vals,
      labels = break_labels
    ) +
    
    labs(
      title = paste0("Median WIS for Fits by Model and Month — ", target_label),
      x     = "Month",
      y     = "Model"
    ) +
    
    theme_minimal(base_size = 18) +
    theme(
      plot.title      = element_text(face = "bold", size = 18, hjust = 0),
      strip.text      = element_text(face = "bold", size = 10),
      axis.text.x     = element_text(angle = 90, hjust = 1, size = 18, face = "bold"),
      axis.text.y     = element_text(size = 18, face = "bold"),
      axis.title      = element_text(size = 18, face = "bold"),
      legend.position = "right",
      legend.text     = element_text(size = 19, face = "bold"),
      legend.title    = element_text(size = 18, face = "bold"),
      panel.grid      = element_blank(),
      panel.spacing   = unit(0.4, "lines")
    )
}

# ── Generate and save one heatmap per target ──────────────────────────────────
targets_list <- list(
  `total_abundance`         = "Total abundance",
  `infectious_per_1000` = "Infectious mosq per 1000",
  `human_cases`             = "Human cases"
)

for (tgt in names(targets_list)) {
  p <- plot_heatmap_by_year(tgt, targets_list[[tgt]])
  
  ggsave(
    filename = paste0("heatmap_WIS_", tgt, ".pdf"),
    plot     = p,
    width    = 22,
    height   = 18,
    dpi      = 300,
    bg       = "white"
  )
  message("Saved: heatmap_WIS_", tgt, ".pdf")
}


library(dplyr)
library(purrr)

years <- c(2006:2019, 2021)
fit_model_files <- list(
  "FullModel"            = "fit_wis_all_FullModel_",
  "FullModel_NoWeather"   = "fit_wis_all_FullModel_NoClimate_",
  "Mosq+Human+Weather"    = "fit_wis_all_Mosq+Human+Climate_",
  "Mosq+Human+NoWeather"  = "fit_wis_all_Mosq+Human+NoClimate_"
)

# ── Load in-sample fit WIS for all 4 models ────────────────────────────────────
fit_wis_all <- map_dfr(names(fit_model_files), function(model_name) {
  map_dfr(years, function(yr) {
    fpath <- paste0(fit_model_files[[model_name]], yr, ".rds")
    if (!file.exists(fpath)) return(NULL)
    readRDS(fpath) %>% mutate(year = yr, model = model_name)
  })
})

# ════════════════════════════════════════════════════════════════
# CLAIM 1: Human case fits comparable across all 4 models
# ════════════════════════════════════════════════════════════════
claim1 <- fit_wis_all %>%
  filter(target == "human_cases") %>%
  group_by(model) %>%
  summarise(median_fit_WIS = median(WIS, na.rm = TRUE), .groups = "drop") %>%
  arrange(median_fit_WIS)
print(claim1)

# ════════════════════════════════════════════════════════════════
# CLAIM 2: Weather-free abundance fits — spring vs. rest of year
# ════════════════════════════════════════════════════════════════
fit_wis_dated <- fit_wis_all %>%
  mutate(
    approx_date = as.Date(paste0(year, "-01-01")) + (week - 1) * 7,
    month       = month(approx_date, label = TRUE, abbr = TRUE),
    season      = if_else(month %in% c("Mar","Apr","May"), "Spring", "Rest of year")
  )

claim2 <- fit_wis_dated %>%
  filter(target == "total_abundance") %>%
  group_by(model, season) %>%
  summarise(median_fit_WIS = median(WIS, na.rm = TRUE), .groups = "drop") %>%
  arrange(season, median_fit_WIS)
print(claim2)

# ════════════════════════════════════════════════════════════════
# CLAIM 3 & 4: IM1000 fit WIS by year, all models — check 2012/2021
# ════════════════════════════════════════════════════════════════
claim3 <- fit_wis_all %>%
  filter(target == "infectious_per_1000") %>%
  group_by(year, model) %>%
  summarise(median_fit_WIS = median(WIS, na.rm = TRUE), .groups = "drop")

# Cross-model summary per year (which years are worst overall)
claim3_by_year <- claim3 %>%
  group_by(year) %>%
  summarise(mean_across_models = mean(median_fit_WIS, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_across_models))
print(claim3_by_year, n = 20)

# Specifically flag 2012 and 2021 by model
claim3 %>% filter(year %in% c(2012, 2021)) %>% arrange(year, model) %>% print(n = 20)

# A "typical" year for comparison — picked year closest to the median
median_year_val <- median(claim3_by_year$mean_across_models)
claim3_by_year %>% mutate(dist_from_median = abs(mean_across_models - median_year_val)) %>%
  arrange(dist_from_median) %>% head(3)

# ════════════════════════════════════════════════════════════════
# CLAIM 3 (corrected): IM1000 fit WIS by year — use MEAN, not median,
# since IM1000 is zero-inflated in early/late season weeks
# ════════════════════════════════════════════════════════════════
claim3_mean <- fit_wis_all %>%
  filter(target == "infectious_per_1000") %>%
  group_by(year, model) %>%
  summarise(mean_fit_WIS = mean(WIS, na.rm = TRUE), .groups = "drop")

claim3_mean_by_year <- claim3_mean %>%
  group_by(year) %>%
  summarise(mean_across_models = mean(mean_fit_WIS, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_across_models))
print(claim3_mean_by_year, n = 20)

claim3_mean %>% filter(year %in% c(2012, 2021)) %>% arrange(year, model) %>% print(n = 20)

# restrict to "active season" weeks only
# (where observed IM1000 > 0), so we're comparing fit quality
# specifically during weeks where there's a real signal to fit 
claim3_active <- fit_wis_all %>%
  filter(target == "infectious_per_1000", actual > 0) %>%
  group_by(year, model) %>%
  summarise(
    mean_fit_WIS_active = mean(WIS, na.rm = TRUE),
    n_active_weeks      = n(),
    .groups = "drop"
  )

claim3_active_by_year <- claim3_active %>%
  group_by(year) %>%
  summarise(mean_across_models = mean(mean_fit_WIS_active, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_across_models))
print(claim3_active_by_year, n = 20)

claim3_active %>% filter(year %in% c(2012, 2021)) %>% arrange(year, model) %>% print(n = 20)
# ── Median fit WIS, restricted to active-season weeks (actual > 0) ────────────
claim3_median_active <- fit_wis_all %>%
  filter(target == "infectious_per_1000", actual > 0) %>%
  group_by(year, model) %>%
  summarise(
    median_fit_WIS_active = median(WIS, na.rm = TRUE),
    n_active_weeks         = n(),
    .groups = "drop"
  )

claim3_median_active_by_year <- claim3_median_active %>%
  group_by(year) %>%
  summarise(mean_across_models = mean(median_fit_WIS_active, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_across_models))
print(claim3_median_active_by_year, n = 20)

claim3_median_active %>% filter(year %in% c(2006, 2012, 2019, 2021)) %>%
  arrange(year, model) %>% print(n = 20)
library(dplyr)
library(ggplot2)

# ── Build the dataframe ─────────────────────────────────────────────────────
trap_data <- tibble(
  year = c(2006:2024),
  n_traps = c(285, 519, 558, 504, 503, 483, 513, 636, 658, 770,
              768, 801, 793, 801, 829, 828, 853, 838, 832)
)

# Flag which years are part of the actual study (2006-2019, 2021)
study_years <- c(2006:2019, 2021)

trap_data <- trap_data %>%
  mutate(
    in_study = year %in% study_years,
    is_pilot = year == 2006,
    group = case_when(
      is_pilot  ~ "2006 (pilot year, excluded from interpretation of fit quality)",
      in_study  ~ "Study years (2006\u20132019, 2021)",
      TRUE      ~ "Outside study period (2020, 2022\u20132024)"
    ),
    group = factor(group, levels = c(
      "2006 (pilot year, excluded from interpretation of fit quality)",
      "Study years (2006\u20132019, 2021)",
      "Outside study period (2020, 2022\u20132024)"
    ))
  )

print(trap_data)

# ── Okabe-Ito palette ──────────────────────
group_colors <- c(
  "2006 (pilot year, excluded from interpretation of fit quality)" = "#D55E00",
  "Study years (2006\u20132019, 2021)"                              = "#0072B2",
  "Outside study period (2020, 2022\u20132024)"                     = "#999999"
)

# ── Reference line: mean trap count across established study years ──────────
mean_established <- trap_data %>%
  filter(in_study, !is_pilot) %>%
  summarise(m = mean(n_traps)) %>%
  pull(m)

# ── Build the figure ──────────────────────────────────────────────────────────
p <- ggplot(trap_data, aes(x = factor(year), y = n_traps, fill = group)) +
  
  geom_col(width = 0.72, color = "white", linewidth = 0.3) +
  
  geom_text(
    aes(label = n_traps),
    vjust = -0.5,
    size = 3.2,
    color = "#333333"
  ) +
  
  geom_hline(
    yintercept = mean_established,
    linetype   = "dashed",
    color      = "#444444",
    linewidth  = 0.6,
    alpha      = 0.7
  ) +
  
  annotate(
    "text",
    x     = 17.5,
    y     = mean_established + 25,
    label = paste0("Mean, established years\n(", round(mean_established), " traps)"),
    hjust = 0,
    size  = 3,
    color = "#444444"
  ) +
  
  scale_fill_manual(values = group_colors, name = NULL) +
  
  scale_y_continuous(limits = c(0, max(trap_data$n_traps) * 1.18),
                     expand = expansion(mult = c(0, 0))) +
  
  labs(
    title = "Maricopa County mosquito trap network size, 2006\u20132024",
    x     = "Year",
    y     = "Number of operational traps"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.text       = element_text(size = 9),
    legend.background = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 9),
    axis.title.x      = element_text(face = "bold", size = 11),
    axis.title.y      = element_text(face = "bold", size = 11),
    plot.title        = element_text(face = "bold", size = 13, hjust = 0),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(p)

# ── Save ────────────────────────────────────────────────────────────────────
ggsave("trap_counts_supplementary_figure.png", p, width = 10, height = 5.5, dpi = 300)
ggsave("trap_counts_supplementary_figure.pdf", p, width = 10, height = 5.5)

write.csv(trap_data, "trap_counts.csv", row.names = FALSE)
