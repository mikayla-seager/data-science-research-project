# Loading in packages ----------------------------------------------------------
pacman::p_load(tidyverse, readr, skimr, stats, forecast, patchwork, janitor, lubridate)

# Manoeuvre data import and clean ----------------------------------------------
# Setting directory
setwd("V:\\Michael Seager\\St. Michael's\\2024\\DSRP\\satellite_data\\manoeuvres")

# Reading in all manoeuvre data
cs2_man_raw <- readLines("cs2man.txt") %>% str_replace_all('     ', " NA ")
fy2d_man_raw <- readLines("manFY2D.txt.fy")
fy2e_man_raw <- readLines("manFY2E.txt.fy")
fy2f_man_raw <- readLines("manFY2F.txt.fy")
fy2h_man_raw <- readLines("manFY2H.txt.fy")
fy4a_man_raw <- readLines("manFY4A.txt.fy")
hy2a_man_raw <- readLines("h2aman.txt") %>% str_replace_all('     ', " NA ")
js1_man_raw <- readLines("ja1man.txt") %>% str_replace_all('     ', " NA ")
js2_man_raw <- readLines("ja2man.txt") %>% str_replace_all('     ', " NA ")
js3_man_raw <- readLines("ja3man.txt") %>% str_replace_all('     ', " NA ")
srl_man_raw <- readLines("srlman.txt") %>% str_replace_all('     ', " NA ")
stl3a_man_raw <- readLines("s3aman.txt") %>% str_replace_all('     ', " NA ")
stl3b_man_raw <- readLines("s3bman.txt") %>% str_replace_all('     ', " NA ")
stl6a_man_raw <- readLines("s6aman.txt") %>% str_replace_all('     ', " NA ")
tpx_man_raw <- readLines("topman.txt") %>% str_replace_all('     ', " NA ")

# Function to clean manoeuvre data for Fengyun satellites
manoeuvre_clean_fy <- function(dataset) {
  
  # Extracting manoeuvre timestamps to a tibble
  output_data <- tibble(
    man_beg = ymd_hms(str_extract(fy2d_man_raw, '(?<=\\")[^"]*(?=\\")')),
    man_end = ymd_hms(str_extract(fy2d_man_raw, '(?<=\")[^"]*(?=\\"(?!.*\\"))'))
  )
  
  # Returning the tibble of manoeuvre timestamps
  return (output_data)
  
}

# Function to clean manoeuvre data for other satellites
manoeuvre_clean <- function(dataset) {
  
  # Converting each line of the dataset to a list of components
  data_list <- lapply(dataset, function(line) {
    unlist(strsplit(line, "\\s+"))
  })
  
  # Converting the list to a dataframe for easier manipulation
  max_cols <- max(sapply(data_list, length))
  padded_data <- lapply(data_list, function(row) {
    length(row) <- max_cols
    return(row)
  })
  output_data <- do.call(rbind, lapply(padded_data, function(x) {
    as.data.frame(t(x), stringsAsFactors = FALSE)
  })) %>% as_tibble()
  
  # Setting column names and tidying date columns based on number of burns
  if (max(as.numeric(output_data$V12)) == 1) {
    colnames(output_data) <- c("ID", "Year1", "DOY1", "Hour1", "Min1", "Year2", "DOY2", "Hour2", "Min2", 
                               "Type1", "Type2", "Burns", "Year3", "DOY3", "Hour3", "Min3", "Sec3", 
                               "BoostDur1", "DV11", "DV21", "DV31", "Acc11", "Acc21", "Acc31", 
                               "DAcc11", "DAcc21", "DAcc31")
    
    output_data <- output_data %>% mutate(across(!c(ID, Type1, Type2), as.numeric),
                                          man_beg = make_datetime(year = Year1, min = round(Hour1*60)+Min1) + days(DOY1-1),
                                          man_end = make_datetime(year = Year2, min = round(Hour2*60)+Min2) + days(DOY2-1),
                                          man_med1 = make_datetime(year = Year3, min = round(Hour3*60)+Min3, sec = Sec3) + days(DOY3-1)) %>%
      select(ID, man_beg, man_end, Type1, Type2, Burns, 
             man_med1, BoostDur1, DV11, DV21, DV31, Acc11, Acc21, Acc31, DAcc11, DAcc21, DAcc31) %>%
      clean_names()
  } else if (max(as.numeric(output_data$V12)) == 2) {
    colnames(output_data) <- c("ID", "Year1", "DOY1", "Hour1", "Min1", "Year2", "DOY2", "Hour2", "Min2", 
                               "Type1", "Type2", "Burns", "Year3", "DOY3", "Hour3", "Min3", "Sec3", 
                               "BoostDur1", "DV11", "DV21", "DV31", "Acc11", "Acc21", "Acc31", 
                               "DAcc11", "DAcc21", "DAcc31", "Year4", "DOY4", "Hour4", "Min4", "Sec4", 
                               "BoostDur2", "DV12", "DV22", "DV32", "Acc12", "Acc22", "Acc32", 
                               "DAcc12", "DAcc22", "DAcc32")
    
    output_data <- output_data %>% mutate(across(!c(ID, Type1, Type2), as.numeric),
                                          man_beg = make_datetime(year = Year1, min = round(Hour1*60)+Min1) + days(DOY1-1),
                                          man_end = make_datetime(year = Year2, min = round(Hour2*60)+Min2) + days(DOY2-1),
                                          man_med1 = make_datetime(year = Year3, min = round(Hour3*60)+Min3, sec = Sec3) + days(DOY3-1),
                                          man_med2 = make_datetime(year = Year4, min = round(Hour4*60)+Min4, sec = Sec4) + days(DOY4-1)) %>%
      select(ID, man_beg, man_end, Type1, Type2, Burns, 
             man_med1, BoostDur1, DV11, DV21, DV31, Acc11, Acc21, Acc31, DAcc11, DAcc21, DAcc31, 
             man_med2, BoostDur2, DV12, DV22, DV32, Acc12, Acc22, Acc32, DAcc12, DAcc22, DAcc32) %>%
      clean_names()
  } else if (max(as.numeric(output_data$V12)) == 3) {
    colnames(output_data) <- c("ID", "Year1", "DOY1", "Hour1", "Min1", "Year2", "DOY2", "Hour2", "Min2", 
                               "Type1", "Type2", "Burns", "Year3", "DOY3", "Hour3", "Min3", "Sec3", 
                               "BoostDur1", "DV11", "DV21", "DV31", "Acc11", "Acc21", "Acc31", 
                               "DAcc11", "DAcc21", "DAcc31", "Year4", "DOY4", "Hour4", "Min4", "Sec4", 
                               "BoostDur2", "DV12", "DV22", "DV32", "Acc12", "Acc22", "Acc32", 
                               "DAcc12", "DAcc22", "DAcc32", "Year5", "DOY5", "Hour5", "Min5", "Sec5", 
                               "BoostDur3", "DV13", "DV23", "DV33", "Acc13", "Acc23", "Acc33", 
                               "DAcc13", "DAcc23", "DAcc33")
    
    output_data <- output_data %>% mutate(across(!c(ID, Type1, Type2), as.numeric),
                                          man_beg = make_datetime(year = Year1, min = round(Hour1*60)+Min1) + days(DOY1-1),
                                          man_end = make_datetime(year = Year2, min = round(Hour2*60)+Min2) + days(DOY2-1),
                                          man_med1 = make_datetime(year = Year3, min = round(Hour3*60)+Min3, sec = Sec3) + days(DOY3-1),
                                          man_med2 = make_datetime(year = Year4, min = round(Hour4*60)+Min4, sec = Sec4) + days(DOY4-1),
                                          man_med3 = make_datetime(year = Year5, min = round(Hour5*60)+Min5, sec = Sec5) + days(DOY5-1)) %>%
      select(ID, man_beg, man_end, Type1, Type2, Burns, 
             man_med1, BoostDur1, DV11, DV21, DV31, Acc11, Acc21, Acc31, DAcc11, DAcc21, DAcc31, 
             man_med2, BoostDur2, DV12, DV22, DV32, Acc12, Acc22, Acc32, DAcc12, DAcc22, DAcc32, 
             man_med3, BoostDur3, DV13, DV23, DV33, Acc13, Acc23, Acc33, DAcc13, DAcc23, DAcc33) %>%
      clean_names()
  } else {
    colnames(output_data) <- c("ID", "Year1", "DOY1", "Hour1", "Min1", "Year2", "DOY2", "Hour2", "Min2")
    
    output_data <- output_data %>% mutate(across(!c(ID), as.numeric),
                                          man_beg = make_datetime(year = Year1, min = round(Hour1*60)+Min1) + days(DOY1-1),
                                          man_end = make_datetime(year = Year2, min = round(Hour2*60)+Min2) + days(DOY2-1)) %>%
      select(ID, man_beg, man_end) %>%
      clean_names()
  }
  
  # Returning the tibble of cleaned manoeuvre data
  return (output_data)
  
}

# Cleaning manoeuvre data using custom function
cs2_man <- manoeuvre_clean(cs2_man_raw)
fy2d_man <- manoeuvre_clean_fy(fy2d_man_raw)
fy2e_man <- manoeuvre_clean_fy(fy2e_man_raw)
fy2f_man <- manoeuvre_clean_fy(fy2f_man_raw)
fy2h_man <- manoeuvre_clean_fy(fy2h_man_raw)
fy4a_man <- manoeuvre_clean_fy(fy4a_man_raw)
hy2a_man <- manoeuvre_clean(hy2a_man_raw)
js1_man <- manoeuvre_clean(js1_man_raw)
js2_man <- manoeuvre_clean(js2_man_raw)
js3_man <- manoeuvre_clean(js3_man_raw)
srl_man <- manoeuvre_clean(srl_man_raw)
stl3a_man <- manoeuvre_clean(stl3a_man_raw)
stl3b_man <- manoeuvre_clean(stl3b_man_raw)
stl6a_man <- manoeuvre_clean(stl6a_man_raw)
tpx_man <- manoeuvre_clean(tpx_man_raw)



# Satellite elements data import and clean -------------------------------------
# Setting directory again
setwd("V:\\Michael Seager\\St. Michael's\\2024\\DSRP\\satellite_data\\orbital_elements")

# Reading in all satellite data
cs2_raw <- read_csv("unpropagated_elements_CryoSat-2.csv") %>% clean_names() %>% rename(Date = x1)
fy2d_raw <- read_csv("unpropagated_elements_Fengyun-2D.csv") %>% clean_names() %>% rename(Date = x1)
fy2e_raw <- read_csv("unpropagated_elements_Fengyun-2E.csv") %>% clean_names() %>% rename(Date = x1)
fy2f_raw <- read_csv("unpropagated_elements_Fengyun-2F.csv") %>% clean_names() %>% rename(Date = x1)
fy2h_raw <- read_csv("unpropagated_elements_Fengyun-2H.csv") %>% clean_names() %>% rename(Date = x1)
fy4a_raw <- read_csv("unpropagated_elements_Fengyun-4A.csv") %>% clean_names() %>% rename(Date = x1)
hy2a_raw <- read_csv("unpropagated_elements_Haiyang-2A.csv") %>% clean_names() %>% rename(Date = x1)
js1_raw <- read_csv("unpropagated_elements_Jason-1.csv") %>% clean_names() %>% rename(Date = x1)
js2_raw <- read_csv("unpropagated_elements_Jason-2.csv") %>% clean_names() %>% rename(Date = x1)
js3_raw <- read_csv("unpropagated_elements_Jason-3.csv") %>% clean_names() %>% rename(Date = x1)
srl_raw <- read_csv("unpropagated_elements_SARAL.csv") %>% clean_names() %>% rename(Date = x1)
stl3a_raw <- read_csv("unpropagated_elements_Sentinel-3A.csv") %>% clean_names() %>% rename(Date = x1)
stl3b_raw <- read_csv("unpropagated_elements_Sentinel-3B.csv") %>% clean_names() %>% rename(Date = x1)
stl6a_raw <- read_csv("unpropagated_elements_Sentinel-6A.csv") %>% clean_names() %>% rename(Date = x1)
tpx_raw <- read_csv("unpropagated_elements_TOPEX.csv") %>% clean_names() %>% rename(Date = x1)

# Function to detect outlier bounds of a vector
outlier_bounds <- function(vector) {
  qrtl1 <- vector %>% quantile(na.rm=TRUE) %>% .[2] %>% as.numeric()
  qrtl3 <- vector %>% quantile(na.rm=TRUE) %>% .[4] %>% as.numeric()
  iqr <- qrtl3 - qrtl1
  lower_bound <- qrtl1-1.5*iqr
  upper_bound <- qrtl3+1.5*iqr
  
  # Returning the outlier boundaries
  return (c(lower_bound, upper_bound))
}

# Function to remove outliers in satellite data
remove_outliers <- function(dataset) {
  
  # Creating an output tibble with outliers removed
  output <- dataset %>% 
    mutate(eccentricity = case_when(eccentricity < outlier_bounds(dataset$eccentricity)[1] ~ NA,
                                    eccentricity > outlier_bounds(dataset$eccentricity)[2] ~ NA,
                                    TRUE ~ eccentricity),
           argument_of_perigee = case_when(argument_of_perigee < outlier_bounds(dataset$argument_of_perigee)[1] ~ NA,
                                           argument_of_perigee > outlier_bounds(dataset$argument_of_perigee)[2] ~ NA,
                                           TRUE ~ argument_of_perigee),
           inclination = case_when(inclination < outlier_bounds(dataset$inclination)[1] ~ NA,
                                   inclination > outlier_bounds(dataset$inclination)[2] ~ NA,
                                   TRUE ~ inclination),
           mean_anomaly = case_when(mean_anomaly < outlier_bounds(dataset$mean_anomaly)[1] ~ NA,
                                    mean_anomaly > outlier_bounds(dataset$mean_anomaly)[2] ~ NA,
                                    TRUE ~ mean_anomaly),
           brouwer_mean_motion = case_when(brouwer_mean_motion < outlier_bounds(dataset$brouwer_mean_motion)[1] ~ NA,
                                           brouwer_mean_motion > outlier_bounds(dataset$brouwer_mean_motion)[2] ~ NA,
                                           TRUE ~ brouwer_mean_motion),
           right_ascension = case_when(right_ascension < outlier_bounds(dataset$right_ascension)[1] ~ NA,
                                       right_ascension > outlier_bounds(dataset$right_ascension)[2] ~ NA,
                                       TRUE ~ right_ascension))
  
  # Returning the dataset without outliers
  return(output)
}

# Removing outliers from satellite datasets
cs2 <- remove_outliers(cs2_raw)
fy2d <- remove_outliers(fy2d_raw)
fy2e <- remove_outliers(fy2e_raw)
fy2f <- remove_outliers(fy2f_raw)
fy2h <- remove_outliers(fy2h_raw)
fy4a <- remove_outliers(fy4a_raw)
hy2a <- remove_outliers(remove_outliers(hy2a_raw))
js1 <- remove_outliers(js1_raw)
js2 <- remove_outliers(js2_raw)
js3 <- remove_outliers(js3_raw)
srl <- remove_outliers(srl_raw)
stl3a <- remove_outliers(stl3a_raw)
stl3b <- remove_outliers(stl3b_raw)
stl6a <- remove_outliers(stl6a_raw)
tpx <- remove_outliers(tpx_raw)



# Plotting and EDA -------------------------------------------------------------
# Function to visualise orbital elements
six_plots <- function(dataset) {
  p1 <- dataset %>% ggplot(aes(x=Date, y=inclination)) + 
    geom_line() + labs(title="Inclination", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  p2 <- dataset %>% ggplot(aes(x=Date, y=eccentricity)) + 
    geom_line() + labs(title="Eccentricity", y="Orbit Shape") + scale_y_continuous(labels = scales::scientific) + 
    theme(text = element_text(family="serif", size=17))
  p3 <- dataset %>% ggplot(aes(x=Date, y=argument_of_perigee)) + 
    geom_line() + labs(title="Argument of Perigee", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  p4 <- dataset %>% ggplot(aes(x=Date, y=mean_anomaly)) + 
    geom_line() + labs(title="Mean Anomaly", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  p5 <- dataset %>% ggplot(aes(x=Date, y=brouwer_mean_motion)) + 
    geom_line() + labs(title="Mean Motion", y="Radians per Epoch") + 
    theme(text = element_text(family="serif", size=17))
  p6 <- dataset %>% ggplot(aes(x=Date, y=right_ascension)) + 
    geom_line() + labs(title="Right Ascension", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  
  # Returning all 6 plots patched together using patchwork
  return ((p1 + p2 + p3) / (p4 + p5 + p6))
}


# Function to produce 3 before/after plots for a given element
raw_outlier_final_plots <- function(dataset_name, date_start, date_end, element) {
  
  data_raw <- get(paste(dataset_name, "_raw", sep=""))
  data <- get(dataset_name)
  data_trim <- data %>% filter(Date >= as.Date(date_start) & Date <= as.Date(date_end))
  
  element_name <- str_to_title(gsub("_"," ",element)) %>% str_replace_all(' Of ', " of ")
  plot_title_1 <- paste("Raw data for", element_name, sep=" ") %>% str_to_sentence()
  plot_title_2 <- paste(element_name, "with outliers removed", sep=" ") %>% str_to_sentence()
  plot_title_3 <- paste(element_name, "with outliers removed and specified date range", sep=" ") %>% str_to_sentence()
  
  if (element == "inclination" | element == "argument_of_perigee" | element == "mean_anomaly" | element == "right_ascension") {
    units <- "Radians"
  } else if (element == "eccentricity") {
    units <- "Orbit Shape"
  } else if (element == "brouwer_mean_motion") {
    units <- "Radians per Epoch"
  }
  
  p1 <- data_raw %>% ggplot(aes(x=Date, y=get(element))) + 
    geom_line() + labs(title=plot_title_1, y=units) + theme(text = element_text(family="serif",
                                                                                size=16))
  p2 <- data %>% ggplot(aes(x=Date, y=get(element))) + 
    geom_line() + labs(title=plot_title_2, y=units) + theme(text = element_text(family="serif",
                                                                                size=16))
  p3 <- data_trim %>% ggplot(aes(x=Date, y=get(element))) + 
    geom_line() + labs(title=plot_title_3, y=units) + theme(text = element_text(family="serif",
                                                                                size=16))
  
  return (p1 / p2 / p3)
  
}

# Getting the three before/after plots for eccentricity on Sentinel-3B
raw_outlier_final_plots("stl3b", "2019-01-01", "2022-01-01", "eccentricity")


# Plotting 6-element plots for all five satellite datasets
six_plots(cs2_raw)
six_plots(cs2)
six_plots(cs2 %>% filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2020-01-01")))
six_plots(js3_raw)
six_plots(js3)
six_plots(js3 %>% filter(Date >= as.Date("2017-01-01") & Date <= as.Date("2022-01-01")))
six_plots(fy2d_raw)
six_plots(fy2d)
six_plots(fy2d %>% filter(Date >= as.Date("2012-01-01") & Date <= as.Date("2015-01-01")))
six_plots(srl_raw)
six_plots(srl)
six_plots(srl %>% filter(Date >= as.Date("2016-08-01") & Date <= as.Date("2022-08-01")))
six_plots(stl3b_raw)
six_plots(stl3b)
six_plots(stl3b %>% filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2022-01-01")))

# Plotting Sentinel-3B manoeuvres against mean motion
stl3b %>% filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2022-01-01")) %>%
  ggplot(aes(x = Date, y = brouwer_mean_motion)) +
  geom_line(aes(color = "Radians per Epoch")) + 
  geom_vline(data = stl3b_man %>% filter(man_beg >= as.Date("2019-01-01") & man_beg <= as.Date("2022-01-01")), 
             aes(xintercept = man_beg, color = "Manoeuvre"), linetype = "dashed") +
  scale_color_manual(name = "Legend",
                     values = c("Radians per Epoch" = "black", "Manoeuvre" = "red")) +
  labs(title = "Mean motion overlayed with ground-truth manoeuvres",
       y = NULL) +
  theme(legend.position = "bottom",
        text = element_text(family="serif", size=17),
        legend.text = element_text(size=15))

# Getting number of manoeuvres per the 5 satellites
cs2_man %>% filter(man_beg >= as.Date("2016-01-01") & man_beg <= as.Date("2020-01-01")) %>% count()
js3_man %>% filter(man_beg >= as.Date("2018-01-01") & man_beg <= as.Date("2022-01-01")) %>% count()
fy2d_man %>% filter(man_beg >= as.Date("2012-01-01") & man_beg <= as.Date("2015-01-01")) %>% count()
hy2a_man %>% filter(man_beg >= as.Date("2017-01-01") & man_beg <= as.Date("2020-01-01")) %>% count()
srl_man %>% filter(man_beg >= as.Date("2016-08-01") & man_beg <= as.Date("2022-08-01")) %>% count()
stl3b_man %>% filter(man_beg >= as.Date("2019-01-01") & man_beg <= as.Date("2022-01-01")) %>% count()




# Functions for ARIMA modelling ------------------------------------------------
# Creating a vector of element names
element_names <- c("eccentricity", "argument_of_perigee", "inclination", "mean_anomaly", "brouwer_mean_motion", "right_ascension")
# Function to get all combinations of elements
get_combinations_excluding_one <- function(names, exclude) {
  remaining_names <- setdiff(names, exclude)
  all_combinations <- list(NULL)
  
  for (i in 1:length(remaining_names)) {
    combs <- combn(remaining_names, i, simplify = FALSE)
    all_combinations <- c(all_combinations, combs)
  }
  
  return(all_combinations)
}

# Generating lists of combinations excluding each element
element_combinations <- lapply(element_names, function(exclude) {
  get_combinations_excluding_one(element_names, exclude)
})

# Assigning names to each list for clarity
names(element_combinations) <- element_names


# Function to model ARIMA for a certain element with a specified combination of external regressors
arima_modelling_extra <- function(dataset, dataset_man, date_start, date_end, date_split, element, extras, output) {
  
  # Filtering the dataset to the specified date range
  data_trim <- dataset %>% filter(Date >= as.Date(date_start) & Date <= as.Date(date_end)) %>% na.omit()
  # Creating training and testing sets determined by the date_split parameter
  data_train <- data_trim %>% filter(Date < as.Date(date_split))
  data_test <- data_trim %>% filter(Date >= as.Date(date_split))
  
  # Filtering the manoeuvre dataset to the specified date range
  dataset_man <- dataset_man %>%
    filter(man_beg >= as.Date(date_start) & man_beg <= as.Date(date_end))
  
  # Creating full and training external regressor matrices, with NULL for no external regressors
  if (is.null(extras)) {
    extra_data_full <- NULL
    extra_data_train <- NULL
  } else {
    extra_data_full <- data_trim %>% select(all_of(extras)) %>% as.matrix() %>% scale()
    extra_data_train <- data_train %>% select(all_of(extras)) %>% as.matrix() %>% scale()
  }
  
  # Handling case of error where no possible ARIMA models can be fit
  tryCatch({
    # Training an ARIMA model on the training set and fitting it to the full dataset
    train_model <- auto.arima(data_train[[element]] %>% scale(), xreg = extra_data_train)
    full_model <- Arima(data_trim[[element]] %>% scale(), model = train_model, xreg = extra_data_full)
    fitted_values <- fitted(full_model) %>% as.numeric()
    
    # Conditional for specified function output
    if (output == "model") {
      
      # Returning the ARIMA model if specified in function parameters
      return (full_model)
      
    } else if (output == "plot") {
      
      # Calculating the residuals between the full dataset and fitted values
      data_trim$Residuals <- abs(data_trim[[element]] %>% scale() - fitted_values)
      
      # Creating a string displaying the ARIMA model specifications
      arima_model_specs <- paste("ARIMAX(", full_model$arma[1], ",", full_model$arma[6], ",", full_model$arma[2], ")", sep = "")
      
      # Creating strings for plot title and subtitle including all relevant specifications
      if (is.null(extras)) {
        plot_subtitle <- NULL
        arima_model_specs <- paste("ARIMA(", full_model$arma[1], ",", full_model$arma[6], ",", full_model$arma[2], ")", sep = "")
      } else {
        extras <- str_to_title(paste(gsub("_"," ",extras), collapse=", ")) %>% str_replace_all('Brouwer ', "") %>% str_replace_all(' Of ', " of ")
        plot_subtitle <- paste("X =", extras, sep=" ")
      }
      plot_title <- paste(arima_model_specs, "for", str_to_title(gsub("_"," ",element))) %>% str_replace_all('Brouwer ', "") %>% str_replace_all(' Of ', " of ")
      
      # Plotting the residuals with red vertical lines indicating manoeuvres
      residuals_plot <- data_trim %>%
        ggplot(aes(x = Date, y = Residuals)) +
        geom_line(aes(color = "Residuals")) + 
        geom_vline(data = dataset_man, aes(xintercept = man_beg, color = "Manoeuvre"), linetype = "dashed") +
        scale_color_manual(name = "Legend",
                           values = c("Residuals" = "black", "Manoeuvre" = "red")) +
        labs(title = plot_title,
             subtitle = plot_subtitle,
             y = NULL) +
        theme(plot.subtitle = element_text(size=13),
              legend.position = "bottom",
              legend.text = element_text(size=15),
              text = element_text(family="serif", size=17))
      
      # Returning the residuals plot if specified in function parameters
      return (residuals_plot)
    }
    
  }, error = function(e) {
    message("Error encountered: ", e$message)
    
    # Returning NULL if no ARIMA model could be fit
    return(NULL)
  })
  
}


# Function to return a plot or external regressors of the best ARIMA model for the given parameters
best_arima_model <- function(dataset, dataset_man, date_start, date_end, date_split, element, output) {
  
  # Creating a tibble of AIC values for all combinations of external regressors for ARIMA on given element
  df_output <- tibble(id = numeric(), aic = numeric())
  for (i in 1:length(element_combinations[[element]])) {
    
    # Combination of external regressor element names
    external_regressors <- element_combinations[[element]][[i]]
    
    # Calculating AIC value for current ARIMA model
    output_aic <- tryCatch({
      arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, element, external_regressors, "model")$aic
    }, error = function(e) {
      print(paste("Error encountered in iteration", i, ":", e$message))
      return (NA)
    })
    
    # Appending AIC value for current ARIMA model to output tibble
    output_tibble <- tibble(id = i, aic = output_aic)
    df_output <- bind_rows(df_output, output_tibble)
  }
  
  # Calculating best combination of external regressors using ARIMA model AIC
  best_id <- df_output %>% arrange(aic) %>% slice(1) %>% pull(id)
  best_reg <- element_combinations[[element]][[best_id]]
  
  # Conditional for specified function output
  if (output == "reg") {
    # Returning list of xreg element names if specified in function parameters
    return (best_reg)
  } else if (output == "aic") {
    # Returning tibble of AIC values for all fitted models if specified in function parameters
    return (df_output %>% arrange(aic))
  }
  
}


# Function for generating six plots of all six elements' ARIMA models for the specified dataset
arima_six_plots <- function(dataset, dataset_man, date_start, date_end, date_split, type="null") {
  
  if (type == "best") {
    # Getting best ARIMA models for all elements of specified dataset
    data_incl_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "inclination", "reg")
    data_ecce_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "eccentricity", "reg")
    data_argp_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "argument_of_perigee", "reg")
    data_anom_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "mean_anomaly", "reg")
    data_brmm_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "brouwer_mean_motion", "reg")
    data_rasc_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "right_ascension", "reg")
  } else if (type == "null") {
    # Setting external regressors as null to calculate basic model
    data_incl_best <- NULL
    data_ecce_best <- NULL
    data_argp_best <- NULL
    data_anom_best <- NULL
    data_brmm_best <- NULL
    data_rasc_best <- NULL
  }
  
  # Creating plots for each best model per element
  data_incl_plot <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "inclination", data_incl_best, "plot")
  data_ecce_plot <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "eccentricity", data_ecce_best, "plot")
  data_argp_plot <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "argument_of_perigee", data_argp_best, "plot")
  data_anom_plot <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "mean_anomaly", data_anom_best, "plot")
  data_brmm_plot <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "brouwer_mean_motion", data_brmm_best, "plot")
  data_rasc_plot <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "right_ascension", data_rasc_best, "plot")
  
  # Plotting all six plots together using patchwork
  (data_incl_plot + data_ecce_plot) / (data_argp_plot + data_anom_plot) / (data_brmm_plot + data_rasc_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
}



# Results and final plots ------------------------------------------------------
# Getting best ARIMA models for all elements of CryoSat-2
cs2_incl_best <- best_arima_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "inclination", "reg")
cs2_ecce_best <- best_arima_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "eccentricity", "reg")
cs2_argp_best <- best_arima_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "argument_of_perigee", "reg")
cs2_anom_best <- best_arima_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "mean_anomaly", "reg")
cs2_brmm_best <- best_arima_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "brouwer_mean_motion", "reg")
cs2_rasc_best <- best_arima_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "right_ascension", "reg")

cs2_incl_best_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "inclination", cs2_incl_best, "plot")
cs2_ecce_best_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "eccentricity", cs2_ecce_best, "plot")
cs2_argp_best_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "argument_of_perigee", cs2_argp_best, "plot")
cs2_anom_best_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "mean_anomaly", cs2_anom_best, "plot")
cs2_brmm_best_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "brouwer_mean_motion", cs2_brmm_best, "plot")
cs2_rasc_best_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "right_ascension", cs2_rasc_best, "plot")

# Plots of best ARIMAX models for all six elements for CryoSat-2
(cs2_incl_best_plot + cs2_ecce_best_plot) / (cs2_argp_best_plot + cs2_anom_best_plot) / (cs2_brmm_best_plot + cs2_rasc_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# Getting best ARIMA models for all elements of Jason 3
js3_incl_best <- best_arima_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "inclination", "reg")
js3_ecce_best <- best_arima_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "eccentricity", "reg")
js3_argp_best <- best_arima_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "argument_of_perigee", "reg")
js3_anom_best <- best_arima_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "mean_anomaly", "reg")
js3_brmm_best <- best_arima_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "brouwer_mean_motion", "reg")
js3_rasc_best <- best_arima_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "right_ascension", "reg")

js3_incl_best_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "inclination", js3_incl_best, "plot")
js3_ecce_best_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "eccentricity", js3_ecce_best, "plot")
js3_argp_best_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "argument_of_perigee", js3_argp_best, "plot")
js3_anom_best_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "mean_anomaly", js3_anom_best, "plot")
js3_brmm_best_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "brouwer_mean_motion", js3_brmm_best, "plot")
js3_rasc_best_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "right_ascension", js3_rasc_best, "plot")

# Plots of best ARIMAX models for all six elements for Jason-3
(js3_incl_best_plot + js3_ecce_best_plot) / (js3_argp_best_plot + js3_anom_best_plot) / (js3_brmm_best_plot + js3_rasc_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# Getting best ARIMA models for all elements of Fengyun 2D
fy2d_incl_best <- best_arima_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "inclination", "reg")
fy2d_ecce_best <- best_arima_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "eccentricity", "reg")
fy2d_argp_best <- best_arima_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "argument_of_perigee", "reg")
fy2d_anom_best <- best_arima_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "mean_anomaly", "reg")
fy2d_brmm_best <- best_arima_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion", "reg")
fy2d_rasc_best <- best_arima_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "right_ascension", "reg")

fy2d_incl_best_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "inclination", fy2d_incl_best, "plot")
fy2d_ecce_best_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "eccentricity", fy2d_ecce_best, "plot")
fy2d_argp_best_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "argument_of_perigee", fy2d_argp_best, "plot")
fy2d_anom_best_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "mean_anomaly", fy2d_anom_best, "plot")
fy2d_brmm_best_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion", fy2d_brmm_best, "plot")
fy2d_rasc_best_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "right_ascension", fy2d_rasc_best, "plot")

# Plots of best ARIMAX models for all six elements for FengYun
(fy2d_incl_best_plot + fy2d_ecce_best_plot) / (fy2d_argp_best_plot + fy2d_anom_best_plot) / (fy2d_brmm_best_plot + fy2d_rasc_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# Getting best ARIMA models for all elements of SARAL
srl_incl_best <- best_arima_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "inclination", "reg")
srl_ecce_best <- best_arima_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "eccentricity", "reg")
srl_argp_best <- best_arima_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "argument_of_perigee", "reg")
srl_anom_best <- best_arima_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "mean_anomaly", "reg")
srl_brmm_best <- best_arima_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "brouwer_mean_motion", "reg")
srl_rasc_best <- best_arima_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "right_ascension", "reg")

srl_incl_best_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "inclination", srl_incl_best, "plot")
srl_ecce_best_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "eccentricity", srl_ecce_best, "plot")
srl_argp_best_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "argument_of_perigee", srl_argp_best, "plot")
srl_anom_best_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "mean_anomaly", srl_anom_best, "plot")
srl_brmm_best_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "brouwer_mean_motion", srl_brmm_best, "plot")
srl_rasc_best_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "right_ascension", srl_rasc_best, "plot")

# Plots of best ARIMAX models for all six elements for SARAL
(srl_incl_best_plot + srl_ecce_best_plot) / (srl_argp_best_plot + srl_anom_best_plot) / (srl_brmm_best_plot + srl_rasc_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# Getting best ARIMAX and ARIMA models for all elements of Sentinel 3B
stl3b_incl_best <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "inclination", "reg")
stl3b_ecce_best <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "eccentricity", "reg")
stl3b_argp_best <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "argument_of_perigee", "reg")
stl3b_anom_best <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "mean_anomaly", "reg")
stl3b_brmm_best <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion", "reg")
stl3b_rasc_best <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "right_ascension", "reg")

stl3b_incl_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "inclination", stl3b_incl_best, "plot")
stl3b_ecce_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "eccentricity", stl3b_ecce_best, "plot")
stl3b_argp_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "argument_of_perigee", stl3b_argp_best, "plot")
stl3b_anom_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "mean_anomaly", stl3b_anom_best, "plot")
stl3b_brmm_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion", stl3b_brmm_best, "plot")
stl3b_rasc_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "right_ascension", stl3b_rasc_best, "plot")

stl3b_incl_basic_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "inclination", NULL, "plot")
stl3b_ecce_basic_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "eccentricity", NULL, "plot")
stl3b_argp_basic_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "argument_of_perigee", NULL, "plot")
stl3b_anom_basic_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "mean_anomaly", NULL, "plot")
stl3b_brmm_basic_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion", NULL, "plot")
stl3b_rasc_basic_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "right_ascension", NULL, "plot")

# Plots of best ARIMAX models for all six elements for Sentinel-3B
(stl3b_incl_best_plot + stl3b_ecce_best_plot) / (stl3b_argp_best_plot + stl3b_anom_best_plot) / (stl3b_brmm_best_plot + stl3b_rasc_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
# Plots of ARIMA and ARIMAX best models for inclination and mean motion for Sentinel-3B
(stl3b_incl_basic_plot + stl3b_brmm_basic_plot) / (stl3b_incl_best_plot + stl3b_brmm_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')



# Getting the best plots for both ARIMA and ARIMAX for each satellite
cs2_arima_six_plots_best <- arima_six_plots(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "best")
js3_arima_six_plots_best <- arima_six_plots(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "best")
fy2d_arima_six_plots_best <- arima_six_plots(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "best")
srl_arima_six_plots_best <- arima_six_plots(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "best")
stl3b_arima_six_plots_best <- arima_six_plots(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "best")

cs2_arima_six_plots_basic <- arima_six_plots(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "null")
js3_arima_six_plots_basic <- arima_six_plots(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "null")
fy2d_arima_six_plots_basic <- arima_six_plots(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "null")
srl_arima_six_plots_basic <- arima_six_plots(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "null")
stl3b_arima_six_plots_basic <- arima_six_plots(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "null")


# Making 10-plot figure of best models for each method and satellite
cs2_brmm_basic_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "brouwer_mean_motion", NULL, "plot")
cs2_brmm_best_plot <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "brouwer_mean_motion", cs2_brmm_best, "plot")
js3_brmm_basic_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "brouwer_mean_motion", NULL, "plot")
js3_brmm_best_plot <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "brouwer_mean_motion", js3_brmm_best, "plot")
fy2d_brmm_basic_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion", NULL, "plot")
fy2d_brmm_best_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion", fy2d_brmm_best, "plot")
srl_brmm_basic_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "brouwer_mean_motion", NULL, "plot")
srl_brmm_best_plot <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "brouwer_mean_motion", srl_brmm_best, "plot")
stl3b_brmm_basic_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion", NULL, "plot")
stl3b_incl_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "inclination", stl3b_incl_best, "plot")

(cs2_brmm_basic_plot + cs2_brmm_best_plot) / (js3_brmm_basic_plot + js3_brmm_best_plot) / (fy2d_brmm_basic_plot + fy2d_brmm_best_plot) / (srl_brmm_basic_plot + srl_brmm_best_plot) / (stl3b_brmm_basic_plot + stl3b_incl_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
(cs2_brmm_best_plot / js3_brmm_best_plot / fy2d_brmm_best_plot / srl_brmm_best_plot / stl3b_incl_best_plot) + plot_layout(guides = "collect") & theme(legend.position = 'bottom')


# Making 5-plot figure of best models for each satellite
final_plot_layout <- "
AAABBB
CCCDDD
#EEEE#
"

cs2_brmm_best_plot_final <- cs2_brmm_best_plot + 
  plot_annotation(title="CryoSat-2", theme=theme(plot.title=element_text(hjust=0.5, size=23, family="serif", face="bold"))) + 
  theme(legend.position = 'none')
js3_brmm_best_plot_final <- js3_brmm_best_plot + 
  plot_annotation(title="Jason-3", theme=theme(plot.title=element_text(hjust=0.5, size=23, family="serif", face="bold"))) + 
  theme(legend.position = 'none')
stl3b_incl_best_plot_final <- stl3b_incl_best_plot + 
  plot_annotation(title="Sentinel-3B", theme=theme(plot.title=element_text(hjust=0.5, size=23, family="serif", face="bold"))) + 
  theme(legend.position = 'none')
srl_brmm_best_plot_final <- srl_brmm_best_plot + 
  plot_annotation(title="SARAL", theme=theme(plot.title=element_text(hjust=0.5, size=23, family="serif", face="bold"))) + 
  theme(legend.position = 'none')
fy2d_brmm_best_plot_final <- fy2d_brmm_best_plot + 
  plot_annotation(title="FengYun 2D", theme=theme(plot.title=element_text(hjust=0.5, size=23, family="serif", face="bold"))) + 
  theme(legend.position = 'right')

wrap_elements(cs2_brmm_best_plot_final) + wrap_elements(js3_brmm_best_plot_final) + 
  wrap_elements(stl3b_incl_best_plot_final) + wrap_elements(srl_brmm_best_plot_final) +
  wrap_elements(fy2d_brmm_best_plot_final) + 
  plot_layout(design = final_plot_layout)


