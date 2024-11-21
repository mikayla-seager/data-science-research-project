# Loading in packages and setting directories ---------------------------------------------------------------------------------------------------------
pacman::p_load(tidyverse, readr, skimr, stats, forecast, patchwork, janitor, lubridate, writexl, readxl, xgboost, caret, conflicted, tidymodels)
# Dealing with conflicts between same-name functions from different packages
conflict_prefer("slice", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")

# Setting directories
manoeuvre_data_wd <- "V:\\Mikayla Seager\\2024\\DSRP\\satellite_data\\manoeuvres"
satellite_data_wd <- "V:\\Mikayla Seager\\2024\\DSRP\\satellite_data\\orbital_elements"
exported_data_wd <- "V:\\Mikayla Seager\\2024\\DSRP\\exported_data"
exported_plots_wd <- "V:\\Mikayla Seager\\2024\\DSRP\\plots"

# Function to write data to xlsx
write_to_xlsx <- function(data_to_write, name = NA) {
  
  # Conditional if name of dataset matches name to write to file
  if (all(is.na(name))) {
    # Getting string name of dataset
    if (typeof(data_to_write) == "character") {
      data_name <- data_to_write
      data_to_write <- get(data_to_write)
    } else {
      data_name <- as.character(substitute(data_to_write))
    }
  } else {
    data_name <- name
  }
  
  # Setting directory to exported data location
  setwd(exported_data_wd)
  # Writing the dataset to xlsx
  write_xlsx(data_to_write, paste(getwd(), "\\",
                                  paste(data_name,"_",sep=""), 
                                  format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
                                  ".xlsx", sep = ""))
}

# Function to read xlsx data in
read_from_xlsx <- function(data_to_read) {
  # Getting string name of dataset
  if (typeof(data_to_read) == "character") {
    data_name <- data_to_read
  } else {
    data_name <- as.character(substitute(data_to_read))
  }
  # Setting directory to exported data location
  setwd(exported_data_wd)
  # Reading in the latest version of the dataset
  Files <- list.files(pattern = "xlsx$")
  INFO <- file.info(Files)
  INFO <- INFO[grepl(paste(data_name,"_20",sep=""), rownames(INFO)), ]
  if (nrow(INFO) == 0) {
    output <- NA
    cat("No file found\n")
  } else {
    MaxTime <- max(INFO$mtime)
    LastFile <- rownames(INFO[INFO$mtime == MaxTime,])
    output <- read_xlsx(LastFile)
  }
  # Returning the latest dataset
  return(output)
}

# Manoeuvre data import and clean ---------------------------------------------------------------------------------------------------------------------
# Setting directory
setwd(manoeuvre_data_wd)

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
    man_beg = ymd_hms(str_extract(dataset, '(?<=\\")[^"]*(?=\\")')),
    man_end = ymd_hms(str_extract(dataset, '(?<=\")[^"]*(?=\\"(?!.*\\"))'))
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

# Cleaning manoeuvre using custom function
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



# Satellite elements data import and clean ------------------------------------------------------------------------------------------------------------
# Setting directory again
setwd(satellite_data_wd)

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

# Removing outliers from datasets
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



# Plotting and EDA functions --------------------------------------------------------------------------------------------------------------------------
# Function to visualise orbital elements
six_plots <- function(dataset) {
  p1 <- dataset %>% ggplot(aes(x=Date, y=inclination)) + 
    geom_line() + labs(title="Inclination", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  p2 <- dataset %>% ggplot(aes(x=Date, y=eccentricity)) + 
    geom_line() + labs(title="Eccentricity", y="Orbit Shape") + 
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





# Functions for ARIMA modelling -----------------------------------------------------------------------------------------------------------------------
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
  data_trim <- dataset %>% filter(Date >= as.Date(date_start) & Date <= as.Date(date_end)) %>% 
    mutate(Date = as.Date(Date)) %>% na.omit()
  # Creating training and testing sets determined by the date_split parameter
  data_train <- data_trim %>% filter(Date < as.Date(date_split))
  data_test <- data_trim %>% filter(Date >= as.Date(date_split))
  
  # Filtering the manoeuvre dataset to the specified date range
  dataset_man <- dataset_man %>%
    filter(man_beg >= as.Date(date_start) & man_beg <= as.Date(date_end)) %>% 
    mutate(man_beg = as.Date(man_beg))
  
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
      
    } else if (output == "residuals") {
      
      # Calculating the residuals between the full dataset and fitted values
      data_trim$Residuals <- abs(data_trim[[element]] %>% scale() - fitted_values)
      
      return (data_trim %>% select(Date, Residuals))
      
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
        geom_line(aes(color = "Residuals"), linewidth = 1) + 
        geom_vline(data = dataset_man, aes(xintercept = man_beg, color = "Manoeuvre"), linewidth = 1, linetype = "dashed") +
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


# Function to write best element combination data
arimax_best_element_combos_write <- function(dataset, incl=NA,ecce=NA,argp=NA,anom=NA,brmm=NA,rasc=NA) {
  # Getting string name of dataset
  if (typeof(dataset) == "character") {
    data_name <- dataset
  } else {
    data_name <- as.character(substitute(dataset))
  }
  # Setting directory to exported data location
  setwd(exported_data_wd)
  # Exporting data for each element
  element_abbrevs <- c("incl","ecce","argp","anom","brmm","rasc")
  file_name <- paste(paste(data_name,"best_element_combos_arimax",sep="_"),".txt",sep="")
  file.create(file_name)
  # Conditional for if data has been created not globally
  if (all(is.na(c(incl,ecce,argp,anom,brmm,rasc)))) {
    for (element in element_abbrevs) {
      
      data <- get(paste(data_name,element,"best",sep="_"))
      write("___", file_name, append=TRUE)
      lapply(data, write, file_name, append=TRUE, ncolumns=1)
      
    }
  } else {
    write("___", file_name, append=TRUE)
    lapply(incl, write, file_name, append=TRUE, ncolumns=1)
    write("___", file_name, append=TRUE)
    lapply(ecce, write, file_name, append=TRUE, ncolumns=1)
    write("___", file_name, append=TRUE)
    lapply(argp, write, file_name, append=TRUE, ncolumns=1)
    write("___", file_name, append=TRUE)
    lapply(anom, write, file_name, append=TRUE, ncolumns=1)
    write("___", file_name, append=TRUE)
    lapply(brmm, write, file_name, append=TRUE, ncolumns=1)
    write("___", file_name, append=TRUE)
    lapply(rasc, write, file_name, append=TRUE, ncolumns=1)
  }
  
}

# Function to read in best element combination data
arimax_best_element_combos_read <- function(dataset) {
  # Getting string name of dataset
  if (typeof(dataset) == "character") {
    data_name <- dataset
  } else {
    data_name <- as.character(substitute(dataset))
  }
  # Setting directory to exported data location
  setwd(exported_data_wd)
  # Setting filename to find
  file_name <- paste(paste(data_name,"best_element_combos_arimax",sep="_"),".txt",sep="")
  # Reading in the latest version of the dataset
  Files <- list.files(pattern = "txt$")
  INFO <- file.info(Files)
  INFO <- INFO[grepl(file_name, rownames(INFO)), ]
  if (nrow(INFO) == 0) {
    output <- NA
    cat("No file found\n")
  } else {
    MaxTime <- max(INFO$mtime)
    LastFile <- rownames(INFO[INFO$mtime == MaxTime,])
    # Obtaining the output list of best combinations
    element_combinations_imported <- read_lines(LastFile)
    split_indices <- which(element_combinations_imported == "___")
    split_points <- c(0, split_indices, length(element_combinations_imported) + 1)
    split_list <- lapply(seq_along(split_points[-1]), function(i) {
      element_combinations_imported[(split_points[i] + 1):(split_points[i + 1] - 1)]
    })
    output <- split_list[sapply(split_list, function(x) length(x) > 0 && !"___" %in% x)]
  }
  
  # Returning the list of best combinations
  return(output)
}



# Functions for XGBoost modelling ---------------------------------------------------------------------------------------------------------------------
# Function to train an XGBoost model on a given satellite and element, returning its residuals or model
xgboost_residuals_get <- function(dataset, dataset_man, date_start, date_end, date_split, element, model_params=NA, output="res") {
  
  # Filtering the dataset to the specified date range
  data_trim <- dataset %>% filter(Date >= as.Date(date_start) & Date <= as.Date(date_end)) %>% na.omit() %>%
    select(c(Date, all_of(element))) %>% 
    mutate(element_og = get(element),
           element_new = scale(element_og),
           lag1 = lag(element_new)) %>% select(-all_of(element)) %>% na.omit()
  
  # Creating training and testing sets determined by the date_split parameter
  data_train <- data_trim %>% filter(Date <= as.Date(date_split))
  data_test <- data_trim %>% filter(Date > as.Date(date_split))
  
  # Standardising the training and testing sets if there is the potential for extrapolation issues
  if (min(data_test$element_new) < min(data_train$element_new) | max(data_test$element_new) > max(data_train$element_new)) {
    data_train <- data_train %>% 
      mutate(element_new = scale(element_new),
             lag1 = lag(element_new)) %>% na.omit()
    data_test <- data_test %>% 
      mutate(element_new = scale(element_new),
             lag1 = lag(element_new)) %>% na.omit()
  }
  
  # Filtering the manoeuvre dataset to the specified date range
  dataset_man <- dataset_man %>%
    filter(man_beg >= as.Date(date_start) & man_beg <= as.Date(date_end))
  
  # Creating tuning parameters
  if (all(is.na(model_params))) {
    # Tuning grid if custom model parameters are not used
    grid_tune <- expand.grid(
      nrounds = 1000,
      max_depth = c(1,2,3),
      eta = c(0.01,0.05,0.1),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 0.5
    )
  } else {
    # Using supploed model parameters instead of tuning
    grid_tune <- expand.grid(
      nrounds = model_params[[1]],
      max_depth = model_params[[2]],
      eta = model_params[[3]],
      gamma = model_params[[4]],
      colsample_bytree = model_params[[5]],
      min_child_weight = model_params[[6]],
      subsample = model_params[[7]]
    )
  }
  
  # Setting training configurations for hyperparameter tuning, including CV and seeds
  fold_length <- floor(nrow(data_train)/10)
  
  set.seed(1234)
  seeds <- vector(mode = "list", length = 11)
  for(i in 1:10) seeds[[i]]<- sample.int(n=1000, 12)
  seeds[[11]]<-sample.int(1000, 1)
  
  train_control <- trainControl(method = "timeslice",
                                initialWindow = ceiling(fold_length*0.8),
                                horizon = fold_length - ceiling(fold_length*0.8),
                                fixedWindow = FALSE,
                                skip = fold_length-1,
                                seeds = seeds,
                                verboseIter = TRUE,
                                allowParallel = TRUE)
  
  # Tuning the XGBoost model
  xgb_tune <- train(x = data_train %>% select(lag1),
                    y = data_train$element_new %>% as.numeric(),
                    trControl = train_control,
                    tuneGrid = grid_tune,
                    method = "xgbTree",
                    watchlist = list(eval = xgb.DMatrix(data = as.matrix(data_test %>% select(lag1)), label = data_test$element_new %>% as.numeric())),
                    early_stopping_rounds = 25,
                    verbose = FALSE,
                    verbosity = 0)
  
  # Obtaining residuals of XGBoost predictions against actual values
  residual_results <- data_trim %>% select(c(Date, element_og, element_new)) %>% 
    mutate(Residuals = abs(scale(predict(xgb_tune, data_trim)) - scale(element_new)),
           Prediction = predict(xgb_tune, data_trim)) %>%
    mutate(`element` = !!quo_name(element))
  
  # Conditional for specified function output
  if (output == "res") {
    # Returning tibble of residuals if specified in function parameters
    return (residual_results)
  } else if (output == "model") {
    # Returning XGBoost model if specified in function parameters
    return (xgb_tune)
  }
  
}


# Function to train an XGBoost model on a given satellite and element with additional predictors
xgboost_extra_residuals_get <- function(dataset, dataset_man, date_start, date_end, date_split, element, model_params=NA, output="res") {
  
  # Filtering the dataset to the specified date range
  data_trim <- dataset %>% filter(Date >= as.Date(date_start) & Date <= as.Date(date_end)) %>% 
    na.omit() %>% mutate_if(is.numeric, scale) %>%
    mutate(element_og = get(element),
           element_new = scale(element_og),
           lag1 = lag(element_new)) %>% 
    select(-all_of(element)) %>% na.omit()
  
  # Creating training and testing sets determined by the date_split parameter
  data_train <- data_trim %>% filter(Date <= as.Date(date_split))
  data_test <- data_trim %>% filter(Date > as.Date(date_split))
  
  # Standardising the training and testing sets if there is the potential for extrapolation issues
  if (min(data_test$element_new) < min(data_train$element_new) | max(data_test$element_new) > max(data_train$element_new)) {
    data_train <- bind_cols(data_train %>% select(c(Date, element_og)),
                            data_train %>% select(-c(Date, element_og)) %>%
                              mutate_if(is.numeric, scale) %>%
                              mutate(lag1 = lag(element_new))) %>% na.omit()
    data_test <- bind_cols(data_test %>% select(c(Date, element_og)),
                           data_test %>% select(-c(Date, element_og)) %>%
                             mutate_if(is.numeric, scale) %>%
                             mutate(lag1 = lag(element_new))) %>% na.omit()
  }
  
  # Filtering the manoeuvre dataset to the specified date range
  dataset_man <- dataset_man %>%
    filter(man_beg >= as.Date(date_start) & man_beg <= as.Date(date_end))
  
  # Creating tuning parameters
  if (all(is.na(model_params))) {
    # Tuning grid if custom model parameters are not used
    grid_tune <- expand.grid(
      nrounds = 1000,
      max_depth = c(1,2,3),
      eta = c(0.01,0.05,0.1),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 0.5
    )
  } else {
    # Using supploed model parameters instead of tuning
    grid_tune <- expand.grid(
      nrounds = model_params[[1]],
      max_depth = model_params[[2]],
      eta = model_params[[3]],
      gamma = model_params[[4]],
      colsample_bytree = model_params[[5]],
      min_child_weight = model_params[[6]],
      subsample = model_params[[7]]
    )
  }
  
  # Setting training configurations for hyperparameter tuning, including CV and seeds
  fold_length <- floor(nrow(data_train)/10)
  
  set.seed(1234)
  seeds <- vector(mode = "list", length = 11)
  for(i in 1:10) seeds[[i]]<- sample.int(n=1000, 12)
  seeds[[11]]<-sample.int(1000, 1)
  
  train_control <- trainControl(method = "timeslice",
                                initialWindow = ceiling(fold_length*0.8),
                                horizon = fold_length - ceiling(fold_length*0.8),
                                fixedWindow = FALSE,
                                skip = fold_length-1,
                                seeds = seeds,
                                verboseIter = TRUE,
                                allowParallel = TRUE)
  
  
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
  
  # Creating a tibble of RMSE values for all combinations of extra features for XGBoost on given element
  df_output <- tibble(id = numeric(), rmse = numeric(), combo = character())
  for (i in 1:length(element_combinations[[element]])) {
    
    # Obtaining this iteration's combination of extra predictor element names
    external_regressors <- element_combinations[[element]][[i]]
    
    # Setting the training and testing sets to use in the tuning process
    x_data_train <- bind_cols(data_train %>% select(all_of(external_regressors)) %>%
                                mutate_if(is.numeric, lag),
                              data_train %>% select(lag1))
    x_data_test <- bind_cols(data_test %>% select(all_of(external_regressors)) %>%
                               mutate_if(is.numeric, lag),
                             data_test %>% select(lag1))
    
    # Tuning the current XGBoost model
    xgb_tune <- train(x = x_data_train,
                      y = data_train$element_new %>% as.numeric(),
                      trControl = train_control,
                      tuneGrid = grid_tune,
                      method = "xgbTree",
                      watchlist = list(eval = xgb.DMatrix(data = as.matrix(data_test %>% select(lag1)), label = data_test$element_new %>% as.numeric())),
                      early_stopping_rounds = 25,
                      verbose = FALSE,
                      verbosity = 0)
    
    # Calculating RMSE value for current XGBoost model
    output_rmse <- RMSE(scale(predict(xgb_tune, data_test)), scale(data_test$element_new))
    
    # Appending RMSE value for current XGBoost model to output tibble
    output_tibble <- tibble(id = i, rmse = output_rmse, combo = paste(external_regressors, collapse = " + "))
    df_output <- bind_rows(df_output, output_tibble)
  }
  
  # Calculating best combination of extra features using XGBoost model RMSE
  best_id <- df_output %>% arrange(rmse) %>% slice(1) %>% pull(id)
  best_reg <- element_combinations[[element]][[best_id]]
  df_output %>% arrange(rmse) %>% print()
  best_reg %>% print()
  
  # Setting the training and testing sets to use in the final tuning process
  x_data_train <- bind_cols(data_train %>% select(all_of(best_reg)) %>%
                              mutate_if(is.numeric, lag),
                            data_train %>% select(lag1))
  x_data_test <- bind_cols(data_test %>% select(all_of(best_reg)) %>%
                             mutate_if(is.numeric, lag),
                           data_test %>% select(lag1))
  
  # Re-training the XGBoost model with the most optimal extra features
  xgb_tune <- train(x = x_data_train,
                    y = data_train$element_new %>% as.numeric(),
                    trControl = train_control,
                    tuneGrid = grid_tune,
                    method = "xgbTree",
                    watchlist = list(eval = xgb.DMatrix(data = as.matrix(data_test %>% select(lag1)), label = data_test$element_new %>% as.numeric())),
                    early_stopping_rounds = 25,
                    verbose = FALSE,
                    verbosity = 0)
  
  # Obtaining residuals of XGBoost predictions against actual values
  residual_results <- data_trim %>% select(c(Date, element_og, element_new)) %>% 
    mutate(Residuals = abs(scale(predict(xgb_tune, data_trim)) - scale(element_new)),
           Prediction = predict(xgb_tune, data_trim)) %>%
    mutate(`element` = !!quo_name(element),
           predictors = paste(best_reg, collapse = " + "))
  
  # Conditional for specified function output
  if (output == "res") {
    # Returning tibble of residuals if specified in function parameters
    return (residual_results)
  } else if (output == "model") {
    # Returning XGBoost model if specified in function parameters
    return (xgb_tune)
  }
  
}


# Function to create a residual manoeuvre plot from a given set of residuals
residual_man_plot_xgb <- function(residuals, dataset_man, date_start, date_end) {
  
  # Getting name of satellite to use in plot title later
  satellite_name <- as.character(substitute(dataset_man)) %>% str_replace_all('_man', "")
  
  # Filtering the manoeuvre dataset to the specified date range
  dataset_man <- dataset_man %>%
    filter(man_beg >= as.Date(date_start) & man_beg <= as.Date(date_end))
  
  # Creating a string of the element used's name
  element_name_lower <- residuals$element %>% unique()
  element_name_upper = str_to_title(gsub("_"," ",element_name_lower)) %>% str_replace_all('Brouwer ', "") %>% str_replace_all(' Of ', " of ")
  
  # Creating vectors of chosen satellite names and abbreviations
  satellite_names <- c("cs2","fy2d","js3","srl","stl3b")
  satellite_abbrevs <- c("CryoSat-2","FengYun 2D","Jason-3","SARAL","Sentinel-3B")
  
  # Determining full satellite name and plot title from input data
  index <- which(satellite_names == satellite_name)
  full_satellite_name <- satellite_abbrevs[index]
  plot_title <- paste(full_satellite_name, ": XGBoost on ", element_name_upper, sep="")
  
  # Creating a plot of the residuals with overlayed manoeuvres
  output_plot <- residuals %>%
    ggplot(aes(x=Date, y=Residuals)) +
    geom_line(aes(color = "Residuals"), linewidth = 1) + 
    geom_vline(data = dataset_man, aes(xintercept = man_beg, color = "Manoeuvre"), linewidth = 1, linetype = "dashed") +
    scale_color_manual(name = "Legend",
                       values = c("Residuals" = "black", "Manoeuvre" = "red")) +
    labs(title = plot_title,
         y = NULL) +
    theme(legend.position = "bottom",
          legend.text = element_text(size=15),
          text = element_text(family="serif", size=17))
  
  # Returning the final plot
  return (output_plot)
  
}



# Qualitative results and plots for ARIMA -------------------------------------------------------------------------------------------------------------
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

# Plots of best ARIMAX models for all six elements for FengYun 2D
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


# Getting best ARIMAX and ARIMA models for all elements of Sentinel-3B
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



# Quantitative functions ------------------------------------------------------------------------------------------------------------------------------
# Function to calculate precision recall metrics from model residuals over a range of thresholds
residual_pre_rec_obtain <- function(resids, mans, thresholds = NA) {
  
  # Initiating a tibble to store final results in
  pre_rec_results <- tibble(threshold = numeric(), precision = numeric(), recall = numeric(),
                            tp = numeric(), fn = numeric(), fp = numeric(), no_mans = numeric())
  
  # Checking for manually chosen thresholds to be used, otherwise uses residuals of input data
  if (all(is.na(thresholds))) {
    threshold_grid <- resids %>% arrange(Residuals) %>% pull(Residuals) %>% as.vector()
  } else {
    threshold_grid <- thresholds
  }
  
  # Looping over every threshold and calculating precision and recall metrics
  for (threshold in threshold_grid) {
    
    # Determining filtered residuals from threshold
    filtered_residuals <- resids %>% filter(Residuals > threshold)
    filtered_mans <- mans
    filtered_residuals_time <- filtered_residuals %>% pull(Date)
    filtered_mans_time <- filtered_mans %>% pull(man_beg)
    
    # Finding the closest matched manoeuvre to each filtered residual
    cuts <- c(-Inf, filtered_mans_time[-1]-diff(filtered_mans_time)/2, Inf)
    index <- findInterval(filtered_residuals_time, cuts)
    filtered_mans_time[index]
    
    # Filtering only residuals within 5 days of a manoeuvre
    temp_tibble <- tibble(filtered_residuals_time, filtered_mans_time[index], index) %>%
      mutate(difference = as.numeric(difftime(filtered_residuals_time, `filtered_mans_time[index]`, units="days"))) %>%
      filter(difference >= -5 & difference <= 5)
    
    # Calculating true positives, false negatives, and false positives
    tp <- temp_tibble %>% pull(index) %>% unique() %>% length()
    fn <- length(filtered_mans_time) - temp_tibble %>% pull(index) %>% unique() %>% length()
    fp <- length(filtered_residuals_time) - temp_tibble %>% count() %>% as.numeric()
    
    # Calculating precision and recall metrics
    precision <- tp/(tp+fp)
    recall <- tp/(tp+fn)
    
    # Printing the precision and recall for the current iteration for debugging purposes
    cat(paste("Threshold:", sprintf('%.3f', threshold), "| Precision:", sprintf('%.3f', precision), "| Recall:", sprintf('%.3f', recall), "\n"))
    
    # Appending the results for this iteration to the output tibble
    pre_rec_results <- bind_rows(tibble(threshold = threshold, precision = precision, recall = recall,
                                        tp = tp, fn = fn, fp = fp, no_mans = length(filtered_mans_time)), 
                                 pre_rec_results)
    
  }
  
  # Calculating f-scores for beta values of 0.5, 1, and 2
  pre_rec_results <- pre_rec_results %>% mutate(f_score = 2/((1/precision)+(1/recall)),
                                                f_0.5 = (1+0.5^2)/((1/precision)+((0.5^2)/recall)),
                                                f_2 = (1+2^2)/((1/precision)+((2^2)/recall)))
  
  # Returning the output tibble of precision and recall metrics
  return (pre_rec_results)
  
}


# Function for obtaining precision and recall, or f1-scores, of all 6 elements for a given satellite for ARIMA models
arima_best_prerec_model <- function(dataset, dataset_man, date_start, date_end, date_split, type="null", output="f1", override=0) {
  
  # Calculating ARIMAX best external regressors if type == best
  if (type == "best") {
    # Getting best ARIMA models for all elements of specified dataset
    dataset_name <- as.character(substitute(dataset))
    best_model_import <- arimax_best_element_combos_read(dataset_name)
    # Checking if the data already exists
    if (all(is.na(best_model_import)) | override == 1 | override == 3) {
      data_incl_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "inclination", "reg")
      data_ecce_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "eccentricity", "reg")
      data_argp_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "argument_of_perigee", "reg")
      data_anom_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "mean_anomaly", "reg")
      data_brmm_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "brouwer_mean_motion", "reg")
      data_rasc_best <- best_arima_model(dataset, dataset_man, date_start, date_end, date_split, "right_ascension", "reg")
      # Writing the data to a text file if it doesn't exist already
      arimax_best_element_combos_write(dataset_name, data_incl_best, data_ecce_best, data_argp_best, data_anom_best, data_brmm_best, data_rasc_best)
    } else {
      cat("Using existing optimal element combination data...\n")
      # Importing already existing data for efficiency
      data_incl_best <- best_model_import[[1]]
      data_ecce_best <- best_model_import[[2]]
      data_argp_best <- best_model_import[[3]]
      data_anom_best <- best_model_import[[4]]
      data_brmm_best <- best_model_import[[5]]
      data_rasc_best <- best_model_import[[6]]
    }
    
    # Setting external regressors as null for ARIMA if type == null
  } else if (type == "null") {
    data_incl_best <- NULL
    data_ecce_best <- NULL
    data_argp_best <- NULL
    data_anom_best <- NULL
    data_brmm_best <- NULL
    data_rasc_best <- NULL
  }
  
  # Getting residuals for each best model per element
  data_incl_best_resids <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "inclination", data_incl_best, "residuals")
  data_ecce_best_resids <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "eccentricity", data_ecce_best, "residuals")
  data_argp_best_resids <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "argument_of_perigee", data_argp_best, "residuals")
  data_anom_best_resids <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "mean_anomaly", data_anom_best, "residuals")
  data_brmm_best_resids <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "brouwer_mean_motion", data_brmm_best, "residuals")
  data_rasc_best_resids <- arima_modelling_extra(dataset, dataset_man, date_start, date_end, date_split, "right_ascension", data_rasc_best, "residuals")
  
  # Cutting manoeuvre data to specified date range
  data_man_trim <- dataset_man %>% filter(man_beg >= as.Date(date_start) & man_beg <= as.Date(date_end)) %>% arrange(man_beg)
  
  # Getting the filenames of potential existing files
  dataset_name <- as.character(substitute(dataset))
  if (type=="null") {model = "arima"} else if (type=="best") {model = "arimax"}
  file_name_of_f1_score_data <- paste(dataset_name,"best_model",model,sep="_")
  file_name_of_pre_rec_data <- paste(dataset_name,"6_element_pre_rec",model,sep="_")
  f1_score_data_import <- read_from_xlsx(file_name_of_f1_score_data)
  pre_rec_data_import <- read_from_xlsx(file_name_of_pre_rec_data)
  
  # Checking if the data already exists
  if (all(is.na(pre_rec_data_import)) | override == 2 | override == 3) {
    # Calculating precision and recall metrics for all six elements
    data_incl_pre_rec <- residual_pre_rec_obtain(data_incl_best_resids, data_man_trim)
    data_ecce_pre_rec <- residual_pre_rec_obtain(data_ecce_best_resids, data_man_trim)
    data_argp_pre_rec <- residual_pre_rec_obtain(data_argp_best_resids, data_man_trim)
    data_anom_pre_rec <- residual_pre_rec_obtain(data_anom_best_resids, data_man_trim)
    data_brmm_pre_rec <- residual_pre_rec_obtain(data_brmm_best_resids, data_man_trim)
    data_rasc_pre_rec <- residual_pre_rec_obtain(data_rasc_best_resids, data_man_trim)
    # Combining precision recall data into one tibble
    full_pre_rec_data <- bind_rows(data_incl_pre_rec %>% mutate(element = "inclination"),
                                   data_ecce_pre_rec %>% mutate(element = "eccentricity"),
                                   data_argp_pre_rec %>% mutate(element = "argument_of_perigee"),
                                   data_anom_pre_rec %>% mutate(element = "mean_anomaly"),
                                   data_brmm_pre_rec %>% mutate(element = "brouwer_mean_motion"),
                                   data_rasc_pre_rec %>% mutate(element = "right_ascension"))
    # Summarising precision recall metrics and selecting the best f1-score for all elements
    f1_results <- full_pre_rec_data %>% group_by(element) %>% arrange(-f_score) %>% slice(1) %>%
      ungroup() %>% arrange(-f_score) %>% select(-c(threshold, no_mans)) %>%
      rename("f1_score"="f_score") %>% select(f1_score, element, everything())
    
    # Writing data to xlsx file since it has not been created
    write_to_xlsx(full_pre_rec_data, name = file_name_of_pre_rec_data)
    write_to_xlsx(f1_results, name = file_name_of_f1_score_data)
    
  } else {
    cat("Using existing precision recall data...\n")
    full_pre_rec_data <- pre_rec_data_import
    f1_results <- f1_score_data_import
  }
  
  # Returning the desired output
  if (output == "pre_rec") {
    return (full_pre_rec_data)
  } else if (output == "f1") {
    return (f1_results)
  }
  
}


# Function for obtaining f1-scores of all 6 elements for a given satellite for XGBoost
xgboost_best_prerec_model <- function(dataset, dataset_man, date_start, date_end, date_split, type="basic", output="f1", override=0) {
  
  # Getting the filenames of potential existing files
  dataset_name <- as.character(substitute(dataset))
  if (type=="basic") {model = "xgboost"}
  file_name_of_model_data <- paste(dataset_name,"6_element_model_params",model,sep="_")
  file_name_of_residual_data <- paste(dataset_name,"6_element_residuals",model,sep="_")
  model_data_import <- read_from_xlsx(file_name_of_model_data)
  residual_data_import <- read_from_xlsx(file_name_of_residual_data)
  
  # Checking if the data already exists
  if (all(is.na(model_data_import)) | override == 1 | override == 3) {
    # Model parameter export
    data_incl_best_model <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "inclination", output="model")
    data_ecce_best_model <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "eccentricity", output="model")
    data_argp_best_model <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "argument_of_perigee", output="model")
    data_anom_best_model <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "mean_anomaly", output="model")
    data_brmm_best_model <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "brouwer_mean_motion", output="model")
    data_rasc_best_model <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "right_ascension", output="model")
    # Combining residual data into one tibble
    full_model_data <- bind_rows(data_incl_best_model$bestTune %>% as_tibble() %>% mutate(satellite=dataset_name, element = "inclination"),
                                 data_ecce_best_model$bestTune %>% as_tibble() %>% mutate(satellite=dataset_name, element = "eccentricity"),
                                 data_argp_best_model$bestTune %>% as_tibble() %>% mutate(satellite=dataset_name, element = "argument_of_perigee"),
                                 data_anom_best_model$bestTune %>% as_tibble() %>% mutate(satellite=dataset_name, element = "mean_anomaly"),
                                 data_brmm_best_model$bestTune %>% as_tibble() %>% mutate(satellite=dataset_name, element = "brouwer_mean_motion"),
                                 data_rasc_best_model$bestTune %>% as_tibble() %>% mutate(satellite=dataset_name, element = "right_ascension"))
    
    # Getting best XGBoost models' residuals for all elements of specified dataset
    data_incl_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "inclination", model_params = data_incl_best_model$bestTune %>% as_tibble())
    data_ecce_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "eccentricity", model_params = data_ecce_best_model$bestTune %>% as_tibble())
    data_argp_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "argument_of_perigee", model_params = data_argp_best_model$bestTune %>% as_tibble())
    data_anom_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "mean_anomaly", model_params = data_anom_best_model$bestTune %>% as_tibble())
    data_brmm_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "brouwer_mean_motion", model_params = data_brmm_best_model$bestTune %>% as_tibble())
    data_rasc_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "right_ascension", model_params = data_rasc_best_model$bestTune %>% as_tibble())
    # Combining residual data into one tibble
    full_resids_data <- bind_rows(data_incl_best_resids %>% mutate(element = "inclination"),
                                  data_ecce_best_resids %>% mutate(element = "eccentricity"),
                                  data_argp_best_resids %>% mutate(element = "argument_of_perigee"),
                                  data_anom_best_resids %>% mutate(element = "mean_anomaly"),
                                  data_brmm_best_resids %>% mutate(element = "brouwer_mean_motion"),
                                  data_rasc_best_resids %>% mutate(element = "right_ascension"))
    # Writing data to xlsx file since it has not been created
    write_to_xlsx(full_resids_data, name = file_name_of_residual_data)
    
  } else {
    cat("Using existing precision recall data...\n")
    data_incl_best_model <- model_data_import %>% filter(element == "inclination")
    data_ecce_best_model <- model_data_import %>% filter(element == "eccentricity")
    data_argp_best_model <- model_data_import %>% filter(element == "argument_of_perigee")
    data_anom_best_model <- model_data_import %>% filter(element == "mean_anomaly")
    data_brmm_best_model <- model_data_import %>% filter(element == "brouwer_mean_motion")
    data_rasc_best_model <- model_data_import %>% filter(element == "right_ascension")
    # Checking if residual data exists, otherwise re-running models using model data imported
    if (all(is.na(residual_data_import))) {
      data_incl_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "inclination", model_params = data_incl_best_model$bestTune %>% as_tibble())
      data_ecce_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "eccentricity", model_params = data_ecce_best_model$bestTune %>% as_tibble())
      data_argp_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "argument_of_perigee", model_params = data_argp_best_model$bestTune %>% as_tibble())
      data_anom_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "mean_anomaly", model_params = data_anom_best_model$bestTune %>% as_tibble())
      data_brmm_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "brouwer_mean_motion", model_params = data_brmm_best_model$bestTune %>% as_tibble())
      data_rasc_best_resids <- xgboost_residuals_get(dataset, dataset_man, date_start, date_end, date_split, "right_ascension", model_params = data_rasc_best_model$bestTune %>% as_tibble())
    } else {
      data_incl_best_resids <- residual_data_import %>% filter(element == "inclination")
      data_ecce_best_resids <- residual_data_import %>% filter(element == "eccentricity")
      data_argp_best_resids <- residual_data_import %>% filter(element == "argument_of_perigee")
      data_anom_best_resids <- residual_data_import %>% filter(element == "mean_anomaly")
      data_brmm_best_resids <- residual_data_import %>% filter(element == "brouwer_mean_motion")
      data_rasc_best_resids <- residual_data_import %>% filter(element == "right_ascension")
    }
  }
  
  # Cutting manoeuvre data to specified date range
  data_man_trim <- dataset_man %>% filter(man_beg >= as.Date(date_start) & man_beg <= as.Date(date_end)) %>% arrange(man_beg)
  
  # Getting the filenames of potential existing files
  dataset_name <- as.character(substitute(dataset))
  if (type=="basic") {model = "xgboost"}
  file_name_of_f1_score_data <- paste(dataset_name,"best_model",model,sep="_")
  file_name_of_pre_rec_data <- paste(dataset_name,"6_element_pre_rec",model,sep="_")
  f1_score_data_import <- read_from_xlsx(file_name_of_f1_score_data)
  pre_rec_data_import <- read_from_xlsx(file_name_of_pre_rec_data)
  
  # Checking if the data already exists
  if (all(is.na(pre_rec_data_import)) | override == 2 | override == 3) {
    # Calculating precision and recall metrics for all six elements
    data_incl_pre_rec <- residual_pre_rec_obtain(data_incl_best_resids, data_man_trim)
    data_ecce_pre_rec <- residual_pre_rec_obtain(data_ecce_best_resids, data_man_trim)
    data_argp_pre_rec <- residual_pre_rec_obtain(data_argp_best_resids, data_man_trim)
    data_anom_pre_rec <- residual_pre_rec_obtain(data_anom_best_resids, data_man_trim)
    data_brmm_pre_rec <- residual_pre_rec_obtain(data_brmm_best_resids, data_man_trim)
    data_rasc_pre_rec <- residual_pre_rec_obtain(data_rasc_best_resids, data_man_trim)
    # Combining precision recall data into one tibble
    full_pre_rec_data <- bind_rows(data_incl_pre_rec %>% mutate(element = "inclination"),
                                   data_ecce_pre_rec %>% mutate(element = "eccentricity"),
                                   data_argp_pre_rec %>% mutate(element = "argument_of_perigee"),
                                   data_anom_pre_rec %>% mutate(element = "mean_anomaly"),
                                   data_brmm_pre_rec %>% mutate(element = "brouwer_mean_motion"),
                                   data_rasc_pre_rec %>% mutate(element = "right_ascension"))
    # Summarising precision recall metrics and selecting the best f1-score for all elements
    f1_results <- full_pre_rec_data %>% group_by(element) %>% arrange(-f_score) %>% slice(1) %>%
      ungroup() %>% arrange(-f_score) %>% select(-c(threshold, no_mans)) %>%
      rename("f1_score"="f_score") %>% select(f1_score, element, everything())
    
    # Writing data to xlsx file since it has not been created
    write_to_xlsx(full_pre_rec_data, name = file_name_of_pre_rec_data)
    write_to_xlsx(f1_results, name = file_name_of_f1_score_data)
    
  } else {
    cat("Using existing precision recall data...\n")
    full_pre_rec_data <- pre_rec_data_import
    f1_results <- f1_score_data_import
  }
  
  # Returning the desired output
  if (output == "pre_rec") {
    return (full_pre_rec_data)
  } else if (output == "f1") {
    return (f1_results)
  }
  
}



# Creating vectors of chosen satellite names and abbreviations
satellite_names <- c("cs2","fy2d","js3","srl","stl3b")
satellite_abbrevs <- c("CryoSat-2","FengYun 2D","Jason-3","SARAL","Sentinel-3B")


# Function to plot a basic precision recall curve based on one set of data
pre_rec_plot_basic <- function(pre_rec_data) {
  pre_rec_data %>%
    na.omit() %>%
    ggplot(aes(x=recall, y=precision)) +
    geom_line()
}


# Function to plot two precision recall curves from ARIMA and ARIMAX models against each other
pre_rec_plot_both <- function(satellite_name) {
  
  # Determining full satellite name and plot title from input data
  index <- which(satellite_names == satellite_name)
  full_satellite_name <- satellite_abbrevs[index]
  plot_title <- paste(full_satellite_name, ": Precision Recall Curves of Best Models", sep="")
  
  # Obtaining precision recall data of the two methods
  data_arima <- get(paste(satellite_name,"_pre_rec_basic",sep=""))
  data_arimax <- get(paste(satellite_name,"_pre_rec_x",sep=""))
  
  # Creating a plot of each model's precision recall curves
  final_plot <- bind_rows(data_arima %>% mutate(model="ARIMA"), 
                          data_arimax %>% mutate(model="ARIMAX")) %>%
    na.omit() %>%
    ggplot(aes(x=recall, y=precision, col=model)) +
    geom_line(linewidth = 1) +
    labs(title = plot_title, col = "Model",
         x = "Recall", y = "Precision") +
    theme(legend.position = "bottom",
          legend.text = element_text(size=16),
          text = element_text(family="serif", size=20))
  
  # Setting the plot's filename to save
  plot_filename <- paste(str_to_upper(satellite_name), "Pre Rec plot - Best models.png")
  
  # Setting directory to exported plots location
  setwd(exported_plots_wd)
  # Checking if the plot exists in order to know whether to export it
  Files <- list.files(pattern = "png$")
  INFO <- file.info(Files)
  INFO <- INFO[grepl(plot_filename, rownames(INFO)), ]
  # Saving the final plot as a png file
  if (nrow(INFO) == 0) {
    cat("Exporting plot\n")
    ggsave(filename = plot_filename, plot = final_plot, width=1920/180, height=1440/180, dpi=180)
  } 
  
  # Returning the final plot
  return (final_plot)
  
}


# Function to plot two precision recall curves from ARIMA and ARIMAX models against each other
pre_rec_plot_three <- function(satellite_name) {
  
  # Determining full satellite name and plot title from input data
  index <- which(satellite_names == satellite_name)
  full_satellite_name <- satellite_abbrevs[index]
  plot_title <- paste(full_satellite_name, ": Precision Recall Curves of Best Models", sep="")
  
  # Obtaining precision recall data of all three methods
  data_arima <- get(paste(satellite_name,"_pre_rec_basic",sep=""))
  data_arimax <- get(paste(satellite_name,"_pre_rec_x",sep=""))
  data_xgboost <- get(paste(satellite_name,"_pre_rec_xgboost",sep=""))
  
  # Creating a plot of each model's precision recall curves
  final_plot <- bind_rows(data_arima %>% mutate(model="ARIMA"), 
                          data_arimax %>% mutate(model="ARIMAX"), 
                          data_xgboost %>% mutate(model="XGBoost")) %>%
    na.omit() %>%
    ggplot(aes(x=recall, y=precision, col=model)) +
    geom_line(linewidth = 1) +
    labs(title = plot_title, col = "Model",
         x = "Recall", y = "Precision") +
    theme(legend.position = "bottom",
          legend.text = element_text(size=16),
          text = element_text(family="serif", size=20))
  
  # Setting the plot's filename to save
  plot_filename <- paste(str_to_upper(satellite_name), "Pre Rec plot - Best models New.png")
  
  # Setting directory to exported plots location
  setwd(exported_plots_wd)
  # Checking if the plot exists in order to know whether to export it
  Files <- list.files(pattern = "png$")
  INFO <- file.info(Files)
  INFO <- INFO[grepl(plot_filename, rownames(INFO)), ]
  # Saving the final plot as a png file
  if (nrow(INFO) == 0) {
    cat("Exporting plot\n")
    ggsave(filename = plot_filename, plot = final_plot, width=1920/180, height=1440/180, dpi=180)
  } 
  
  # Returning the final plot
  return (final_plot)
  
}


# Function to plot precision recall curves of all 6 elements for a given satellite and ARIMA model
pre_rec_plot_6_elements <- function(satellite_name, model) {
  
  # Determining full satellite name, model name, and plot title from input data
  index <- which(satellite_names == satellite_name)
  full_satellite_name <- satellite_abbrevs[index]
  if (model=="arima") {full_model_name <- "ARIMA"} 
  else if (model=="arimax") {full_model_name <- "ARIMAX"}
  else if (model=="xgboost") {full_model_name <- "XGBoost"}
  plot_title <- paste(full_satellite_name, "with", full_model_name)
  
  # Obtaining precision recall data of all 6 elements
  data_to_plot <- get(paste(satellite_name,"_6_element_pre_rec_",model, sep=""))
  
  # Creating a plot of each element's precision recall curves
  final_plot <- data_to_plot %>%
    na.omit() %>%
    mutate(element = str_to_title(gsub("_"," ",element)) %>% str_replace_all('Brouwer ', "") %>% str_replace_all(' Of ', " of ")) %>%
    ggplot(aes(x=recall, y=precision, col=element)) +
    geom_line(linewidth = 1) +
    labs(title = plot_title, col = "Element",
         x = "Recall", y = "Precision") +
    theme(legend.position = "bottom",
          legend.text = element_text(size=16),
          text = element_text(family="serif", size=20))
  
  # Setting the plot's filename to save
  plot_filename <- paste(str_to_upper(satellite_name), str_to_upper(model), "Pre Rec plot - 6 Elements.png")
  
  # Setting directory to exported plots location
  setwd(exported_plots_wd)
  # Checking if the plot exists in order to know whether to export it
  Files <- list.files(pattern = "png$")
  INFO <- file.info(Files)
  INFO <- INFO[grepl(plot_filename, rownames(INFO)), ]
  # Saving the final plot as a png file
  if (nrow(INFO) == 0) {
    cat("Exporting plot\n")
    ggsave(filename = plot_filename, plot = final_plot, width=1920/180, height=1440/180, dpi=180)
  } 
  
  # Returning the final plot
  return (final_plot)
  
}


# Function to plot best f-scores against beta per model for a chosen satellite
f_score_plot_three <- function(satellite_name) {
  
  # Creating vectors of chosen satellite names and abbreviations
  satellite_names <- c("cs2","fy2d","js3","srl","stl3b")
  satellite_abbrevs <- c("CryoSat-2","FengYun 2D","Jason-3","SARAL","Sentinel-3B")
  
  # Determining full satellite name and plot title from input data
  index <- which(satellite_names == satellite_name)
  full_satellite_name <- satellite_abbrevs[index]
  plot_title <- paste(full_satellite_name, ": F-Scores of all three models", sep="")
  
  # Obtaining precision recall data of all three methods
  data_arima <- get(paste(satellite_name,"_pre_rec_basic",sep=""))
  data_arimax <- get(paste(satellite_name,"_pre_rec_x",sep=""))
  data_xgboost <- get(paste(satellite_name,"_pre_rec_xgboost",sep=""))
  
  # Calculating best f0.5-scores for each method
  f0.5_results <- bind_rows(
    data_arima %>% mutate(model = "ARIMA"),
    data_arimax %>% mutate(model = "ARIMAX"),
    data_xgboost %>% mutate(model = "XGBoost")) %>% group_by(model) %>% 
    arrange(-f_0.5) %>% slice(1) %>% select(-c(threshold, no_mans, f_score)) %>% mutate(beta = "0.5") %>% 
    rename("f_score"="f_0.5") %>% select(model, beta, f_score, precision, recall) %>% ungroup()
  
  # Calculating best f1-scores for each method
  f1_results <- bind_rows(
    data_arima %>% mutate(model = "ARIMA"),
    data_arimax %>% mutate(model = "ARIMAX"),
    data_xgboost %>% mutate(model = "XGBoost")) %>% group_by(model) %>% 
    arrange(-f_score) %>% slice(1) %>% select(-c(threshold, no_mans)) %>% mutate(beta = "1") %>% 
    select(model, beta, f_score, precision, recall) %>% ungroup()
  
  # Calculating best f2-scores for each method
  f2_results <- bind_rows(
    data_arima %>% mutate(model = "ARIMA"),
    data_arimax %>% mutate(model = "ARIMAX"),
    data_xgboost %>% mutate(model = "XGBoost")) %>% group_by(model) %>% 
    arrange(-f_2) %>% slice(1) %>% select(-c(threshold, no_mans, f_score)) %>% mutate(beta = "2") %>% 
    rename("f_score"="f_2") %>% select(model, beta, f_score, precision, recall) %>% ungroup()
  
  # Printing a tibble of the best f-scores
  print(bind_rows(f0.5_results, f1_results, f2_results))
  
  # Creating a plot of best f-scores against each beta value, grouped by model
  final_plot <- bind_rows(f0.5_results, f1_results, f2_results) %>%
    ggplot(aes(x=beta, y=f_score, col=model, group=model)) +
    geom_point(size = 5) + geom_line(aes(group = rev(model)), linewidth=1, linetype="dashed") +
    labs(title = plot_title, col = "Model",
         x = "Beta value", y = "F-score") +
    theme(legend.position = "bottom",
          legend.text = element_text(size=16),
          text = element_text(family="serif", size=20))
  
  # Returning the final plot
  return (final_plot)
  
}



# Quantitative results for all methods ----------------------------------------------------------------------------------------------------------------
# Calculating top f1-scores for each model and satellite
cs2_best_model_arima <- arima_best_prerec_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", type="null", output="f1")
cs2_best_model_arimax <- arima_best_prerec_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", type="best", output="f1")
cs2_best_model_xgboost <- xgboost_best_prerec_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", type="basic", output="f1")
fy2d_best_model_arima <- arima_best_prerec_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", type="null", output="f1")
fy2d_best_model_arimax <- arima_best_prerec_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", type="best", output="f1")
fy2d_best_model_xgboost <- xgboost_best_prerec_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", type="basic", output="f1")
js3_best_model_arima <- arima_best_prerec_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", type="null", output="f1")
js3_best_model_arimax <- arima_best_prerec_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", type="best", output="f1")
js3_best_model_xgboost <- xgboost_best_prerec_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", type="basic", output="f1")
srl_best_model_arima <- arima_best_prerec_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", type="null", output="f1")
srl_best_model_arimax <- arima_best_prerec_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", type="best", output="f1")
srl_best_model_xgboost <- xgboost_best_prerec_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", type="basic", output="f1")
stl3b_best_model_arima <- arima_best_prerec_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", type="null", output="f1")
stl3b_best_model_arimax <- arima_best_prerec_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", type="best", output="f1")
stl3b_best_model_xgboost <- xgboost_best_prerec_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", type="basic", output="f1")

# Calculating precision recall metrics of all 6 elements of each satellite and arima model
cs2_6_element_pre_rec_arima <- arima_best_prerec_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", type="null", output="pre_rec")
cs2_6_element_pre_rec_arimax <- arima_best_prerec_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", type="best", output="pre_rec")
cs2_6_element_pre_rec_xgboost <- xgboost_best_prerec_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", type="basic", output="pre_rec")
fy2d_6_element_pre_rec_arima <- arima_best_prerec_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", type="null", output="pre_rec")
fy2d_6_element_pre_rec_arimax <- arima_best_prerec_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", type="best", output="pre_rec")
fy2d_6_element_pre_rec_xgboost <- xgboost_best_prerec_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", type="basic", output="pre_rec")
js3_6_element_pre_rec_arima <- arima_best_prerec_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", type="null", output="pre_rec")
js3_6_element_pre_rec_arimax <- arima_best_prerec_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", type="best", output="pre_rec")
js3_6_element_pre_rec_xgboost <- xgboost_best_prerec_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", type="basic", output="pre_rec")
srl_6_element_pre_rec_arima <- arima_best_prerec_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", type="null", output="pre_rec")
srl_6_element_pre_rec_arimax <- arima_best_prerec_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", type="best", output="pre_rec")
srl_6_element_pre_rec_xgboost <- xgboost_best_prerec_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", type="basic", output="pre_rec")
stl3b_6_element_pre_rec_arima <- arima_best_prerec_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", type="null", output="pre_rec")
stl3b_6_element_pre_rec_arimax <- arima_best_prerec_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", type="best", output="pre_rec")
stl3b_6_element_pre_rec_xgboost <- xgboost_best_prerec_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", type="basic", output="pre_rec")


# Obtaining data of best models for each satellite for ARIMA
cs2_best_element_arima <- read_from_xlsx("cs2_best_model_arima")[1,2] %>% as.character()
fy2d_best_element_arima <- read_from_xlsx("fy2d_best_model_arima")[1,2] %>% as.character()
js3_best_element_arima <- read_from_xlsx("js3_best_model_arima")[1,2] %>% as.character()
srl_best_element_arima <- read_from_xlsx("srl_best_model_arima")[1,2] %>% as.character()
stl3b_best_element_arima <- read_from_xlsx("stl3b_best_model_arima")[1,2] %>% as.character()

cs2_basic_arima_resids <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", cs2_best_element_arima, NULL, "residuals")
fy2d_basic_arima_resids <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", fy2d_best_element_arima, NULL, "residuals")
js3_basic_arima_resids <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", js3_best_element_arima, NULL, "residuals")
srl_basic_arima_resids <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", srl_best_element_arima, NULL, "residuals")
stl3b_basic_arima_resids <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", stl3b_best_element_arima, NULL, "residuals")


# Obtaining data of best models for each satellite for ARIMAX
cs2_best_element_arimax <- read_from_xlsx("cs2_best_model_arimax")[1,2] %>% as.character()
fy2d_best_element_arimax <- read_from_xlsx("fy2d_best_model_arimax")[1,2] %>% as.character()
js3_best_element_arimax <- read_from_xlsx("js3_best_model_arimax")[1,2] %>% as.character()
srl_best_element_arimax <- read_from_xlsx("srl_best_model_arimax")[1,2] %>% as.character()
stl3b_best_element_arimax <- read_from_xlsx("stl3b_best_model_arimax")[1,2] %>% as.character()

cs2_arimax_best_xreg <- best_arima_model(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", cs2_best_element_arimax, "reg")
fy2d_arimax_best_xreg <- best_arima_model(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", fy2d_best_element_arimax, "reg")
js3_arimax_best_xreg <- best_arima_model(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", js3_best_element_arimax, "reg")
srl_arimax_best_xreg <- best_arima_model(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", srl_best_element_arimax, "reg")
stl3b_arimax_best_xreg <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", stl3b_best_element_arimax, "reg")

cs2_x_arima_resids <- arima_modelling_extra(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", cs2_best_element_arimax, cs2_arimax_best_xreg, "residuals")
fy2d_x_arima_resids <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", fy2d_best_element_arimax, fy2d_arimax_best_xreg, "residuals")
js3_x_arima_resids <- arima_modelling_extra(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", js3_best_element_arimax, js3_arimax_best_xreg, "residuals")
srl_x_arima_resids <- arima_modelling_extra(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", srl_best_element_arimax, srl_arimax_best_xreg, "residuals")
stl3b_x_arima_resids <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", stl3b_best_element_arimax, stl3b_arimax_best_xreg, "residuals")


# Obtaining data of best models for each satellite for XGBoost
cs2_xgboost_best_model <- xgboost_residuals_get(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "brouwer_mean_motion", output="model")
fy2d_xgboost_best_model <- xgboost_residuals_get(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion", output="model")
js3_xgboost_best_model <- xgboost_residuals_get(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "brouwer_mean_motion", output="model")
srl_xgboost_best_model <- xgboost_residuals_get(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "brouwer_mean_motion", output="model")
stl3b_xgboost_best_model <- xgboost_residuals_get(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion", output="model")

cs2_xgboost_resids <- xgboost_residuals_get(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "brouwer_mean_motion")
fy2d_xgboost_resids <- xgboost_residuals_get(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion")
js3_xgboost_resids <- xgboost_residuals_get(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "brouwer_mean_motion")
srl_xgboost_resids <- xgboost_residuals_get(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "brouwer_mean_motion")
stl3b_xgboost_resids <- xgboost_residuals_get(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion")

cs2_xgboost_resid_plot <- residual_man_plot_xgb(cs2_xgboost_resids %>% mutate(Residuals = scale(Residuals)), cs2_man, "2016-01-01", "2020-01-01")
fy2d_xgboost_resid_plot <- residual_man_plot_xgb(fy2d_xgboost_resids %>% mutate(Residuals = scale(Residuals)), fy2d_man, "2012-01-01", "2015-01-01")
js3_xgboost_resid_plot <- residual_man_plot_xgb(js3_xgboost_resids %>% mutate(Residuals = scale(Residuals)), js3_man, "2018-01-01", "2022-01-01")
srl_xgboost_resid_plot <- residual_man_plot_xgb(srl_xgboost_resids %>% mutate(Residuals = scale(Residuals)), srl_man, "2016-08-01", "2022-08-01")
stl3b_xgboost_resid_plot <- residual_man_plot_xgb(stl3b_xgboost_resids %>% mutate(Residuals = scale(Residuals)), stl3b_man, "2019-01-01", "2022-01-01")

# Exporting the best model parameters for future use to speed up the training process
xgboost_best_model_params <- bind_rows(
  cs2_xgboost_best_model$bestTune %>% as_tibble() %>% mutate(satellite="cs2",element="brouwer_mean_motion"),
  fy2d_xgboost_best_model$bestTune %>% as_tibble() %>% mutate(satellite="fy2d",element="brouwer_mean_motion"),
  js3_xgboost_best_model$bestTune %>% as_tibble() %>% mutate(satellite="js3",element="brouwer_mean_motion"),
  srl_xgboost_best_model$bestTune %>% as_tibble() %>% mutate(satellite="srl",element="brouwer_mean_motion"),
  stl3b_xgboost_best_model$bestTune %>% as_tibble() %>% mutate(satellite="stl3b",element="brouwer_mean_motion"))
write_to_xlsx(xgboost_best_model_params)


# Obtaining data of best models for each satellite for XGBoost with extra predictors
cs2_xgboost_extra_resids <- xgboost_extra_residuals_get(cs2, cs2_man, "2016-01-01", "2020-01-01", "2019-02-11", "brouwer_mean_motion")
fy2d_xgboost_extra_resids <- xgboost_extra_residuals_get(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion")
js3_xgboost_extra_resids <- xgboost_extra_residuals_get(js3, js3_man, "2018-01-01", "2022-01-01", "2021-02-11", "brouwer_mean_motion")
srl_xgboost_extra_resids <- xgboost_extra_residuals_get(srl, srl_man, "2016-08-01", "2022-08-01", "2021-05-18", "brouwer_mean_motion")
stl3b_xgboost_extra_resids <- xgboost_extra_residuals_get(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion")

cs2_xgboost_extra_resid_plot <- residual_man_plot_xgb(cs2_xgboost_extra_resids %>% mutate(Residuals = scale(Residuals)), cs2_man, "2016-01-01", "2020-01-01")
fy2d_xgboost_extra_resid_plot <- residual_man_plot_xgb(fy2d_xgboost_extra_resids %>% mutate(Residuals = scale(Residuals)), fy2d_man, "2012-01-01", "2015-01-01")
js3_xgboost_extra_resid_plot <- residual_man_plot_xgb(js3_xgboost_extra_resids %>% mutate(Residuals = scale(Residuals)), js3_man, "2018-01-01", "2022-01-01")
srl_xgboost_extra_resid_plot <- residual_man_plot_xgb(srl_xgboost_extra_resids %>% mutate(Residuals = scale(Residuals)), srl_man, "2016-08-01", "2022-08-01")
stl3b_xgboost_extra_resid_plot <- residual_man_plot_xgb(stl3b_xgboost_extra_resids %>% mutate(Residuals = scale(Residuals)), stl3b_man, "2019-01-01", "2022-01-01")


# Trimming the manoeuvre data to the specified date ranges
cs2_man_trim <- cs2_man %>% filter(man_beg >= as.Date("2016-01-01") & man_beg <= as.Date("2020-01-01"))
fy2d_man_trim <- fy2d_man %>% filter(man_beg >= as.Date("2012-01-01") & man_beg <= as.Date("2015-01-01")) %>% arrange(man_beg)
js3_man_trim <- js3_man %>% filter(man_beg >= as.Date("2018-01-01") & man_beg <= as.Date("2022-01-01"))
srl_man_trim <- srl_man %>% filter(man_beg >= as.Date("2016-08-01") & man_beg <= as.Date("2022-08-01"))
stl3b_man_trim <- stl3b_man %>% filter(man_beg >= as.Date("2019-01-01") & man_beg <= as.Date("2022-01-01"))

# Calculating precision and recall for basic ARIMA models
cs2_pre_rec_basic <- residual_pre_rec_obtain(cs2_basic_arima_resids, cs2_man_trim)
fy2d_pre_rec_basic <- residual_pre_rec_obtain(fy2d_basic_arima_resids, fy2d_man_trim)
js3_pre_rec_basic <- residual_pre_rec_obtain(js3_basic_arima_resids, js3_man_trim)
srl_pre_rec_basic <- residual_pre_rec_obtain(srl_basic_arima_resids, srl_man_trim)
stl3b_pre_rec_basic <- residual_pre_rec_obtain(stl3b_basic_arima_resids, stl3b_man_trim)

# Calculating precision and recall for ARIMAX models
cs2_pre_rec_x <- residual_pre_rec_obtain(cs2_x_arima_resids, cs2_man_trim)
fy2d_pre_rec_x <- residual_pre_rec_obtain(fy2d_x_arima_resids, fy2d_man_trim)
js3_pre_rec_x <- residual_pre_rec_obtain(js3_x_arima_resids, js3_man_trim)
srl_pre_rec_x <- residual_pre_rec_obtain(srl_x_arima_resids, srl_man_trim)
stl3b_pre_rec_x <- residual_pre_rec_obtain(stl3b_x_arima_resids, stl3b_man_trim)

# Calculating precision and recall for XGBoost models
cs2_pre_rec_xgboost <- residual_pre_rec_obtain(cs2_xgboost_resids, cs2_man_trim)
fy2d_pre_rec_xgboost <- residual_pre_rec_obtain(fy2d_xgboost_resids, fy2d_man_trim)
js3_pre_rec_xgboost <- residual_pre_rec_obtain(js3_xgboost_resids, js3_man_trim)
srl_pre_rec_xgboost <- residual_pre_rec_obtain(srl_xgboost_resids, srl_man_trim)
stl3b_pre_rec_xgboost <- residual_pre_rec_obtain(stl3b_xgboost_resids, stl3b_man_trim)

# Calculating precision and recall for XGBoost models with extra predictors
cs2_pre_rec_xgboost_extra <- residual_pre_rec_obtain(cs2_xgboost_extra_resids, cs2_man_trim)
fy2d_pre_rec_xgboost_extra <- residual_pre_rec_obtain(fy2d_xgboost_extra_resids, fy2d_man_trim)
js3_pre_rec_xgboost_extra <- residual_pre_rec_obtain(js3_xgboost_extra_resids, js3_man_trim)
srl_pre_rec_xgboost_extra <- residual_pre_rec_obtain(srl_xgboost_extra_resids, srl_man_trim)
stl3b_pre_rec_xgboost_extra <- residual_pre_rec_obtain(stl3b_xgboost_extra_resids, stl3b_man_trim)


# Summarising the best f1-scores of all methods' best models
bind_rows(
  cs2_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="cs2",element="brouwer_mean_motion"),
  fy2d_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="fy2d",element="brouwer_mean_motion"),
  js3_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="js3",element="brouwer_mean_motion"),
  srl_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="srl",element="brouwer_mean_motion"),
  stl3b_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="stl3b",element="brouwer_mean_motion"))

bind_rows(
  cs2_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="cs2",element="brouwer_mean_motion"),
  fy2d_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="fy2d",element="brouwer_mean_motion"),
  js3_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="js3",element="brouwer_mean_motion"),
  srl_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="srl",element="brouwer_mean_motion"),
  stl3b_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="stl3b",element="inclination"))

bind_rows(
  cs2_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="cs2",element="brouwer_mean_motion"),
  fy2d_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="fy2d",element="brouwer_mean_motion"),
  js3_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="js3",element="brouwer_mean_motion"),
  srl_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="srl",element="brouwer_mean_motion"),
  stl3b_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="stl3b",element="brouwer_mean_motion"))

bind_rows(
  cs2_pre_rec_xgboost_extra %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="cs2",element="brouwer_mean_motion"),
  fy2d_pre_rec_xgboost_extra %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="fy2d",element="brouwer_mean_motion"),
  js3_pre_rec_xgboost_extra %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="js3",element="brouwer_mean_motion"),
  srl_pre_rec_xgboost_extra %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="srl",element="brouwer_mean_motion"),
  stl3b_pre_rec_xgboost_extra %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite="stl3b",element="brouwer_mean_motion"))


# Creating plots of all three methods' best models' precision recall curves
pre_rec_plot_three("cs2")
pre_rec_plot_three("fy2d")
pre_rec_plot_three("js3")
pre_rec_plot_three("srl")
pre_rec_plot_three("stl3b")

# Creating plots of all three methods' precision recall curves for all elements
pre_rec_plot_6_elements("cs2","arima")
pre_rec_plot_6_elements("fy2d","arima")
pre_rec_plot_6_elements("js3","arima")
pre_rec_plot_6_elements("srl","arima")
pre_rec_plot_6_elements("stl3b","arima")

pre_rec_plot_6_elements("cs2","arimax")
pre_rec_plot_6_elements("fy2d","arimax")
pre_rec_plot_6_elements("js3","arimax")
pre_rec_plot_6_elements("srl","arimax")
pre_rec_plot_6_elements("stl3b","arimax")

pre_rec_plot_6_elements("cs2","xgboost")
pre_rec_plot_6_elements("fy2d","xgboost")
pre_rec_plot_6_elements("js3","xgboost")
pre_rec_plot_6_elements("srl","xgboost")
pre_rec_plot_6_elements("stl3b","xgboost")



# Final plots -----------------------------------------------------------------------------------------------------------------------------------------
# Figure 3.1
raw_outlier_final_plots("stl3b", "2019-01-01", "2022-01-01", "eccentricity") +
  plot_layout(axis_titles = "collect") & 
  scale_y_continuous(labels = function(x)x*10000) &
  labs(y = expression("Orbit Shape " ~ "\u00D7" ~ 10^5)) &
  theme(text = element_text(family="serif", size=24),
        title = element_text(size=20),
        axis.title = element_text(size=24))



# Figure 3.2
six_plots_3.2 <- function(dataset) {
  p1 <- dataset %>% ggplot(aes(x=Date, y=inclination)) + 
    geom_line(linewidth = 1) + labs(title="Inclination", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  p2 <- dataset %>% mutate(eccentricity = eccentricity*10000) %>% 
    ggplot(aes(x=Date, y=eccentricity)) + 
    geom_line(linewidth = 1) + labs(title="Eccentricity", y = "Orbit Shape") + 
    annotate(
      "text", 
      x = as.POSIXct("2019-01-01"),
      y = Inf,
      label = expression("\u00D7" ~ 10^5),
      hjust = 1.4,
      vjust = -0.4,
      size = 7,
      family = "serif"
    ) + coord_cartesian(clip = "off") + theme(text = element_text(family="serif", size=17))
  p3 <- dataset %>% ggplot(aes(x=Date, y=argument_of_perigee)) + 
    geom_line(linewidth = 1) + labs(title="Argument of Perigee", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  p4 <- dataset %>% ggplot(aes(x=Date, y=mean_anomaly)) + 
    geom_line(linewidth = 1) + labs(title="Mean Anomaly", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  p5 <- dataset %>% mutate(brouwer_mean_motion = scale(brouwer_mean_motion)) %>% 
    ggplot(aes(x=Date, y=brouwer_mean_motion)) + 
    geom_line(linewidth = 1) + labs(title="Mean Motion", y="Radians per Epoch") + 
    annotate(
      "text", 
      x = as.POSIXct("2019-01-01"),
      y = Inf,
      label = expression(frac(y - mu, sigma)),
      hjust = 1.5,
      vjust = 0.5,
      size = 6,
      family = "serif"
    ) + coord_cartesian(clip = "off") + theme(text = element_text(family="serif", size=17))
  p6 <- dataset %>% ggplot(aes(x=Date, y=right_ascension)) + 
    geom_line(linewidth = 1) + labs(title="Right Ascension", y="Radians") + 
    theme(text = element_text(family="serif", size=17))
  
  # Returning all 6 plots patched together using patchwork
  return ((p1 + p2 + p3) / (p4 + p5 + p6))
  # return (p2)
}

six_plots_3.2(stl3b %>% filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2022-01-01"))) &
  theme(text = element_text(family="serif", size=31),
        title = element_text(size=25),
        axis.title = element_text(size=28))



# Figure 3.3
stl3b %>% filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2022-01-01")) %>%
  mutate(brouwer_mean_motion = scale(brouwer_mean_motion)) %>% 
  ggplot(aes(x = Date, y = brouwer_mean_motion)) +
  geom_line(aes(color = "Radians per Epoch"), linewidth = 1) + 
  geom_vline(data = stl3b_man %>% filter(man_beg >= as.Date("2019-01-01") & man_beg <= as.Date("2022-01-01")), 
             aes(xintercept = man_beg, color = "Manoeuvre"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(name = "Legend",
                     values = c("Radians per Epoch" = "black", "Manoeuvre" = "red")) +
  labs(title = "Mean motion overlayed with ground-truth manoeuvres",
       y = NULL) + 
  annotate(
    "text", 
    x = as.POSIXct("2019-01-01"),
    y = Inf,
    label = "Std.",
    hjust = 1.2,
    vjust = -2.9,
    angle = 90,
    size = 9,
    family = "serif",
    colour = "grey25"
  ) + coord_cartesian(clip = "off") +
  theme(legend.position = "bottom",
        text = element_text(family="serif", size=31),
        title = element_text(size=25),
        axis.title = element_text(size=28),
        legend.text = element_text(size=28),
        legend.title = element_text(size=31),
        plot.margin = margin(5.5,5.5,5.5,5.5))



# Figure 3.5
df_cv <- data.frame(
  x_min = c(rep(0, 10), seq(8, 100, by = 10)),
  x_max = c(seq(8, 98, by = 10), seq(10, 100, by = 10)),
  y = rep(1:10, 2),
  color = c(rep("Training", 10), rep("Validation", 10))
)

df_cv$xmin <- df_cv$x_min
df_cv$xmax <- df_cv$x_max
df_cv$ymin <- df_cv$y - 0.5
df_cv$ymax <- df_cv$y + 0.5

ggplot(df_cv, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color)) +
  geom_rect(col="black") +
  scale_fill_manual(values = c("Training" = "#00BFC4", "Validation" = "#F8766D")) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(limits = c(0.5, 10.5), breaks = 1:10) +
  labs(x = "Percentage of Training Set (%)",
       y = "Fold Number",
       fill = "Data Split") +
  theme(legend.position = "bottom",
        text = element_text(family="serif", size=31),
        title = element_text(size=25),
        axis.title = element_text(size=28),
        legend.text = element_text(size=28),
        legend.title = element_text(size=30))



# Figure 4.1
js3_xgboost_results_plot_1 <- js3_xgboost_resids %>%
  filter(Date >= as.Date("2021-04-01") & Date <= as.Date("2021-09-01")) %>%
  ggplot(aes(x=Date, y=element_new)) +
  geom_line(aes(color="True Values"), linewidth = 1) + geom_line(aes(y=Prediction, color="Predictions"), linewidth = 1) +
  scale_color_manual(name = "Legend",
                     values = c("True Values" = "royalblue", "Predictions" = "hotpink1"),
                     breaks = c("True Values", "Predictions")) +
  labs(title = "Jason-3: XGBoost on Mean Motion", y = NULL) +
  theme(legend.position = "bottom")

js3_xgboost_results_plot_2 <- residual_man_plot_xgb(
  js3_xgboost_resids %>% filter(Date >= as.Date("2021-04-01") & Date <= as.Date("2021-09-01")), 
  js3_man, "2018-01-01", "2022-01-01") +
  labs(title = NULL) +
  scale_color_manual(name = NULL,
                     values = c("Residuals" = "black", "Manoeuvre" = "red"),
                     breaks = c("Residuals", "Manoeuvre")) +
  theme(legend.position = "bottom")

(js3_xgboost_results_plot_1/js3_xgboost_results_plot_2) + 
  plot_layout(guides = "collect", axis_titles = "collect") & 
  theme(legend.position = "bottom",
        legend.text = element_text(size=20),
        text = element_text(family="serif", size=23),
        title = element_text(size=20),
        axis.title.x = element_text(size=22),
        legend.title = element_text(size=22))



# Figure 4.2
fy2d_incl_basic_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "inclination", NULL, "plot")
fy2d_ecce_basic_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "eccentricity", NULL, "plot")
fy2d_argp_basic_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "argument_of_perigee", NULL, "plot")
fy2d_anom_basic_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "mean_anomaly", NULL, "plot")
fy2d_brmm_basic_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "brouwer_mean_motion", NULL, "plot")
fy2d_rasc_basic_plot <- arima_modelling_extra(fy2d, fy2d_man, "2012-01-01", "2015-01-01", "2014-04-24", "right_ascension", NULL, "plot")

(fy2d_incl_basic_plot + fy2d_ecce_basic_plot) / 
  (fy2d_argp_basic_plot + fy2d_anom_basic_plot) / 
  (fy2d_brmm_basic_plot + fy2d_rasc_basic_plot) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')

((fy2d_ecce_basic_plot + scale_x_date(limits = c(as.Date("2013-06-01"), as.Date("2015-01-01")))) / 
    (fy2d_rasc_basic_plot + scale_x_date(limits = c(as.Date("2013-06-01"), as.Date("2015-01-01")))) / 
    (fy2d_brmm_basic_plot + scale_x_date(limits = c(as.Date("2013-06-01"), as.Date("2015-01-01"))))) + 
  plot_layout(guides = "collect", axis_titles = "collect") & 
  theme(legend.position = "bottom",
        legend.text = element_text(size=23),
        text = element_text(family="serif", size=26),
        title = element_text(size=23),
        axis.title.x = element_text(size=25),
        legend.title = element_text(size=25))



# Figure 4.3
stl3b_brmm_best <- best_arima_model(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion", "reg")
stl3b_brmm_best_plot <- arima_modelling_extra(stl3b, stl3b_man, "2019-01-01", "2022-01-01", "2021-04-24", "brouwer_mean_motion", stl3b_brmm_best, "plot")

stl3b_brmm_best_plot + scale_x_date(limits = c(as.Date("2020-07-01"), as.Date("2022-01-01"))) & 
  theme(legend.position = "bottom",
        legend.text = element_text(size=23),
        text = element_text(family="serif", size=26),
        title = element_text(size=23),
        plot.subtitle = element_text(size=20),
        axis.title.x = element_text(size=25),
        legend.title = element_text(size=25))



# Figure 4.4
cs2_pre_rec_plot_arima <- pre_rec_plot_6_elements("cs2","arima")
stl3b_pre_rec_plot_arimax <- pre_rec_plot_6_elements("stl3b","arimax")

(cs2_pre_rec_plot_arima + stl3b_pre_rec_plot_arimax) + 
  plot_layout(guides = "collect", axis_titles = "collect") & theme(legend.position = "bottom") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=18),
        text = element_text(family="serif", size=22),
        title = element_text(size=19),
        axis.title.x = element_text(size=22),
        legend.title = element_text(size=21))



# Final tables ----------------------------------------------------------------------------------------------------------------------------------------
# Table 3.1
# Getting number of manoeuvres per the 5 satellites
tibble(
  satellite = c("cs2","fy2d","js3","srl","stl3b"),
  manoeuvres = c(
    cs2_man %>% filter(man_beg >= as.Date("2016-01-01") & man_beg <= as.Date("2020-01-01")) %>% count() %>% as.numeric(),
    fy2d_man %>% filter(man_beg >= as.Date("2012-01-01") & man_beg <= as.Date("2015-01-01")) %>% count() %>% as.numeric(),
    js3_man %>% filter(man_beg >= as.Date("2018-01-01") & man_beg <= as.Date("2022-01-01")) %>% count() %>% as.numeric(),
    srl_man %>% filter(man_beg >= as.Date("2016-08-01") & man_beg <= as.Date("2022-08-01")) %>% count() %>% as.numeric(),
    stl3b_man %>% filter(man_beg >= as.Date("2019-01-01") & man_beg <= as.Date("2022-01-01")) %>% count() %>% as.numeric())
)



# Table 4.1
bind_rows(
  cs2_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMA"),
  cs2_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMAX"),
  cs2_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "cs2", model = "XGBoost"),
  fy2d_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMA"),
  fy2d_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMAX"),
  fy2d_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "fy2d", model = "XGBoost"),
  js3_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMA"),
  js3_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMAX"),
  js3_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "js3", model = "XGBoost"),
  srl_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMA"),
  srl_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMAX"),
  srl_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "srl", model = "XGBoost"),
  stl3b_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMA"),
  stl3b_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMAX"),
  stl3b_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "stl3b", model = "XGBoost")) %>%
  arrange(model) %>% arrange(satellite) %>% select(c(satellite, model, f_score)) %>%
  pivot_wider(names_from = model, values_from = f_score)



# Table 4.2
# Obtaining the best f0.5-score per satellite and method
f0.5_results_final <- bind_rows(
  cs2_pre_rec_basic %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMA"),
  cs2_pre_rec_x %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMAX"),
  cs2_pre_rec_xgboost %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "cs2", model = "XGBoost"),
  fy2d_pre_rec_basic %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMA"),
  fy2d_pre_rec_x %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMAX"),
  fy2d_pre_rec_xgboost %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "fy2d", model = "XGBoost"),
  js3_pre_rec_basic %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMA"),
  js3_pre_rec_x %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMAX"),
  js3_pre_rec_xgboost %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "js3", model = "XGBoost"),
  srl_pre_rec_basic %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMA"),
  srl_pre_rec_x %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMAX"),
  srl_pre_rec_xgboost %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "srl", model = "XGBoost"),
  stl3b_pre_rec_basic %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMA"),
  stl3b_pre_rec_x %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMAX"),
  stl3b_pre_rec_xgboost %>% arrange(-f_0.5) %>% slice(1) %>% mutate(satellite = "stl3b", model = "XGBoost")) %>%
  arrange(model) %>% arrange(satellite) %>% select(c(satellite, model, f_0.5))

# Obtaining the best f1-score per satellite and method
f1_results_final <- bind_rows(
  cs2_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMA"),
  cs2_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMAX"),
  cs2_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "cs2", model = "XGBoost"),
  fy2d_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMA"),
  fy2d_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMAX"),
  fy2d_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "fy2d", model = "XGBoost"),
  js3_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMA"),
  js3_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMAX"),
  js3_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "js3", model = "XGBoost"),
  srl_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMA"),
  srl_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMAX"),
  srl_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "srl", model = "XGBoost"),
  stl3b_pre_rec_basic %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMA"),
  stl3b_pre_rec_x %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMAX"),
  stl3b_pre_rec_xgboost %>% arrange(-f_score) %>% slice(1) %>% mutate(satellite = "stl3b", model = "XGBoost")) %>%
  arrange(model) %>% arrange(satellite) %>% select(f_score) %>% rename("f_1"="f_score")

# Obtaining the best f2-score per satellite and method
f2_results_final <- bind_rows(
  cs2_pre_rec_basic %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMA"),
  cs2_pre_rec_x %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "cs2", model = "ARIMAX"),
  cs2_pre_rec_xgboost %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "cs2", model = "XGBoost"),
  fy2d_pre_rec_basic %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMA"),
  fy2d_pre_rec_x %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "fy2d", model = "ARIMAX"),
  fy2d_pre_rec_xgboost %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "fy2d", model = "XGBoost"),
  js3_pre_rec_basic %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMA"),
  js3_pre_rec_x %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "js3", model = "ARIMAX"),
  js3_pre_rec_xgboost %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "js3", model = "XGBoost"),
  srl_pre_rec_basic %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMA"),
  srl_pre_rec_x %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "srl", model = "ARIMAX"),
  srl_pre_rec_xgboost %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "srl", model = "XGBoost"),
  stl3b_pre_rec_basic %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMA"),
  stl3b_pre_rec_x %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "stl3b", model = "ARIMAX"),
  stl3b_pre_rec_xgboost %>% arrange(-f_2) %>% slice(1) %>% mutate(satellite = "stl3b", model = "XGBoost")) %>%
  arrange(model) %>% arrange(satellite) %>% select(f_2)

# Combining results into output tibble
bind_cols(f0.5_results_final, f1_results_final, f2_results_final)



# Table 5.1
# Getting the average mean motion of the 5 satellites
cs2_mean_mm <- cs2 %>% filter(Date >= as.Date("2016-01-01") & Date <= as.Date("2020-01-01")) %>% 
  pull(brouwer_mean_motion) %>% mean(na.rm = TRUE)
fy2d_mean_mm <- fy2d %>% filter(Date >= as.Date("2012-01-01") & Date <= as.Date("2015-01-01")) %>% 
  pull(brouwer_mean_motion) %>% mean(na.rm = TRUE)
js3_mean_mm <- js3 %>% filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2022-01-01")) %>% 
  pull(brouwer_mean_motion) %>% mean(na.rm = TRUE)
srl_mean_mm <- srl %>% filter(Date >= as.Date("2016-08-01") & Date <= as.Date("2022-08-01")) %>% 
  pull(brouwer_mean_motion) %>% mean(na.rm = TRUE)
stl3b_mean_mm <- stl3b %>% filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2022-01-01")) %>% 
  pull(brouwer_mean_motion) %>% mean(na.rm = TRUE)

# Function to calculate the average orbit height of a satellite
orbital_height_calc <- function(satellite_mm) {
  G <- 6.674*10^-11
  M <- 5.972*10^24
  orb_rad <- ((G*M)/((satellite_mm/60)^2))^(1/3)
  orb_height <- (orb_rad - 6371000)/1000
  cat(paste(round(orb_height, 2), "km", sep=""))
  return (orb_height)
}

# Getting the average orbit height of the 5 satellites
cs2_mean_oh <- orbital_height_calc(cs2_mean_mm)
fy2d_mean_oh <- orbital_height_calc(fy2d_mean_mm)
js3_mean_oh <- orbital_height_calc(js3_mean_mm)
srl_mean_oh <- orbital_height_calc(srl_mean_mm)
stl3b_mean_oh <- orbital_height_calc(stl3b_mean_mm)

# Combining results into output tibble
tibble(satellite = c("cs2","fy2d","js3","srl","stl3b"),
       mean_motion = c(cs2_mean_mm, fy2d_mean_mm, js3_mean_mm, srl_mean_mm, stl3b_mean_mm),
       orbit_height = c(cs2_mean_oh, fy2d_mean_oh, js3_mean_oh, srl_mean_oh, stl3b_mean_oh))


