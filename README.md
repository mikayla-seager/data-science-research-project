# Data Science Research Project Code
This repository contains the R code for Mikayla Seager's Data Science Research Project (Part A) progress report, as well as the necessary dataset files in order to run the code. The code in all in one file named `Research Project - Progress Report Code - a1748578`, and is used for every aspect of the project. The code initially imports the datasets of all 15 provided satellites as well as their ground-truth manoeuvre data, but only makes use of 5 of them, which are CryoSat-2 (`cs2`), Jason-3 (`js3`), FengYun 2D (`fy2d`), SARAL (`srl`), and Sentinel-3B (`stl3b`).

The first two sections of code, after importing the required packages, are focused on reading in all of the datasets as lines of strings for the manoeuvre data, and tibbles for the orbital element data. Both groups of datasets are then cleaned to produce tidier, easier-to-work-with tibbles for further analysis. Specifically, functions are created to perform various forms of string and dataframe manipulations on the manoeuvre data, as well as functions that remove outliers for the orbital element data.

Next, a function to visualise plots of all 6 orbital elements together in one figure is created and used for all satellites, as well as a function that produces three plots of the data cleaning process for any chosen element and satellite. These plots were shown in the progress report itself in the exploratory data analysis section.

After all of the data cleaning and visualisation code has been run, multiple functions to train various ARIMA models and obtain the best possible models and plots based on AIC are created. The function `arima_modelling_extra` is the primary function of this code, which was used to both output ARIMA model specifications and residual plots for both ARIMA and ARIMAX models, depending on the parameters parsed to it. Subsequent functions that utilise this main function are also created, in order to automatically iterate over all specified ARIMA models to obtain the best one, as well as generate six-plot figures of residual plots for each element for one satellite and model.
