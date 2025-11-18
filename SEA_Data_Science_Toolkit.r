# Title: COVID-19 Data Visualization Script
# Description: This R script loads the 'country_wise_latest.csv' file,
# cleans the data, and generates 5 different plots (Bar, Pie, Stacked Bar,
# Line, Histogram) to analyze the pandemic's impact.
# Required Libraries: tidyverse (ggplot2, dplyr, readr, tidyr)

# --- 1. SETUP ---

# Install necessary packages if you don't have them
# install.packages("tidyverse")

# Load libraries
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr) # For pivot_longer

# Set a clean theme for all plots
theme_set(theme_minimal())

# --- 2. LOAD AND CLEAN DATA ---

# Define file path
"E:\\Studies\\GBs\\Sem 5\\Toolkit\\Sea\\country_wise_latest.csv" <- "country_wise_latest.csv" # nolint: line_length_linter.

# Load data
# We use read_csv from the readr package
tryCatch({
  data <- read_csv("E:\\Studies\\GBs\\Sem 5\\Toolkit\\Sea\\country_wise_latest.csv")
  
  # Clean column names to make them R-friendly
  # This renames columns with spaces, slashes, or special characters
  data_clean <- data %>%
    rename(
      Country = `Country/Region`,
      Deaths_per_100_Cases = `Deaths / 100 Cases`,
      Recovered_per_100_Cases = `Recovered / 100 Cases`,
      Deaths_per_100_Recovered = `Deaths / 100 Recovered`,
      Confirmed_last_week = `Confirmed last week`,
      One_week_change = `1 week change`,
      One_week_perc_increase = `1 week % increase`,
      WHO_Region = `WHO Region`,
      New_cases = `New cases`,
      New_deaths = `New deaths`,
      New_recovered = `New recovered`
    )
  
  print("Data loaded and cleaned successfully.")