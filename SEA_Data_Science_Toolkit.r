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
  
  # --- 3. DATA PREPARATION FOR PLOTS ---
  
  # Prep 1: Get Top 10 countries by confirmed cases
  top_10_confirmed <- data_clean %>%
    arrange(desc(Confirmed)) %>%
    top_n(10, Confirmed)
  
  print("Prepared data for Top 10 Bar Chart.")
  
  # Prep 2: Summarize data by WHO Region
  region_summary <- data_clean %>%
    group_by(WHO_Region) %>%
    summarise(
      Total_Confirmed = sum(Confirmed, na.rm = TRUE),
      Total_Deaths = sum(Deaths, na.rm = TRUE),
      Total_Recovered = sum(Recovered, na.rm = TRUE),
      Total_Active = sum(Active, na.rm = TRUE)
    ) %>%
    # Calculate percentage for pie chart
    mutate(
      Conf_Percentage = Total_Confirmed / sum(Total_Confirmed),
      Total_Global = sum(Total_Confirmed)
    )
  
  print("Prepared data for Regional Pie and Stacked Bar Charts.")
  
  # Prep 3: Data for Stacked Bar Chart (Case Breakdown)
  region_summary_long <- region_summary %>%
    select(WHO_Region, Total_Deaths, Total_Recovered, Total_Active) %>%
    pivot_longer(
      cols = -WHO_Region,
      names_to = "Case_Type",
      values_to = "Count"
    ) %>%
    # Clean up labels for the legend
    mutate(Case_Type = str_replace(Case_Type, "Total_", ""))

  # Prep 4: Data for Line Chart (1-week growth for top 10)
  top_10_growth <- data_clean %>%
    arrange(desc(Confirmed)) %>%
    top_n(10, Confirmed) %>%
    select(Country, Confirmed_last_week, Confirmed) %>%
    pivot_longer(
      cols = -Country,
      names_to = "Time_Point",
      values_to = "Cases"
    ) %>%
    # Make time point names clearer
    mutate(Time_Point = ifelse(Time_Point == "Confirmed", "This Week", "Last Week"))

  print("Prepared data for 1-Week Growth Line Chart.")