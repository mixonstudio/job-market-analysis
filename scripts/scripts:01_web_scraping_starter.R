# Job Market Analysis: Getting the Data
# Author: S

library(dplyr)
library(readr)
library(httr)
library(jsonlite)
library(lubridate)
library(tidyr)

# ============================================================================
# APPROACH 1: Bureau of Labor Statistics (BLS) API
# ============================================================================

# BLS provides official employment statistics
# Free API, no registration required for basic usage
# Data includes employment by occupation and metro area

get_bls_data <- function(series_id, start_year = 2022, end_year = 2024) {
  
  # BLS API endpoint
  url <- "https://api.bls.gov/publicAPI/v2/timeseries/data/"
  
  # Build request body
  body <- list(
    seriesid = series_id,
    startyear = as.character(start_year),
    endyear = as.character(end_year)
  )
  
  # Make API request
  response <- POST(url, 
                   body = body, 
                   encode = "json",
                   add_headers("Content-Type" = "application/json"))
  
  # Parse response
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    return(data$Results$series[[1]]$data)
  } else {
    cat("API request failed with status:", status_code(response), "\n")
    return(NULL)
  }
}

# Example: Data Analyst employment in Philadelphia MSA
# Series ID format: OEUM[AREA_CODE][OCCUPATION_CODE]
# We'll need to look up the specific codes, but this shows the concept

# ============================================================================
# APPROACH 2: Use Existing Job Market Datasets
# ============================================================================

# There are several good public datasets we can use:
# 1. Kaggle job posting datasets
# 2. GitHub repositories with scraped data
# 3. Government employment statistics

# Let's create a function to load and process a sample dataset
load_sample_job_data <- function() {
  
  # For now, let's create a realistic sample dataset
  # In reality, you'd download from Kaggle or another source
  
  set.seed(123) # for reproducible "random" data
  
  cities <- c("Philadelphia", "Boston", "New York", "Chicago", 
              "Seattle", "Portland", "San Francisco", "Los Angeles")
  
  job_titles <- c("Data Analyst", "Product Manager", "UX Researcher", 
                  "Program Manager", "Policy Analyst", "Civic Designer")
  
  # Generate sample data that mimics real job posting patterns
  sample_data <- expand_grid(
    city = cities,
    job_title = job_titles,
    month = seq(as.Date("2023-01-01"), as.Date("2024-08-01"), by = "month")
  ) %>%
    mutate(
      # Simulate job posting counts with seasonal patterns
      base_jobs = case_when(
        city %in% c("New York", "San Francisco", "Boston") ~ 45,
        city %in% c("Seattle", "Chicago") ~ 35,
        TRUE ~ 25
      ),
      
      # Add job title multipliers
      title_multiplier = case_when(
        job_title == "Data Analyst" ~ 1.5,
        job_title == "Product Manager" ~ 1.2,
        job_title == "Program Manager" ~ 1.0,
        TRUE ~ 0.8
      ),
      
      # Add seasonal patterns (September surge!)
      seasonal_multiplier = case_when(
        month(month) == 9 ~ 1.4,  # September surge
        month(month) == 1 ~ 1.3,  # January hiring
        month(month) %in% c(11, 12) ~ 0.7, # Holiday slowdown
        month(month) %in% c(6, 7, 8) ~ 0.9, # Summer slowdown
        TRUE ~ 1.0
      ),
      
      # Calculate final job counts with some randomness
      job_count = round(base_jobs * title_multiplier * seasonal_multiplier * 
                          runif(n(), 0.8, 1.2)),
      
      # Add salary estimates
      avg_salary = case_when(
        city %in% c("San Francisco", "New York") ~ 85000,
        city %in% c("Boston", "Seattle") ~ 75000,
        city %in% c("Chicago", "Los Angeles") ~ 70000,
        TRUE ~ 65000
      ) * title_multiplier * runif(n(), 0.9, 1.1)
    ) %>%
    select(city, job_title, month, job_count, avg_salary)
  
  return(sample_data)
}

# ============================================================================
# APPROACH 3: Government Job Boards (USAJobs API)
# ============================================================================

# USAJobs has a proper API for federal positions
get_usajobs_data <- function(keyword = "data analyst", location = "Philadelphia") {
  
  # USAJobs API endpoint
  base_url <- "https://data.usajobs.gov/api/search"
  
  # Build query parameters
  params <- list(
    Keyword = keyword,
    LocationName = location,
    ResultsPerPage = 500
  )
  
  # Make request with required headers
  response <- GET(base_url, 
                  query = params,
                  add_headers(
                    "Host" = "data.usajobs.gov",
                    "User-Agent" = "your-email@example.com"  # Replace with your email
                  ))
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text"))
    return(data$SearchResult$SearchResultItems)
  } else {
    cat("USAJobs API request failed\n")
    return(NULL)
  }
}

# ============================================================================
# LET'S TEST WITH SAMPLE DATA
# ============================================================================

# Generate our sample dataset
cat("Generating sample job market data...\n")
job_data <- load_sample_job_data()

# Preview the data
cat("Dataset overview:\n")
print(summary(job_data))

cat("\nFirst few rows:\n")
print(head(job_data))

cat("\nChecking for September surge pattern:\n")
september_data <- job_data %>%
  group_by(month) %>%
  summarise(total_jobs = sum(job_count), .groups = 'drop') %>%
  mutate(month_name = month.name[month(month)])

print(september_data)

# Save the sample data
write_csv(job_data, "data/raw/sample_job_market_data.csv")
cat("\nSample data saved to data/raw/sample_job_market_data.csv\n")
