# Coding File
# This document will be used to organize and work on the specific code of each of our objectives
# All necessary code will transferred to the qmd document when thoroughly tested and approved

library(rvest)
library(dplyr)
library(stringr)

# 1. Scrape the table
url <- "https://en.wikipedia.org/wiki/List_of_largest_cities"
webpage <- read_html(url)
tables <- html_table(webpage, fill = TRUE)


table_index <- which(sapply(tables, function(t) "Jakarta" %in% t[[1]]))
city_table <- tables[[table_index]]

# 2. Fix Column Names

colnames(city_table) <- c(
  "City", "Country", "UN_Estimate", "Definition",
  "City_Pop", "City_Area", "City_Density",      # City Proper
  "Urban_Pop", "Urban_Area", "Urban_Density",   # Urban Area
  "Metro_Pop", "Metro_Area", "Metro_Density"    # Metropolitan
)

# 3. Data Wrangling Pipeline
clean_data <- city_table %>%
  #Remove Garbage Header Row 
  slice(-1) %>%
  
  select(
    City, Country, UN_Estimate,
    City_Pop, City_Area, City_Density,
    Urban_Pop, Urban_Area, Urban_Density
  ) %>%
  
  # Remove Footnotes and Commas ---
  mutate(across(everything(), ~str_remove_all(., "\\[.*?\\]"))) %>% 
  mutate(across(everything(), ~str_remove_all(., ","))) %>%
  

  # Convert empty strings, dashes, and em-dashes to NA
  mutate(across(everything(), ~na_if(str_trim(.), ""))) %>%
  mutate(across(everything(), ~na_if(str_trim(.), "—"))) %>%
  mutate(across(everything(), ~na_if(str_trim(.), "-"))) %>%
  mutate(across(everything(), ~na_if(str_trim(.), "–"))) %>%
  

  filter(
    !is.na(City_Pop),
    !is.na(City_Area),
    !is.na(City_Density),
    !is.na(Urban_Pop),
    !is.na(Urban_Area),
    !is.na(Urban_Density)
  )

# 4. View Result
View(clean_data)
