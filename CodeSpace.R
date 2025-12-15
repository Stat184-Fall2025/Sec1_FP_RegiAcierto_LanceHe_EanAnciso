# Coding File
# This document will be used to organize and work on the specific code of each of our objectives
# All necessary code will transferred to the qmd document when thoroughly tested and approved

library(rvest)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)

# 1. Scrape the table
url <- "https://en.wikipedia.org/wiki/List_of_largest_cities"
webpage <- read_html(url)
tables <- html_table(webpage, fill = TRUE)

# Find the correct table containing "Jakarta"
table_index <- which(sapply(tables, function(t) "Jakarta" %in% t[[1]]))
city_table <- tables[[table_index]]

# 2. Fix Column Names
colnames(city_table) <- c(
  "City", "Country", "UN_Estimate_Pop", "Definition",
  "City_Pop", "City_Area_km2", "City_Density_per_km2",      # City Proper
  "Urban_Pop", "Urban_Area_km2", "Urban_Density_per_km2",   # Urban Area
  "Metro_Pop", "Metro_Area_km2", "Metro_Density_per_km2"    # Metropolitan
)

# 3. Data Wrangling Pipeline
clean_data <- city_table %>%

    slice(-1) %>%
  
  select(
    City, Country, UN_Estimate_Pop,
    City_Pop, City_Area_km2, City_Density_per_km2,
    Urban_Pop, Urban_Area_km2, Urban_Density_per_km2
  ) %>%
  
  mutate(across(everything(), ~str_remove_all(., "\\[.*?\\]"))) %>% 
  mutate(across(everything(), ~str_remove_all(., ","))) %>%
  
  mutate(across(everything(), ~na_if(str_trim(.), ""))) %>%
  mutate(across(everything(), ~na_if(str_trim(.), "—"))) %>%
  mutate(across(everything(), ~na_if(str_trim(.), "-"))) %>%
  mutate(across(everything(), ~na_if(str_trim(.), "–"))) %>%
  
  filter(
    !is.na(City_Pop), !is.na(City_Area_km2), !is.na(City_Density_per_km2),
    !is.na(Urban_Pop), !is.na(Urban_Area_km2), !is.na(Urban_Density_per_km2)
  ) %>%
  
  mutate(across(3:9, as.numeric)) %>%
  
  mutate(Country = str_trim(Country))

# 4. View Result
View(clean_data)





## First table
firstTable <- clean_data %>%
  group_by(Country) %>%
  summarise(
    entries = n(),
    total_pop = sum(UN_Estimate_Pop, na.rm = TRUE),
    total_city_pop = sum(City_Pop, na.rm = TRUE),
    total_urban_pop = sum(Urban_Pop, na.rm = TRUE)
  ) %>%
  arrange(desc(entries))

View(firstTable)


# Filter for the Target 6 Countries
target_countries <- c("United States", "China", "India", "Japan", "Brazil", "Indonesia")
country_data <- clean_data %>%
  filter(Country %in% target_countries)


# Table A: Urban Area Statistics 
urban_area_stats <- country_data %>%
  group_by(Country) %>%
  summarise(
    # Population Stats
    Pop_Min  = min(Urban_Pop, na.rm = TRUE),
    Pop_Q1   = quantile(Urban_Pop, 0.25, na.rm = TRUE),
    Pop_Mean = mean(Urban_Pop, na.rm = TRUE),
    Pop_Q3   = quantile(Urban_Pop, 0.75, na.rm = TRUE),
    Pop_Max  = max(Urban_Pop, na.rm = TRUE),
    Pop_SD   = sd(Urban_Pop, na.rm = TRUE),
    
    # Area Stats
    Area_Min  = min(Urban_Area_km2, na.rm = TRUE),
    Area_Q1   = quantile(Urban_Area_km2, 0.25, na.rm = TRUE),
    Area_Mean = mean(Urban_Area_km2, na.rm = TRUE),
    Area_Q3   = quantile(Urban_Area_km2, 0.75, na.rm = TRUE),
    Area_Max  = max(Urban_Area_km2, na.rm = TRUE),
    Area_SD   = sd(Urban_Area_km2, na.rm = TRUE)
  )


View(urban_area_stats)

# Table B: City Proper Statistics 
city_proper_stats <- country_data %>%
  group_by(Country) %>%
  summarise(
    # Population Stats
    Pop_Min  = min(City_Pop, na.rm = TRUE),
    Pop_Q1   = quantile(City_Pop, 0.25, na.rm = TRUE),
    Pop_Mean = mean(City_Pop, na.rm = TRUE),
    Pop_Q3   = quantile(City_Pop, 0.75, na.rm = TRUE),
    Pop_Max  = max(City_Pop, na.rm = TRUE),
    Pop_SD   = sd(City_Pop, na.rm = TRUE),
    
    # Area Stats
    Area_Min  = min(City_Area_km2, na.rm = TRUE),
    Area_Q1   = quantile(City_Area_km2, 0.25, na.rm = TRUE),
    Area_Mean = mean(City_Area_km2, na.rm = TRUE),
    Area_Q3   = quantile(City_Area_km2, 0.75, na.rm = TRUE),
    Area_Max  = max(City_Area_km2, na.rm = TRUE),
    Area_SD   = sd(City_Area_km2, na.rm = TRUE)
  )


View(city_proper_stats)





target_countries <- c("United States", "China", "India", "Japan", "Brazil", "Indonesia")

urban_data <- clean_data %>%
  filter(Country %in% target_countries)

city_proper_data <- clean_data %>%
  filter(Country %in% target_countries)

# Plot 1: Urban Population by Area
# X = Urban Area, Y = Urban Population
urban_data %>%
  ggplot(
    aes(
      x = Urban_Area_km2,
      y = Urban_Pop / 1000000,
      color = Country,
      group = Country
    )
  ) +
  geom_point(alpha = 0.5) +
  geom_line() +
  scale_x_log10() +
  labs(
    title = "City Proper Population by Area",
    x = "City Proper Area (km²)",
    y = "City Proper Population (Millions)"
  )

# Line plot for city proper population by area for top 6 cities
city_proper_data <- clean_data %>%
  filter(
    Country %in% c("United States", "China", "India", "Japan", "Brazil", "Indonesia")
  )

View(city_proper_data)

city_proper_data %>%
  ggplot(
    aes(
      x = City_Area_km2,
      y = City_Pop / 1000000,
      color = Country,
      group = Country
    )
  ) +
  geom_point(alpha = 0.5) +
  geom_line() +
  scale_x_log10() +
  labs(
    title = "City Proper Population by Area",
    x = "City Proper Area (km²)",
    y = "City Proper Population (Millions)"
  )
    title = "Urban Population by Area",
    x = "Urban Area (km²)",
    y = "Urban Population (Millions)"
  )

# Plot 2: City Proper Population by Area
# X = City Area, Y = City Population
city_proper_data %>%
  ggplot(
    aes(
      x = City_Area_km2,
      y = City_Pop / 1000000,
      color = Country,
      group = Country
    )
  ) +
  geom_point(alpha = 0.5) +
  geom_line() +
  scale_x_log10() +
  labs(
    title = "City Proper Population by Area",
    x = "City Proper Area (km²)",
    y = "City Proper Population (Millions)"
  )
