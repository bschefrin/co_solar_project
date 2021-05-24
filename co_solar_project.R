#Project examining home PV solar in Colorado and doing a CBA comparing the benefits from lower carbon 
#emissions vs the costs of subsidies for installation. 

# Loading packages and data ---------------------------------------------------------------------------
library(pacman)

#packages for project
p_load(tidyverse, lubridate, janitor)

# reading in data
solar_data <- read_csv("C:/Users/Jake/Desktop/r_projects/shiny_projects/co_solar_power/electricity_net_metering_in_colorado.csv")

# Data Cleaning and prep--------------------------------------------------------------------------------

# cleaning names

solar_data_2 <- clean_names(solar_data)

#selecting in PV data for residential and commercial 
solar_data_3 <- solar_data_2 %>% 
  select(year, month, state, utility_number, utility_name, residential_capacity_photovoltaic,
         residential_customer_photovoltaic, residential_sold_back_photovoltaic, commercial_capacity_photovoltaic,
         commercial_customer_photovoltaic, commercial_sold_back_photovoltaic, industrial_capacity_photovoltaic,
         industrial_customer_photovoltaic, industrial_sold_back_photovoltaic, total_capacity_photovoltaic,
         total_customer_photovoltaic, total_sold_back_photovoltaic)

# converting megawatts to kilowatts
solar_data_4 <- solar_data_3 %>% 
  mutate(residential_capacity_photovoltaic = residential_capacity_photovoltaic*1000,
         residential_sold_back_photovoltaic = residential_sold_back_photovoltaic*1000,
         commercial_capacity_photovoltaic = commercial_capacity_photovoltaic *1000,
         commercial_sold_back_photovoltaic = commercial_sold_back_photovoltaic*1000,
         industrial_capacity_photovoltaic = industrial_capacity_photovoltaic*1000,
         industrial_sold_back_photovoltaic = industrial_sold_back_photovoltaic*1000,
         total_capacity_photovoltaic = total_capacity_photovoltaic*1000,
         total_sold_back_photovoltaic = total_sold_back_photovoltaic*1000)

# Using lubridate to convert months & year to date format and dropping year, month columns
solar_data_5 <- solar_data_4 %>% 
  mutate(date = make_date(year, month)) %>% 
  select(-c(year, month)) %>% 
  select(date, everything())

# Filtering out State Adjustment and High West Energy. High West Energy doesn't produce any PV.
# Longroad Energy has exaclty one data point so no use in retaining it.
# I am unsure what the State Adjustment utility actually means. If you do know please let me know!
solar_data_6 <- solar_data_5 %>% 
  filter(utility_number != 61098) %>% 
  filter(utility_number != 99999) %>% 
  filter(utility_number != 27058)



