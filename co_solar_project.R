#Project examining home PV solar in Colorado and doing a CBA comparing the benefits from lower carbon 
#emissions vs the costs of subsidies for installation. 

# Loading packages and data ---------------------------------------------------------------------------
library(pacman)

#packages for project
p_load(tidyverse, lubridate, janitor, viridis)

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
         commercial_capacity_photovoltaic = commercial_capacity_photovoltaic *1000,
         industrial_capacity_photovoltaic = industrial_capacity_photovoltaic*1000,
         total_capacity_photovoltaic = total_capacity_photovoltaic*1000)

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

# Adding Kw per customer column for residential, commercial, industrial, and total. 
solar_data_7 <- solar_data_6 %>% 
  mutate(kw_per_customer_residential = residential_capacity_photovoltaic/residential_customer_photovoltaic,
         kw_per_customer_commercial = commercial_capacity_photovoltaic/commercial_customer_photovoltaic,
         kw_per_customer_industrial = industrial_capacity_photovoltaic/industrial_customer_photovoltaic,
         kw_per_customer_total = total_capacity_photovoltaic/total_customer_photovoltaic)

# Replacing all NA with 0
solar_data_8 <- solar_data_7 %>% 
  replace(is.na(.), 0) 

# Making all utility names the same. This applies especially to Poudre Valley, there are 3 different 
# ways it is listed but all the same utility number. Also Public Service Co of Colorado is known
# commonly as Xcel Energy, so I am renaming ii to Xcel Energy
solar_data_9 <- solar_data_8 %>% 
  mutate(utility_name = if_else(utility_number == 15466, "Xcel Energy", utility_name),
         utility_name = if_else(utility_number == 56146, "Black Hills Colorado Electric LLC", utility_name),
         utility_name = if_else(utility_number == 15257, "Poudre Valley REA, Inc", utility_name)
         )

# cleaning up environment, run this at your own risk.....
rm(list=setdiff(ls(), "solar_data_9"))


# Residential Solar--------------------------------------------------------------------------------------

# getting residential PV data from master data frame
residential_solar <- solar_data_9 %>% 
  select(date, utility_number, utility_name, residential_capacity_photovoltaic,
         residential_customer_photovoltaic, residential_sold_back_photovoltaic,
         kw_per_customer_residential)

# Residential Customer Graphs

# Giving Xcel its own graph due to much higher population compared to all other utility areas
residential_solar_xcel <- residential_solar %>% 
  filter(utility_number == 15466)

residential_solar_xcel_graph <- ggplot(data = residential_solar_xcel) +
  geom_line(mapping = aes(x = date, y = residential_customer_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Customers with Home PV (Xcel Energy)", subtitle= "2011-2020", 
       x = "Date", y = "Total customers", fill = "Utility") +
  theme_minimal()
  
      

residential_solar_not_xcel <- residential_solar %>% 
  filter(utility_number != 15466)

residential_solar_not_xcel_graph <- ggplot(data = residential_solar_not_xcel) +
  geom_line(mapping = aes(x = date, y = residential_customer_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Customers with Home PV (Xcel Excluded)", subtitle= "2011-2020",
       x = "Date", y = "Total customers") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# Residential Capacity Graphs

# Once again Xcel gets its own graph
residential_solar_xcel_capacity_graph <- ggplot(data = residential_solar_xcel) +
  geom_line(mapping = aes(x = date, y = residential_capacity_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Home PV Capacity (Xcel Energy)", subtitle= "2011-2020",
       x = "Date", y = "Total capacity (Kw)") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

residential_solar_not_xcel_capacity_graph <- ggplot(data = residential_solar_not_xcel) +
  geom_line(mapping = aes(x = date, y = residential_capacity_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Home PV Capacity (Xcel Excluded)", subtitle= "2011-2020",
       x = "Date", y = "Total capacity (Kw)") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()


