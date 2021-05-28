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

# Using lubridate to convert months & year to date format and dropping year, month columns. No
# data available from Xcel in 2011 so dropping all 2011 data
solar_data_5 <- solar_data_4 %>% 
  filter(year >= 2012) %>% 
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

# Replacing all NA with 0. 
solar_data_8 <- solar_data_7 %>% 
  replace(is.na(.), 0) 

# Making all utility names the same. This applies especially to Poudre Valley, there are 3 different 
# ways it is listed but all the same utility number. Also Public Service Co of Colorado is known
# commonly as Xcel Energy, so I am renaming ii to Xcel Energy. 
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
  labs(title = "Total Customers with Home PV (Xcel Energy)", subtitle= "2012-2019", 
       x = "Date", y = "Total customers", fill = "Utility") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()
  
      

residential_solar_not_xcel <- residential_solar %>% 
  filter(utility_number != 15466)

residential_solar_not_xcel_graph <- ggplot(data = residential_solar_not_xcel) +
  geom_line(mapping = aes(x = date, y = residential_customer_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Customers with Home PV (Xcel Excluded)", subtitle= "2012-2019",
       x = "Date", y = "Total customers") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# Residential Capacity Graphs

# Once again Xcel gets its own graph
residential_solar_xcel_capacity_graph <- ggplot(data = residential_solar_xcel) +
  geom_line(mapping = aes(x = date, y = residential_capacity_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Home PV Capacity (Xcel Energy)", subtitle= "2012-2019",
       x = "Date", y = "Total capacity (Kw)") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

residential_solar_not_xcel_capacity_graph <- ggplot(data = residential_solar_not_xcel) +
  geom_line(mapping = aes(x = date, y = residential_capacity_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Home PV Capacity (Xcel Excluded)", subtitle= "2012-2019",
       x = "Date", y = "Total capacity (Kw)") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# Residential Kw per customer

# Xcel gets to party with everyone now yay. Had to drop Inter Mountain Rural Electric 
# due to data entry error and dropped Moon Lake because they had exactly 1 residential PV customer.
# Go that person and their 6kw set up 
residential_solar_kw_per_customer <- residential_solar %>% 
  filter(utility_number != 9336) %>% 
  filter(utility_number != 12866) 

residential_solar_kw_per_customer_graph <- ggplot(data = residential_solar_kw_per_customer) +
  geom_smooth(mapping = aes(x = date, y = kw_per_customer_residential, color = utility_name), size = 1.5) +
  labs(title = "Average Residential PV size (Smoothed)", subtitle= "2012-2019",
       x = "Date", y = "Kw per customer") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  scale_y_continuous(breaks = c(2, 4, 6, 8)) +
  theme_minimal()

# Commercial Solar ----------------------------------------------------------------------------------
#Getting all commercial PV data into master data frame
commercial_solar <- solar_data_9 %>% 
  select(date, utility_number, utility_name, commercial_capacity_photovoltaic, commercial_customer_photovoltaic,
         commercial_sold_back_photovoltaic, kw_per_customer_commercial)

# Commercial Customer Graphs

# Giving Xcel its own graph due to much higher population compared to all other utility areas
commercial_solar_xcel <- commercial_solar %>% 
  filter(utility_number == 15466)

commercial_solar_xcel_graph <- ggplot(data = commercial_solar_xcel) +
  geom_line(mapping = aes(x = date, y = commercial_customer_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Commercial Customers with PV (Xcel Energy)", subtitle= "2012-2019", 
       x = "Date", y = "Total customers", fill = "Utility") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# All other utilities
commercial_solar_not_xcel <- commercial_solar %>% 
  filter(utility_number != 15466)

commercial_solar_not_xcel_graph <- ggplot(data = commercial_solar_not_xcel) +
  geom_line(mapping = aes(x = date, y = commercial_customer_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Commercial Customers with PV (Xcel Excluded)", subtitle= "2012-2019",
       x = "Date", y = "Total customers") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# Commercial Capacity Graphs

# Once again Xcel gets its own graph
commercial_solar_xcel_capacity_graph <- ggplot(data = commercial_solar_xcel) +
  geom_line(mapping = aes(x = date, y = commercial_capacity_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Commercial PV Capacity (Xcel Energy)", subtitle= "2012-2019",
       x = "Date", y = "Total capacity (Kw)") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

commercial_solar_not_xcel_capacity_graph <- ggplot(data = commercial_solar_not_xcel) +
  geom_line(mapping = aes(x = date, y = commercial_capacity_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Commercial PV Capacity (Xcel Excluded)", subtitle= "2012-2019",
       x = "Date", y = "Total capacity (Kw)") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# Commercial Kw per customer

# Xcel gets to party with everyone now yay. 
commercial_solar_kw_per_customer_graph <- ggplot(data = commercial_solar) +
  geom_smooth(mapping = aes(x = date, y = kw_per_customer_commercial, color = utility_name), size = 1.5) +
  labs(title = "Average Commercial PV Size (Smoothed)", subtitle= "2012-2019",
       x = "Date", y = "Kw per customer") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  scale_y_continuous() +
  theme_minimal()

# Industrial Solar -------------------------------------------------------------------------------------

industrial_solar <- solar_data_9 %>% 
  select(date, utility_number, utility_name, industrial_capacity_photovoltaic, industrial_customer_photovoltaic,
         industrial_sold_back_photovoltaic, kw_per_customer_industrial)

# Industrial solar customer Graph
industrial_solar_customer_graph <- ggplot(data = industrial_solar) +
  geom_line(mapping = aes(x = date, y = industrial_customer_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Indusrial Customers with PV", subtitle= "2012-2019", 
       x = "Date", y = "Total customers", fill = "Utility") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# Industrial solar capacity graph

industrial_solar_capacity_graph <- ggplot(data = industrial_solar) +
  geom_line(mapping = aes(x = date, y = industrial_capacity_photovoltaic, color = utility_name), size = 1.5) +
  labs(title = "Total Industrial PV Capacity ", subtitle= "2012-2019",
       x = "Date", y = "Total capacity (Kw)") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  theme_minimal()

# industrial Kw per customer

industrial_solar_kw_per_customer_graph <- ggplot(data = industrial_solar) +
  geom_line(mapping = aes(x = date, y = kw_per_customer_industrial, color = utility_name), size = 1.5) +
  labs(title = "Average Industrial PV Size (Smoothed)", subtitle= "2012-2019",
       x = "Date", y = "Kw per customer") +
  scale_color_viridis(name = "Utility Co", discrete = TRUE) +
  scale_y_continuous() +
  theme_minimal()
