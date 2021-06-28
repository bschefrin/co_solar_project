#Project examining home PV solar in Colorado and doing a CBA comparing the benefits from lower carbon 
#emissions vs the costs of subsidies for installation. 

# Loading packages and data ---------------------------------------------------------------------------
library(pacman)

#packages for project
p_load(tidyverse, lubridate, janitor, viridis, plotly, scales)

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


solar_data_10 <- solar_data_9 %>% 
  group_by(utility_number) %>% 
  arrange(date) %>% 
  mutate(residential_monthly_customer_change = residential_customer_photovoltaic - 
           (lag(residential_customer_photovoltaic))) %>% 
  mutate(residential_monthly_capacity_change = residential_capacity_photovoltaic -
           (lag(residential_capacity_photovoltaic))) %>% 
  ungroup()
  

# Residential Solar--------------------------------------------------------------------------------------

# getting residential PV data from master data frame
residential_solar <- solar_data_10 %>% 
  select(date, utility_number, utility_name, residential_capacity_photovoltaic, residential_monthly_capacity_change,
         residential_customer_photovoltaic, residential_monthly_customer_change, residential_sold_back_photovoltaic,
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

#CBA  --------------------------------------------------------------------------------------------------

# Examining residential capacity changes and constructing a taxpayer cost
cba_residential_solar_capacity_change <- residential_solar %>% 
  select(date, utility_name, residential_monthly_capacity_change) %>% 
  pivot_wider(names_from = utility_name, values_from = residential_monthly_capacity_change) %>% 
  # Only want yearly totals due to NREL cost per watt data
  mutate(date = format(date, format = "%Y")) %>% 
  replace(is.na(.), 0) %>% 
  group_by(date) %>% 
  #collapsing all year rows into one
  summarise_each(funs(sum)) %>% 
  # Column for total added capacity for year
  mutate(total_kw_change = rowSums(.[2:10])) %>% 
  # NREL cost per watt data only goes through 2017
  filter(date <= 2017) %>% 
  #NREL cost per watt data over time for residential PV systems
  mutate(cost_per_kw = matrix(c(4480, 3920, 3440, 3180, 2980, 2800), ncol = 1),
         utility_cost_per_kw = matrix(c(2570, 2270, 2090, 1850, 1380, 1130), ncol = 1)) %>% 
  # Estimated amount spent on home PV for Utilities with the available data
  mutate(total_cost = total_kw_change*cost_per_kw,
         # Fed Tax Credit worth 30% of total cost
         fed_tax_credit = total_cost*0.3,
         # Assumes a sales tax rate of 8.9%, higher than average but probably closer to a true value
         sales_tax_credit = total_cost*0.089,
         total_tax_credit = fed_tax_credit + sales_tax_credit,
         customer_cost = total_cost - total_tax_credit,
         # Assumption 1kW saves 1.3505 MT of co2 per year over a 25 year lifecycle
         co2_saved = total_kw_change*29.719,
         # Social cost of carbon priced at $51/MT
         scc_calculation = co2_saved*51,
         utility_capacity = total_tax_credit/utility_cost_per_kw)

cba_cost_graph <- plot_ly(data = cba_residential_solar_capacity_change,
                     x = ~date, y = ~customer_cost, type = 'bar', name = 'Customer Cost') %>% 
  add_trace(y = ~fed_tax_credit, name = 'Fed Tax Credit') %>% 
  add_trace(y = ~sales_tax_credit, name = 'Sales Tax Credit') %>% 
  layout(title = 'Residential PV Sales and Tax Credits for CO',yaxis = list(title = 'Total Sales ($)'), 
         xaxis = list(title = 'Year'), barmode = 'stack')

cba_price_per_watt <- ggplot(data = cba_residential_solar_capacity_change) +
  geom_line(mapping =  aes(x = date, y = cost_per_kw, group = 1), size = 1.5, color = 'dodgerblue2') +
  labs(title = "NREL Cost Per kW 2012-2017", subtitle = "5.7 kW system",
       x = "Year", y = "Value ($)") +
  theme_minimal()

credit_vs_scc_data <- cba_residential_solar_capacity_change %>% 
  select(date, total_tax_credit, scc_calculation) %>% 
  pivot_longer(cols = c(total_tax_credit, scc_calculation), names_to = 'total')

credit_vs_scc_graph <- ggplot(data = credit_vs_scc_data) +
  geom_line(mapping =  aes(x = date, y = value, group = total, color = total), size = 1.5) +
  labs(title = "Tax Cost vs Social Cost of Carbon (SCC) for Colorado", subtitle = "2012 - 2017",
       x = "Year", y = "Value ($)", color = "") +
  scale_y_continuous(labels = unit_format(unit = 'M', scale = 1e-6)) +
  scale_color_manual(labels = c('SCC', 'Tax Cost'), values = c('cyan3', 'olivedrab4')) +
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
