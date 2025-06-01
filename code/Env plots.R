library(ggplot2)
library(dplyr)
library(ggforce)
library(lubridate)
library(ggpubr)
library(tidyr)
library(readxl)

#import data
ndvi_data <- read.csv("raw_data/Env_Data/NDVI-2024-09-20.csv")
soil_m_data <-read.csv("raw_data/Env_Data/SoilMoisture-2024-09-20.csv")
active_layer_data <- read.csv("raw_data/Env_Data/ALT-2024-09-20.csv")
weather_station_data <- read_xlsx("raw_data/Env_Data/weather_stations_2024.xlsx", skip = 1)
precipitation <- read_xlsx("raw_data/Env_Data/SLprecipitation.xlsx")

##soil moisture
#first average sm per plot
soil_m_data <- soil_m_data %>%
  slice(-(15:30)) # rows are deleted because they have high (more than 3 NAs)

soil_m_data <- mutate(
  soil_m_data, across(R:RG, as.numeric))

soil_m_data <- soil_m_data %>%
  mutate(time_stamp = as.Date(time_stamp))

soil_m_data <- mutate(
  soil_m_data, Average = rowMeans(select(soil_m_data, R:RG), na.rm = TRUE))

soil_m_data <- soil_m_data %>%
  mutate(Plot = sub("_.*", "", site))

soil_m_data <- soil_m_data |>
  mutate(Block = ifelse(grepl("D", Plot), "D",
                        ifelse(grepl("M", Plot), "M", "Other")))


soil <- soil_m_data %>%
  rowwise() %>%
  mutate(
    Mean_Value = mean(c(R, C, G, W, WG, RG), na.rm = TRUE),
    SD_Value = sd(c(R, C, G, W, WG, RG), na.rm = TRUE)
  ) %>%
  ungroup()

# Aggregate by Plot, Block, and time_stamp
summary_soil <- soil %>%
  group_by(Block, time_stamp) %>%
  summarise(
    Mean_of_Means = mean(Mean_Value, na.rm = TRUE),
    SD_of_Means = sd(Mean_Value, na.rm = TRUE)
  )

sm <- ggplot(summary_soil, 
             aes(
               x = time_stamp, 
               y = Mean_of_Means/10, 
               colour = Block, 
               group = Block
             )) +
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin = Mean_of_Means/10 - SD_of_Means/10, 
                    ymax = Mean_of_Means/10 + SD_of_Means/10), 
                width = 1, 
                color = "black") +  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  labs(x = "Date", y = "Soil moisture (%)")

plot(sm)


##NDVI########################################################
ndvi_data <- ndvi_data |>
  mutate(Block = ifelse(grepl("D", site), "Dryas",
                        ifelse(grepl("M", site), "Moss", "Other")))

ndvi_avg <- ndvi_data %>%
  group_by(time_stamp, Block) %>%
  summarise(
    R = mean(R),
    C = mean(C),
    G = mean(G),
    W = mean(W),
    WG = mean(WG),
    RG = mean(RG)
  )

ndvi_avg <- ndvi_avg %>%
  mutate(time_stamp = as.Date(time_stamp))

#remove data 05-07 as not in line with expected curve
ndvi_avg <- ndvi_avg[-c(11, 12), ]

#pivot data
ndvi_long <- ndvi_avg |>
  pivot_longer(cols = c(R, C, G, W, WG, RG),
               names_to = "Plot",
               values_to = "Ndvi")

ndvi_long <- ndvi_long |>
  mutate(Site = paste0(Block, Plot))


  

ndvi_plot <- ggplot(ndvi_long,
                    aes(
                      x = time_stamp,
                      y = Ndvi/1000,
                      shape = Plot,
                      group = Site,
                      linetype = Block
                    )) +
  geom_point() + geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  labs(x = "Date", y = "NDVI", colour = "Site")

plot(ndvi_plot)


ndvi_plot <- ggplot(ndvi_long,
                    aes(
                      x = time_stamp,
                      y = Ndvi/1000,
                      color = Plot,  # Use color for treatments
                      shape = Plot
                    )) +
  geom_point(alpha = 0.7) +  # Add transparency
  geom_line(alpha = 0.7) +   # Add transparency
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = "Date", y = "NDVI", color = "Site", shape = "Site") +
  facet_wrap(~ Block, ncol = 2) +  # Facet by habitat
  theme_minimal() +  # Use a clean theme
  theme(legend.position = "right")  # Adjust legend position if needed

print(ndvi_plot)
              

##ALT########################################################
#Avg depth per site, maybe only per block
active_layer_data <- active_layer_data %>%
  mutate(time_stamp = as.Date(time_stamp))

active_layer_data <- mutate(
  active_layer_data, Average = rowMeans(select(active_layer_data, R:RG), na.rm = TRUE))

active_layer_data <- active_layer_data |>
  mutate(Block = ifelse(grepl("D", site), "D",
                        ifelse(grepl("M", site), "M", "Other")))

active_layer_average <- active_layer_data %>%
  rowwise() %>%
  mutate(
    Mean_Value = mean(c(R, C, G, W, WG, RG), na.rm = TRUE),
    SD_Value = sd(c(R, C, G, W, WG, RG), na.rm = TRUE)
  ) %>%
  ungroup()


active_layer_average <- active_layer_average |>
  group_by(Block, time_stamp) |>
  summarise(
    Mean_of_Means = mean(Mean_Value, na.rm = TRUE),
    SD_of_Means = sd(Mean_Value, na.rm = TRUE)
  )


AL_plot <- ggplot(active_layer_average, 
             aes(
               x = time_stamp, 
               y = Mean_of_Means, 
               colour = Block, 
               group = Block
             )) +
  geom_point() + 
  geom_line() +
  geom_errorbar(aes(ymin = Mean_of_Means - SD_of_Means, 
                    ymax = Mean_of_Means + SD_of_Means), 
                width = 1, 
                color = "black") +  
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  labs(x = "Date", y = "ALT (cm)")

plot(AL_plot)

AL_plot <- ggplot(active_layer_data,
                  aes(
                    x = time_stamp,
                    y = Average,
                    colour = site, 
                    group = site,
                    linetype = Block
                  )) + 
  geom_point() + geom_line() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  labs(x = "Date", y = "Active layer depth (cm)", colour = "Site")

plot(AL_plot)


##Weather Station##############################################

#temperature plot + Humidity plot
aggregated_data <- weather_station_data %>%
  mutate(three_hour_block = floor_date(Date, "3 hours")) %>%
  group_by(three_hour_block) %>%
  summarise(
    avg_temp = mean(Temp, na.rm = TRUE),
    avg_rh = mean(RH, na.rm = TRUE)
  )

# Plot the aggregated data
Temp_plot <- ggplot(aggregated_data, aes(x = three_hour_block)) +
  geom_line(aes(y = avg_temp, color = "Temperature")) +
  geom_line(aes(y = avg_rh * 0.5, color = "Relative Humidity")) +  # Scale RH to match Temp range
  scale_y_continuous(
    name = "Temperature",
    sec.axis = sec_axis(~ . / 0.5, name = "Relative Humidity (%)")
  ) +
  labs(x = "Date", color = "") +
  theme_minimal()

print(Temp_plot)

##Precipitation##############################################
precipitation$Time <- as.Date(precipitation$Time, format = "%d.%m.%Y")

Precp_plot <- ggplot(precipitation,
                     aes(
                       x = Time,
                       y = `Precipitation (24 h)`
                     )) +
  geom_col(fill = "#619CFF") +
  theme_minimal() + 
  labs(x = "Date", y = "Precipitation (mm)")
plot(Precp_plot)

sum <- filter(summary_soil, Block =="D") |>
summarise(meansoil = mean(SD_of_Means))
print(sum)
