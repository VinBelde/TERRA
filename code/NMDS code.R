library(ggplot2)
library(dplyr)
library(ggforce)
library(lubridate)
library(ggpubr)
library(tidyr)
library(readxl)
library(vegan)


#import data
ndvi_data <- read.csv("raw_data/Env_Data/NDVI-2024-09-20.csv")
soil_m_data <-read.csv("raw_data/Env_Data/SoilMoisture-2024-09-20.csv")
active_layer_data <- read.csv("raw_data/Env_Data/ALT-2024-09-20.csv")


##Distance plot################################################
#make long plots
long_ALT <- active_layer_data %>%
  pivot_longer(cols = c(R, C, G, W, WG, RG), names_to = "variable", values_to = "value")

long_ALT <- long_ALT %>%
  mutate(site_code = paste0(site, variable))


long_ALT <- long_ALT %>%
  group_by(site_code) %>%
  summarise(value = mean(value, na.rm = TRUE))

long_ALT <- long_ALT %>%
  rename(site = site_code) |>
  rename(ALT = value)

#ndvi
long_ndvi <- ndvi_data %>%
  pivot_longer(cols = c(R, C, G, W, WG, RG), names_to = "variable", values_to = "value")

long_ndvi <- long_ndvi %>%
  mutate(site_code = paste0(site, variable))


long_ndvi <- long_ndvi %>%
  group_by(site_code) %>%
  summarise(value = mean(value, na.rm = TRUE))

long_ndvi <- long_ndvi %>%
  rename(site = site_code) |>
  rename(NDVI = value)

#Soil moisture
soil_m_data <- soil_m_data |>
  mutate(across(c(R, C, G, W, WG, RG), as.numeric))

soil_m_data <- soil_m_data %>%
  mutate(main_site = sub("_.*", "", site))

long_SM <- soil_m_data %>%
  pivot_longer(cols = c(R, C, G, W, WG, RG), names_to = "variable", values_to = "value")

long_SM <- long_SM %>%
  mutate(site_code = paste0(main_site, variable))

long_SM <- long_SM %>%
  select(site = site_code, value) |>
  rename(SM = value)

long_SM <- long_SM |>
  group_by(site) |>
  summarise(SM = mean(SM, na.rm = TRUE))


######Join all plots together########################
All_env <- left_join(long_ALT, long_ndvi) |>
  left_join(long_SM)

#scale the data
Site_code <- All_env$site
Scaled_env <- scale(All_env[, -1])
Scaled_env <- data.frame(Site = Site_code, Scaled_env)

ENV_NMDS <- metaMDS(Scaled_env[, -1], distance = "euclidian", k = 2, trymax = 100) |>
  plot()

colors <- ifelse(substr(Scaled_env$Site, 1, 1) == "D", "red", "#619CFF")
points <- scores(ENV_NMDS, display = "sites")
points(points, col = colors, pch = 19)
text(points, labels = Scaled_env$Site, pos = 3, cex = 0.8)

