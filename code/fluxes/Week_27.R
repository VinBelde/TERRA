# 1 file per campaign
# lost Par sensor data for day 3 and was unable to plan it in again, so missing data

library(tidyverse)
library(dataDownloader)
library(lubridate)
library(fluxible)

source("code/fluxes/fun.R")

# read them, eventually select columns, rename coloumns (depends on the logger settings) using function 
# use read_delim or read_csv
# read also the field record with the time of each measurements and the metadata

CO2_CH4_1 <- import_CO2_CH4("raw_data/week27/CO2_CH4_2024-07-01.data")
CO2_CH4_2 <- import_CO2_CH4("raw_data/week27/CO2_CH4_2024-07-02.data")

# let's put everything together

CO2_CH4 <- full_join(CO2_CH4_1, CO2_CH4_2)

str(CO2_CH4) # just checking

# now we need to import the data from the PAR_temp logger

PAR_temp_1 <- import_PAR_temp("raw_data/week27/PAR_Temp_2024-07-01.dat")
PAR_temp_2 <- import_PAR_temp("raw_data/week27/PAR_Temp_2024-07-02.dat")

PAR_temp <- full_join(PAR_temp_1, PAR_temp_2)

str(PAR_temp)

fieldnotes <- read_csv("raw_data/week27/Fieldnotes_week_27.csv") |>
  mutate(
    datetime_start = ymd_hms(paste(DATE, START_TIME))
  )

head(fieldnotes)

# merge data from both loggers

conc_df <- left_join(CO2_CH4, PAR_temp)

str(conc_df)

# use fluxible to calculate fluxes

# use flux_match to match the field record and the concentration data

# seperate CO2 and CH4 files

# here add start and end cuts and correct time mismatch

conc_co2_27 <- flux_match(conc_df, fieldnotes, conc_col = "CO2", start_col = "datetime_start", measurement_length = 180, time_diff = -30, startcrop = 25)

conc_ch4_27 <- flux_match(conc_df, fieldnotes, conc_col = "CH4", start_col = "datetime_start", measurement_length = 180, time_diff = -30, startcrop = 25)

conc_co2_27 <- conc_co2_27 |>
  mutate(
    PAR_in_chamber = case_when(
      TYPE == "C" ~ NA_real_, # the PAR sensor was still inside the transparent chamber while we were doing the measurements with the cap
      TYPE != "C" ~ PAR_in_chamber
    )
  )

conc_ch4_27 <- conc_ch4_27 |>
  mutate(
    PAR_in_chamber = case_when(
      TYPE == "C" ~ NA_real_, # the PAR sensor was still inside the transparent chamber while we were doing the measurements with the cap
      TYPE != "C" ~ PAR_in_chamber
    )
  )
# fux_fitting to fit a model to the concentration over time and calculate a slope

slopes_co2_27 <- flux_fitting(conc_co2_27, fit_type = "exp")
str(slopes_co2_27)
slopes_ch4_27 <- flux_fitting(conc_ch4_27, fit_type = "exp")

# flux_quality and flux_plot to check the quality and see if we need to modify anything

slopes_co2_27 <- flux_quality(slopes_co2_27, fit_type = "exp", weird_fluxes_id = c(
  11, #intercept does not match fit
  120 #not 100% sure but fit does not match data too well
))

slopes_ch4_27 <- flux_quality(slopes_ch4_27, fit_type = "exp", ambient_conc = 2000)

#Lower ylim ch4 necessary after id 54
flux_plot(slopes_co2_27, f_plotname = "week27_co2", f_ylim_upper = 600, output = "pdfpages")
flux_plot(slopes_ch4_27, f_plotname = "week27_ch4", f_ylim_lower = 1970, f_ylim_upper = 2025, y_text_position = 2000, output = "pdfpages")


# flux_calc to calculate the fluxes
flux_co2_27_chamber <- slopes_co2_27 |>
  filter(TYPE != "C",
         f_cut == "keep" # we had a cut in quality
  ) |>
  flux_calc(
    slope_col = "f_slope_corr",
    chamber_volume = 6.283, #need to check and add tube volumes
    plot_area = 0.314,
    temp_air_col = "T_in_chamber",
    cols_ave = c("PAR_in_chamber", "PAR_out", "T_out"),
    cols_keep = c("f_start", "SITE", "BLOCK", "PLOT_ID", "WARMING", "GRUBBING", "RAIN", "TYPE")
  ) |>
  mutate(
    chamber = case_when(
      TYPE == "L" ~ "transparent_chamber",
      TYPE == "D" ~ "dark_chamber"
    ),
    gas = "CO2"
  )

flux_ch4_27_chamber <- slopes_ch4_27 |>
  filter(
    TYPE != "C") |>
  mutate(
    slope_ppm = f_slope_corr * 001 # we need to feed ppm to the function
  ) |>
  flux_calc(
    slope_col = "slope_ppm",
    chamber_volume = 6.283, #need to check and add tube volumes
    plot_area = 0.314,
    temp_air_col = "T_in_chamber",
    cols_ave = c("PAR_in_chamber", "PAR_out", "T_out"),
    cols_keep = c("f_start", "SITE", "BLOCK", "PLOT_ID", "WARMING", "GRUBBING", "RAIN", "TYPE")
  ) |>
  mutate(
    chamber = case_when(
      TYPE == "L" ~ "transparent_chamber",
      TYPE == "D" ~ "dark_chamber"
    ),
    gas = "CH4"
  )

flux_co2_27_tube <- slopes_co2_27 |>
  filter(TYPE == "C",
         f_cut == "keep" # we had a cut in quality
  ) |>
  flux_calc(
    slope_col = "f_slope_corr",
    chamber_volume = 1.178, #need to check and add tube volumes
    plot_area = 0.078,
    temp_air_col = "T_in_chamber",
    cols_ave = c("PAR_in_chamber", "PAR_out", "T_out"),
    cols_keep = c("f_start", "SITE", "BLOCK", "PLOT_ID", "WARMING", "GRUBBING", "RAIN", "TYPE")
  ) |>
  mutate(
    chamber = "dark_tube",
    gas = "CO2"
  )

flux_ch4_27_tube <- slopes_ch4_27 |>
  filter(TYPE == "C") |>
  mutate(
    slope_ppm = f_slope_corr * 001 # we need to feed ppm to the function
  ) |>
  flux_calc(
    slope_col = "slope_ppm",
    chamber_volume = 1.178, #need to check and add tube volumes
    plot_area = 0.078,
    temp_air_col = "T_in_chamber",
    cols_ave = c("PAR_in_chamber", "PAR_out", "T_out"),
    cols_keep = c("f_start", "SITE", "BLOCK", "PLOT_ID", "WARMING", "GRUBBING", "RAIN", "TYPE")
  ) |>
  mutate(
    chamber = "dark_tube",
    gas = "CH4"
  )


# we regroup everything

fluxes_27 <- full_join(flux_ch4_27_chamber, flux_co2_27_chamber) |>
  full_join(flux_ch4_27_tube) |>
  full_join(flux_co2_27_tube)


# let's make a plot
fluxes_27  |>
  group_by(PLOT_ID, SITE, gas, chamber) |>
  summarise(
    flux_ave = mean(flux),
    sd_flux = sd(flux)
  ) |>
  mutate(
    lower = flux_ave - sd_flux,
    upper = flux_ave + sd_flux
  ) |>
  ggplot() +
  # geom_point(aes(PLOT_ID, flux_ave, color = SITE)) +
  geom_col(aes(PLOT_ID, flux_ave, fill = SITE), position = "dodge") +
  geom_errorbar(aes(x = PLOT_ID, ymin = lower, ymax = upper, color = SITE), position = "dodge") +
  facet_grid(gas ~ chamber, scale = "free")

# this is a very ugly plot, it should be improved (color blind palette and co)

# we create one df per gas and change the unit for CH4

fluxes_CO2_27 <- fluxes_27 |>
  filter(
    gas == "CO2"
  ) |>
  arrange(f_start) |>
  select(!f_fluxID) # we remove flux_ID because it will be repeated with the next batch of data


fluxes_CH4_27 <- fluxes_27 |>
  filter(
    gas == "CH4"
  ) |>
  mutate(
    flux = flux * 1000 # converting to micromol
  ) |>
  arrange(f_start) |>
  select(!f_fluxID) # we remove flux_ID because it will be repeated with the next batch of data
