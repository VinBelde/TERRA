# I am not sure if we want to have one file per campaign or do everything with the same arguments
# Since the conditions might change I feel that one file per campaign can be smart

library(tidyverse)
library(dataDownloader)
library(lubridate)
library(fluxible)

source("code/fluxes/fun.R")

# download the data from OSF using data dataDownloader
get_file(node = "rba87",
         file = "Fieldnotes.csv",
         path = "raw_data",
         remote_path = "raw_data/ecosystem_fluxes")

get_file(node = "rba87",
         file = "CO2_CH4_2024-06-17.data",
         path = "raw_data/week25",
         remote_path = "raw_data/ecosystem_fluxes/Week_25")

get_file(node = "rba87",
         file = "CO2_CH4_2024-06-18.data",
         path = "raw_data/week25",
         remote_path = "raw_data/ecosystem_fluxes/Week_25")
get_file(node = "rba87",
         file = "CO2_CH4_2024-06-19.data",
         path = "raw_data/week25",
         remote_path = "raw_data/ecosystem_fluxes/Week_25")

get_file(node = "rba87",
         file = "PAR_Temp_2024-06-17.dat",
         path = "raw_data/week25",
         remote_path = "raw_data/ecosystem_fluxes/Week_25")

get_file(node = "rba87",
         file = "PAR_Temp_2024-06-18.dat",
         path = "raw_data/week25",
         remote_path = "raw_data/ecosystem_fluxes/Week_25")

get_file(node = "rba87",
         file = "PAR_Temp_2024-06-19.dat",
         path = "raw_data/week25",
         remote_path = "raw_data/ecosystem_fluxes/Week_25")

# read them, eventually select columns, rename coloumns (depends on the logger settings)
# use read_delim or read_csv
# read also the field record with the time of each measurements and the metadata

CO2_CH4_1 <- read_delim("raw_data/week25/CO2_CH4_2024-06-17.data", delim = "\t", skip = 5) |>
  filter(DATAH != "DATAU") |> #removing the line with the units
  mutate(
    DATE = ymd(DATE),
    TIME = hms(TIME),
    CO2 = as.double(CO2),
    CH4 = as.double(CH4),
    datetime = ymd_hms(paste(DATE, TIME))
  ) |>
  select(datetime, CH4, CO2) # I am removing the remark column. Since it was not always sync with the measurement it is annoying in the rest

head(CO2_CH4_1)

# I wrote a function for that, no way I am copy pasting those lines everytime

CO2_CH4_2 <- import_CO2_CH4("raw_data/week25/CO2_CH4_2024-06-18.data")
CO2_CH4_3 <- import_CO2_CH4("raw_data/week25/CO2_CH4_2024-06-19.data")

# let's put everything together

CO2_CH4 <- full_join(CO2_CH4_1, CO2_CH4_2) |>
  full_join(CO2_CH4_3)

str(CO2_CH4) # just checking

# now we need to import the data from the PAR_temp logger

PAR_temp_1 <- read_delim("raw_data/week25/PAR_Temp_2024-06-17.dat", delim = ",", skip = 1) |>
  rename(
    datetime = TMSTAMP
  ) |>
  select(datetime, PAR_in_chamber, PAR_out, T_in_chamber, T_out)

head(PAR_temp_1)

# again, a function

PAR_temp_2 <- import_PAR_temp("raw_data/week25/PAR_Temp_2024-06-18.dat")
PAR_temp_3 <- import_PAR_temp("raw_data/week25/PAR_Temp_2024-06-19.dat")

PAR_temp <- full_join(PAR_temp_1, PAR_temp_2) |>
  full_join(PAR_temp_3)

str(PAR_temp)

fieldnotes <- read_csv("raw_data/Fieldnotes.csv") |>
  mutate(
    datetime_start = ymd_hms(paste(DATE, START_TIME))
  )

head(fieldnotes)

# merge data from both loggers?
# yes

conc_df <- left_join(CO2_CH4, PAR_temp)

str(conc_df)

# just some graph to check that the data are complete

conc_df |>
  # select(!remark) |>
  pivot_longer(cols = c(CH4, CO2, PAR_in_chamber, PAR_out, T_in_chamber, T_out), names_to = "measurement") |>
  ggplot(aes(datetime, value)) +
  geom_point() +
  facet_grid(measurement~., scales = "free") +
  scale_x_datetime(date_breaks = "5 hour", minor_breaks = "1 hour")



# use fluxible to calculate fluxes

# use flux_match to match the field record and the concentration data

# we should make two separate files for CO2 and CH4

# here you need to think if we need to cut the measurements, or if there was a time mismatch at some point

conc_co2_25 <- flux_match(conc_df, fieldnotes, conc_col = "CO2", start_col = "datetime_start", measurement_length = 180)

conc_ch4_25 <- flux_match(conc_df, fieldnotes, conc_col = "CH4", start_col = "datetime_start", measurement_length = 180)

conc_co2_25 <- conc_co2_25 |>
  mutate(
    PAR_in_chamber = case_when(
      TYPE == "C" ~ NA_real_, # the PAR sensor was still inside the transparent chamber while we were doing the measurements with the cap
      TYPE != "C" ~ PAR_in_chamber
    )
  )

conc_ch4_25 <- conc_ch4_25 |>
  mutate(
    PAR_in_chamber = case_when(
      TYPE == "C" ~ NA_real_, # the PAR sensor was still inside the transparent chamber while we were doing the measurements with the cap
      TYPE != "C" ~ PAR_in_chamber
    )
  )
# fux_fitting to fit a model to the concentration over time and calculate a slope

slopes_co2_25 <- flux_fitting(conc_co2_25, fit_type = "exp", start_cut = 20)
str(slopes_co2_25)
slopes_ch4_25 <- flux_fitting(conc_ch4_25, fit_type = "exp")

# flux_quality and flux_plot to check the quality and see if we need to modify anything

slopes_co2_25 <- flux_quality(slopes_co2_25, fit_type = "exp", slope_col = "f_slope_tz")

slopes_ch4_25 <- flux_quality(slopes_ch4_25, fit_type = "exp", slope_col = "f_slope_tz", ambient_conc = 2000)

# table showing percentage of ok, zero and discard

quality_count_co2 <- slopes_co2_25 |>
  count(f_quality_flag) |>
  mutate(Percentage = (n / sum(n))*100)

quality_count_ch4 <- slopes_ch4_25 |>
  count(f_quality_flag) |>
  mutate(Percentage = (n / sum(n))*100)


flux_plot(slopes_co2_25, fit_type = "exp", f_plotname = "week25_co2", f_ylim_upper = 600)
flux_plot(slopes_ch4_25, fit_type = "exp", f_plotname = "week25_ch4", f_ylim_lower = 1995, f_ylim_upper = 2010, y_text_position = 2000)


# flux_calc to calculate the fluxes
flux_co2_25_chamber <- slopes_co2_25 |>
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

flux_ch4_25_chamber <- slopes_ch4_25 |>
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

flux_co2_25_tube <- slopes_co2_25 |>
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

flux_ch4_25_tube <- slopes_ch4_25 |>
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

fluxes_25 <- full_join(flux_ch4_25_chamber, flux_co2_25_chamber) |>
  full_join(flux_ch4_25_tube) |>
  full_join(flux_co2_25_tube)


# let's make a plot
fluxes_25  |>
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

fluxes_CO2 <- fluxes_25 |>
    filter(
        gas == "CO2"
    ) |>
    arrange(f_start) |>
    select(!f_fluxID) # we remove flux_ID because it will be repeated with the next batch of data


fluxes_CH4 <- fluxes_25 |>
    filter(
        gas == "CH4"
    ) |>
    mutate(
        flux = flux * 1000 # converting to micromol
    ) |>
    arrange(f_start) |>
    select(!f_fluxID) # we remove flux_ID because it will be repeated with the next batch of data


# need to upload to OSF: 1 file for CO2, 1 for CH4, continuous adding (function for that?)

# only for week 25

write_csv(fluxes_CO2, "clean_data/fluxes_CO2.csv")

write_csv(fluxes_CH4, "clean_data/fluxes_CH4.csv")


# from week 27

# download the previous fluxes files from OSF

# get_file(node = "rba87",
#          file = "fluxes_CO2.csv",
#          path = "clean_data",
#          remote_path = "ecosystem_fluxes")

# get_file(node = "rba87",
#          file = "fluxes_CH4.csv",
#          path = "clean_data",
#          remote_path = "ecosystem_fluxes")

# # read them

# fluxes_CO2_previous <- read_csv("clean_data/fluxes_CO2.csv", col_types = "dddTcccccccdddcc")

# fluxes_CH4_previous <- read_csv("clean_data/fluxes_CH4.csv", col_types = "dddTcccccccdddcc") 




# # full join with the one that was just produced
# # we add a distinct as a safety in case this is run several times (no duplicate of data)

# fluxes_CO2_to_upload <- bind_rows(fluxes_CO2, fluxes_CO2_previous) |>
#     distinct(f_start, gas, SITE, BLOCK, PLOT_ID, chamber, .keep_all = TRUE) |> # we do the distinct on non measured variables only to avoid issues with attributes on other columns
#     arrange(f_start)

# str(fluxes_CO2_to_upload) # important to check that the nb of rows matches what it should be

# fluxes_CH4_to_upload <- bind_rows(fluxes_CH4, fluxes_CH4_previous) |>
#     distinct(f_start, gas, SITE, BLOCK, PLOT_ID, chamber, .keep_all = TRUE) |> # we do the distinct on non measured variables only to avoid issues with attributes on other columns
#     arrange(f_start)

# str(fluxes_CH4_to_upload) # important to check that the nb of rows matches what it should be


# # now we write the csv files and upload them to OSF
# write_csv(fluxes_CO2_to_upload, "clean_data/fluxes_CO2.csv")

# write_csv(fluxes_CH4_to_upload, "clean_data/fluxes_CH4.csv")
