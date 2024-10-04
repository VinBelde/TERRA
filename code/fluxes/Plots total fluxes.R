#make one big file of CH4 fluxes
fluxes_CH4_all <- full_join(fluxes_CH4_25, fluxes_CH4_27) |>
  full_join(fluxes_CH4_29) |>
  full_join(fluxes_CH4_31) |>
  full_join(fluxes_CH4_33) |>
  full_join(fluxes_CH4_35)

#export this df to csv
write.csv(fluxes_CH4_all, "clean_data/fluxes_CH4.csv")

#same for CO2 fluxes
fluxes_CO2_all <- full_join(fluxes_CO2_25, fluxes_CO2_27) |>
  full_join(fluxes_CO2_29) |>
  full_join(fluxes_CO2_31) |>
  full_join(fluxes_CO2_33) |>
  full_join(fluxes_CO2_35)

#export this df to csv
write.csv(fluxes_CO2_all, "clean_data/fluxes_CO2.csv")


##START IF NO CHANGES HAVE BEEN MADE TO DATA PROCESSING 
library(ggplot2)
library(dplyr)
library(ggforce)
library(lubridate)
library(ggpubr)

fluxes_CH4_all <- read.csv("clean_data/fluxes_CH4.csv")
fluxes_CO2_all <- read.csv("clean_data/fluxes_CO2.csv")

#Note, output of fluxes is mmol/m2/h for CO2 and µmol/m2/h for CH4
#Time to make some plots
#CH4 fluxes per plot per treatment

# Filter the data to include only 'flux' values between 0 and 5000
filtered_fluxes_CH4_all <- fluxes_CH4_all |>
  filter(flux > -500 & flux < 3000) |>
  mutate(flux = flux/3600)


# Create the plot with filtered data
CH4_fluxes_plot <- ggplot(filtered_fluxes_CH4_all,
                          aes(
                            x = SITE,
                            y = flux,
                            colour = SITE
                          )) +
  geom_sina() +
  labs(y = 'CH4 flux (µmol/m2/s)') +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID))

print(CH4_fluxes_plot)

#Same but CO2
filtered_fluxes_CO2_all <- fluxes_CO2_all |>
  filter(flux > -10 & flux < 10)

CO2_fluxes_plot <- ggplot(filtered_fluxes_CO2_all,
                          aes(
                            x = SITE,
                            y = flux,
                            colour = SITE
                          )) +
  geom_sina() +
  labs(y = 'CO2 flux (mmol/m2/h)') +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID))

print(CO2_fluxes_plot)


#Ch4 fluxes over time per treatment
fluxes_CH4_all_wk <- mutate(
  fluxes_CH4_all, 
  week_number = week(datetime)) |>
  mutate(flux = flux/3600)


CH4_fluxes_over_time <- ggplot(fluxes_CH4_all_wk,
                               aes(
                                 x = week_number,
                                 y = flux,
                                 colour = SITE
                               )) +
  geom_point() +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID)) +
  ylim(-0.5,0.5) +
  geom_smooth(method = "lm", se= FALSE) +
  stat_regline_equation(
    aes(label = paste(..rr.label.., sep = "~~~")),
    label.y = c(0.45, 0.35),
    show.legend = FALSE
  ) + 
  stat_regline_equation(
    aes(label = paste(..eq.label.., sep = "~~~")),
    label.y = c(-0.35, -0.45), 
    show.legend = FALSE
  ) +
  labs(y = 'CH4 flux (µmol/m2/s)', 
       x = 'Week number')
plot(CH4_fluxes_over_time)

#CO2 fluxes over time per treatment
fluxes_CO2_all_wk <- mutate(
  fluxes_CO2_all, 
  week_number = week(datetime))


CO2_fluxes_over_time <- ggplot(fluxes_CO2_all_wk,
                               aes(
                                 x = week_number,
                                 y = flux,
                                 colour = SITE
                               )) +
  geom_point() +
  ylim(-4,4) +
  geom_smooth(method = "lm", se= FALSE) +
  stat_regline_equation(
    aes(label = paste(..rr.label.., sep = "~~~")),
    label.y = c(3.5, 2.5),
    show.legend = FALSE
  ) + 
  stat_regline_equation(
    aes(label = paste(..eq.label.., sep = "~~~")),
    label.y = c(-2.5, -3.5), 
    show.legend = FALSE
  ) +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID)) +
  labs(y = 'CH4 flux (mmol/m2/h)', 
       x = 'Week number')
plot(CO2_fluxes_over_time)