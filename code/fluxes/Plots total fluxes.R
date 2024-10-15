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

# Modify dataframes for plotting
fluxes_CH4_all <- fluxes_CH4_all |>
  filter(flux > -500 & flux < 3000) |>
  mutate(flux = flux/3600) |> #modifiy output to µmol/m2/s
  mutate(week_number = week(datetime)) |> #add a column with week number
  filter(TYPE == "C") #in CH4 only care about Cap measurements


# modified dataframe for CO2
fluxes_CO2_all <- fluxes_CO2_all |>
  filter(flux > -10 & flux < 10) |> #too high fluxes are filted
  mutate(week_number = week(datetime)) |>
  filter(TYPE != "C") |> #only care about L and D
  group_by(SITE, BLOCK, PLOT_ID, week_number) |> #create gep (gross ecosystem production), here it shows CO2 fixation if negative or emission if positive
    mutate(GEP = ifelse(sum(TYPE == "L") == 1 & sum(TYPE == "D") == 1, 
                    flux[TYPE == "L"] - flux[TYPE == "D"], 
                    NA)) |>
  ungroup()

# Create CH4 flux per Site_ID
CH4_fluxes_plot <- ggplot(fluxes_CH4_all,
                          aes(
                            x = SITE,
                            y = flux,
                            colour = SITE
                          )) +
  geom_sina() +
  labs(y = 'CH4 flux (µmol/m2/s)') +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID))

print(CH4_fluxes_plot)

# Create CO2 flux per Site_ID
CO2_fluxes_plot <- ggplot(fluxes_CO2_all,
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
CH4_fluxes_over_time <- ggplot(fluxes_CH4_all,
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
CO2_fluxes_over_time <- ggplot(fluxes_CO2_all,
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
  labs(y = 'CH4 flux (mmol/m2/s)', 
       x = 'Week number')
plot(CO2_fluxes_over_time)

#CH4 fluxes per temperature
CH4_fluxes_Temp <- ggplot(fluxes_CH4_all,
                          aes(
                            x = T_out,
                            y = flux,
                            colour = SITE
                          )) +
  geom_point() +
  labs(y = 'CH4 flux (µmol/m2/s)') +
  geom_smooth(method = "lm", se= FALSE) +
  stat_regline_equation(
    aes(label = paste(..rr.label.., sep = "~~~")),
    label.y = c(0.115, 0.125),
    show.legend = FALSE
  ) 
plot(CH4_fluxes_Temp)

#CO2 fluxes per par
CO2_fluxes_par <- ggplot(fluxes_CO2_all,
                         aes(
                           x = PAR_out,
                           y = GEP,
                           colour = SITE
                         )) + 
  geom_point() +
  geom_smooth(method = "lm", se= FALSE) +
  stat_regline_equation(
    aes(label = paste(..rr.label.., sep = "~~~")),
    label.y = c(3.5, 2.5),
    show.legend = FALSE
  ) 
plot(CO2_fluxes_par)
