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
library(patchwork)

fluxes_CH4_all <- read.csv("clean_data/fluxes_CH4.csv")
fluxes_CO2_all <- read.csv("clean_data/fluxes_CO2.csv")

#Note, output of fluxes is mmol/m2/h for CO2 and µmol/m2/h for CH4

# Modify dataframes for plotting
fluxes_CH4_all <- fluxes_CH4_all |>
  #filter(flux > -500 & flux < 3000) |>
  mutate(flux = flux/3600) |> #modifiy output to µmol/m2/s
  mutate(week_number = week(datetime)) |> #add a column with week number
  filter(TYPE == "C") #in CH4 only care about Cap measurements


# modified dataframe for CO2
fluxes_CO2_all <- fluxes_CO2_all |>
  filter(flux > -100 & flux < 100) |> #too high fluxes are filted
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
  labs(y = 'CH4 flux (µmol/m2/s)', color = "Block", x = "") +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID)) 

print(CH4_fluxes_plot)

#anova
anova_ch4 <- aov(flux ~ SITE, data = fluxes_CH4_all)
summary(anova_ch4)

#anova only per treatment
data_moss <- subset(fluxes_CH4_all, SITE == "Moss")
data_dryas <- subset(fluxes_CH4_all, SITE == "Dryas")

summarise(data_dryas, 
          mean_flux = mean(flux, na.rm = TRUE),
          sd_flux = sd(flux, na.rm = TRUE))

summarise(data_moss, 
          mean_flux = mean(flux, na.rm = TRUE),
          sd_flux = sd(flux, na.rm = TRUE))


anova_moss <- aov(flux ~ PLOT_ID, data = data_moss)
summary(anova_moss)
anova_dryas <- aov(flux ~ PLOT_ID, data = data_dryas)
summary(anova_dryas)

#ttest between control and treatments
pairwise_t_test_moss <- pairwise.t.test(data_moss$flux, data_moss$PLOT_ID, p.adjust.method = "bonferroni")
print(pairwise_t_test_moss)
# Dryas site
pairwise_t_test_dryas <- pairwise.t.test(data_dryas$flux, data_dryas$PLOT_ID, p.adjust.method = "bonferroni")
print(pairwise_t_test_dryas)



# Create CO2 flux per Site_ID
CO2_fluxes_plot <- ggplot(fluxes_CO2_all,
                          aes(
                            x = SITE,
                            y = flux,
                            colour = SITE
                          )) +
  geom_sina() +
  labs(y = 'CO2 flux (mmol/m2/h)', x ="", color = "Block") +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID))

print(CO2_fluxes_plot)


#Anova Dark
data_moss_co2 <- subset(fluxes_CO2_all, SITE == "Moss") |>
  subset(TYPE == "D")
data_dryas_co2 <- subset(fluxes_CO2_all, SITE == "Dryas") |>
  subset(TYPE == "D")

summarise(data_dryas_co2, 
          mean_flux = mean(flux, na.rm = TRUE),
          sd_flux = sd(flux, na.rm = TRUE))

summarise(data_moss_co2, 
          mean_flux = mean(flux, na.rm = TRUE),
          sd_flux = sd(flux, na.rm = TRUE))


fluxes_CO2_D <- subset(fluxes_CO2_all, TYPE == "D")

anova_co2 <- aov(flux ~ SITE, data = fluxes_CO2_D)
summary(anova_co2)

#anova Light
data_moss_co2L <- subset(fluxes_CO2_all, SITE == "Moss") |>
  subset(TYPE == "L")
data_dryas_co2L <- subset(fluxes_CO2_all, SITE == "Dryas") |>
  subset(TYPE == "L")

summarise(data_dryas_co2L, 
          mean_flux = mean(flux, na.rm = TRUE),
          sd_flux = sd(flux, na.rm = TRUE))

summarise(data_moss_co2L, 
          mean_flux = mean(flux, na.rm = TRUE),
          sd_flux = sd(flux, na.rm = TRUE))


fluxes_CO2_L <- subset(fluxes_CO2_all, TYPE == "L")

anova_co2 <- aov(flux ~ SITE, data = fluxes_CO2_L)
summary(anova_co2)

#Ch4 fluxes over time per treatment
CH4_fluxes_over_time <- ggplot(fluxes_CH4_all,
                               aes(
                                 x = week_number,
                                 y = flux,
                                 colour = SITE
                               )) +
  geom_point() +
  facet_grid(rows = vars(TYPE), cols = vars(PLOT_ID)) +
  ylim(-1.2, 1.2) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(
    aes(label = paste(..rr.label.., sep = "~~~")),
    label.y = c(1, 1.1),
    show.legend = FALSE
  ) +
  stat_regline_equation(
    aes(label = paste(..eq.label.., sep = "~~~")),
    label.y = c(-1, -1.1),
    show.legend = FALSE
  ) +
  labs(y = 'CH4 flux (µmol/m2/s)',
       x = 'Week number',
       color = "Block") +
  scale_x_continuous(labels = function(x) as.integer(x))

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
  labs(y = 'CO2 flux (mmol/m2/s)', 
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
  labs(y = 'CH4 flux (µmol/m2/s)', color = "Block", x = "Soil temperature (°C)") +
  geom_smooth(method = "lm", se= FALSE) +
  stat_regline_equation(
    aes(label = paste(..rr.label.., sep = "~~~")),
    label.y = c(0.855, 1.055),
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

#add fluxes from each plot together
CH4_fluxes_together <- fluxes_CH4_all |>
  group_by(SITE, BLOCK, PLOT_ID) |>
  summarize(
    totalflux = sum(flux, na.rm = TRUE),
    averagetemp = mean(T_out, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(WARMING = ifelse(PLOT_ID %in% c("W", "WG"), "otc", "control")) |>
  mutate(GRUBBING = ifelse(PLOT_ID %in% c("G", "WG", "RG"), "grubbing", "control")) |>
  mutate(RAIN = ifelse(PLOT_ID %in% c("R", "RG"), "ros", "control"))


#create plot fluxes per treatment
CH4_fluxes_together_plot <- ggplot(CH4_fluxes_together,
                                   aes(
                                     x = WARMING,
                                     y = totalflux,
                                     colour = SITE
                                   )) +
  geom_boxplot() + 
  labs(
    y = "total methane flux",
    x = "",
    
    
    
    
  
  )
print(CH4_fluxes_together_plot)


#plot that shows temperature per week in otc and non otc plots
Temp_per_site <- ggplot(fluxes_CH4_all,
                        aes(
                          x = WARMING,
                          y = T_out,
                          colour = WARMING
                        )) +
  geom_boxplot() + 
  facet_wrap(facets = vars(week_number),
             labeller = labeller(week_number = function(x) paste("Week", x))) +
  labs(y = 'Soil temperature (°C)', color = "Treatment", x = "")

print(Temp_per_site)

#temperature per flux per site
fluxes_CH4_all_indv <- fluxes_CH4_all |>
  mutate(ID = paste(PLOT_ID, BLOCK))

CH4_fluxes_Temp_indv <- ggplot(fluxes_CH4_all_indv,
                          aes(
                            x = T_out,
                            y = flux,
                            colour = ID
                          )) +
  geom_point() +
  labs(y = 'CH4 flux (µmol/m2/s)') +
  geom_smooth(method = "lm", se= FALSE) +
  stat_regline_equation(
    aes(label = paste(..rr.label.., sep = "~~~")),
    show.legend = FALSE
  ) 
plot(CH4_fluxes_Temp_indv)

CH4_fluxes_over_time / ( CH4_fluxes_plot | CH4_fluxes_Temp)

