library(ggplot2)
library(dplyr)
library(ggforce)
library(lubridate)
library(ggpubr)
library(stringr)
library(patchwork)

mxaf_run1 <- read.csv("raw_data/Qpcr mxaf run 1/TERRA mxaf spring C+W -  Quantification Cq Results.csv")
mxaf_run2 <- read.csv("raw_data/Qpcr mxaf run 2/MXaf eva green -  Quantification Cq Results.csv")
bacteria_16s <-read.csv("raw_data/qpcr_WC_spring1/TERRA spring plate 1 WC -  Quantification Cq Results.csv")
pmoa <-read.csv("raw_data/Pmoa qpcr/TERRA PMOA test run C+W -  Quantification Cq Results.csv")
Archaea_16S <- read.csv("raw_data/Archaea qpcr/806r spring C + -  Quantification Cq Results.csv")

####16S Bacteria######################################################################
#modify df 
bacteria_16s <- mutate(bacteria_16s,
       Site = case_when(
         str_detect(Sample, regex("c", ignore_case = TRUE)) ~ "Control",
         str_detect(Sample, regex("w", ignore_case = TRUE)) ~ "Warming"))
bacteria_16s <- mutate(bacteria_16s,
                     Block = case_when(
                       str_detect(Sample, regex("d", ignore_case = TRUE)) ~ "Dryas",
                       str_detect(Sample, regex("m", ignore_case = TRUE)) ~ "Moss"))
bacteria_16s <- filter(bacteria_16s,
                     Biological.Set.Name == "16S")

#Make a boxplot
bac_plot <- ggplot(bacteria_16s,
                          aes(
                            x = Site,
                            y = Log.Starting.Quantity,
                            colour = Block
                          )) +
  geom_boxplot()
print(bac_plot)

anova16s <- aov(Log.Starting.Quantity ~ Site * Block, data = bacteria_16s)
summary(anova16s)

bacteria_16s_summary <- bacteria_16s %>%
  group_by(Site, Block) %>%
  summarise(
    mean_log_qty = mean(Log.Starting.Quantity, na.rm = TRUE),
    sd_log_qty = sd(Log.Starting.Quantity, na.rm = TRUE),
    n = n()
  )

print(bacteria_16s_summary)


###MxaF##############################################################################
#modify df run 1 & 2
mxaf_run1 <- mutate(mxaf_run1,
                       Site = case_when(
                         str_detect(Sample, regex("c", ignore_case = TRUE)) ~ "Control",
                         str_detect(Sample, regex("w", ignore_case = TRUE)) ~ "Warming"))
mxaf_run1 <- mutate(mxaf_run1,
                       Block = case_when(
                         str_detect(Sample, regex("d", ignore_case = TRUE)) ~ "Dryas",
                         str_detect(Sample, regex("m", ignore_case = TRUE)) ~ "Moss"))
mxaf_run1_filtered <- filter(mxaf_run1, str_detect(Content, "Unkn"))

mxaf_run2 <- mutate(mxaf_run2,
                    Site = case_when(
                      str_detect(Sample, regex("c", ignore_case = TRUE)) ~ "Control",
                      str_detect(Sample, regex("w", ignore_case = TRUE)) ~ "Warming"))
mxaf_run2 <- mutate(mxaf_run2,
                    Block = case_when(
                      str_detect(Sample, regex("d", ignore_case = TRUE)) ~ "Dryas",
                      str_detect(Sample, regex("m", ignore_case = TRUE)) ~ "Moss"))
mxaf_run2_filtered <- filter(mxaf_run2, str_detect(Content, "Unkn"))



#simple plots
mxaf_plot_1 <- ggplot(mxaf_run1_filtered,
                   aes(
                     x = Site,
                     y = Log.Starting.Quantity,
                     colour = Block
                   )) +
  geom_boxplot()

mxaf_plot_2 <- ggplot(mxaf_run2_filtered,
                      aes(
                        x = Site,
                        y = Log.Starting.Quantity,
                        colour = Block
                      )) +
  geom_boxplot()

combined_plot <- mxaf_plot_1 + mxaf_plot_2 +
  plot_layout(ncol = 2) & # Arrange plots in one column
  scale_y_continuous(limits = range(c(mxaf_run1_filtered$Log.Starting.Quantity, mxaf_run2_filtered$Log.Starting.Quantity)))
print(combined_plot)

print(mxaf_plot_1)
print(mxaf_plot_2)


anovamxaf1 <- aov(Log.Starting.Quantity ~ Site * Block, data = mxaf_run1_filtered)
summary(anovamxaf1)

anovamxaf2 <- aov(Log.Starting.Quantity ~ Site * Block, data = mxaf_run2_filtered)
summary(anovamxaf2)

mxaf1summary <- mxaf_run1_filtered %>%
  group_by(Site, Block) %>%
  summarise(
    mean_log_qty = mean(Log.Starting.Quantity, na.rm = TRUE),
    sd_log_qty = sd(Log.Starting.Quantity, na.rm = TRUE),
    n = n()
  )

mxaf2summary <- mxaf_run2_filtered %>%
  group_by(Site, Block) %>%
  summarise(
    mean_log_qty = mean(Log.Starting.Quantity, na.rm = TRUE),
    sd_log_qty = sd(Log.Starting.Quantity, na.rm = TRUE),
    n = n()
  )

print(mxaf1summary)
print(mxaf2summary)
###PMOA#################################################################################
pmoa_filtered <- mutate(pmoa,
                    Site = case_when(
                      str_detect(Sample, regex("c", ignore_case = TRUE)) ~ "Control",
                      str_detect(Sample, regex("w", ignore_case = TRUE)) ~ "Warming"))
pmoa_filtered <- mutate(pmoa_filtered,
                    Block = case_when(
                      str_detect(Sample, regex("d", ignore_case = TRUE)) ~ "Dryas",
                      str_detect(Sample, regex("m", ignore_case = TRUE)) ~ "Moss"))
pmoa_filtered <- filter(pmoa_filtered, str_detect(Content, "Unkn"))

pmoa_plot <- ggplot(pmoa_filtered,
                      aes(
                        x = Site,
                        y = Log.Starting.Quantity,
                        colour = Block
                      )) +
  geom_boxplot()
print(pmoa_plot)

###Archaea##############################################################################
archaea_filtered <- mutate(Archaea_16S,
                        Site = case_when(
                          str_detect(Sample, regex("c", ignore_case = TRUE)) ~ "Control",
                          str_detect(Sample, regex("w", ignore_case = TRUE)) ~ "Warming"))
archaea_filtered <- mutate(archaea_filtered,
                        Block = case_when(
                          str_detect(Sample, regex("d", ignore_case = TRUE)) ~ "Dryas",
                          str_detect(Sample, regex("m", ignore_case = TRUE)) ~ "Moss"))
archaea_filtered <- filter(archaea_filtered, str_detect(Content, "Unkn"))

archaea_plot <- ggplot(archaea_filtered,
                    aes(
                      x = Site,
                      y = Log.Starting.Quantity,
                      colour = Block
                    )) +
  geom_boxplot()
print(archaea_plot)

anovaarchaea <- aov(Log.Starting.Quantity ~ Site * Block, data = archaea_filtered)
summary(anovaarchaea)
summarise()

archaeasummary <- archaea_filtered %>%
  group_by(Site, Block) %>%
  summarise(
    mean_log_qty = mean(Log.Starting.Quantity, na.rm = TRUE),
    sd_log_qty = sd(Log.Starting.Quantity, na.rm = TRUE),
    n = n()
  )

print(archaeasummary)

###Link gene copies to fluxes################################################
#run plots total fluxes for df
CH4_fluxes_together_sample <- mutate(fluxes_CH4_29,
  Sample = paste0(BLOCK, PLOT_ID)) |>
  filter(TYPE =="C") |>
  mutate(flux = flux/3600) |>
  filter(!is.na(flux))

#16S##
bac_ch4_flux <- left_join(CH4_fluxes_together_sample, bacteria_16s) |>
  filter(Log.Starting.Quantity > 5)

plot1 <- ggplot(bac_ch4_flux,
               aes(
                 x=Log.Starting.Quantity,
                 y=totalflux
               )) +
  geom_point()
print(plot1)

#MxaF##
mxaf_run1_ch4_flux <- left_join(CH4_fluxes_together_sample, mxaf_run1_filtered) |>
  filter(Log.Starting.Quantity > 3.5)

# Plot 1 with trendline and R^2 value
mxaf_ch4_plot1 <- ggplot(mxaf_run1_ch4_flux,
                         aes(
                           x=Log.Starting.Quantity,
                           y=flux
                         )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = '#619CFF') + # Add trendline
  labs(x = "MxaF starting quantity (log10)", y = "Flux (µmol/m2/s)") +
  annotate("text", x = min(mxaf_run1_ch4_flux$Log.Starting.Quantity), y = max(mxaf_run1_ch4_flux$flux),
           label = paste("R^2 = ", round(summary(lm(flux ~ Log.Starting.Quantity, data = mxaf_run1_ch4_flux))$r.squared, 2)),
           hjust = 0, vjust = 2, color = "#619CFF") # Add R^2 value

print(mxaf_ch4_plot1)

mxaf_run2_ch4_flux <- left_join(CH4_fluxes_together_sample, mxaf_run2_filtered) |>
  filter((!is.na(Log.Starting.Quantity)))

# Plot 2 with trendline and R^2 value
mxaf_ch4_plot2 <- ggplot(mxaf_run2_ch4_flux,
                         aes(
                           x=Log.Starting.Quantity,
                           y=flux
                         )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = '#619CFF') + # Add trendline
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = min(mxaf_run2_ch4_flux$Log.Starting.Quantity), y = max(mxaf_run2_ch4_flux$flux),
           label = paste("R^2 = ", round(summary(lm(flux ~ Log.Starting.Quantity, data = mxaf_run2_ch4_flux))$r.squared, 2)),
           hjust = 0, vjust = 2, color = "#619CFF") # Add R^2 value

print(mxaf_ch4_plot2)

# Combined plot with shared y-axis and x-axis limits
combined_plot_flux <- mxaf_ch4_plot1 + mxaf_ch4_plot2 +
  plot_layout(ncol = 2) &
  scale_y_continuous(limits = range(c(mxaf_run1_ch4_flux$flux, mxaf_run2_ch4_flux$flux))) &
  scale_x_continuous(limits = range(c(mxaf_run1_filtered$Log.Starting.Quantity, mxaf_run2_filtered$Log.Starting.Quantity)))

print(combined_plot_flux)

