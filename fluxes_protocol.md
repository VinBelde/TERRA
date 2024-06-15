Protocol flux measurements TERRA project 2024
================
Joseph Gaudard

## General

- make sure the licor and your clock/phone are precisely synchronized
  and at the right time (also check the time zone of the licor)
- when turning up the Li-7810, it needs some minutes to warm up

## Connecting the chamber

- inlet at the top of the chamber, outlet at the bottom
- inlet and outlet as far as possible from each other
- setup the thermocouple inside the chamber

## Connecting the lid

- connect the tubes to the chamber tubes
- push the thermocouple inside
- make sure everything is air tight with the patafix

## Chamber measurements

- place the chamber on the plot
- make it airtight with the chain
- measure during 3 minutes
- do one measurement with the transparent chamber (NEE) and one with the
  chamber covered with a dark cloth (ER)

## Methane

- methane will be logged at the same time as CO<sub>2</sub>

## Lid measurements

- connect the lid to the licor instead of the chamber
- place the lid on the tubes without pushing too hard (to avoid pushing
  gases out of the soil)
- measure for 3 minutes

## Recording of measurements

- make sure to have a field record sheet stating the time at which the
  measurement was started, the plot ID, the type (NEE or ER) and the
  setup (lid or chamber)
- since CH<sub>4</sub> and CO<sub>2</sub> are measured simultaneously by
  the gas analyzer we do not need to precise what we are doing when

## Soil moisture

- take three measurements on the edge of the plots (east, north, west,
  in that order)

## Extracting data from Campbell logger (PAR and temperature sensors)

- Connect field computer to wifi CO2CHAMBER
- Open loggernet
- Click “connect”
- select station “CO2CHAMBER\_ WiFi”
- select Table1 in the rolling menu
- Click connect (top left)
- Check the station time stamp
- Click custom
- select Table1
- set File mode to “append to End of File”
- set File format to TOACI1
- Click “Start collection”

## Extracting data from the Li-7810

- connect to the wifi TG10-01782, password is licorenv
- in a browser go to <http://tg10-01782.local>
- click on the setting button, and then the downloading one
- specify start and end datetime, click export

## Naming convention

Data from Campbell logger: PAR_TEMP_yyyy-mm-dd

Data from Li-7810: CO2_CH4_yyyy-mm-dd

## Backup of data

- backup the data after each day of measurement
  <!-- - in case there is something weird in that last file, you can check the previous ones to see if it is better (corrupt/lost data)  -->
- raw data used for processing should be stored on a cloud (OSF;
  <https://osf.io/rba87/>)
- take a picture of handwritten notes at the end of everyday

## Processing of data

- raw data are downloaded from OSF, not used locally (use the
  datadownloader package
  <https://github.com/Between-the-Fjords/dataDownloader>)
- all scripts are part of the project on github
- process the data using the Fluxible package
  (<https://plant-functional-trait-course.github.io/fluxible/>)
- clean flux data are put back on OSF, ready to be downloaded for
  analysis
