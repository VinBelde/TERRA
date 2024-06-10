Protocol flux measurements TERRA project 2024
================
Joseph Gaudard

## General

- make sure the licor and your clock/phone are precisely synchronized
  and at the right time (also check the time zone of the licor)

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

## Backup of data

- backup the data after each day of measurement
- make a new file everytime
- use the last file of the campaign (week? TBD) for data processing
- in case there is something weird in that last file, you can check the
  previous ones to see if it is better (corrupt/lost data)
- raw data used for processing should be stored on a cloud (OSF;
  <https://osf.io/rba87/>)

## Processing of data

- raw data are downloaded from OSF, not used locally (use the
  datadownloader package
  <https://github.com/Between-the-Fjords/dataDownloader>)
- all scripts are part of the project on github
- process the data using the Fluxible package
  (<https://plant-functional-trait-course.github.io/fluxible/>)
- clean flux data are put back on OSF, ready to be downloaded for
  analysis
