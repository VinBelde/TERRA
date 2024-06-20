import_CO2_CH4 <- function(file
){
    df <- read_delim(file, delim = "\t", skip = 5) |>
    filter(DATAH != "DATAU") |> #removing the line with the units
    mutate(
        DATE = ymd(DATE),
        TIME = hms(TIME),
        CO2 = as.double(CO2),
        CH4 = as.double(CH4),
        datetime = ymd_hms(paste(DATE, TIME)),
        remark = REMARK
    ) |>
    select(datetime, remark, CH4, CO2)

    print(head(df))
    df

    
}


import_PAR_temp <- function(file
){
    df <-read_delim(file, delim = ",", skip = 1) |>
    rename(
        datetime = TMSTAMP
    ) |>
    select(datetime, PAR_in_chamber, PAR_out, T_in_chamber, T_out)

    print(head(df))

    df
}