# general data handling
library(dplyr)
library(readr)
library(tidyr)

# date-time handling
library(lubridate)
library(hms)

# XLSX handling
library(openxlsx2)

# get sunrise/sunset data
library(suntools)

# get temperature data
library(readnoaa)

setwd("/home/kaelyn/Desktop/Bats_NW/WPZ_acoustic")

# Load in TXT files
sonobat_txt_dir <- file.path(getwd(), "sonobat_txt", "2026")
sonobat_txt_files <- list.files(sonobat_txt_dir, full.names = TRUE)

sbat_list <- lapply(sonobat_txt_files, function(x) {
    read_delim(x, col_types = cols(.default = "c"))
    }) |>
    setNames(basename(sonobat_txt_files))

# Format like XLSX

#lon, lat
lon <- -122.35083578760666
lat <- 47.66851544625889
wpz_coords <- matrix(c(lon, lat), nrow = 1)
tz <- "America/Los_Angeles"

create_zoo_data <- function(sonobat_df, tz, lon, lat) {
    # Format coordinates for suntools::sunriset
    crds <- matrix(c(lon, lat), nrow = 1)
    
    zdf <- sonobat_df %>%
        mutate(night_date = parse_date_time(MonitoringNight,
                                            orders = c("mdy", "ymd"),
                                            tz = tz)) %>%
        complete(night_date = seq.POSIXt(rollback(night_date[1],
                                                  roll_to_first = TRUE),
                                         rollforward(night_date[1]),
                                         by = "DSTday"),
                 fill = list(SppAccp = "NoRec",
                             HiF = "x",
                             LoF = "x")) %>%
        mutate(MonitoringNight = if_else(is.na(MonitoringNight),
                                         strftime(night_date, "%-m/%-d/%Y"),
                                         MonitoringNight),
               Timestamp = if_else(is.na(Timestamp),
                                   sub("(..)$", ":\\1", strftime(night_date +
                                                                     days(1),
                                                "%Y-%m-%dT%H:%M:%OS6%z")),
                                   Timestamp)) %>%
        mutate(posix = parse_date_time(Timestamp, "YmdHMOSz")) %>%
        mutate(lposix = with_tz(posix, tz)) %>%
        mutate(Month = month(lposix, label = TRUE),
               Date = date(lposix),
               Time = strftime(lposix, "%I:%M:%S %p")) %>%
        mutate(Sunset = sunriset(crds, night_date, direction = "sunset",
                                 POSIXct.out = TRUE)$time,
               Sunrise = sunriset(crds, night_date + days(1),
                                  direction = "sunrise",
                                  POSIXct.out = TRUE)$time,
               `Elapsed (hr)` = if_else(SppAccp == "NoRec", NA,
                                        time_length(interval(Sunset, lposix),
                                                    "hour"))) %>%
        mutate(Sunset = format(Sunset, "%I:%M:%S %p"),
               Sunrise = format(Sunrise, "%I:%M:%S %p")) %>%
        mutate(SppAccp = case_when(SppAccp == "x" ~ NA, .default = SppAccp),
               HiF = case_when(HiF == "x" ~ NA, .default = "HiF"),
               LoF = case_when(LoF == "x" ~ NA, .default = "LoF")) %>%
        unite("FSpec", HiF, LoF, sep = "/", remove = FALSE, na.rm = TRUE) %>%
        mutate(FSpec = case_when(FSpec == "" ~ "NoID", .default = FSpec),
               Species = coalesce(SppAccp, FSpec)) %>%
        mutate(dst = dst(lposix)) %>%
        rename(Temp = `Temperature Int`)
}

zd_list <- lapply(sbat_list, function(x) create_zoo_data(x, tz, lon, lat))

# Combine months and arrange data
zd_df <- bind_rows(zd_list) %>%
    distinct() %>%
    group_by(MonitoringNight) %>%
    mutate(ind = (n() > 1 & Species == "NoRec")) %>%
    filter(!ind) %>%
    ungroup() %>%
    group_by(Month) %>%
    mutate(`Month Ind` = case_when(posix == min(posix) ~ 
                                       strftime(posix, format = "%b"),
                                   .default = "")) %>%
    ungroup() %>%
    arrange(posix) %>%
    select(Month, MonitoringNight, Date, Time, Sunrise, Sunset, `Elapsed (hr)`, Species,
          Timestamp, Temp, `Month Ind`)

# Fill temperature values
# Get nearest NOAA stations for temperature fill, test individually
noaas <- noaa_nearby(lat, lon)
noaa_station <- "USW00094290"

temps <- noaa_daily(noaa_station, as.character(min(zd_df$Date)),
                    as.character(max(zd_df$Date)),
                    datatypes = "TMIN") %>%
    select(date, tmin) %>%
    rename(Date = date)

zd_temp <- zd_df %>%
    left_join(temps, by = join_by(Date)) %>%
    mutate(Temp = coalesce(Temp, as.character(tmin))) %>%
    select(-tmin)

# Save to XLSX
#xlsx_df <- zd_df %>% 
#    mutate(across(where(is.character), ~replace_na(., "")))

book_path <- file.path(getwd(), "xlsx_files", "ZooData2026.xlsx")
wb <- wb_load(book_path)
sheet_name <- "2026Data"
exist_rows <- nrow(wb_read(wb, sheet_name, col_names = FALSE))
#wb <- wb_add_data(wb, sheet_name, zd_df,
#                  start_row = exist_rows + 1, col_names = FALSE,
#                  na = "")
wb <- wb_add_data(wb, sheet_name, zd_temp, na = "")
#wb <- wb_add_numfmt(wb, sheet_name,
#                    dims = wb_dims(
#                        rows = (exist_rows + 1):(exist_rows + nrow(zd_df)),
#                        cols = 3),
#                    numfmt = 20)
wb <- wb_set_col_widths(wb,
                        sheet = sheet_name,
                        cols = 1:ncol(zd_temp),
                        widths = "auto")
new_path <- file.path(getwd(), "xlsx_files", 
                     paste0("ZooData2026_", Sys.Date(), ".xlsx"))
wb_save(wb, new_path, overwrite = TRUE)

