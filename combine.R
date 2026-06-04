library(dplyr)
library(openxlsx2)

setwd("/home/kaelyn/Desktop/Bats_NW/WPZ_acoustic")

# Load in TXT files
sonobat_txt_dir <- file.path(getwd(), "sonobat_txt", "2026")
sonobat_txt_files <- list.files(sonobat_txt_dir, full.names = TRUE)

sbat_list <- lapply(sonobat_txt_files, read.delim) |>
    setNames(basename(sonobat_txt_files))

# Format like XLSX

zoo_data_format <- function(sonobat_df) {
    zdf <- sonobat_df %>%
        mutate(posix = strptime(gsub(":", "", Timestamp),
                                format = "%Y-%m-%dT%H%M%OS%z")) %>%
        mutate(Month = format(posix, "%B"),
               Date = as.Date(posix),
               Time = as.numeric(posix) %% 86400 / 86400) %>%
        mutate(Sunrise = "",
               Sunset = "",
               `Elapsed (hr)` = "") %>%
        mutate(SppAccp = case_when(SppAccp == "x" ~ "", .default = SppAccp),
               HiF = case_when(HiF == "x" ~ "", .default = HiF),
               LoF = case_when(LoF == "x" ~ "", .default = LoF)) %>%
        rename(Species = SppAccp,
               Temp = Temperature.Int)
}

zd_list <- lapply(sbat_list, zoo_data_format)

# Combine months
zd_df <- bind_rows(zd_list) %>%
    group_by(Month) %>%
    mutate(`Month Ind` = case_when(posix == min(posix) ~ 
                                       strftime(posix, format = "%b"),
                                   .default = "")) %>%
    ungroup() %>%
    arrange(posix) %>%
    select(Month, Date, Time, Sunrise, Sunset, `Elapsed (hr)`, Species,
           Timestamp, Temp, HiF, LoF, `Month Ind`)

# Save to XLSX

book_path <- file.path(getwd(), "xlsx_files", "ZooData2026.xlsx")
wb <- wb_load(book_path)
sheet_name <- "2026Data"
exist_rows <- nrow(wb_read(wb, sheet_name, col_names = FALSE))
wb <- wb_add_data(wb, sheet_name, zd_df,
                  start_row = exist_rows + 1, col_names = FALSE)
wb <- wb_add_numfmt(wb, sheet_name,
                    dims = wb_dims(
                        rows = (exist_rows + 1):(exist_rows + nrow(zd_df)),
                        cols = 3),
                    numfmt = 20)
wb <- wb_set_col_widths(wb,
                        sheet = sheet_name,
                        cols = 1:ncol(zd_df),
                        widths = "auto")
new_path <- file.path(getwd(), "xlsx_files", 
                     paste0("ZooData2026_", Sys.Date(), ".xlsx"))
wb_save(wb, new_path, overwrite = TRUE)
