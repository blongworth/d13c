# get data from files
library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

#directory for yearly dirs
top_dir_path <- "H://arg/"
files <- dir(path = top_dir_path, full.names = TRUE, recursive = FALSE, pattern = "(optima|PRISM)\\d{2}.xlsx")
str(files)

# function to read and parse a yearly irms file
readIRMSfile <- function(file) {
  #define col names and types
  # need to add fields if it's a prism file
  if (grepl("PRISM", file)) {
    col_names <- c("date", "time", "cf", "index", "port", "name", "pres", "ref", "raw13c", 
                   "raw18o", "err13c", "err18o", "blank", "pdbcorr13c", 
                   "pdbcorr18o", "craigcor13c", "craigcor18o")
    col_types <- c("numeric", "numeric", "text", "numeric", "text", "text", "numeric",
                   "text",  "numeric",  "numeric",  "numeric",  "numeric",  
                   "numeric",  "numeric",  "numeric",  "numeric",  "numeric") 
    range = cell_cols("A:Q")
  } else {  
    col_names <- c("date", "time", "index", "name", "pres", "ref", "raw13c", 
                   "raw18o", "err13c", "err18o", "blank", "pdbcorr13c", 
                   "pdbcorr18o", "craigcor13c", "craigcor18o")
    col_types <- c("numeric",  "numeric",  "numeric",  "text",  "numeric",
                   "text",  "numeric",  "numeric",  "numeric",  "numeric",  
                   "numeric",  "numeric",  "numeric",  "numeric",  "numeric")  
    range = cell_cols("A:O")
  }
  
  # read and parse the file
  read_xlsx(file, range = range,
            col_names = col_names, col_types = col_types) %>%
    filter(grepl("^\\d{5}", date)) %>%
    mutate(date = excel_numeric_to_date(as.numeric(date)),
           time = format(as.POSIXct(Sys.Date() + as.numeric(time)), "%H:%M", tz="UTC"),
           inst = ifelse(grepl("optima", file), "optima", "prism")) %>%
    select(date, time, index, name, pres, ref, craigcor13c)
}
         
alldata <- map_dfr(files, readIRMSfile)

write_csv(alldata, "data/irms_data.csv")
