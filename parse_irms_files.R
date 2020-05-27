# get data from files
library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

#directory for yearly dirs
top_dir_path <- "H://arg/"
files <- dir(path = top_dir_path, full.names = TRUE, recursive = FALSE, pattern = "[optima|PRISM]\\d{2}.xlsx")
str(files)

# read and parse a single file
col_names <- c("date", "time", "index", "name", "pres", "ref", "raw13c", 
               "raw18o", "err13c", "err18o", "blank", "pdbcorr13c", 
               "pdbcorr18o", "craigcor13c", "craigcor18o")
data <- read_xlsx(files[1], range = cell_cols("A:O"), col_names = col_names)
datafilt <- data %>%
  select(date, time, name, craigcor13c) %>% 
  filter(grepl("^\\d{5}", date)) %>%
  mutate(date = excel_numeric_to_date(as.numeric(date)),
         time = format(as.POSIXct(Sys.Date() + as.numeric(time)), "%H:%M", tz="UTC"))

readIRMSfile <- function(file) {
col_names <- c("date", "time", "index", "name", "pres", "ref", "raw13c", 
               "raw18o", "err13c", "err18o", "blank", "pdbcorr13c", 
               "pdbcorr18o", "craigcor13c", "craigcor18o")
col_types <- c("numeric",  "numeric",  "numeric",  "text",  "numeric",
               "text",  "numeric",  "numeric",  "numeric",  "numeric",  
               "numeric",  "numeric",  "numeric",  "numeric",  "numeric")  

read_xlsx(file, range = cell_cols("A:O"),
          col_names = col_names, col_types = col_types) %>%
  filter(grepl("^\\d{5}", date)) %>%
  mutate(date = excel_numeric_to_date(as.numeric(date)),
         time = format(as.POSIXct(Sys.Date() + as.numeric(time)), "%H:%M", tz="UTC")) %>%
  select(-blank)
  
}
         
data <- readIRMSfile(files[1])
