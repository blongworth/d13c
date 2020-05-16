# get data from files
library(purrr)
library(readxl)

#directory for yearly dirs
top_dir_path <- "H://arg/Mass Spec Data"
files <- dir(path = top_dir_path, full.names = TRUE, recursive = TRUE, pattern = "*.xls")
str(files)

col_names <- c("date", "time", "index", "port", "name", "pres", "ref", "raw
data <- read_xls(files[1])
data
