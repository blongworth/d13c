# get data from files
library(tidyverse)
library(purrr)
library(readxl)
library(janitor)

# TODO:
# Add extra import columns
# format ms_date with time
# join to get rec_num

#directory for yearly dirs
top_dir_path <- "H://arg/"
files <- dir(path = top_dir_path, full.names = TRUE, recursive = FALSE, pattern = "(optima|PRISM)\\d{2}.xlsx")
str(files)

# function to read and parse a yearly irms file
# fields in mass_spec table:
# "ms_date"            "sample_num"         "port"               "pres_rms"          
# [5] "ref_id"             "raw_13c"            "raw_18o"            "err_13c"           
# [9] "err_18o"            "c13_p_1std"         "o18_p_1std"         "c13_cr_1std"       
# [13] "o18_cr_1std"        "del45_3std"         "del46_3std"         "c13_cr_3std"       
# [17] "o18_cr_3std"        "smow18o"            "flag"               "sample_type"       
# [21] "sample_desc"        "beam_size"          "cf"                 "mass_spec"         
# [25] "new_13c_corr"       "new_18o_corr"       "new_test_13C_craig" "new_test_18o_craig"
# [29] "rec_num"    
readIRMSfile <- function(file) {
  #define col names and types
  # need to add fields if it's a prism file
  if (grepl("PRISM", file)) {
    col_names <- c("ms_date", "time", "cf", "index", "port",
                   "sample_desc", "pres_rms", "ref_id", "raw_13c", 
                   "raw_18o", "err_13c", "err_18o", "blank", 
                   "c13_p_1std", "o18_p_1std", "c13_cr_1std", "o18_cr_1std")
    col_types <- c("numeric", "numeric", "text", "numeric", "text", "text", "numeric",
                   "text",  "numeric",  "numeric",  "numeric",  "numeric",  
                   "numeric",  "numeric",  "numeric",  "numeric",  "numeric") 
    range = cell_cols("A:Q")
  } else {  
    col_names <- c("ms_date", "time", "index", 
                   "sample_desc", "pres_rms", "ref_id", "raw_13c", 
                   "raw_18o", "err_13c", "err_18o", "blank", 
                   "c13_p_1std", "o18_p_1std", "c13_cr_1std", "o18_cr_1std")
    col_types <- c("numeric",  "numeric",  "numeric",  "text",  "numeric",
                   "text",  "numeric",  "numeric",  "numeric",  "numeric",  
                   "numeric",  "numeric",  "numeric",  "numeric",  "numeric")  
    range = cell_cols("A:O")
  }
  
  # read and parse the file
  read_xlsx(file, range = range,
            col_names = col_names, col_types = col_types) %>%
    filter(grepl("^\\d{5}", ms_date)) %>%
    mutate(ms_date = excel_numeric_to_date(as.numeric(ms_date)),
           time = format(as.POSIXct(Sys.Date() + as.numeric(time)), "%H:%M", tz="UTC"),
           mass_spec = ifelse(grepl("optima", file), "OPTIMA", "PRISM"),
           port = ifelse("port" %in% colnames(.), port, NA)) %>%
    select(ms_date, port, pres_rms, ref_id, raw_13c, raw_18o, 
           err_13c, err_18o, c13_p_1std, o18_p_1std, c13_cr_1std,
           o18_cr_1std)
}
         
alldata <- map_dfr(files, readIRMSfile)

write_csv(alldata, "data/irms_data.csv")
