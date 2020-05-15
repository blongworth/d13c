# get data for d13c analysis

library(tidyverse)
library(DBI)
library(glue)
library(amstools)
library(here)


# Use amstools to get the standards table
standards <- getStdTable()

# a function to grab data for a list of rec_nums from the gs table
get13c <- function(type, recnums) {
  table <- switch (type,
                   "oc" = "organic_carbon",
                   "hy" = "inorganic_carbon",
                   "ws" = "water_strip",
                   "gs" = "gas_sample"
  )
  
  db <- conNOSAMS()
  if (type %in% c("hy", "gs")) {
    query <- glue_sql(paste(glue("SELECT {type}_num, rec_num, {type}_date AS date, 
                                 {type}_rd AS rd, {type}_dc_13 AS d13c, {type}_dc13_src AS irms"),
                            "FROM {`table`}
                            WHERE rec_num IN ({recnums*})",
                            glue("AND {type}_dc_13 IS NOT NULL")),
                      .con = db)
  } else if (type == "oc") {
    query <- glue_sql(paste(glue("SELECT {type}_num, rec_num, date_run AS date, 
                                 oc_devel AS rd, {type}_dc_13 AS d13c, {type}_dc13_src AS irms"),
                            "FROM {`table`}
                            WHERE rec_num IN ({recnums*})",
                            glue("AND {type}_dc_13 IS NOT NULL")),
                      .con = db)
  } else if (type == "ws") {
    query <- glue_sql(paste(glue("SELECT {type}_num, rec_num, {type}_strip_date AS date, 
                                 {type}_r_d AS rd, {type}_delta_c13 AS d13c, {type}_dc13_from AS irms"),
                            "FROM {`table`}
                            WHERE rec_num IN ({recnums*})",
                            glue("AND {type}_delta_c13 IS NOT NULL")),
                      .con = db)
  }
  recs <- dbSendQuery(db, query)
  data <- dbFetch(recs)
  dbClearResult(recs)
  data
}

data <- map_dfr(list("hy", "gs", "oc", "ws"), get13c, standards$rec_num)

# Remove bad or empty irms and date, recode rd
data <- data %>%
  filter(irms %in% c("O", "P"),
         !is.na(date)) %>%
  mutate(date = as.Date(date),
         rd = factor(ifelse(is.na(rd), 0, rd)),
         irms = factor(irms)) %>%
  inner_join(select(standards, rec_num, name))
save(list = c("data", "standards"), file = "data/NOSAMS_d13c.rda")
write_csv(data, here("data/NOSAMS_d13c.csv"))
