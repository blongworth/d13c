# get data for d13c analysis

# TODO: add total sample size (small may fractionate in prep)

library(tidyverse)
library(DBI)
library(glue)
library(amstools)
library(here)



# a function to grab data for a list of rec_nums from the gs table
# this would probably be simpler as 4 separate queries.
get13c <- function(type, recnums) {
  table <- switch (type,
                   "oc" = "organic_carbon",
                   "hy" = "inorganic_carbon",
                   "ws" = "water_strip",
                   "gs" = "gas_sample"
  )
  
  db <- conNOSAMS()
  if (type == "hy") {
    query <- glue_sql(paste(glue("SELECT 
                                    {type}_num, rec_num, 
                                    {type}_date AS date, 
                                    {type}_rd AS rd, 
                                    {type}_dc_13 AS d13c, 
                                    {type}_c13_yield AS c13qty, 
                                    {type}_co2_yield AS tot_qty, 
                                    {type}_dc13_src AS irms"),
                            "FROM {`table`}
                            WHERE rec_num IN ({recnums*})",
                            glue("AND {type}_dc_13 IS NOT NULL")),
                      .con = db)
  } else if (type == "gs") {
    query <- glue_sql(paste(glue("SELECT {type}_num, rec_num, 
                                    {type}_date AS date, 
                                    {type}_rd AS rd, 
                                    {type}_dc_13 AS d13c, 
                                    {type}_c13 AS c13qty, 
                                    {type}_co2_yield AS tot_qty, 
                                    {type}_dc13_src AS irms"),
                            "FROM {`table`}
                            WHERE rec_num IN ({recnums*})",
                            glue("AND {type}_dc_13 IS NOT NULL")),
                      .con = db)
  } else if (type == "oc") {
    query <- glue_sql(paste(glue("SELECT {type}_num, rec_num, 
                                    date_run AS date, 
                                    oc_devel AS rd, {type}_dc_13 AS d13c, 
                                    {type}_c13_analysis AS c13qty, 
                                    {type}_co2_yield AS tot_qty, 
                                    {type}_dc13_src AS irms"),
                            "FROM {`table`}
                            WHERE rec_num IN ({recnums*})",
                            glue("AND {type}_dc_13 IS NOT NULL")),
                      .con = db)
  } else if (type == "ws") {
    query <- glue_sql(paste(glue("SELECT {type}_num, rec_num, 
                                    {type}_strip_date AS date, 
                                    {type}_r_d AS rd, 
                                    {type}_delta_c13 AS d13c, 
                                    {type}_c13 AS c13qty, 
                                    {type}_co2_yield AS tot_qty, 
                                    {type}_dc13_from AS irms"),
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


# get the standards table
db <- conNOSAMS()
standards <- dbGetQuery(db, "SELECT * FROM standards")

# get all d13C with rec_num in standards table
data <- map_dfr(list("hy", "gs", "oc", "ws"), get13c, standards$rec_num)

# Remove bad or empty irms and date, recode rd, flag outliers, add process
data <- data %>%
  filter(irms %in% c("O", "P"),
         !is.na(date)) %>%
  mutate(date = as.Date(date),
         rd = factor(ifelse(is.na(rd), 0, rd)),
         #sample_type = map_chr(tp_num, getProcess, con),
         irms = factor(irms)) %>%
  group_by(rec_num) %>% 
  filter(!is.na(d13c)) %>% 
  mutate(outlier = is.na(removeOutliers(d13c))) %>% 
  ungroup() %>% 
  inner_join(select(standards, rec_num, sample_id, d13_cons))



save(list = c("data", "standards"), file = "data/NOSAMS_d13c.rda")
write_csv(data, here("data/NOSAMS_proc_d13c.csv"))
