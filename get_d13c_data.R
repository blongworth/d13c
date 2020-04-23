# get data for d13c analysis

library(tidyverse)
library(odbc)
library(amstools)

con <- conNOSAMS()

dbListFields(con, "gas_sample")

dbGetQuery(con, "select * from no_os where tp_num > 250000")
dbGetQuery(con, "select * from mass_spec where ms_date > '2020-04-01'")
dbGetQuery(con, "select * from gas_sample where gs_date > '2020-04-01'")

# dc13 and source are stored in process tables. Ick.

dbListTables(con)
