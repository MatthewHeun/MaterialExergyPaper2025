# This script downloads energy (E) and exergy (X)
# data from the CL-PFU database
# for South Africa in 2013 and
# saves in the data folder for later use.

#
# Read information from the Mexer database.
#

# Establish the connection to the Mexer database
conn_params <- list(dbname = "ScratchMDB",
                    user = "dbcreator",
                    host = "mexer.site",
                    port = 6432)
conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
                       dbname = conn_params$dbname,
                       host = conn_params$host,
                       port = conn_params$port,
                       user = conn_params$user)
on.exit(DBI::dbDisconnect(conn))

# Read the ECC data once
zaf_2013_ecc <- PFUPipelineTools::pl_filter_collect("PSUTReAllChopAllDsAllGrAll",
                                                    Dataset == "CL-PFU IEA",
                                                    Country == "ZAF",
                                                    Year == 2013,
                                                    LastStage == "Final",
                                                    IncludesNEU == FALSE,
                                                    ProductAggregation == "Specified",
                                                    IndustryAggregation == "Specified",
                                                    conn = conn,
                                                    collect = TRUE,
                                                    matrix_class = "matrix")

DBI::dbDisconnect(conn)

# Save to data folder
zaf_2013_ecc |>
  saveRDS(file = file.path("data", "zaf_2013_ecc.rds"))
