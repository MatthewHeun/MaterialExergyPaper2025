# This script downloads energy (E) and exergy (X)
# data from the CL-PFU database
# for South Africa in 2013 and
# saves in the data folder for later use.

#
# Read information from the Mexer database.
#

# Establish the connection to the Mexer database
conn <- PFUPipelineTools::get_scratchmdb_conn()
on.exit(DBI::dbDisconnect(conn))

# Read the ECC data
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

# Read the phi vector
conn <- PFUPipelineTools::get_mexerdb_conn(user = "dbcreator")
zaf_2013_phi <- PFUPipelineTools::pl_filter_collect("Phivecs",
                                                    Dataset == "CL-PFU",
                                                    Country == "ZAF",
                                                    Year == 2013,
                                                    conn = conn,
                                                    collect = TRUE,
                                                    matrix_class = "matrix")

DBI::dbDisconnect(conn)

# Save to data folder
zaf_2013_ecc |>
  saveRDS(file = file.path("data", "zaf_2013_ecc.rds"))
zaf_2013_phi |>
  saveRDS(file = file.path("data", "zaf2013_phi.rds"))
