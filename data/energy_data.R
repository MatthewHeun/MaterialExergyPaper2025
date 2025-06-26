# This script downloads energy (E) and exergy (X)
# data from the CL-PFU database
# for South Africa in 2013.
# Then, it creates Y_prime matrices from the
# energy and exergy required to convert 1 ton
# of iron ore to pig iron
# according the the examples spreadsheet.


#
# Read information from the Mexer database.
#

# Establish the connection to the Mexer database
conn_params <- list(dbname = "MexerDB",
                    user = "dbcreator",
                    host = "mexer.site",
                    port = 6432)
conn <- DBI::dbConnect(drv = RPostgres::Postgres(),
                       dbname = conn_params$dbname,
                       host = conn_params$host,
                       port = conn_params$port,
                       user = conn_params$user)
on.exit(DBI::dbDisconnect(conn))

# Read the data once
psut_zaf_2013 <- PFUPipelineTools::pl_filter_collect("PSUTReAllChopAllDsAllGrAll",
                                                     Dataset == "CL-PFU IEA",
                                                     Country == "ZAF",
                                                     Year == 2013,
                                                     LastStage == "Final",
                                                     IncludesNEU == TRUE,
                                                     ProductAggregation == "Despecified",
                                                     IndustryAggregation == "Despecified",
                                                     conn = conn,
                                                     collect = TRUE,
                                                     matrix_class = "matrix") |>
  dplyr::mutate(
    Dataset = factor(Dataset, levels = c("CL-PFU IEA", "CL-PFU MW", "CL-PFU IEA+MW"))
  ) |>
  dplyr::arrange(Country, Year, EnergyType, LastStage, IncludesNEU, Dataset)

psut_zaf_2013_io <- psut_zaf_2013 |>
  Recca::calc_io_mats()

#
# Formulate Y_prime matrices.
#

# Read energy and exergy requirements
# from the material conversion chain spreadsheet.

new_Y <- openxlsx2::read_xlsx(file = "~/data/Example_mcc.xlsx", named_region = "the name")






# To read this file in R, use code like the following:
foo <- readRDS("~/Desktop/psut_fra_2020_for_baptiste.rds")
# To look at the matrices, use code like the following in the Console of RStudio:
foo$R[[1]] |> View()



# Disconnect from the Mexer database
DBI::dbDisconnect(conn)
