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
psut_io_zaf_2013 <- PFUPipelineTools::pl_filter_collect("PSUTReAllChopAllDsAllGrAll",
                                                        Dataset == "CL-PFU IEA",
                                                        Country == "ZAF",
                                                        Year == 2013,
                                                        LastStage == "Final",
                                                        IncludesNEU == FALSE,
                                                        ProductAggregation == "Despecified",
                                                        IndustryAggregation == "Despecified",
                                                        conn = conn,
                                                        collect = TRUE,
                                                        matrix_class = "matrix") |>
  dplyr::arrange(EnergyType) |>
  Recca::calc_io_mats()

psut_io_zaf_2013_path <- file.path("data", "psut_io_zaf_2013.xlsx")
psut_io_zaf_2013 |>
  Recca::write_ecc_to_excel(path = psut_io_zaf_2013_path,
                            overwrite_file = TRUE)




psut_io_zaf_2013 <- PFUPipelineTools::pl_filter_collect("PSUTReAllChopAllDsAllGrAll",
                                                        Dataset == "CL-PFU IEA",
                                                        Country == "ZAF",
                                                        Year == 2013,
                                                        LastStage == "Final",
                                                        IncludesNEU == FALSE,
                                                        ProductAggregation == "Despecified",
                                                        IndustryAggregation == "Despecified",
                                                        conn = conn,
                                                        collect = TRUE,
                                                        matrix_class = "matrix") |>
  dplyr::arrange(EnergyType)

psut_io_zaf_2013_path <- file.path("data", "psut_io_zaf_2013.xlsx")
psut_io_zaf_2013 |>
  Recca::write_ecc_to_excel(path = psut_io_zaf_2013_path,
                            overwrite_file = TRUE)



# Add tab names
openxlsx2::wb_load(file = psut_io_zaf_2013_path) |>
  openxlsx2::wb_set_sheet_names(old = 1, new = "E") |>
  openxlsx2::wb_set_sheet_names(old = 2, new = "X") |>
  openxlsx2::wb_save(file = psut_io_zaf_2013_path, overwrite = TRUE)

#
# Formulate Y_prime matrices.
#

# Read energy and exergy requirements
# from the material conversion chain spreadsheet.

mcc_energy_reqts <- openxlsx2::read_xlsx(file = file.path("data", "Paper Examples 3.xlsx"),
                                         named_region = "mcc_energy_reqts") |>
  dplyr::select(EnergyCarrier, `E [TJ]`, `X [TJ]`) |>
  dplyr::rename(E = "E [TJ]", X = "X [TJ]", rownames = "EnergyCarrier") |>
  tidyr::pivot_longer(cols = c("E", "X"),
                      names_to = "EnergyType",
                      values_to = "matvals") |>
  dplyr::mutate(
    colnames = "Iron and steel",
    rowtypes = "Product",
    coltypes = "Industry"
  ) |>
  dplyr::group_by(EnergyType) |>
  matsindf::collapse_to_matrices() |>
  dplyr::rename(Y_prime = "matvals")

#
# Build a new energy conversion chain that
# supplies exactly the amount of energy
# needed for the material conversion chain

ecc <- dplyr::left_join(psut_io_zaf_2013, mcc_energy_reqts, by = "EnergyType") |>
  Recca::new_Y() |>
  dplyr::mutate(
    R = NULL,
    U = NULL,
    V = NULL,
    Y = NULL,
    U_EIOU = NULL,
    U_feed = NULL,
    r_EIOU = NULL
  ) |>
  dplyr::rename(
    R = R_prime,
    U = U_prime,
    V = V_prime,
    Y = Y_prime,
    U_EIOU = U_EIOU_prime,
    U_feed = U_feed_prime,
    r_EIOU = r_EIOU_prime
  ) |>
  dplyr::select(Dataset, ValidFromVersion, ValidToVersion, Country, Method, EnergyType, LastStage,
                IncludesNEU, Year, R, U, V, Y, U_EIOU, U_feed, r_EIOU, S_units)

ecc |>
  Recca::write_ecc_to_excel(path = file.path("data", "energy_ecc.xlsx"),
                            overwrite_file = TRUE)

# Set the names of the tabs in the workbook






# Disconnect from the Mexer database
DBI::dbDisconnect(conn)
