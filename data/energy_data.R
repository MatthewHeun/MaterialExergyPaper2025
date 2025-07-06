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
                                                    matrix_class = "matrix") |>
  dplyr::arrange(EnergyType) |>
  dplyr::mutate(
    worksheet_names = paste(Country, Year, EnergyType, sep = "_")
  ) |>
  Recca::calc_io_mats()

# Disconnect from the Mexer database
DBI::dbDisconnect(conn)


# Save full ZAF data to an Excel file for inspection.
zaf_2013_ecc_path <- file.path("data", "zaf_2013_ecc.xlsx")
zaf_2013_ecc |>
  Recca::write_ecc_to_excel(path = zaf_2013_ecc_path,
                            worksheet_names = "worksheet_names",
                            overwrite_file = TRUE)

#
# Formulate Y_prime matrices that describe
# the MCC's energy requirements.
#

# Read ECC requirements
# from the MCC spreadsheet.
mcc_energy_reqts <- openxlsx2::read_xlsx(file = file.path("data", "Paper Examples.xlsx"),
                                         named_region = "mcc_energy_reqts") |>
  # Use the TJ versions
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
# supplies exactly the amount of energy (and exergy)
# required for the material conversion chain
#

ecc_supply_to_mcc <- dplyr::left_join(zaf_2013_ecc,
                                      mcc_energy_reqts,
                                      by = "EnergyType") |>
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
                IncludesNEU, Year, R, U, V, Y, U_EIOU, U_feed, r_EIOU, S_units,
                worksheet_names) |>
  dplyr::mutate(
    # Convert to kJ everywhere
    R = matsbyname::hadamardproduct_byname(R, 1e9),
    U = matsbyname::hadamardproduct_byname(U, 1e9),
    U_feed = matsbyname::hadamardproduct_byname(U_feed, 1e9),
    U_EIOU = matsbyname::hadamardproduct_byname(U_EIOU, 1e9),
    V = matsbyname::hadamardproduct_byname(V, 1e9),
    Y = matsbyname::hadamardproduct_byname(Y, 1e9),
    S_units = matsbyname::setcolnames_byname(S_units, colnames = "kJ")
  ) |>
  dplyr::rename(
    R_X = R, U_X = U, U_feed_X = U_feed, U_eiou_X = U_EIOU,
    r_EIOU_X = r_EIOU, V_X = V, Y_X = Y, S_units_X = S_units
  )

# Save the ECC that supplies the MCC to an Excel file for inspection.
ecc_supply_to_mcc |>
  Recca::write_ecc_to_excel(path = file.path("data", "ecc_supply_to_mcc.xlsx"),
                            worksheet_names = "worksheet_names",
                            overwrite_file = TRUE)



#
# Read the MCC exergy matrices
#

mcc_ruvy_wb <- openxlsx2::wb_load(file = file.path("data", "Paper Examples 4.xlsx"))
mcc_regions <- openxlsx2::wb_get_named_regions(wb = mcc_ruvy_wb)


mcc <- sapply(X = c("R_B", "U_B", "U_feed_B", "U_eiou_B",
                    "r_eiou_B", "V_B", "Y_B", "S_units_B"),
              FUN = function(this_matrix_name) {
                df <- openxlsx2::read_xlsx(file = file.path("data",
                                                            "Paper Examples 4.xlsx"),
                                           named_region = this_matrix_name,
                                           row_names = TRUE)
                # Convert all NA values to 0
                df[is.na(df)] <- 0
                this_matrix <- df |>
                  # Convert the data frame to a matrix
                  as.matrix() |>
                  # Then to a Matrix
                  Matrix::Matrix(sparse = TRUE)
                # Bundle in a vector for easier conversion to a tibble
                c(this_matrix)
              },
              simplify = FALSE,
              USE.NAMES = TRUE) |>
  tibble::as_tibble_row() |>
  # Add EnergyType column
  dplyr::mutate(
    EnergyType = "B"
  )





#
# Modify the ECC by removing the Y matrix
#



#
# Modify the MCC by removing the "Supply [of X]"
# rows from the R matrix.
#





#
# Sum the ECC and MCC matrices
#


#
# Write the combined energy and material
# conversion chains
# to an Excel file for inspection
#
