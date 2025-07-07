# Then, it creates Y_prime matrices from the
# energy and exergy required to convert 1 ton
# of iron ore to pig iron
# according the the examples spreadsheet.


#
# Read information downloaded from the Mexer database.
# See the file download_data.R.
#

zaf_2013_ecc <- file.path("data", "zaf_2013_ecc.rds") |>
  readRDS() |>
  dplyr::arrange(EnergyType) |>
  dplyr::mutate(
    WorksheetNames = paste(Country, Year, EnergyType, sep = "_")
  ) |>
  Recca::calc_io_mats()

#
# Save full ZAF data to an Excel file for later inspection.
#

zaf_2013_ecc_path <- file.path("data", "zaf_2013_ecc.xlsx")
zaf_2013_ecc |>
  Recca::write_ecc_to_excel(path = zaf_2013_ecc_path,
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE)

#
# Formulate Y_prime matrices that describe
# the MCC's energy requirements.
#

# Read ECC requirements
# from the MCC spreadsheet.
mcc_energy_reqts <- openxlsx2::read_xlsx(file = file.path("data",
                                                          "Paper Examples.xlsx"),
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
  # At this point, delete the energy row.
  # We don't use it.
  dplyr::filter(EnergyType == "X") |>
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
                IncludesNEU, Year, R, U, V, Y, U_feed, U_EIOU, r_EIOU, S_units,
                WorksheetNames) |>
  dplyr::mutate(
    # Convert from TJ to kJ everywhere
    R = matsbyname::hadamardproduct_byname(R, 1e9),
    U = matsbyname::hadamardproduct_byname(U, 1e9),
    U_feed = matsbyname::hadamardproduct_byname(U_feed, 1e9),
    U_EIOU = matsbyname::hadamardproduct_byname(U_EIOU, 1e9),
    V = matsbyname::hadamardproduct_byname(V, 1e9),
    Y = matsbyname::hadamardproduct_byname(Y, 1e9),
    S_units = matsbyname::setcolnames_byname(S_units, colnames = "kJ")
  )

# Save the ECC that supplies the MCC to an Excel file for inspection.
ecc_supply_to_mcc |>
  Recca::write_ecc_to_excel(path = file.path("data", "ecc_supply_to_mcc.xlsx"),
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE)


#
# Modify the ECC by removing the Y matrix and
# preparing for summation.
#

ecc_supply_to_mcc_long <- ecc_supply_to_mcc |>
  dplyr::mutate(
    WorksheetNames = NULL,
    Y = NULL,
    Dataset = NULL,
    ValidFromVersion = NULL,
    ValidToVersion = NULL,
    Country = NULL,
    Method = NULL,
    EnergyType = NULL,
    LastStage = NULL,
    IncludesNEU = NULL,
    Year = NULL
  ) |>
  tidyr::pivot_longer(cols = c("R", "U", "V",
                               "U_feed", "U_EIOU", "r_EIOU",
                               "S_units"),
                      names_to = "matnames",
                      values_to = "X")


#
# Read the MCC exergy matrices.
# These are in kJ and material exergy (B).
#

mcc_mats <- file.path("data", "Paper Examples.xlsx") |>
  Recca::read_ecc_from_excel(worksheets = "MCC_B_RUVY_matrices_mat_level")


#
# Modify the MCC by removing the "Supply [of X]"
# rows from the R matrix and prepare for summation.
#

mcc_mats_long <- mcc_mats |>
  dplyr::mutate(
    WorksheetNames = NULL,
    R = matsbyname::select_rows_byname(.data[["R"]], remove_pattern = "^Supply")
  ) |>
  tidyr::pivot_longer(cols = c("R", "U", "V", "Y",
                               "U_feed", "U_EIOU", "r_EIOU",
                               "S_units"),
                      names_to = "matnames",
                      values_to = "B")


#
# Sum the ECC and MCC matrices
#

bx_mats <- dplyr::left_join(mcc_mats_long,
                            ecc_supply_to_mcc_long,
                            by = "matnames") |>
  dplyr::mutate(
    BX = matsbyname::sum_byname(.data[["B"]], .data[["X"]])
  ) |>
  tidyr::pivot_longer(cols = c("B", "X", "BX"),
                      names_to = "EnergyType",
                      values_to = "matvals") |>
  tidyr::pivot_wider(names_from = "matnames", values_from = "matvals")


#
# Write the combined energy and material
# conversion chains
# to an Excel file for inspection
#

bx_mats |>
  Recca::write_ecc_to_excel(path = file.path("data", "BX.xlsx"),
                            worksheet_names = "EnergyType",
                            overwrite_file = TRUE)
