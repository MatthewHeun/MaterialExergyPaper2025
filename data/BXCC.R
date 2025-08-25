# This script creates a
# unified material and energy conversion chain (BXCC)
# for the blast furnace example in the paper.

#
# Read information downloaded from the Mexer database.
# See the script download_data.R,
# which downloads and creates the zaf_2013_ecc.rds file.
#

zaf_2013_ecc <- file.path("data", "zaf_2013_ecc.rds") |>
  readRDS() |>
  dplyr::mutate(
    WorksheetNames = paste0(Country, "_", Year, "_", EnergyType)
  )

#
# Verify the inter-industry exergy balance
#

zaf_2013_ecc |>
  Recca::verify_SUT_energy_balance()

#
# Save full ZAF data to an Excel file for later inspection.
#

zaf_2013_ecc_path <- file.path("data", "zaf_2013_ecc.xlsx")
zaf_2013_ecc |>
  Recca::write_ecc_to_excel(path = zaf_2013_ecc_path,
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)

#
# Read ECC requirements from the MCC spreadsheet.
# These energy requirements are in TJ.
#

mcc_energy_reqts <- openxlsx2::read_xlsx(file = file.path("data",
                                                          "Paper Examples.xlsx"),
                                         named_region = "mcc_energy_reqts") |>
  dplyr::select(-`X [kJ]`) |>
  dplyr::rename(X = "X [TJ]", rownames = "EnergyCarrier") |>
  tidyr::pivot_longer(cols = "X",
                      names_to = "EnergyType",
                      values_to = "matvals") |>
  dplyr::mutate(
    colnames = "Iron and steel",
    rowtypes = "Product",
    coltypes = "Industry"
  ) |>
  dplyr::group_by(EnergyType) |>
  # Formulate interms of a Y_prime matrix that we need to satisfy.
  matsindf::collapse_to_matrices() |>
  dplyr::rename(Y_prime = "matvals")


#
# Calculate an ECC that contains only the energy carriers
# we need to supply the MCC.
# This ECC is in TJ.
# Note that many pieces of the ECC are covered by our MCC,
# so we trim those out to focus only on the rows and columns
# of interest for supplying the MCC
# before calculating the input-output matrices.
# The pieces we need are the ECC to supply
# are identified by row and column names of the Y matrix.
# The row names of interest are the energy carriers needed
# by the MCC.
# These row names come from the row names of the Y_prime matrix
# in the mcc_energy_reqts matrix.
# The column names of interest are the two sectors
# involved in the MCC:
# the Iron and steel and Mining and quarrying sectors
#

Y_rownames_supply <- mcc_energy_reqts$Y_prime[[1]] |>
  rownames()
Y_colnames_supply <- c("Iron and steel", "Mining and quarrying")

xcc_supply_to_bcc <- zaf_2013_ecc |>
  dplyr::filter(EnergyType == "X") |>
  Recca::calc_io_mats() |>
  dplyr::mutate(
    # Isolate only the rows we need
    Y_prime = matsbyname::select_rows_byname(
      .data[["Y"]],
      retain_pattern = RCLabels::make_or_pattern(Y_rownames_supply)
    ),
    # Isolate only the columns we need
    Y_prime = matsbyname::select_cols_byname(
      .data[["Y_prime"]],
      retain_pattern = RCLabels::make_or_pattern(Y_colnames_supply)
    )
  ) |>
  # Calculate _prime, etc matrices that supply only those
  # energy carriers and sectors we need.
  Recca::new_Y() |>
  dplyr::mutate(
    R = NULL, U = NULL, V = NULL, Y = NULL, U_feed = NULL, U_EIOU = NULL, r_EIOU = NULL
  ) |>
  dplyr::rename(
    R = R_prime, U = U_prime, V = V_prime, Y = Y_prime,
    U_feed = U_feed_prime, U_EIOU = U_EIOU_prime, r_EIOU = r_EIOU_prime
  ) |>
  dplyr::mutate(
    # Create a new Y matrix that has just an Iron and steel sector
    # that is the sum of the Iron and steel and Mining and quarying sectors.
    # For now, call it the y vector.
    y = .data[["Y"]] |>
      matsbyname::rowsums_byname(colname = "Iron and steel"),
    # Delete the existing Y matrix.
    Y = NULL
  ) |>
  # Rename the y vector to be Y, the new Y matrix.
  dplyr::rename(Y = y) |>
  # Now add the Y_prime matrix from mcc_energy_reqts
  dplyr::left_join(mcc_energy_reqts, by = "EnergyType") |>
  Recca::new_Y() |>
  dplyr::mutate(
    R = NULL, U = NULL, V = NULL, Y = NULL,
    U_EIOU = NULL, U_feed = NULL, r_EIOU = NULL
  ) |>
  dplyr::rename(
    R = R_prime, U = U_prime, V = V_prime, Y = Y_prime,
    U_EIOU = U_EIOU_prime, U_feed = U_feed_prime, r_EIOU = r_EIOU_prime
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
# This ECC is in TJ.
xcc_supply_to_bcc |>
  Recca::write_ecc_to_excel(path = file.path("data", "ecc_supply_to_mcc.xlsx"),
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)

#
# Modify the XCC by removing the Y matrix
# in preparation for summation with the MCC.
#

xcc_supply_to_bcc_long <- xcc_supply_to_bcc |>
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
                      values_to = "X") |>
  # There are several near-zero but not zero values
  # in these matrices.
  # Eliminate small values.
  dplyr::mutate(
    X = matsbyname::clean_byname(.data[["X"]], tol = 1e-8)
  )


#
# Read the BCC exergy matrices.
# These are in kJ and material exergy (B).
#

bcc_mats <- file.path("data", "Paper Examples.xlsx") |>
  Recca::read_ecc_from_excel(worksheets = "MCC_B_RUVY_matrices_mat_level")


#
# Modify the BCC by removing the "Supply [of X]"
# rows from the R matrix and removing
# the [from Supply] suffix from row and column names
# in other matrices to prepare for summation.
#

bcc_mats_long <- bcc_mats |>
  dplyr::mutate(
    WorksheetNames = NULL,
    R = matsbyname::select_rows_byname(.data[["R"]], remove_pattern = "^Supply")
  ) |>
  tidyr::pivot_longer(cols = c("R", "U", "V", "Y",
                               "U_feed", "U_EIOU", "r_EIOU",
                               "S_units"),
                      names_to = "matnames",
                      values_to = "B") |>
  # Rename Product margins by removing " [from Supply]",
  # thereby preparing for summation.
  dplyr::mutate(
    B = .data[["B"]] |>
      matsbyname::rename_via_pattern_byname(margin = "Product",
                                            regexp_pattern = " [from Supply]",
                                            replacement = "",
                                            fixed = TRUE)
  )


#
# Sum the XCC and BCC matrices
#

bx_mats <- dplyr::left_join(bcc_mats_long,
                            xcc_supply_to_bcc_long,
                            by = "matnames") |>
  dplyr::mutate(
    BX = matsbyname::sum_byname(.data[["B"]], .data[["X"]])
  ) |>
  tidyr::pivot_longer(cols = c("B", "X", "BX"),
                      names_to = "EnergyType",
                      values_to = "matvals") |>
  tidyr::pivot_wider(names_from = "matnames", values_from = "matvals")


#
# Write the combined material and energy
# conversion chains
# to an Excel file for inspection
#

bx_mats |>
  Recca::write_ecc_to_excel(path = file.path("data", "BXCC.xlsx"),
                            worksheet_names = "EnergyType",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)

#
# Write the data into the Paper Examples.xlsx file, too.
#

bx_mats |>
  Recca::write_ecc_to_excel(path = file.path("data", "Paper Examples.xlsx"),
                            worksheet_names = "EnergyType",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)

#
# Write the data to an RDS file for use in the paper
#

bx_mats |>
  saveRDS(file = file.path("data", "BXCC.rds"))

#
# Double-check energy balances and
# calculate rational efficiencies
#

efficiencies <- bx_mats |>
  dplyr::filter(EnergyType == "BX") |>
  Recca::verify_SUT_energy_balance() |>
  Recca::calc_eta_i()

#
# Calculate heat loss to compare with values
# in Paper Examples.xlsx
#

heat_loss <- bx_mats |>
  dplyr::mutate(
    Q_loss = matsbyname::colsums_byname(.data[["U"]]) |>
      matsbyname::transpose_byname() |>
      matsbyname::difference_byname(matsbyname::rowsums_byname(.data[["V"]]))
  )

#
# Calculate efficiencies
#

eta_i_mat <- efficiencies$eta_i[[1]] |>
  as.matrix()
eta_i_df <- eta_i_mat |>
  as.data.frame(eta_i_mat, row.names = NULL) |>
  tibble::rownames_to_column(var = "machine")
eta_i_path <- file.path("data", "eta_i.xlsx")
eta_i_df |>
  openxlsx2::write_xlsx(file = eta_i_path, overwrite = TRUE)
