# This script creates a
# unified material and energy conversion chain (BXCC)
# for the blast furnace example in the paper.

# Read information downloaded from the Mexer database.
# See the script download_data.R,
# which creates the zaf_2013_ecc.rds file.

zaf_2013_ecc <- file.path("data", "zaf_2013_ecc.rds") |>
  readRDS()

# Verify the inter-industry balance before we get started
zaf_2013_ecc |>
  Recca::verify_inter_industry_balance()

# Read ECC requirements from the MCC spreadsheet.
# These energy requirements are in TJ.

mcc_e_reqts <- openxlsx2::read_xlsx(file = file.path("data",
                                                     "Paper Examples.xlsx"),
                                    named_region = "mcc_EX_reqts") |>
  dplyr::select(-`E [kJ]`, -`X [kJ]`, -`X [TJ]`) |>
  dplyr::rename(
    E = "E [TJ]",
    rownames = "EnergyCarrier"
  ) |>
  tidyr::pivot_longer(cols = "E",
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

# Calculate an ECC that contains only the energy carriers
# consumed by the MCC.
# This ECC is in TJ.
# Note that many pieces of the ECC are covered by our MCC,
# so we trim those out to focus only on the rows and columns
# of interest for supplying the MCC
# before calculating the input-output matrices.
# The pieces we need
# are identified by row and column names of the Y matrix.
# The row names of interest are the energy carriers needed
# by the MCC.
# These row names come from the row names of the Y_prime matrix
# in the mcc_energy_reqts matrix.
# The column names of interest are the two sectors
# involved in the MCC:
# the Iron and steel and Mining and quarrying sectors

Y_rownames_supply <- mcc_e_reqts$Y_prime[[1]] |>
  rownames()
Y_colnames_supply <- c("Iron and steel", "Mining and quarrying")

ecc_supply_to_mcc <- zaf_2013_ecc |>
  dplyr::filter(EnergyType == "E") |>
  Recca::calc_io_mats() |>
  dplyr::mutate(
    # Isolate only the rows of Y we need
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
  # Use the Y_prime matrix to
  # calculate *_prime matrices that supply only those
  # energy carriers and sectors we need,
  # namely Iron and steel and Mining and quarrying
  Recca::new_Y() |>
  # Keep only the _prime matrices
  dplyr::mutate(
    R = NULL, U = NULL, V = NULL, Y = NULL, U_feed = NULL, U_EIOU = NULL, r_EIOU = NULL
  ) |>
  # Rename _prime matrices to be R, U, V, Y etc.
  dplyr::rename(
    R = R_prime, U = U_prime, V = V_prime, Y = Y_prime,
    U_feed = U_feed_prime, U_EIOU = U_EIOU_prime, r_EIOU = r_EIOU_prime
  ) |>
  dplyr::mutate(
    # Create a new Y matrix that has just an Iron and steel sector
    # that is the sum of the Iron and steel and Mining and quarrying sectors
    # (the two sectors in Y_colnames_supply).
    # This is necessary, because the MCC's
    # Iron and steel sector includes its mining operations.
    # Call it the Y matrix,
    # in the process deleting the existing Y matrix.
    Y = .data[["Y"]] |>
      matsbyname::rowsums_byname(colname = "Iron and steel"),
  ) |>
  # At this point, we have a consistent ECC that
  # includes only energy types needed to supply energy to the MCC.
  # But the scale of the ECC is wrong.
  # It is too large. We need to righ-size the ECC.
  # To do so, we add the Y_prime matrix from the MCC energy requirements.
  # Now add the Y_prime matrix from mcc_energy_reqts to the data frame
  dplyr::left_join(mcc_e_reqts, by = "EnergyType") |>
  # Calculate the ECC needed to supply the Y_prime demand
  Recca::new_Y() |>
  dplyr::mutate(
    # Keep only the _prime matrices
    R = NULL, U = NULL, V = NULL, Y = NULL, U_EIOU = NULL, U_feed = NULL, r_EIOU = NULL
  ) |>
  dplyr::rename(
    R = R_prime, U = U_prime, V = V_prime, Y = Y_prime,
    U_EIOU = U_EIOU_prime, U_feed = U_feed_prime, r_EIOU = r_EIOU_prime
  ) |>
  dplyr::select(Dataset, ValidFromVersion, ValidToVersion, Country, Method, EnergyType, LastStage,
                IncludesNEU, Year, R, U, V, Y, U_feed, U_EIOU, r_EIOU, S_units) |>
  dplyr::mutate(
    # Convert from TJ to kJ everywhere
    R = matsbyname::hadamardproduct_byname(R, 1e9),
    U = matsbyname::hadamardproduct_byname(U, 1e9),
    U_feed = matsbyname::hadamardproduct_byname(U_feed, 1e9),
    U_EIOU = matsbyname::hadamardproduct_byname(U_EIOU, 1e9),
    V = matsbyname::hadamardproduct_byname(V, 1e9),
    Y = matsbyname::hadamardproduct_byname(Y, 1e9),
    S_units = matsbyname::setcolnames_byname(S_units, colnames = "kJ")
  ) |>
  dplyr::mutate(
    WorksheetNames = paste(Country, Year, EnergyType, sep = "_")
  )

# Save the ECC that supplies the MCC to an Excel file for inspection.
# This ECC is in TJ.

ecc_supply_to_mcc |>
  Recca::write_ecc_to_excel(path = file.path("data", "ecc_supply_to_mcc.xlsx"),
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)


# Note: Step numbers below align with the the file CalcFlowChart.ppt.

#
# Step 1: Endogenize heat losses for the ECC that supplies the MCC
#

## Create heat loss allocation matrix

heat_loss_allocation_mat <- file.path("data", "HeatLossFlows.xlsx") |>
  readxl::read_excel() |>
  dplyr::select(Industry, HeatLossFlow) |>
  dplyr::mutate(
    Qloss = "Transformation losses",
    HeatLossFlow = RCLabels::paste_pref_suff(pref = HeatLossFlow, suff = Qloss),
    Qloss = NULL
  ) |>
  dplyr::mutate(
    matvals = 1
  ) |>
  dplyr::rename(
    rownames = "Industry",
    colnames = "HeatLossFlow"
  ) |>
  matsindf::rowcolval_to_mat(rowtypes = "Industry", coltypes = "Product")

## Endogenize heat losses for the ECC that supplies the MCC

ecc_supply_to_mcc_with_losses <- ecc_supply_to_mcc |>
  dplyr::mutate(
    # Add the heat loss allocation matrix to the data frame
    "{Recca::balance_cols$losses_alloc_colname}" := list(heat_loss_allocation_mat)
  ) |>
  # Endogenize the heat losses
  Recca::endogenize_losses(replace_cols = TRUE)

## Save trimmed ZAF data to an Excel file for later inspection.

ecc_supply_to_mcc_with_losses_path <- file.path("data", "ecc_supply_to_mcc_with_losses.xlsx")
ecc_supply_to_mcc_with_losses |>
  Recca::write_ecc_to_excel(path = ecc_supply_to_mcc_with_losses_path,
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)

#
# Step 2: Convert the ECC with endogenized heat losses to exergy
#

## Create the phi vector

### Database phi vector
phi_db <- file.path("data", "zaf2013_phi.rds") |>
  readRDS() |>
  purrr::pluck("phi", 1)
### Waste heat phi vector
phi_waste_heat <- file.path("data", "HeatLossFlows.xlsx") |>
  readxl::read_excel() |>
  dplyr::select(HeatLossFlow, phi) |>
  unique() |>
  dplyr::rename(
    rownames = "HeatLossFlow",
    matvals = "phi"
  ) |>
  dplyr::mutate(
    colnames = "phi"
  ) |>
  matsindf::rowcolval_to_mat(rowtypes = "Product", coltypes = "phi")
### Check for any identical rownames
common_rows <- intersect(rownames(phi_db), rownames(phi_waste_heat))
### Eliminate common rows from phi_waste_heat
phi_waste_heat <- matsbyname::select_rows_byname(phi_waste_heat,
                                                 remove_pattern = RCLabels::make_or_pattern(common_rows))
### Verify we have no common rows
common_rows_2 <- intersect(rownames(phi_db), rownames(phi_waste_heat))
assertthat::assert_that(length(common_rows_2) == 0)

## Add the vectors together
phi_vec <- matsbyname::sum_byname(phi_db, phi_waste_heat)

xcc_supply_to_mcc_with_losses <- ecc_supply_to_mcc_with_losses |>
  ## Add the phi vector that already includes phi for heat losses
  dplyr::mutate(
    "{Recca::psut_cols$phi}" := list(phi_vec)
  ) |>
  ## Convert to exergy using the phi vector
  Recca::extend_to_exergy(mat_piece = "noun",
                          # R and U matrices have from notation,
                          # V and Y matrices have arrow notation.
                          notation = list(RCLabels::from_notation, RCLabels::arrow_notation)) |>
  dplyr::mutate(
    ## Add worksheet names
    WorksheetNames = paste0(Country, "_", Year, "_", EnergyType)
  )

## Save trimmed ZAF data to an Excel file for later inspection.

xcc_supply_to_mcc_with_losses_path <- file.path("data", "xcc_supply_to_mcc_with_losses.xlsx")
xcc_supply_to_mcc_with_losses |>
  dplyr::filter(EnergyType == "X") |>
  Recca::write_ecc_to_excel(path = xcc_supply_to_mcc_with_losses_path,
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)

#
# Step 3: Endogenize mass losses
#

## First, read the mass matrices

mcc_m_mats <- file.path("data", "Paper Examples.xlsx") |>
  Recca::read_ecc_from_excel(worksheets = "MCC_M_RUVY_matrices_mat_level") |>
  # Verify that inter-industry balances is observed
  Recca::verify_inter_industry_balance(delete_balance_if_verified = TRUE) |>
  # Verify that intra-industry balance is observed
  Recca::verify_intra_industry_balance(delete_balance_if_verified = TRUE) |>
  dplyr::mutate(
    which = "M"
  )

## Here, we should endogenize mass losses, but we
## don't need to do so.
## Mass losses are already endogenized in mcc_m_mats.

#
# Step 4: Create the energy matrices with m*h_bar/MW in matrices
#

## Read the enthalpy RUVY matrices

mcc_h_mats <- file.path("data", "Paper Examples.xlsx") |>
  Recca::read_ecc_from_excel(worksheets = "MCC_h_RUVY_matrices_mat_level") |>
  dplyr::mutate(
    which = "H"
  )

## Read the molecular weight RUVY matrices

mcc_mw_mats <- file.path("data", "Paper Examples.xlsx") |>
  Recca::read_ecc_from_excel(worksheets = "MCC_MW_RUVY_matrices_mat_level") |>
  dplyr::mutate(
    which = "MW"
  )

## Calculate M*H/MW

S_units_mat <- Matrix::Matrix(1,
                              nrow = nrow(mcc_m_mats$U[[1]]),
                              ncol = 1,
                              dimnames = list(rownames(mcc_m_mats$U[[1]]), "kJ"))

mcc_e_mats <- dplyr::bind_rows(mcc_m_mats, mcc_h_mats, mcc_mw_mats) |>
  dplyr::mutate(
    S_units = NULL,
    WorksheetNames = NULL
  ) |>
  tidyr::pivot_longer(cols = c(R, U, V, Y, U_feed, U_EIOU, r_EIOU)) |>
  tidyr::pivot_wider(names_from = which, values_from = value) |>
  dplyr::mutate(
    E = matsbyname::hadamardproduct_byname(M, H) |>
      matsbyname::quotient_byname(MW) |>
      matsbyname::replaceNaN_byname() |>
      matsbyname::clean_byname(),
    # We don't need these matrices any more,
    # now that we have the energy versions.
    M = NULL,
    H = NULL,
    MW = NULL
  ) |>
  tidyr::pivot_wider(names_from = "name", values_from = "E") |>
  ### Add S_units matrix back
  dplyr::mutate(
    S_units = list(S_units_mat)
  )

## Read electricity (massless energy) inputs to each processing stage

elect_inputs <- openxlsx2::read_xlsx(file = file.path("data",
                                                      "Paper Examples.xlsx"),
                                     sheet = "BXCC Q loss",
                                     named_region = "Electricity_input")

## Calculate R matrix electricity supply.
## This item will be added to the R matrix.

elect_inputs_vec_R <- elect_inputs_vec_U |>
  matsbyname::transpose_byname() |>
  matsbyname::colsums_byname() |>
  matsbyname::setrownames_byname("Supply [of Electricity]") |>
  matsbyname::setcolnames_byname("Electricity [from Supply]")

## Calculate U matrix electricity usage.
## This item will be added to the U matrix.

elect_inputs_vec_U <- file.path("data", "Paper Examples.xlsx") |>
  openxlsx2::wb_load() |>
  openxlsx2::wb_to_df(sheet = "BXCC Q loss",
                      named_region = "Electricity_input",
                      row_names = TRUE) |>
  as.matrix() |>
  matsbyname::setcolnames_byname("Electricity [from Supply]") |>
  matsbyname::setrowtype("Industry") |>
  matsbyname::setcoltype("Product") |>
  ### Will add to U matrix, so transpose
  matsbyname::transpose_byname()

#
# Step 5: Add mass and massless energy matrices for the MCC
#

E_m_mats <- mcc_e_mats |>
  dplyr::mutate(
    "{Recca::balance_cols$losses_alloc_colname}" :=
      RCLabels::make_list(Recca::balance_cols$default_losses_alloc |>
                            matsbyname::setcolnames_byname("Waste heat"),
                          n = dplyr::n(),
                          lenx = 1),
    # Add electricity to the U matrix
    Elect = list(elect_inputs_vec_U),
    U = matsbyname::sum_byname(U, Elect),
    # Add electricity to the R matrix
    Elect = list(elect_inputs_vec_R),
    R = matsbyname::sum_byname(R, Elect)
  )

#
# Step 6: Calculate waste heat (by endogenizing losses)
#

Q_waste_heat <- E_m_mats |>
  ## Endogenize losses to calculate waste heat
  Recca::endogenize_losses(replace_cols = TRUE) |>
  dplyr::mutate(
    waste_heat = matsbyname::select_cols_byname(V, retain_pattern = "Waste heat")
  ) |>
  purrr::pluck("V", 1) |>
  matsbyname::select_cols_byname("Waste heat") |>
  matsbyname::setcolnames_byname("Q_waste_heat")

#
# Step 7: Convert mass flow matrices to exergy
#

# Read the phi matrices
mcc_phi_mats <- file.path("data", "Paper Examples.xlsx") |>
  Recca::read_ecc_from_excel(worksheets = "MCC_phi_RUVY_matrices_mat_level") |>
  dplyr::mutate(
    WorksheetNames = NULL
  ) |>
  tidyr::pivot_longer(cols = c(R, U, V, Y, r_EIOU, U_EIOU, U_feed, S_units),
                      names_to = "matnames",
                      values_to = "phi") |>
  dplyr::mutate(
    which = NULL,
    WorksheetNames = NULL
  )

## Multiply the mass matrices by phi to develop exergy matrices for mass flows.
b_m_mats <- mcc_m_mats |>
  tidyr::pivot_longer(cols = c(R, U, V, Y, r_EIOU, U_EIOU, U_feed, S_units),
                      names_to = "matnames",
                      values_to = "m") |>
  dplyr::mutate(
    which = NULL,
    WorksheetNames = NULL
  ) |>
  ### Add the phi matrices
  dplyr::full_join(mcc_phi_mats, by = "matnames") |>
  dplyr::mutate(
    m = matsbyname::clean_byname(m),
    phi = matsbyname::clean_byname(phi),
    ### Multiply masses by phi
    B = matsbyname::hadamardproduct_byname(m, phi) |>
      matsbyname::clean_byname()
  ) |>
  dplyr::mutate(
    m = NULL,
    phi = NULL
  ) |>
  tidyr::pivot_wider(names_from = matnames, values_from = B) |>
  dplyr::mutate(
    ### We multiplied kg by kJ/kg, so the unit is not kJ.
    S_units = S_units |>
      matsbyname::setcolnames_byname("kJ")
  )

#
# Step 8: Calculate exergy of waste heat flows of the mass matrices
#

## Read the phi vector read from the Excel file.
T_waste_heat <- file.path("data", "Paper Examples.xlsx") |>
  openxlsx2::wb_load() |>
  openxlsx2::wb_to_df(sheet = "BXCC Q loss",
                      named_region = "T_waste_heat",
                      row_names = TRUE) |>
  as.matrix() |>
  matsbyname::setrowtype("Industry") |>
  matsbyname::setcoltype("Product")
T_0 <- 298.15 # K
phi_waste_heat <- 1 - matsbyname::quotient_byname(T_0, T_waste_heat) |>
  matsbyname::setcolnames_byname("phi")
## Calculate exergy of waste heat
X_waste_heat <- matsbyname::hadamardproduct_byname(
  Q_waste_heat |> matsbyname::setcolnames_byname("X_waste_heat"),
  phi_waste_heat |> matsbyname::setcolnames_byname("X_waste_heat")
)

#
# Step 9: Convert electricity supply to MCC to exergy
#

## Step 9 is not needed, as the phi value for electricity is 1.0.

#
# Step 10: Sum exergy matrices for mass, waste heat, and electricity
# to form the B matrices
#

bcc_mats <- b_m_mats |>
  dplyr::mutate(
    ## Add waste heat exergy to the mass V and Y matrices
    V = matsbyname::sum_byname(V, X_waste_heat),
    Y = matsbyname::sum_byname(Y, X_waste_heat |>
                                 matsbyname::colsums_byname(rowname = "Transformation losses") |>
                                 matsbyname::transpose_byname()),
    ## Add electricity to the R and U matrices
    R = matsbyname::sum_byname(R, elect_inputs_vec_R),
    U = matsbyname::sum_byname(U, elect_inputs_vec_U)
  ) |>
  ## Verify that inter-industry flows remain in balance.
  Recca::verify_inter_industry_balance(delete_balance_if_verified = TRUE)

#
# Step 11: Add the XCC to BCC
#

## Clean up the xcc_supply_to_mcc_with_losses data frame
## in preparation for summation with the MCC.

xcc_supply_to_bcc_long <- xcc_supply_to_mcc_with_losses |>
  dplyr::filter(EnergyType == "X") |>
  dplyr::mutate(
    ### Remove unneeded columns
    WorksheetNames = NULL,
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
  tidyr::pivot_longer(cols = c("R", "U", "V", "Y", "U_feed", "U_EIOU", "r_EIOU", "S_units"),
                      names_to = "matnames",
                      values_to = "X") |>
  ### Modify the XCC by removing the Y matrix.
  ### Final exergy will be injected into the MCC
  ### after summing the MCC and the XCC.
  dplyr::filter(!matnames == "Y") |>
  ### There are several near-zero but not zero values in these matrices.
  ### Eliminate small values.
  dplyr::mutate(
    X = matsbyname::clean_byname(.data[["X"]], tol = 1e-8)
  )

## Modify the BCC by removing the "Supply [of X]"
## rows from the R matrix and removing
## the [from Supply] suffix from row and column names
## in other matrices to prepare for summation.

bcc_mats_long <- bcc_mats |>
  dplyr::mutate(
    WorksheetNames = NULL,
    R = matsbyname::select_rows_byname(.data[["R"]], remove_pattern = "^Supply")
  ) |>
  tidyr::pivot_longer(cols = c("R", "U", "V", "Y", "U_feed", "U_EIOU", "r_EIOU", "S_units"),
                      names_to = "matnames",
                      values_to = "B") |>
  ### Rename Product margins by removing " [from Supply]",
  ### thereby preparing for summation.
  dplyr::mutate(
    B = .data[["B"]] |>
      matsbyname::rename_via_pattern_byname(margin = "Product",
                                            regexp_pattern = " [from Supply]",
                                            replacement = "",
                                            fixed = TRUE)
  )

## Sum the XCC and BCC matrices

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

## Write the combined material and energy conversion chains
## to an Excel file for inspection

bx_mats |>
  Recca::write_ecc_to_excel(path = file.path("data", "BXCC.xlsx"),
                            worksheet_names = "EnergyType",
                            overwrite_file = TRUE,
                            overwrite_worksheets = TRUE)

#
# Step 12: Endogenize exergy imbalance
#          to calculate exergy destruction
#

bx_mats_with_exergy_destruction <- bx_mats |>
  dplyr::filter(EnergyType == "BX") |>
  dplyr::mutate(
    "{Recca::balance_cols$losses_alloc_colname}" :=
      RCLabels::make_list(Recca::balance_cols$default_losses_alloc |>
                            matsbyname::setcolnames_byname("Destroyed exergy"),
                          n = dplyr::n(),
                          lenx = 1)
  ) |>
  Recca::endogenize_losses(losses_sector = "Exergy destruction")

#
# Write the data to an RDS file for use in the paper
#

bx_mats_with_exergy_destruction |>
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
