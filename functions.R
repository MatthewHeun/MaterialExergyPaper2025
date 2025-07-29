
# Type "?sprintf" at the console for details of fmt strings.
read_named_cell <- function(cell_name,
                            unit = NULL,
                            mathrm = FALSE,
                            fmt = "%.2f",
                            file = file.path("data", "Paper Examples.xlsx")) {
  val_df <- openxlsx2::read_xlsx(file = file,
                                 named_region = cell_name,
                                 col_names = FALSE)
  val <- val_df[[1]][[1]]
  out <- sprintf(fmt = fmt, val)
  if (!is.null(unit)) {
    if (mathrm) {
      unit <- paste0("\\\\mathrm{", unit, "}")
    }
    out <- paste0(out, "~", unit)
  }
  return(out)
}



# digits is can be defined as a single value,
# interpreted as digits for the entire matrix.
# Or, specify as a vector:  c(overall, rownames, Col1, Col2, Col3, etc.)
# Default is digits = 2.
print_named_matrix <- function(matrix_name,
                               latex_label,
                               bgcolor = "white",
                               file = file.path("data", "Paper Examples.xlsx"),
                               digits = 2,
                               caption = NULL,
                               size = "normalsize") {
  # Read the matrix as a data frame
  mat_df <- openxlsx2::read_xlsx(file = file,
                                 named_region = matrix_name,
                                 row_names = TRUE) |>
    # Add a background colour to cells in every column
    dplyr::mutate(
      dplyr::across(dplyr::everything(),
                    ~ ifelse(is.na(.),
                             # When NA, add just color
                             paste0("\\cellcolor{", bgcolor, "} "),
                             # When not NA, include the value
                             paste0("\\cellcolor{", bgcolor, "} ", .)
                    )
      )
    ) |>
    tibble::rownames_to_column(var = " ")


  # Create an xtable from the matrix
  xt <- xtable::xtable(mat_df,
                       digits = digits,
                       caption = caption,
                       align = c("r", "r", rep("c", ncol(mat_df)-1)),
                       label = latex_label)
  # Print the matrix as a xtable
  print(xt,
        include.rownames = FALSE,
        # sanitize.colnames.function = identity,
        sanitize.text.function = identity,
        rotate.colnames = TRUE,
        hline.after = NULL,
        floating.environment = "figure",
        size = size,
        table.placement = NULL,
        format.args = list(big.mark = ",", decimal.mark = "."))
}

# Set a name in the Excel sheet for the numbers of the table only.
# Set rownames in the rownames_latex argument.
# Set column names and units in arguments (or accept defaults).
print_statepoint_table <- function(statepoint_table_name,
                                   latex_label,
                                   file = file.path("data", "Paper Examples.xlsx"),
                                   digits = 2,
                                   caption = NULL,
                                   size = "normalsize",
                                   rownames_latex,
                                   colnames_latex = c("$m_i$ \\\\ $\\mathrm{[kg_i]}$",
                                                      "$x_i$ \\\\ $\\mathrm{[kg_i/kg_m]}$",
                                                      "$MW_i$ \\\\ $\\mathrm{[kg_i/mol_i]}$",
                                                      "$N_i$ \\\\ $\\mathrm{[mol_i]}$",
                                                      "$h_i$ \\\\ $\\mathrm{[kJ_i/kg_i]}$",
                                                      "$b_{ch,i}$ \\\\ $\\mathrm{[kJ_i/mol_i]}$",
                                                      "$y_i$ \\\\ $\\mathrm{[mol_i/mol_m]}$",
                                                      "$\\ln(y_i)$ \\\\ $\\mathrm{[-]}$",
                                                      "$b_{c,i}$ \\\\ $\\mathrm{[kJ_i/mol_i]}$",
                                                      "$b_i$ \\\\ $\\mathrm{[kJ_i/mol_i]}$",
                                                      "$b_m$ \\\\ $\\mathrm{[kJ_i/mol_m]}$",
                                                      "$B_m$ \\\\ $\\mathrm{[kJ_i]}$")) {

  # Read the matrix as a data frame
  table_df <- openxlsx2::read_xlsx(file = file,
                                   named_region = statepoint_table_name,
                                   row_names = FALSE,
                                   col_names = FALSE)
  rownames(table_df) <- rownames_latex
  colnames(table_df) <- paste0("\\makecell{", colnames_latex, "}")

  xt <- xtable::xtable(table_df,
                       digits = digits,
                       caption = caption,
                       label = latex_label)

  print(xt,
        include.rownames = TRUE,
        sanitize.colnames.function = identity,
        sanitize.rownames.function = identity,
        rotate.colnames = FALSE,
        size = size,
        booktabs = TRUE,
        hline.after = c(-1, 0, nrow(xt)-1, nrow(xt)),
        table.placement = NULL,
        caption.placement = "top",
        format.args = list(big.mark = ",", decimal.mark = "."))
}


# Read the efficiencies table and print it into the paper
print_efficiencies_table <- function(efficiencies_table_name,
                                     latex_label,
                                     file = file.path("data", "Paper Examples.xlsx"),
                                     digits = 2,
                                     caption = NULL,
                                     size = "normalsize",
                                     convert_to_perc = FALSE) {

  table_df <- openxlsx2::read_xlsx(file = file,
                                   named_region = efficiencies_table_name,
                                   row_names = TRUE,
                                   col_names = TRUE) |>
    # Make sure all columns are numeric
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.numeric))
  if (convert_to_perc) {
    table_df <- table_df * 100
  }

  xt <- xtable::xtable(table_df,
                       digits = digits,
                       caption = caption,
                       label = latex_label)
  print(xt,
        include.rownames = TRUE,
        sanitize.colnames.function = identity,
        sanitize.rownames.function = identity,
        rotate.colnames = FALSE,
        size = size,
        booktabs = TRUE,
        # hline.after = c(-1, 0, nrow(xt)-1, nrow(xt)),
        table.placement = NULL,
        caption.placement = "top",
        format.args = list(big.mark = ",", decimal.mark = "."))
}
