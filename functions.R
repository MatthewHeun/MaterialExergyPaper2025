# Type "?sprintf" at the console for details of fmt strings.
read_named_cell <- function(cell_name,
                            fmt = "%.2f",
                            file = file.path("data", "Example.xlsx")) {
  val_df <- openxlsx2::read_xlsx(file = file,
                                 named_region = cell_name,
                                 col_names = FALSE)
  val <- val_df[[1]][[1]]
  sprintf(fmt = fmt, val)
}

# digits is can be defined as a single value,
# interpreted as digits for the entire matrix.
# Or, specify as a vector:  c(overall, rownames, Col1, Col2, Col3, etc.)
# Default is digits = 2.
print_named_matrix <- function(matrix_name,
                               latex_label,
                               file = file.path("data", "Example.xlsx"),
                               digits = 2,
                               caption = NULL,
                               size = "\\normalsize") {
  # Read the matrix as a data frame
  mat_df <- openxlsx2::read_xlsx(file = file,
                                 named_region = matrix_name,
                                 row_names = TRUE) |>
    tibble::rownames_to_column(var = " ")

  # Create an xtable from the matrix
  xt <- xtable::xtable(mat_df,
                       digits = digits,
                       caption = caption,
                       align = rep("c", ncol(mat_df) + 1),
                       label = latex_label)
  # Print the matrix as a xtable
  print(xt,
        include.rownames = FALSE,
        # sanitize.colnames.function = identity,
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
                                   rownames_latex,
                                   file = file.path("data", "Example.xlsx"),
                                   digits = 2,
                                   caption = NULL,
                                   size = "\\normalsize",
                                   col_header_latex = c("$m_i$ \\\\ $\\mathrm{[kg_i]}$",
                                                        "MW \\\\ $\\mathrm{[kg_i/mol_i]}$",
                                                        "$N_i$ \\\\ $\\mathrm{[mol_i]}$",
                                                        "$b_{ch,i}$ \\\\ $\\mathrm{[kJ_i/mol_i]}$",
                                                        "$y_i$ \\\\ $\\mathrm{[mol_i/mol_m]}$",
                                                        "$\\ln(y_i)$ \\\\ $\\mathrm{[-]}$",
                                                        "$b_{c,i}$ \\\\ $\\mathrm{[kJ_i/mol_i]}$",
                                                        "$b_i$ \\\\ $\\mathrm{[kJ_i/mol_i]}$",
                                                        "$b_m$ \\\\ $\\mathrm{[kJ_i/mol_m]}$",
                                                        "$B_m$ \\\\ $\\mathrm{[kJ_i]}$")) {

  # Read the matrix as a data frame
  table_df <- openxlsx2::read_xlsx(file = file.path("data", "Example.xlsx"),
                                   named_region = "statepoint_table",
                                   row_names = FALSE,
                                   col_names = FALSE)
  rownames(table_df) <- rownames_latex
  colnames(table_df) <- paste0("\\makecell{", col_header_latex, "}")

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
