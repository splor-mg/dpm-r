#' @import data.table

#' Create rows for a linktable from a specific data frame
#'
#' The rows include the 'key_name' column obtained by concatenating the specified 'key_columns' as well each individual column.
#' @export
create_link <- function(df, key_name, key_columns, ...) {
  dt <- as_data_table(df)
  cols_to_drop <- setdiff(names(dt), key_columns)
  dt[, (cols_to_drop) := NULL]
  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]
  data.table::setcolorder(dt, key_name)
  unique(dt)
}

#' @export
create_linktable <- function(tables) {
  transposed_tables <- list_transpose(tables)
  result <- purrr::pmap(transposed_tables, create_link) |>
    data.table::rbindlist(fill = TRUE) |>
    unique()
  data.table::setcolorder(result, transposed_tables$key_name)
  result[]
}

#' Create a fact table for linktable modelling
#'
#' This function creates a new column in a data frame by concatenating all columns specified in the key list and then drops those columns from the resulting data table. It also allows the user to drop additional columns by specifying them in the `drop_columns` argument.
#'
#' @param df A frame to be transformed.
#' @param key_name A string with the new key column's name
#' @param key_columns A character vector of columns to be concatenated to generate the new 'key_name' column
#' @param drop_columns A character vector of additional columns to drop from the resulting data table. Defaults to NULL.
#'
#' @return A data.table with the new key column and without the original key columns and any additional specified columns.
#'
#' @examples
#' df <- data.frame(
#'   uo_cod = c(1501, 1501, 1251, 1251, 1251),
#'   uo_sigla = c("SEPLAG", "SEPLAG", "PM", "PM", "PM"),
#'   fonte_cod = c(60, 10, 10, 10, 60),
#'   vl_receita_exec = c(60, 70, 80, 90, 100)
#' )
#'
#' create_fact_table(df, key_name = "chave_rec", key_columns c("uo_cod", "fonte_cod"))
#' create_fact_table(df, key_name = "chave_rec", key_columns c("uo_cod", "fonte_cod"), drop_columns = "uo_sigla")
#' @export
create_fact_table <- function(df, key_name, key_columns, drop_columns = NULL) {
  dt <- as_data_table(df)
  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]
  dt[, (c(key_columns, drop_columns)) := NULL]
  data.table::setcolorder(dt, key_name)
  dt[]
}

#' @export
create_fact_tables <- function(tables) {
  transposed_tables <- list_transpose(tables)
  pmap(transposed_tables, create_fact_table)
}
