#' @import data.table

create_link <- function(df, key_name, key_columns, ...) {
  dt <- as_data_table(df)
  cols_to_drop <- setdiff(names(dt), key_columns)
  dt[, (cols_to_drop) := NULL]
  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]
  data.table::setcolorder(dt, key_name)
  unique(dt)
}

#' Create a Fact Table for Linktable Modelling
#'
#' This function creates a new column in a data frame by concatenating all columns specified in the key list and then drops those columns from the resulting data table. It also allows the user to drop additional columns by specifying them in the `drop_columns` argument.
#'
#' @param df A frame to be transformed.
#' @param key A named list where the name is used as the new key column's name, and the value is a character vector of columns to be concatenated.
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
#' create_fact_table(df, key_name = list(chave_rec = c("uo_cod", "fonte_cod")))
#' create_fact_table(df, key_name = list(chave_rec = c("uo_cod", "fonte_cod")), drop_columns = "uo_sigla")
create_fact_table <- function(df, key_name, key_columns, drop_columns = NULL) {
  dt <- as_data_table(df)
  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]
  dt[, (c(key_columns, drop_columns)) := NULL]
  data.table::setcolorder(dt, key_name)
  dt[]
}

