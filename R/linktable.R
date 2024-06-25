#' @import data.table

#' @export
create_link <- function(df, key, keys) {
  key_name <- names(key)
  key_columns <- key[[key_name]]

  dt <- as_data_table(df)

  cols_to_drop <- setdiff(names(dt), key_columns)
  dt[, (cols_to_drop) := NULL]
  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]

  unique(dt)

  for (key_name in names(config$keys)) {
    key_columns <- config$keys[[key_name]]
    if (all(key_columns %in% names(dt))) {
      dt[,
        (key_name) := do.call(paste, c(.SD, sep = "|")),
        .SDcols = key_columns
      ]
    }
  }
  dt[]
}

#' @export
create_linktable <- function(tables, keys) {
  result <- tables |>
    purrr::map(\(table) create_link(table$df, table$key, keys)) |>
    data.table::rbindlist(fill = TRUE) |>
    unique()

  data.table::setcolorder(result, names(keys))

  result[]
}

#' Create a fact table for a linktable data model
#'
#' This function creates a new column in a data frame by concatenating all columns specified in the key list and then drops those columns from the resulting data table.
#' It also allows the user to drop additional columns by specifying them in the `drop_columns` argument.
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
#' create_fact_table(df, key = list(chave_rec = c("uo_cod", "fonte_cod")))
#' create_fact_table(df, key = list(chave_rec = c("uo_cod", "fonte_cod")), drop_columns = "uo_sigla")
#'
#' @export
create_fact_table <- function(df, key, drop_columns = NULL) {
  key_name <- names(key)
  key_columns <- key[[key_name]]

  dt <- as_data_table(df)

  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]
  dt[, (c(key_columns, drop_columns)) := NULL]
  data.table::setcolorder(dt, key_name)
  dt[]
}

#' @export
create_fact_tables <- function(tables) {
  result <- purrr::map(tables, \(table) create_fact_table(table$df, table$key))
  result
}
