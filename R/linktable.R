#' @import data.table

#' @export
create_link <- function(df, key, ...) {
  key_name <- names(key)
  key_columns <- key[[key_name]]

  dt <- as_data_table(df)

  cols_to_drop <- setdiff(names(dt), key_columns)
  dt[, (cols_to_drop) := NULL]
  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]
  data.table::setcolorder(dt, key_name)
  unique(dt)
}

#' @export
create_linktable <- function(tables) {

  result <- tables |>
            purrr::map(\(table) create_link(table$df, table$key)) |>
            data.table::rbindlist(fill = TRUE) |>
            unique()

  key_columns <- purrr::map(tables, \(table) names(table$key)) |>
                unlist() |>
                unique()
  data.table::setcolorder(result, key_columns)

  result[]
}


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
