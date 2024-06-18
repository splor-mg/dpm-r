#' @import data.table

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
  transposed_tables <- purrr::list_transpose(tables)
  result <- purrr::pmap(transposed_tables, create_link)
  result <- dplyr::bind_rows(result) |> unique()
  # https://stackoverflow.com/questions/55706560/make-rbindlist-skip-ignore-or-change-class-attribute-of-the-column
  # data.table::rbindlist(fill = TRUE)


  data.table::setcolorder(result, unique(transposed_tables$key_name))
  result[]
}


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
  transposed_tables <- purrr::list_transpose(tables)
  purrr::pmap(transposed_tables, create_fact_table)
}
