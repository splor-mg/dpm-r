#' @import data.table

#' @export
link <- function(df, key, cols) {
  dt <- as_data_table(df)
  cols_to_drop <- setdiff(names(dt), cols)
  dt[, (cols_to_drop) := NULL]
  dt[, (key) := do.call(paste, c(.SD, sep = "|")), .SDcols = cols]
  data.table::setcolorder(dt, key)
  unique(dt)
}

safe_link <- purrr::safely(link)

create_link_table <- function(datasets, keys) {
  dt_transformed <- lapply(datasets, link, keys = keys)
  unique(data.table::rbindlist(dt_transformed, fill = TRUE))
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
#' create_fact_table(df, key = list(chave_rec = c("uo_cod", "fonte_cod")))
#' create_fact_table(df, key = list(chave_rec = c("uo_cod", "fonte_cod")), drop_columns = "uo_sigla")
#'
#' @export
create_fact_table <- function(df, key, drop_columns = NULL) {
  dt <- as_data_table(df)
  key_name <- names(key)
  cols <- key[[key_name]]
  if (!all(cols %in% names(dt))) {
    dt[, (key_name) := NA_character_]
    } else {
      dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = cols]
    }
  cols_rm <- intersect(names(dt), unique(unlist(key)))
  dt[, (c(cols_rm, drop_columns)) := NULL]
  data.table::setcolorder(dt, key_name)
  dt[]
}

create_fact_table_ <- function(resource_name, package_name, config) {
  datapackage <- read_package(glue("datapackages/{package_name}/datapackage.json"))
  resource <- read_resource(datapackage, resource_name) |> as.data.table()
  key_name <- config$packages[[package_name]][[resource_name]]
  create_fact_table(resource, config$keys[key_name])
}

create_fact_tables <- function(package_name, config) {
  map(names(config$packages[[package_name]]), create_fact_table_, package_name, config)
}

#' @export
linktable <- function() {
  config <- RcppTOML::parseTOML("relationships.toml")

  resource_names <- map(config$packages, names) |> unlist() |> unname()

  fact_tables <- map(names(config$packages), create_fact_tables, config) |>
    rrapply::rrapply(classes = "data.frame", how = "flatten") |>
    set_names(resource_names)

  resources <- map(names(config$packages), \(package_name) read_datapackage(glue("datapackages/{package_name}/datapackage.json"))) |>
    rrapply::rrapply(classes = "data.frame", how = "flatten")

  link_table <- create_link_table(resources, keys = config$keys)

  fact_tables |> imap(\(value, key) fwrite(value, glue("data/{key}.csv")))

  fwrite(link_table, "data/link.csv")
}
