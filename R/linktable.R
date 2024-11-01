#' @import data.table
#' @import glue
#' @import RcppTOML


#' @export
create_linktable <- function(relationships = 'relationships.toml'){
  result <- create_tables(relationships)
  tables <- result$tables
  keys <- result$keys

  result <- tables |>
    purrr::imap(\(table, table_name) create_link(table, table$df, table$key, keys, table_name)) |>
    data.table::rbindlist(fill = TRUE) |>
    unique()

  data.table::setcolorder(result, names(keys))
  fwrite(result, "data/link.csv")

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
create_fact_tables <- function(relationships = 'relationships.toml') {
  result <- create_tables(relationships)
  tables <- result$tables

  result <- tables |>
    purrr::iwalk(function(table, name) {
      fact_table <- create_fact_table(table$df, table$key, table$drop_columns)
      fwrite(fact_table, glue::glue("data/{name}.csv"))
    })

  result[]
}
