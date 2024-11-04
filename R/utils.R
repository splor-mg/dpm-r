is_url <-
function(x) {
  grepl("http:|https:", x)
}

as_data_table <- function(df) {
  if (data.table::is.data.table(df)) {
    result <- data.table::copy(df)
  } else {
    result <- data.table::as.data.table(df)
  }
  result
}

create_tables <- function(relationships = 'relationships.toml') {
  config <- parseTOML(relationships)
  create_table <- function(resource, resource_name) {
    df <- dpm::read_datapackage(resource$path) %>% .[[resource_name]]
    key_name <- resource$key_name
    key <- config$keys[key_name]
    keys <- config$keys

    list(
      df = df,
      key = key,
      drop_columns = NULL
    )
  }

  tables <- config$data |> purrr::imap(create_table)

  list(
    tables = tables,
    keys = config$keys
  )
}

create_link <- function(table, df, key, keys, table_name) {

  key_name <- names(key)
  key_columns <- key[[key_name]]
  dt <- as_data_table(df)

  if (!all(key_columns %in% names(dt))) {
    msg <- paste0(key_columns[!key_columns %in% names(dt)], collapse = ', ')
    stop(glue::glue("Coluna(s) '{msg}' em relationships.toml não está presente na base de dados '{table_name}', Criando chave nula."))
  }

  cols_to_drop <- setdiff(names(dt), key_columns)
  dt[, (cols_to_drop) := NULL]

  for (key_name in names(keys)) {
    key_columns <- keys[[key_name]]
    if (all(key_columns %in% names(dt))) {
      dt[,
         (key_name) := do.call(paste, c(.SD, sep = "|")),
         .SDcols = key_columns
      ]
    }
  }
  unique(dt)
  dt[]
}

create_fact_table <- function(df, key, drop_columns = NULL) {
  key_name <- names(key)
  key_columns <- key[[key_name]]

  dt <- as_data_table(df)

  dt[, (key_name) := do.call(paste, c(.SD, sep = "|")), .SDcols = key_columns]
  dt[, (c(key_columns, drop_columns)) := NULL]
  data.table::setcolorder(dt, key_name)
  dt[]
}
