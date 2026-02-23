#' DuckDB connection wrapper with view registration and query execution.
#'
#' @noRd
Connection <- R6::R6Class("Connection",
  public = list(
    #' @field registered_views Character vector of registered view/table names.
    registered_views = character(0),

    #' @description Create a connection backed by the given cache.
    #' @param cache CacheManager instance.
    initialize = function(cache) {
      private$.cache <- cache
      private$.conn  <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      # Ensure required extensions are available
      tryCatch({
        DBI::dbExecute(private$.conn, "INSTALL json")
        DBI::dbExecute(private$.conn, "LOAD json")
      }, error = function(e) {
        tryCatch(DBI::dbExecute(private$.conn, "LOAD json"), error = function(e2) NULL)
      })
    },

    #' @description Close the underlying DuckDB connection.
    close = function() {
      if (!is.null(private$.conn)) {
        DBI::dbDisconnect(private$.conn, shutdown = TRUE)
        private$.conn <- NULL
      }
    },

    #' @description Lazily register one or more views.
    #' @param ... View names to register.
    ensure_views = function(...) {
      for (name in c(...)) {
        self$ensure_view(name)
      }
    },

    #' @description Lazily register a single view.
    #' @param view_name View name to register.
    ensure_view = function(view_name) {
      if (view_name %in% self$registered_views) return(invisible(NULL))

      path <- private$.cache$ensure_parquet(view_name)
      path_str <- forward_slashes(path)

      if (view_name == "card_legalities") {
        private$register_legalities_view(path_str)
        return(invisible(NULL))
      }

      replace_clause <- private$build_csv_replace(path_str, view_name)
      sql <- sprintf(
        "CREATE OR REPLACE VIEW %s AS SELECT *%s FROM read_parquet('%s')",
        view_name, replace_clause, path_str
      )
      DBI::dbExecute(private$.conn, sql)
      self$registered_views <- c(self$registered_views, view_name)
    },

    #' @description Execute SQL and return a data.frame.
    #' @param sql SQL query string.
    #' @param params Optional list of positional parameters.
    #' @return A data.frame of results.
    execute = function(sql, params = NULL) {
      if (!is.null(params) && length(params) > 0) {
        DBI::dbGetQuery(private$.conn, sql, params = params)
      } else {
        DBI::dbGetQuery(private$.conn, sql)
      }
    },

    #' @description Execute SQL and return a single scalar value.
    #' @param sql SQL query returning one row, one column.
    #' @param params Optional list of positional parameters.
    #' @return The scalar value, or NULL.
    execute_scalar = function(sql, params = NULL) {
      df <- self$execute(sql, params)
      if (nrow(df) == 0 || ncol(df) == 0) return(NULL)
      df[[1]][1]
    },

    #' @description Execute SQL and return a JSON string.
    #' @param sql SQL query string.
    #' @param params Optional list of positional parameters.
    #' @return JSON array string.
    execute_json = function(sql, params = NULL) {
      wrapped <- sprintf("SELECT to_json(list(sub)) FROM (%s) sub", sql)
      df <- self$execute(wrapped, params)
      if (nrow(df) == 0 || is.na(df[[1]][1])) return("[]")
      df[[1]][1]
    },

    #' @description Create a DuckDB table from a data.frame.
    #' @param table_name Name for the new table.
    #' @param data A data.frame to load.
    register_table_from_data = function(table_name, data) {
      if (is.null(data) || nrow(data) == 0) return(invisible(NULL))
      DBI::dbExecute(private$.conn,
        sprintf("DROP TABLE IF EXISTS %s", table_name))
      DBI::dbWriteTable(private$.conn, table_name, data, overwrite = TRUE)
      self$registered_views <- c(self$registered_views, table_name)
    },

    #' @description Create a DuckDB table from a list of lists (dicts).
    #' @param table_name Name for the new table.
    #' @param data A list of named lists.
    register_table_from_list = function(table_name, data) {
      if (is.null(data) || length(data) == 0) return(invisible(NULL))
      DBI::dbExecute(private$.conn,
        sprintf("DROP TABLE IF EXISTS %s", table_name))
      tmp <- tempfile(fileext = ".json")
      on.exit(unlink(tmp), add = TRUE)
      jsonlite::write_json(data, tmp, auto_unbox = TRUE)
      path_fwd <- forward_slashes(tmp)
      DBI::dbExecute(private$.conn, sprintf(
        "CREATE TABLE %s AS SELECT * FROM read_json_auto('%s')",
        table_name, path_fwd))
      self$registered_views <- c(self$registered_views, table_name)
    },

    #' @description Create a DuckDB table from a NDJSON file.
    #' @param table_name Name for the new table.
    #' @param ndjson_path Path to the NDJSON file.
    register_table_from_ndjson = function(table_name, ndjson_path) {
      DBI::dbExecute(private$.conn,
        sprintf("DROP TABLE IF EXISTS %s", table_name))
      path_fwd <- forward_slashes(ndjson_path)
      DBI::dbExecute(private$.conn, sprintf(
        "CREATE TABLE %s AS SELECT * FROM read_json_auto('%s', format='newline_delimited')",
        table_name, path_fwd))
      self$registered_views <- c(self$registered_views, table_name)
    },

    #' @description Access the underlying DBI connection.
    #' @return The DBI connection object.
    raw = function() {
      private$.conn
    }
  ),

  private = list(
    .conn  = NULL,
    .cache = NULL,

    build_csv_replace = function(path_str, view_name) {
      schema_df <- DBI::dbGetQuery(private$.conn, sprintf(
        "SELECT column_name, column_type FROM (DESCRIBE SELECT * FROM read_parquet('%s'))",
        path_str))

      schema <- stats::setNames(schema_df$column_type, schema_df$column_name)

      # Build candidate set from both layers
      candidates <- character(0)

      # Layer 1: Static baseline
      if (view_name %in% names(STATIC_LIST_COLUMNS)) {
        candidates <- c(candidates, STATIC_LIST_COLUMNS[[view_name]])
      }

      # Layer 2: Dynamic heuristic — VARCHAR columns ending in 's'
      for (col in names(schema)) {
        if (schema[[col]] != "VARCHAR") next
        if (col %in% IGNORED_COLUMNS) next
        if (grepl("s$", col)) {
          candidates <- c(candidates, col)
        }
      }
      candidates <- unique(candidates)

      # Filter to columns that actually exist as VARCHAR
      final_cols <- sort(candidates[candidates %in% names(schema) &
                                      schema[candidates] == "VARCHAR"])

      exprs <- character(0)

      # CSV -> array expressions
      for (col in final_cols) {
        exprs <- c(exprs, sprintf(
          'CASE WHEN "%s" IS NULL OR TRIM("%s") = \'\' THEN []::VARCHAR[] ELSE string_split("%s", \', \') END AS "%s"',
          col, col, col, col))
      }

      # Layer 4: JSON casting
      json_cols <- sort(JSON_CAST_COLUMNS[
        JSON_CAST_COLUMNS %in% names(schema) & schema[JSON_CAST_COLUMNS] == "VARCHAR"])
      for (col in json_cols) {
        exprs <- c(exprs, sprintf('TRY_CAST("%s" AS JSON) AS "%s"', col, col))
      }

      if (length(exprs) == 0) return("")
      paste0(" REPLACE (", paste(exprs, collapse = ", "), ")")
    },

    register_legalities_view = function(path_str) {
      schema_df <- DBI::dbGetQuery(private$.conn, sprintf(
        "SELECT column_name FROM (DESCRIBE SELECT * FROM read_parquet('%s'))",
        path_str))
      all_cols <- schema_df$column_name
      format_cols <- all_cols[all_cols != "uuid"]

      if (length(format_cols) == 0) {
        DBI::dbExecute(private$.conn, sprintf(
          "CREATE OR REPLACE VIEW card_legalities AS SELECT * FROM read_parquet('%s')",
          path_str))
      } else {
        cols_sql <- paste(sprintf('"%s"', format_cols), collapse = ", ")
        DBI::dbExecute(private$.conn, sprintf(
          "CREATE OR REPLACE VIEW card_legalities AS SELECT uuid, format, status FROM (UNPIVOT (SELECT * FROM read_parquet('%s')) ON %s INTO NAME format VALUE status) WHERE status IS NOT NULL",
          path_str, cols_sql))
      }
      self$registered_views <- c(self$registered_views, "card_legalities")
    }
  )
)
