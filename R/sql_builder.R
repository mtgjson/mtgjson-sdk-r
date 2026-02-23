#' SQL Builder for parameterized DuckDB queries.
#'
#' All user-supplied values go through DuckDB parameter binding ($1, $2, ...),
#' never through string interpolation. Methods return `invisible(self)` for
#' chaining.
#'
#' @export
SqlBuilder <- R6::R6Class("SqlBuilder",
  public = list(
    #' @description Create a builder targeting the given table or view.
    #' @param base_table The DuckDB table/view name to query from.
    initialize = function(base_table) {
      private$.select   <- "*"
      private$.distinct <- FALSE
      private$.from     <- base_table
      private$.joins    <- character(0)
      private$.where    <- character(0)
      private$.params   <- list()
      private$.group_by <- character(0)
      private$.having   <- character(0)
      private$.order_by <- character(0)
      private$.limit    <- NULL
      private$.offset   <- NULL
    },

    #' @description Set the columns to select (replaces default `*`).
    #' @param ... Column names or expressions.
    select = function(...) {
      private$.select <- c(...)
      invisible(self)
    },

    #' @description Add DISTINCT to the SELECT clause.
    distinct = function() {
      private$.distinct <- TRUE
      invisible(self)
    },

    #' @description Add a JOIN clause.
    #' @param clause Full JOIN clause string.
    join = function(clause) {
      private$.joins <- c(private$.joins, clause)
      invisible(self)
    },

    #' @description Add a WHERE condition with $N placeholders.
    #' @param condition SQL condition with $1, $2, ... placeholders.
    #' @param ... Values bound to the placeholders.
    where = function(condition, ...) {
      params <- list(...)
      offset <- length(private$.params)
      remapped <- condition
      if (length(params) > 0) {
        for (i in seq(length(params), 1)) {
          remapped <- gsub(
            paste0("\\$", i),
            paste0("$", offset + i),
            remapped,
            fixed = FALSE
          )
        }
      }
      private$.where <- c(private$.where, remapped)
      private$.params <- c(private$.params, params)
      invisible(self)
    },

    #' @description Add a case-insensitive LIKE condition.
    #' @param column Column name.
    #' @param value LIKE pattern.
    where_like = function(column, value) {
      idx <- length(private$.params) + 1L
      private$.where <- c(private$.where,
        sprintf("LOWER(%s) LIKE LOWER($%d)", column, idx))
      private$.params <- c(private$.params, list(value))
      invisible(self)
    },

    #' @description Add an IN condition with parameterized values.
    #' @param column Column name.
    #' @param values List of values for the IN clause.
    where_in = function(column, values) {
      if (length(values) == 0) {
        private$.where <- c(private$.where, "FALSE")
        return(invisible(self))
      }
      placeholders <- character(length(values))
      for (i in seq_along(values)) {
        idx <- length(private$.params) + 1L
        placeholders[i] <- paste0("$", idx)
        private$.params <- c(private$.params, list(values[[i]]))
      }
      private$.where <- c(private$.where,
        sprintf("%s IN (%s)", column, paste(placeholders, collapse = ", ")))
      invisible(self)
    },

    #' @description Add an equality condition.
    #' @param column Column name.
    #' @param value Value to match.
    where_eq = function(column, value) {
      idx <- length(private$.params) + 1L
      private$.where <- c(private$.where, sprintf("%s = $%d", column, idx))
      private$.params <- c(private$.params, list(value))
      invisible(self)
    },

    #' @description Add a >= condition.
    #' @param column Column name.
    #' @param value Minimum value (inclusive).
    where_gte = function(column, value) {
      idx <- length(private$.params) + 1L
      private$.where <- c(private$.where, sprintf("%s >= $%d", column, idx))
      private$.params <- c(private$.params, list(value))
      invisible(self)
    },

    #' @description Add a <= condition.
    #' @param column Column name.
    #' @param value Maximum value (inclusive).
    where_lte = function(column, value) {
      idx <- length(private$.params) + 1L
      private$.where <- c(private$.where, sprintf("%s <= $%d", column, idx))
      private$.params <- c(private$.params, list(value))
      invisible(self)
    },

    #' @description Add a regex match condition.
    #' @param column Column name.
    #' @param pattern Regular expression pattern.
    where_regex = function(column, pattern) {
      idx <- length(private$.params) + 1L
      private$.where <- c(private$.where,
        sprintf("regexp_matches(%s, $%d)", column, idx))
      private$.params <- c(private$.params, list(pattern))
      invisible(self)
    },

    #' @description Add a fuzzy string match condition (Jaro-Winkler similarity).
    #' @param column Column name.
    #' @param value Target string.
    #' @param threshold Minimum similarity score (0-1, default 0.8).
    where_fuzzy = function(column, value, threshold = 0.8) {
      if (!is.numeric(threshold) || threshold < 0 || threshold > 1) {
        stop(sprintf("threshold must be between 0 and 1, got %s", deparse(threshold)))
      }
      idx <- length(private$.params) + 1L
      private$.where <- c(private$.where,
        sprintf("jaro_winkler_similarity(%s, $%d) > %s", column, idx, threshold))
      private$.params <- c(private$.params, list(value))
      invisible(self)
    },

    #' @description Add OR-combined conditions.
    #' @param ... Lists of c(condition_sql, param_value).
    where_or = function(...) {
      conditions <- list(...)
      if (length(conditions) == 0) return(invisible(self))
      or_parts <- character(length(conditions))
      for (i in seq_along(conditions)) {
        cond <- conditions[[i]][[1]]
        param <- conditions[[i]][[2]]
        idx <- length(private$.params) + 1L
        remapped <- gsub("$1", paste0("$", idx), cond, fixed = TRUE)
        or_parts[i] <- remapped
        private$.params <- c(private$.params, list(param))
      }
      private$.where <- c(private$.where,
        sprintf("(%s)", paste(or_parts, collapse = " OR ")))
      invisible(self)
    },

    #' @description Add GROUP BY columns.
    #' @param ... Column names or expressions.
    group_by = function(...) {
      private$.group_by <- c(private$.group_by, c(...))
      invisible(self)
    },

    #' @description Add a HAVING condition.
    #' @param condition SQL condition with $N placeholders.
    #' @param ... Values bound to the placeholders.
    having = function(condition, ...) {
      params <- list(...)
      offset <- length(private$.params)
      remapped <- condition
      if (length(params) > 0) {
        for (i in seq(length(params), 1)) {
          remapped <- gsub(
            paste0("\\$", i),
            paste0("$", offset + i),
            remapped,
            fixed = FALSE
          )
        }
      }
      private$.having <- c(private$.having, remapped)
      private$.params <- c(private$.params, params)
      invisible(self)
    },

    #' @description Add ORDER BY clauses.
    #' @param ... Order clauses (e.g. "name ASC").
    order_by = function(...) {
      private$.order_by <- c(private$.order_by, c(...))
      invisible(self)
    },

    #' @description Set the maximum number of rows to return.
    #' @param n Non-negative integer limit.
    limit = function(n) {
      if (!is.numeric(n) || n < 0 || n != as.integer(n)) {
        stop(sprintf("limit must be a non-negative integer, got %s", deparse(n)))
      }
      private$.limit <- as.integer(n)
      invisible(self)
    },

    #' @description Set the number of rows to skip.
    #' @param n Non-negative integer offset.
    offset = function(n) {
      if (!is.numeric(n) || n < 0 || n != as.integer(n)) {
        stop(sprintf("offset must be a non-negative integer, got %s", deparse(n)))
      }
      private$.offset <- as.integer(n)
      invisible(self)
    },

    #' @description Build the final SQL string and parameter list.
    #' @return A list with `sql` (character) and `params` (list).
    build = function() {
      distinct_str <- if (private$.distinct) "DISTINCT " else ""
      parts <- c(
        sprintf("SELECT %s%s", distinct_str, paste(private$.select, collapse = ", ")),
        sprintf("FROM %s", private$.from)
      )

      for (j in private$.joins) {
        parts <- c(parts, j)
      }

      if (length(private$.where) > 0) {
        parts <- c(parts,
          paste("WHERE", paste(private$.where, collapse = " AND ")))
      }

      if (length(private$.group_by) > 0) {
        parts <- c(parts,
          paste("GROUP BY", paste(private$.group_by, collapse = ", ")))
      }

      if (length(private$.having) > 0) {
        parts <- c(parts,
          paste("HAVING", paste(private$.having, collapse = " AND ")))
      }

      if (length(private$.order_by) > 0) {
        parts <- c(parts,
          paste("ORDER BY", paste(private$.order_by, collapse = ", ")))
      }

      if (!is.null(private$.limit)) {
        parts <- c(parts, sprintf("LIMIT %d", private$.limit))
      }

      if (!is.null(private$.offset)) {
        parts <- c(parts, sprintf("OFFSET %d", private$.offset))
      }

      list(sql = paste(parts, collapse = "\n"), params = private$.params)
    }
  ),

  private = list(
    .select   = NULL,
    .distinct = NULL,
    .from     = NULL,
    .joins    = NULL,
    .where    = NULL,
    .params   = NULL,
    .group_by = NULL,
    .having   = NULL,
    .order_by = NULL,
    .limit    = NULL,
    .offset   = NULL
  )
)
