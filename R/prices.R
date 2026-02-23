#' Price query interface.
#'
#' @noRd
PriceQuery <- R6::R6Class("PriceQuery",
  public = list(
    initialize = function(conn, cache) {
      private$.conn  <- conn
      private$.cache <- cache
      private$.loaded <- FALSE
    },

    #' @description Get full price data for a card UUID (nested list).
    get = function(uuid) {
      private$ensure()
      if (!("prices_today" %in% private$.conn$registered_views)) return(NULL)
      df <- private$.conn$execute(
        "SELECT * FROM prices_today WHERE uuid = $1 ORDER BY source, provider, category, finish, date",
        params = list(uuid))
      if (nrow(df) == 0) return(NULL)
      # Reconstruct nested structure
      result <- list()
      for (i in seq_len(nrow(df))) {
        r <- as.list(df[i, ])
        if (is.null(result[[r$source]])) result[[r$source]] <- list()
        if (is.null(result[[r$source]][[r$provider]]))
          result[[r$source]][[r$provider]] <- list(currency = r$currency %||% "USD")
        if (is.null(result[[r$source]][[r$provider]][[r$category]]))
          result[[r$source]][[r$provider]][[r$category]] <- list()
        if (is.null(result[[r$source]][[r$provider]][[r$category]][[r$finish]]))
          result[[r$source]][[r$provider]][[r$category]][[r$finish]] <- list()
        result[[r$source]][[r$provider]][[r$category]][[r$finish]][[r$date]] <- r$price
      }
      result
    },

    #' @description Get latest prices for a card UUID.
    today = function(uuid, provider = NULL, finish = NULL, category = NULL) {
      private$ensure()
      if (!("prices_today" %in% private$.conn$registered_views)) return(data.frame())
      parts <- c(
        "SELECT * FROM prices_today",
        "WHERE uuid = $1",
        "AND date = (SELECT MAX(p2.date) FROM prices_today p2 WHERE p2.uuid = $1)")
      params <- list(uuid)
      idx <- 2L
      if (!is.null(provider)) {
        parts <- c(parts, sprintf("AND provider = $%d", idx))
        params <- c(params, list(provider)); idx <- idx + 1L
      }
      if (!is.null(finish)) {
        parts <- c(parts, sprintf("AND finish = $%d", idx))
        params <- c(params, list(finish)); idx <- idx + 1L
      }
      if (!is.null(category)) {
        parts <- c(parts, sprintf("AND category = $%d", idx))
        params <- c(params, list(category)); idx <- idx + 1L
      }
      private$.conn$execute(paste(parts, collapse = " "), params)
    },

    #' @description Get price history for a card UUID.
    history = function(uuid, provider = NULL, finish = NULL, category = NULL,
                       date_from = NULL, date_to = NULL) {
      private$ensure()
      if (!("prices_today" %in% private$.conn$registered_views)) return(data.frame())
      parts <- c("SELECT * FROM prices_today WHERE uuid = $1")
      params <- list(uuid)
      idx <- 2L
      if (!is.null(provider)) {
        parts <- c(parts, sprintf("AND provider = $%d", idx))
        params <- c(params, list(provider)); idx <- idx + 1L
      }
      if (!is.null(finish)) {
        parts <- c(parts, sprintf("AND finish = $%d", idx))
        params <- c(params, list(finish)); idx <- idx + 1L
      }
      if (!is.null(category)) {
        parts <- c(parts, sprintf("AND category = $%d", idx))
        params <- c(params, list(category)); idx <- idx + 1L
      }
      if (!is.null(date_from)) {
        parts <- c(parts, sprintf("AND date >= $%d", idx))
        params <- c(params, list(date_from)); idx <- idx + 1L
      }
      if (!is.null(date_to)) {
        parts <- c(parts, sprintf("AND date <= $%d", idx))
        params <- c(params, list(date_to)); idx <- idx + 1L
      }
      parts <- c(parts, "ORDER BY date ASC")
      private$.conn$execute(paste(parts, collapse = " "), params)
    },

    #' @description Get price trend statistics for a card.
    #' @return A named list or NULL.
    price_trend = function(uuid, provider = NULL, finish = NULL,
                           category = "retail") {
      private$ensure()
      if (!("prices_today" %in% private$.conn$registered_views)) return(NULL)
      parts <- c(
        "SELECT MIN(price) AS min_price, MAX(price) AS max_price,",
        "ROUND(AVG(price), 2) AS avg_price, MIN(date) AS first_date,",
        "MAX(date) AS last_date, COUNT(*) AS data_points",
        "FROM prices_today WHERE uuid = $1 AND category = $2")
      params <- list(uuid, category)
      idx <- 3L
      if (!is.null(provider)) {
        parts <- c(parts, sprintf("AND provider = $%d", idx))
        params <- c(params, list(provider)); idx <- idx + 1L
      }
      if (!is.null(finish)) {
        parts <- c(parts, sprintf("AND finish = $%d", idx))
        params <- c(params, list(finish)); idx <- idx + 1L
      }
      df <- private$.conn$execute(paste(parts, collapse = " "), params)
      if (nrow(df) == 0 || is.na(df$data_points[1]) || df$data_points[1] == 0)
        return(NULL)
      as.list(df[1, ])
    },

    #' @description Find the cheapest printing of a card by name.
    cheapest_printing = function(name, provider = "tcgplayer", finish = "normal",
                                 category = "retail") {
      private$ensure()
      private$.conn$ensure_views("cards")
      if (!("prices_today" %in% private$.conn$registered_views)) return(NULL)
      sql <- paste(
        "SELECT c.uuid, c.setCode, c.number, p.price, p.date",
        "FROM cards c JOIN prices_today p ON c.uuid = p.uuid",
        "WHERE c.name = $1 AND p.provider = $2",
        "AND p.finish = $3 AND p.category = $4",
        "AND p.date = (SELECT MAX(p2.date) FROM prices_today p2",
        "WHERE p2.uuid = c.uuid AND p2.provider = $2",
        "AND p2.finish = $3 AND p2.category = $4)",
        "ORDER BY p.price ASC LIMIT 1")
      df <- private$.conn$execute(sql, params = list(name, provider, finish, category))
      if (nrow(df) == 0) return(NULL)
      as.list(df[1, ])
    },

    #' @description Find the cheapest printing of each card.
    cheapest_printings = function(provider = "tcgplayer", finish = "normal",
                                  category = "retail", limit = 100L, offset = 0L) {
      private$ensure()
      private$.conn$ensure_views("cards")
      if (!("prices_today" %in% private$.conn$registered_views)) return(data.frame())
      sql <- sprintf(paste(
        "SELECT c.name, arg_min(c.setCode, p.price) AS cheapest_set,",
        "arg_min(c.number, p.price) AS cheapest_number,",
        "arg_min(c.uuid, p.price) AS cheapest_uuid,",
        "MIN(p.price) AS min_price",
        "FROM cards c JOIN prices_today p ON c.uuid = p.uuid",
        "WHERE p.provider = $1 AND p.finish = $2 AND p.category = $3",
        "AND p.date = (SELECT MAX(date) FROM prices_today)",
        "GROUP BY c.name ORDER BY min_price ASC",
        "LIMIT %d OFFSET %d"), limit, offset)
      private$.conn$execute(sql, params = list(provider, finish, category))
    },

    #' @description Find the most expensive printing of each card.
    most_expensive_printings = function(provider = "tcgplayer", finish = "normal",
                                        category = "retail", limit = 100L,
                                        offset = 0L) {
      private$ensure()
      private$.conn$ensure_views("cards")
      if (!("prices_today" %in% private$.conn$registered_views)) return(data.frame())
      sql <- sprintf(paste(
        "SELECT c.name, arg_max(c.setCode, p.price) AS priciest_set,",
        "arg_max(c.number, p.price) AS priciest_number,",
        "arg_max(c.uuid, p.price) AS priciest_uuid,",
        "MAX(p.price) AS max_price",
        "FROM cards c JOIN prices_today p ON c.uuid = p.uuid",
        "WHERE p.provider = $1 AND p.finish = $2 AND p.category = $3",
        "AND p.date = (SELECT MAX(date) FROM prices_today)",
        "GROUP BY c.name ORDER BY max_price DESC",
        "LIMIT %d OFFSET %d"), limit, offset)
      private$.conn$execute(sql, params = list(provider, finish, category))
    }
  ),

  private = list(
    .conn   = NULL,
    .cache  = NULL,
    .loaded = FALSE,

    ensure = function() {
      if (private$.loaded) return(invisible(NULL))
      if ("prices_today" %in% private$.conn$registered_views) {
        private$.loaded <- TRUE
        return(invisible(NULL))
      }
      path <- tryCatch(
        private$.cache$ensure_json("all_prices_today"),
        error = function(e) {
          warning("Price data not available: ", conditionMessage(e))
          NULL
        })
      if (!is.null(path)) load_prices_to_duckdb(path, private$.conn)
      private$.loaded <- TRUE
    }
  )
)

#' Escape a string for safe embedding in JSON.
#' @noRd
json_escape <- function(s) gsub('"', '\\"', s, fixed = TRUE)

#' Parse AllPricesToday JSON, stream-flatten to NDJSON, load into DuckDB.
#' Uses sprintf for JSON serialization (10-100x faster than jsonlite::toJSON per row).
#' @noRd
load_prices_to_duckdb <- function(path, conn) {
  dec <- decompress_gz(path)
  on.exit(if (!is.null(dec$tmp)) unlink(dec$tmp), add = TRUE)

  raw <- jsonlite::fromJSON(dec$path, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  data <- raw[["data"]]
  if (is.null(data)) return(invisible(NULL))
  rm(raw)

  tmp <- tempfile(fileext = ".ndjson")
  tmp_con <- file(tmp, "w")

  batch <- character(10000L)
  batch_idx <- 0L
  count <- 0L

  for (uuid in names(data)) {
    formats <- data[[uuid]]
    if (!is.list(formats)) next
    for (src in names(formats)) {
      providers <- formats[[src]]
      if (!is.list(providers)) next
      for (provider in names(providers)) {
        price_data <- providers[[provider]]
        if (!is.list(price_data)) next
        currency <- json_escape(price_data[["currency"]] %||% "USD")
        for (cat_name in c("buylist", "retail")) {
          cat_data <- price_data[[cat_name]]
          if (!is.list(cat_data)) next
          for (finish in names(cat_data)) {
            date_prices <- cat_data[[finish]]
            if (!is.list(date_prices)) next
            for (dt in names(date_prices)) {
              price <- date_prices[[dt]]
              if (!is.null(price)) {
                batch_idx <- batch_idx + 1L
                batch[batch_idx] <- sprintf(
                  '{"uuid":"%s","source":"%s","provider":"%s","currency":"%s","category":"%s","finish":"%s","date":"%s","price":%s}',
                  uuid, src, provider, currency, cat_name, finish, dt, as.numeric(price))
                if (batch_idx >= 10000L) {
                  writeLines(batch[seq_len(batch_idx)], tmp_con)
                  count <- count + batch_idx
                  batch_idx <- 0L
                }
              }
            }
          }
        }
      }
    }
  }

  if (batch_idx > 0L) {
    writeLines(batch[seq_len(batch_idx)], tmp_con)
    count <- count + batch_idx
  }
  close(tmp_con)

  if (count > 0L) {
    conn$register_table_from_ndjson("prices_today", tmp)
  }
  unlink(tmp)
}
