#' Price query interface.
#'
#' @noRd
PriceQuery <- R6::R6Class("PriceQuery",
  public = list(
    initialize = function(conn) {
      private$.conn  <- conn
    },

    #' @description Get full price data for a card UUID (nested list).
    get = function(uuid) {
      private$ensure()
      if (!("all_prices_today" %in% private$.conn$registered_views)) return(NULL)
      df <- private$.conn$execute(
        "SELECT * FROM all_prices_today WHERE uuid = $1 ORDER BY source, provider, price_type, finish, date",
        params = list(uuid))
      if (nrow(df) == 0) return(NULL)
      # Reconstruct nested structure
      result <- list()
      for (i in seq_len(nrow(df))) {
        r <- as.list(df[i, ])
        if (is.null(result[[r$source]])) result[[r$source]] <- list()
        if (is.null(result[[r$source]][[r$provider]]))
          result[[r$source]][[r$provider]] <- list(currency = r$currency %||% "USD")
        if (is.null(result[[r$source]][[r$provider]][[r$price_type]]))
          result[[r$source]][[r$provider]][[r$price_type]] <- list()
        if (is.null(result[[r$source]][[r$provider]][[r$price_type]][[r$finish]]))
          result[[r$source]][[r$provider]][[r$price_type]][[r$finish]] <- list()
        result[[r$source]][[r$provider]][[r$price_type]][[r$finish]][[r$date]] <- r$price
      }
      result
    },

    #' @description Get latest prices for a card UUID.
    today = function(uuid, provider = NULL, finish = NULL, price_type = NULL) {
      private$ensure()
      if (!("all_prices_today" %in% private$.conn$registered_views)) return(data.frame())
      parts <- c(
        "SELECT * FROM all_prices_today",
        "WHERE uuid = $1",
        "AND date = (SELECT MAX(p2.date) FROM all_prices_today p2 WHERE p2.uuid = $1)")
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
      if (!is.null(price_type)) {
        parts <- c(parts, sprintf("AND price_type = $%d", idx))
        params <- c(params, list(price_type)); idx <- idx + 1L
      }
      private$.conn$execute(paste(parts, collapse = " "), params)
    },

    #' @description Get price history for a card UUID.
    history = function(uuid, provider = NULL, finish = NULL, price_type = NULL,
                       date_from = NULL, date_to = NULL) {
      private$ensure_history()
      if (!("all_prices" %in% private$.conn$registered_views)) return(data.frame())
      parts <- c("SELECT * FROM all_prices WHERE uuid = $1")
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
      if (!is.null(price_type)) {
        parts <- c(parts, sprintf("AND price_type = $%d", idx))
        params <- c(params, list(price_type)); idx <- idx + 1L
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
                           price_type = "retail") {
      private$ensure_history()
      if (!("all_prices" %in% private$.conn$registered_views)) return(NULL)
      parts <- c(
        "SELECT MIN(price) AS min_price, MAX(price) AS max_price,",
        "ROUND(AVG(price), 2) AS avg_price, MIN(date) AS first_date,",
        "MAX(date) AS last_date, COUNT(*) AS data_points",
        "FROM all_prices WHERE uuid = $1 AND price_type = $2")
      params <- list(uuid, price_type)
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
                                 price_type = "retail") {
      private$ensure()
      private$.conn$ensure_views("cards")
      if (!("all_prices_today" %in% private$.conn$registered_views)) return(NULL)
      sql <- paste(
        "SELECT c.uuid, c.setCode, c.number, p.price, p.date",
        "FROM cards c JOIN all_prices_today p ON c.uuid = p.uuid",
        "WHERE c.name = $1 AND p.provider = $2",
        "AND p.finish = $3 AND p.price_type = $4",
        "AND p.date = (SELECT MAX(p2.date) FROM all_prices_today p2",
        "WHERE p2.uuid = c.uuid AND p2.provider = $2",
        "AND p2.finish = $3 AND p2.price_type = $4)",
        "ORDER BY p.price ASC LIMIT 1")
      df <- private$.conn$execute(sql, params = list(name, provider, finish, price_type))
      if (nrow(df) == 0) return(NULL)
      as.list(df[1, ])
    },

    #' @description Find the cheapest printing of each card.
    cheapest_printings = function(provider = "tcgplayer", finish = "normal",
                                  price_type = "retail", limit = 100L, offset = 0L) {
      private$ensure()
      private$.conn$ensure_views("cards")
      if (!("all_prices_today" %in% private$.conn$registered_views)) return(data.frame())
      sql <- sprintf(paste(
        "SELECT c.name, arg_min(c.setCode, p.price) AS cheapest_set,",
        "arg_min(c.number, p.price) AS cheapest_number,",
        "arg_min(c.uuid, p.price) AS cheapest_uuid,",
        "MIN(p.price) AS min_price",
        "FROM cards c JOIN all_prices_today p ON c.uuid = p.uuid",
        "WHERE p.provider = $1 AND p.finish = $2 AND p.price_type = $3",
        "AND p.date = (SELECT MAX(date) FROM all_prices_today)",
        "GROUP BY c.name ORDER BY min_price ASC",
        "LIMIT %d OFFSET %d"), limit, offset)
      private$.conn$execute(sql, params = list(provider, finish, price_type))
    },

    #' @description Find the most expensive printing of each card.
    most_expensive_printings = function(provider = "tcgplayer", finish = "normal",
                                        price_type = "retail", limit = 100L,
                                        offset = 0L) {
      private$ensure()
      private$.conn$ensure_views("cards")
      if (!("all_prices_today" %in% private$.conn$registered_views)) return(data.frame())
      sql <- sprintf(paste(
        "SELECT c.name, arg_max(c.setCode, p.price) AS priciest_set,",
        "arg_max(c.number, p.price) AS priciest_number,",
        "arg_max(c.uuid, p.price) AS priciest_uuid,",
        "MAX(p.price) AS max_price",
        "FROM cards c JOIN all_prices_today p ON c.uuid = p.uuid",
        "WHERE p.provider = $1 AND p.finish = $2 AND p.price_type = $3",
        "AND p.date = (SELECT MAX(date) FROM all_prices_today)",
        "GROUP BY c.name ORDER BY max_price DESC",
        "LIMIT %d OFFSET %d"), limit, offset)
      private$.conn$execute(sql, params = list(provider, finish, price_type))
    }
  ),

  private = list(
    .conn = NULL,

    ensure = function() {
      tryCatch(
        private$.conn$ensure_views("all_prices_today"),
        error = function(e) {
          warning("Price data not available: ", conditionMessage(e))
        })
    },

    ensure_history = function() {
      tryCatch(
        private$.conn$ensure_views("all_prices"),
        error = function(e) {
          warning("Price history data not available: ", conditionMessage(e))
        })
    }
  )
)
