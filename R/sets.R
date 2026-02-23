#' Set query interface.
#'
#' @noRd
SetQuery <- R6::R6Class("SetQuery",
  public = list(
    initialize = function(conn) {
      private$.conn <- conn
    },

    #' @description Get a set by code.
    get = function(code) {
      private$ensure()
      df <- private$.conn$execute("SELECT * FROM sets WHERE code = $1",
                                  params = list(toupper(code)))
      if (nrow(df) == 0) return(NULL)
      df
    },

    #' @description List sets with optional filters.
    list = function(set_type = NULL, name = NULL, limit = 1000L, offset = 0L) {
      private$ensure()
      q <- SqlBuilder$new("sets")
      if (!is.null(set_type)) q$where_eq("type", set_type)
      if (!is.null(name)) {
        if (grepl("%", name, fixed = TRUE)) q$where_like("name", name)
        else q$where_eq("name", name)
      }
      q$order_by("releaseDate DESC")
      q$limit(limit)$offset(offset)
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Search sets with flexible filters.
    search = function(name = NULL, set_type = NULL, block = NULL,
                      release_year = NULL, limit = 100L) {
      private$ensure()
      q <- SqlBuilder$new("sets")
      if (!is.null(name))     q$where_like("name", paste0("%", name, "%"))
      if (!is.null(set_type)) q$where_eq("type", set_type)
      if (!is.null(block))    q$where_like("block", paste0("%", block, "%"))
      if (!is.null(release_year)) {
        idx <- length(q$.__enclos_env__$private$.params) + 1L
        q$.__enclos_env__$private$.where <- c(
          q$.__enclos_env__$private$.where,
          sprintf("EXTRACT(YEAR FROM CAST(releaseDate AS DATE)) = $%d", idx))
        q$.__enclos_env__$private$.params <- c(
          q$.__enclos_env__$private$.params, list(release_year))
      }
      q$order_by("releaseDate DESC")
      q$limit(limit)
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Get aggregate price statistics for a set.
    get_financial_summary = function(set_code, provider = "tcgplayer",
                                     currency = "USD", finish = "normal",
                                     category = "retail") {
      private$.conn$ensure_views("cards")
      if (!("prices_today" %in% private$.conn$registered_views)) return(NULL)

      sql <- "
        SELECT
          COUNT(DISTINCT c.uuid) AS card_count,
          ROUND(SUM(p.price), 2) AS total_value,
          ROUND(AVG(p.price), 2) AS avg_value,
          MIN(p.price) AS min_value,
          MAX(p.price) AS max_value,
          MAX(p.date) AS date
        FROM cards c
        JOIN prices_today p ON c.uuid = p.uuid
        WHERE c.setCode = $1
          AND p.provider = $2
          AND p.currency = $3
          AND p.finish = $4
          AND p.category = $5
          AND p.date = (SELECT MAX(p2.date) FROM prices_today p2)
      "
      df <- private$.conn$execute(sql,
        params = list(toupper(set_code), provider, currency, finish, category))
      if (nrow(df) == 0 || is.na(df$card_count[1]) || df$card_count[1] == 0)
        return(NULL)
      as.list(df[1, ])
    },

    #' @description Count total number of sets.
    count = function() {
      private$ensure()
      private$.conn$execute_scalar("SELECT COUNT(*) FROM sets") %||% 0L
    }
  ),

  private = list(
    .conn = NULL,
    ensure = function() {
      private$.conn$ensure_views("sets")
    }
  )
)
