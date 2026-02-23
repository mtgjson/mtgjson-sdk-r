#' Token query interface.
#'
#' @noRd
TokenQuery <- R6::R6Class("TokenQuery",
  public = list(
    initialize = function(conn) {
      private$.conn <- conn
    },

    #' @description Get a single token by UUID.
    get_by_uuid = function(uuid) {
      private$ensure()
      df <- private$.conn$execute("SELECT * FROM tokens WHERE uuid = $1",
                                  params = list(uuid))
      if (nrow(df) == 0) return(NULL)
      df
    },

    #' @description Get multiple tokens by UUID.
    get_by_uuids = function(uuids) {
      if (length(uuids) == 0) return(data.frame())
      private$ensure()
      q <- SqlBuilder$new("tokens")$where_in("uuid", as.list(uuids))
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Get tokens by exact name.
    get_by_name = function(name, set_code = NULL) {
      private$ensure()
      q <- SqlBuilder$new("tokens")$where_eq("name", name)
      if (!is.null(set_code)) q$where_eq("setCode", set_code)
      q$order_by("setCode DESC", "number ASC")
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Search tokens with flexible filters.
    search = function(name = NULL, set_code = NULL, colors = NULL,
                      types = NULL, artist = NULL, limit = 100L, offset = 0L) {
      private$ensure()
      q <- SqlBuilder$new("tokens")

      if (!is.null(name)) {
        if (grepl("%", name, fixed = TRUE)) q$where_like("name", name)
        else q$where_eq("name", name)
      }
      if (!is.null(set_code)) q$where_eq("setCode", set_code)
      if (!is.null(types))    q$where_like("type", paste0("%", types, "%"))
      if (!is.null(artist))   q$where_like("artist", paste0("%", artist, "%"))

      if (!is.null(colors)) {
        for (color in colors) {
          idx <- length(q$.__enclos_env__$private$.params) + 1L
          q$.__enclos_env__$private$.where <- c(
            q$.__enclos_env__$private$.where,
            sprintf("list_contains(colors, $%d)", idx))
          q$.__enclos_env__$private$.params <- c(
            q$.__enclos_env__$private$.params, list(color))
        }
      }

      q$order_by("name ASC", "number ASC")
      q$limit(limit)$offset(offset)
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Get all tokens for a specific set.
    for_set = function(set_code) {
      self$search(set_code = set_code, limit = 1000L)
    },

    #' @description Count tokens matching optional filters.
    count = function(...) {
      private$ensure()
      filters <- list(...)
      if (length(filters) == 0) {
        return(private$.conn$execute_scalar("SELECT COUNT(*) FROM tokens") %||% 0L)
      }
      q <- SqlBuilder$new("tokens")$select("COUNT(*)")
      for (nm in names(filters)) q$where_eq(nm, filters[[nm]])
      built <- q$build()
      private$.conn$execute_scalar(built$sql, built$params) %||% 0L
    }
  ),

  private = list(
    .conn = NULL,
    ensure = function() {
      private$.conn$ensure_views("tokens")
    }
  )
)
