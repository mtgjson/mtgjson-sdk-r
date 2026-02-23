#' Format legality query interface.
#'
#' @noRd
LegalityQuery <- R6::R6Class("LegalityQuery",
  public = list(
    initialize = function(conn) {
      private$.conn <- conn
    },

    #' @description Get all format legalities for a card UUID.
    #' @return Named character vector (format = status).
    formats_for_card = function(uuid) {
      private$ensure()
      df <- private$.conn$execute(
        "SELECT format, status FROM card_legalities WHERE uuid = $1",
        params = list(uuid))
      if (nrow(df) == 0) return(character(0))
      stats::setNames(df$status, df$format)
    },

    #' @description Get all cards legal in a format.
    legal_in = function(format_name, limit = 100L, offset = 0L) {
      private$.conn$ensure_views("cards", "card_legalities")
      sql <- sprintf(
        "SELECT DISTINCT c.* FROM cards c JOIN card_legalities cl ON c.uuid = cl.uuid WHERE cl.format = $1 AND cl.status = 'Legal' ORDER BY c.name ASC LIMIT %d OFFSET %d",
        limit, offset)
      private$.conn$execute(sql, params = list(format_name))
    },

    #' @description Check if a card is legal in a format.
    #' @return Logical.
    is_legal = function(uuid, format_name) {
      private$ensure()
      result <- private$.conn$execute_scalar(
        "SELECT COUNT(*) FROM card_legalities WHERE uuid = $1 AND format = $2 AND status = 'Legal'",
        params = list(uuid, format_name))
      (result %||% 0L) > 0L
    },

    #' @description Get cards banned in a format.
    banned_in = function(format_name, limit = 100L, offset = 0L) {
      private$cards_by_status(format_name, "Banned", limit, offset)
    },

    #' @description Get cards restricted in a format.
    restricted_in = function(format_name, limit = 100L, offset = 0L) {
      private$cards_by_status(format_name, "Restricted", limit, offset)
    },

    #' @description Get cards suspended in a format.
    suspended_in = function(format_name, limit = 100L, offset = 0L) {
      private$cards_by_status(format_name, "Suspended", limit, offset)
    },

    #' @description Get cards not legal in a format.
    not_legal_in = function(format_name, limit = 100L, offset = 0L) {
      private$cards_by_status(format_name, "Not Legal", limit, offset)
    }
  ),

  private = list(
    .conn = NULL,

    ensure = function() {
      private$.conn$ensure_views("card_legalities")
    },

    cards_by_status = function(format_name, status, limit = 100L, offset = 0L) {
      private$ensure()
      private$.conn$ensure_views("cards")
      sql <- sprintf(
        "SELECT c.name, c.uuid FROM cards c JOIN card_legalities cl ON c.uuid = cl.uuid WHERE cl.format = $1 AND cl.status = $2 ORDER BY c.name ASC LIMIT %d OFFSET %d",
        limit, offset)
      private$.conn$execute(sql, params = list(format_name, status))
    }
  )
)
