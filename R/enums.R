#' Enum/keyword query interface (JSON-based, no SQL).
#'
#' @noRd
EnumQuery <- R6::R6Class("EnumQuery",
  public = list(
    initialize = function(cache) {
      private$.cache <- cache
    },

    #' @description Get all MTG keyword categories and their values.
    #' @return A named list.
    keywords = function() {
      raw <- private$.cache$load_json("keywords")
      raw[["data"]] %||% list()
    },

    #' @description Get all card types with sub- and supertypes.
    #' @return A named list.
    card_types = function() {
      raw <- private$.cache$load_json("card_types")
      raw[["data"]] %||% list()
    },

    #' @description Get all enumerated values used by MTGJSON fields.
    #' @return A named list.
    enum_values = function() {
      raw <- private$.cache$load_json("enum_values")
      raw[["data"]] %||% list()
    }
  ),

  private = list(
    .cache = NULL
  )
)
