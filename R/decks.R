#' Deck query interface (JSON-based, no SQL).
#'
#' @noRd
DeckQuery <- R6::R6Class("DeckQuery",
  public = list(
    initialize = function(cache) {
      private$.cache <- cache
      private$.data  <- NULL
    },

    #' @description List decks with optional filters.
    list = function(set_code = NULL, deck_type = NULL) {
      private$ensure()
      results <- private$.data
      if (!is.null(set_code)) {
        code_upper <- toupper(set_code)
        results <- Filter(function(d) toupper(d[["code"]] %||% "") == code_upper,
                          results)
      }
      if (!is.null(deck_type)) {
        results <- Filter(function(d) (d[["type"]] %||% "") == deck_type, results)
      }
      if (length(results) == 0) return(data.frame())
      do.call(rbind, lapply(results, function(d) {
        as.data.frame(d[c("code", "name", "fileName", "type", "releaseDate")],
                      stringsAsFactors = FALSE)
      }))
    },

    #' @description Search decks by name substring.
    search = function(name = NULL, set_code = NULL) {
      private$ensure()
      results <- private$.data
      if (!is.null(name)) {
        name_lower <- tolower(name)
        results <- Filter(
          function(d) grepl(name_lower, tolower(d[["name"]] %||% ""), fixed = TRUE),
          results)
      }
      if (!is.null(set_code)) {
        code_upper <- toupper(set_code)
        results <- Filter(function(d) toupper(d[["code"]] %||% "") == code_upper,
                          results)
      }
      if (length(results) == 0) return(data.frame())
      do.call(rbind, lapply(results, function(d) {
        as.data.frame(d[c("code", "name", "fileName", "type", "releaseDate")],
                      stringsAsFactors = FALSE)
      }))
    },

    #' @description Count total number of decks.
    count = function() {
      private$ensure()
      length(private$.data)
    }
  ),

  private = list(
    .cache = NULL,
    .data  = NULL,

    ensure = function() {
      if (!is.null(private$.data)) return(invisible(NULL))
      tryCatch({
        raw <- private$.cache$load_json("deck_list")
        private$.data <- raw[["data"]] %||% list()
      }, error = function(e) {
        private$.data <- list()
      })
    }
  )
)
