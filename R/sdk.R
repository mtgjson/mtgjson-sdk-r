#' MTGJSON SDK - DuckDB-backed query client for MTGJSON card data.
#'
#' Auto-downloads Parquet data from the MTGJSON CDN and provides
#' R6-based query interfaces for the full MTG dataset.
#'
#' @export
MtgjsonSDK <- R6::R6Class("MtgjsonSDK",
  public = list(
    #' @description Initialize the SDK.
    #' @param cache_dir Directory for cached data files. NULL for platform default.
    #' @param offline If TRUE, never download from CDN.
    #' @param timeout HTTP request timeout in seconds.
    #' @param on_progress Optional callback function(filename, downloaded, total).
    initialize = function(cache_dir = NULL, offline = FALSE, timeout = 120,
                          on_progress = NULL) {
      private$.cache <- CacheManager$new(
        cache_dir = cache_dir, offline = offline,
        timeout = timeout, on_progress = on_progress)
      private$.conn <- Connection$new(private$.cache)

      private$.cards       <- NULL
      private$.sets        <- NULL
      private$.prices      <- NULL
      private$.decks       <- NULL
      private$.sealed      <- NULL
      private$.skus        <- NULL
      private$.identifiers <- NULL
      private$.legalities  <- NULL
      private$.tokens      <- NULL
      private$.enums       <- NULL
      private$.booster     <- NULL
    },

    #' @description Execute raw SQL against the DuckDB database.
    #' @param query SQL query string.
    #' @param params Optional list of positional parameters.
    #' @return A data.frame.
    sql = function(query, params = NULL) {
      private$.conn$execute(query, params)
    },

    #' @description Check for new MTGJSON data and reset if stale.
    #' @return TRUE if data was stale and reset, FALSE otherwise.
    refresh = function() {
      if (!private$.cache$is_stale()) return(FALSE)
      private$.conn$registered_views <- character(0)
      private$.cards       <- NULL
      private$.sets        <- NULL
      private$.prices      <- NULL
      private$.decks       <- NULL
      private$.sealed      <- NULL
      private$.skus        <- NULL
      private$.identifiers <- NULL
      private$.legalities  <- NULL
      private$.tokens      <- NULL
      private$.enums       <- NULL
      private$.booster     <- NULL
      TRUE
    },

    #' @description Export all loaded data to a persistent DuckDB file.
    #' @param path Output path for the .duckdb file.
    #' @return The output path (invisibly).
    export_db = function(path) {
      if (file.exists(path)) file.remove(path)
      path_str <- forward_slashes(path)
      raw_conn <- private$.conn$raw()
      DBI::dbExecute(raw_conn, sprintf("ATTACH '%s' AS export_db", path_str))
      tryCatch({
        for (view_name in sort(private$.conn$registered_views)) {
          DBI::dbExecute(raw_conn, sprintf(
            "CREATE TABLE export_db.%s AS SELECT * FROM %s",
            view_name, view_name))
        }
      }, finally = {
        DBI::dbExecute(raw_conn, "DETACH export_db")
      })
      invisible(path)
    },

    #' @description Close the DuckDB connection and free resources.
    close = function() {
      private$.conn$close()
      private$.cache$close()
    }
  ),

  active = list(
    #' @field cards CardQuery interface.
    cards = function() {
      if (is.null(private$.cards))
        private$.cards <- CardQuery$new(private$.conn)
      private$.cards
    },

    #' @field sets SetQuery interface.
    sets = function() {
      if (is.null(private$.sets))
        private$.sets <- SetQuery$new(private$.conn)
      private$.sets
    },

    #' @field prices PriceQuery interface.
    prices = function() {
      if (is.null(private$.prices))
        private$.prices <- PriceQuery$new(private$.conn, private$.cache)
      private$.prices
    },

    #' @field decks DeckQuery interface.
    decks = function() {
      if (is.null(private$.decks))
        private$.decks <- DeckQuery$new(private$.cache)
      private$.decks
    },

    #' @field sealed SealedQuery interface.
    sealed = function() {
      if (is.null(private$.sealed))
        private$.sealed <- SealedQuery$new(private$.conn)
      private$.sealed
    },

    #' @field skus SkuQuery interface.
    skus = function() {
      if (is.null(private$.skus))
        private$.skus <- SkuQuery$new(private$.conn, private$.cache)
      private$.skus
    },

    #' @field identifiers IdentifierQuery interface.
    identifiers = function() {
      if (is.null(private$.identifiers))
        private$.identifiers <- IdentifierQuery$new(private$.conn)
      private$.identifiers
    },

    #' @field legalities LegalityQuery interface.
    legalities = function() {
      if (is.null(private$.legalities))
        private$.legalities <- LegalityQuery$new(private$.conn)
      private$.legalities
    },

    #' @field tokens TokenQuery interface.
    tokens = function() {
      if (is.null(private$.tokens))
        private$.tokens <- TokenQuery$new(private$.conn)
      private$.tokens
    },

    #' @field enums EnumQuery interface.
    enums = function() {
      if (is.null(private$.enums))
        private$.enums <- EnumQuery$new(private$.cache)
      private$.enums
    },

    #' @field booster BoosterSimulator interface.
    booster = function() {
      if (is.null(private$.booster))
        private$.booster <- BoosterSimulator$new(private$.conn)
      private$.booster
    },

    #' @field meta MTGJSON build metadata.
    meta = function() {
      tryCatch(
        private$.cache$load_json("meta"),
        error = function(e) list()
      )
    },

    #' @field views Currently registered DuckDB views/tables.
    views = function() {
      sort(private$.conn$registered_views)
    }
  ),

  private = list(
    .cache       = NULL,
    .conn        = NULL,
    .cards       = NULL,
    .sets        = NULL,
    .prices      = NULL,
    .decks       = NULL,
    .sealed      = NULL,
    .skus        = NULL,
    .identifiers = NULL,
    .legalities  = NULL,
    .tokens      = NULL,
    .enums       = NULL,
    .booster     = NULL
  )
)
