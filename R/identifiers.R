#' Identifier cross-reference query interface.
#'
#' @noRd
KNOWN_ID_COLUMNS <- c(
  "cardKingdomEtchedId", "cardKingdomFoilId", "cardKingdomId",
  "cardsphereId", "cardsphereFoilId",
  "mcmId", "mcmMetaId",
  "mtgArenaId", "mtgoFoilId", "mtgoId",
  "multiverseId",
  "scryfallId", "scryfallIllustrationId", "scryfallOracleId",
  "tcgplayerEtchedProductId", "tcgplayerProductId"
)

#' @noRd
IdentifierQuery <- R6::R6Class("IdentifierQuery",
  public = list(
    initialize = function(conn) {
      private$.conn <- conn
    },

    #' @description Generic identifier lookup.
    find_by = function(id_type, value) {
      if (!(id_type %in% KNOWN_ID_COLUMNS)) {
        stop(sprintf("Unknown identifier type '%s'. Known types: %s",
                     id_type, paste(sort(KNOWN_ID_COLUMNS), collapse = ", ")))
      }
      private$find_by_col(id_type, value)
    },

    find_by_scryfall_id             = function(id) private$find_by_col("scryfallId", id),
    find_by_scryfall_oracle_id      = function(id) private$find_by_col("scryfallOracleId", id),
    find_by_scryfall_illustration_id = function(id) private$find_by_col("scryfallIllustrationId", id),
    find_by_tcgplayer_id            = function(id) private$find_by_col("tcgplayerProductId", id),
    find_by_tcgplayer_etched_id     = function(id) private$find_by_col("tcgplayerEtchedProductId", id),
    find_by_mtgo_id                 = function(id) private$find_by_col("mtgoId", id),
    find_by_mtgo_foil_id            = function(id) private$find_by_col("mtgoFoilId", id),
    find_by_mtg_arena_id            = function(id) private$find_by_col("mtgArenaId", id),
    find_by_multiverse_id           = function(id) private$find_by_col("multiverseId", id),
    find_by_mcm_id                  = function(id) private$find_by_col("mcmId", id),
    find_by_mcm_meta_id             = function(id) private$find_by_col("mcmMetaId", id),
    find_by_card_kingdom_id         = function(id) private$find_by_col("cardKingdomId", id),
    find_by_card_kingdom_foil_id    = function(id) private$find_by_col("cardKingdomFoilId", id),
    find_by_card_kingdom_etched_id  = function(id) private$find_by_col("cardKingdomEtchedId", id),
    find_by_cardsphere_id           = function(id) private$find_by_col("cardsphereId", id),
    find_by_cardsphere_foil_id      = function(id) private$find_by_col("cardsphereFoilId", id),

    #' @description Get all external identifiers for a card UUID.
    get_identifiers = function(uuid) {
      private$.conn$ensure_views("card_identifiers")
      df <- private$.conn$execute(
        "SELECT * FROM card_identifiers WHERE uuid = $1",
        params = list(uuid))
      if (nrow(df) == 0) return(NULL)
      as.list(df[1, ])
    }
  ),

  private = list(
    .conn = NULL,

    find_by_col = function(id_column, value) {
      private$.conn$ensure_views("cards", "card_identifiers")
      private$.conn$execute(
        sprintf("SELECT c.* FROM cards c JOIN card_identifiers ci ON c.uuid = ci.uuid WHERE ci.%s = $1",
                id_column),
        params = list(value))
    }
  )
)
