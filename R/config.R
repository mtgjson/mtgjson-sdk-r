# CDN URLs, cache paths, and SDK defaults.

#' @noRd
CDN_BASE <- "https://mtgjson.com/api/v5"

#' @noRd
META_URL <- paste0(CDN_BASE, "/Meta.json")

#' Mapping of logical view names to CDN parquet file paths.
#' @noRd
PARQUET_FILES <- list(
  cards                       = "parquet/cards.parquet",
  tokens                      = "parquet/tokens.parquet",
  sets                        = "parquet/sets.parquet",
  card_identifiers            = "parquet/cardIdentifiers.parquet",
  card_legalities             = "parquet/cardLegalities.parquet",
  card_foreign_data           = "parquet/cardForeignData.parquet",
  card_rulings                = "parquet/cardRulings.parquet",
  card_purchase_urls          = "parquet/cardPurchaseUrls.parquet",
  set_translations            = "parquet/setTranslations.parquet",
  token_identifiers           = "parquet/tokenIdentifiers.parquet",
  set_booster_content_weights = "parquet/setBoosterContentWeights.parquet",
  set_booster_contents        = "parquet/setBoosterContents.parquet",
  set_booster_sheet_cards     = "parquet/setBoosterSheetCards.parquet",
  set_booster_sheets          = "parquet/setBoosterSheets.parquet",
  all_printings               = "parquet/AllPrintings.parquet"
)

#' Mapping of logical data names to CDN JSON file paths.
#' @noRd
JSON_FILES <- list(
  all_prices_today = "AllPricesToday.json.gz",
  tcgplayer_skus   = "TcgplayerSkus.json.gz",
  keywords         = "Keywords.json",
  card_types       = "CardTypes.json",
  deck_list        = "DeckList.json",
  enum_values      = "EnumValues.json",
  meta             = "Meta.json"
)

#' Known list columns that don't follow the plural naming convention.
#' @noRd
STATIC_LIST_COLUMNS <- list(
  cards = c(
    "artistIds", "attractionLights", "availability", "boosterTypes",
    "cardParts", "colorIdentity", "colorIndicator", "colors", "finishes",
    "frameEffects", "keywords", "originalPrintings", "otherFaceIds",
    "printings", "producedMana", "promoTypes", "rebalancedPrintings",
    "subsets", "subtypes", "supertypes", "types", "variations"
  ),
  tokens = c(
    "artistIds", "availability", "boosterTypes", "colorIdentity",
    "colorIndicator", "colors", "finishes", "frameEffects", "keywords",
    "otherFaceIds", "producedMana", "promoTypes", "reverseRelated",
    "subtypes", "supertypes", "types"
  )
)

#' VARCHAR columns that are NOT lists, even if they match the plural heuristic.
#' @noRd
IGNORED_COLUMNS <- c(
  "text", "originalText", "flavorText", "printedText",
  "identifiers", "legalities", "leadershipSkills", "purchaseUrls",
  "relatedCards", "rulings", "sourceProducts", "foreignData",
  "translations", "toughness", "status", "format", "uris", "scryfallUri"
)

#' VARCHAR columns containing JSON strings to cast to DuckDB JSON type.
#' @noRd
JSON_CAST_COLUMNS <- c(
  "identifiers", "legalities", "leadershipSkills", "purchaseUrls",
  "relatedCards", "rulings", "sourceProducts", "foreignData", "translations"
)

#' Platform-appropriate cache directory.
#' @noRd
default_cache_dir <- function() {
  tools::R_user_dir("mtgjson-sdk", "cache")
}
