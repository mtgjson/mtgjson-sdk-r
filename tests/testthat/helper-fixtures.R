# Shared test fixtures — creates mini parquet/JSON data in DuckDB for testing.

library(DBI)
library(duckdb)

#' Create a test Connection with fixture data pre-loaded.
#' @return A Connection R6 object with cards, tokens, sets, card_identifiers,
#'   card_legalities, and all_prices_today tables.
#' @noRd
create_test_connection <- function() {
  cache <- CacheManager$new(cache_dir = tempdir(), offline = TRUE)
  conn <- Connection$new(cache)

  # Cards fixture
  cards_df <- data.frame(
    uuid        = c("uuid-bolt-1", "uuid-bolt-2", "uuid-counterspell", "uuid-creature", "uuid-promo"),
    name        = c("Lightning Bolt", "Lightning Bolt", "Counterspell", "Goblin Guide", "Lightning Bolt"),
    asciiName   = c("Lightning Bolt", "Lightning Bolt", "Counterspell", "Goblin Guide", "Lightning Bolt"),
    faceName    = c(NA, NA, NA, NA, NA),
    setCode     = c("A25", "M11", "A25", "ZEN", "PROMO"),
    number      = c("141", "150", "44", "126", "1"),
    type        = c("Instant", "Instant", "Instant", "Creature - Goblin Scout", "Instant"),
    rarity      = c("uncommon", "common", "uncommon", "rare", "rare"),
    text        = c("Lightning Bolt deals 3 damage to any target.",
                    "Lightning Bolt deals 3 damage to any target.",
                    "Counter target spell.",
                    "Haste. Whenever Goblin Guide attacks, defending player reveals the top card.",
                    "Lightning Bolt deals 3 damage to any target."),
    manaValue   = c(1, 1, 2, 1, 1),
    power       = c(NA, NA, NA, "2", NA),
    toughness   = c(NA, NA, NA, "2", NA),
    artist      = c("Christopher Moeller", "Christopher Moeller", "Jason Rainville", "Warren Mahy", "Christopher Moeller"),
    layout      = c("normal", "normal", "normal", "normal", "normal"),
    language    = c("English", "English", "English", "English", "English"),
    manaCost    = c("{R}", "{R}", "{U}{U}", "{R}", "{R}"),
    isPromo     = c(FALSE, FALSE, FALSE, FALSE, TRUE),
    isFunny     = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    isOnlineOnly = c(FALSE, FALSE, FALSE, FALSE, FALSE),
    side        = c(NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(conn$raw(), "DROP TABLE IF EXISTS cards")
  DBI::dbWriteTable(conn$raw(), "cards", cards_df)
  conn$registered_views <- c(conn$registered_views, "cards")

  # Tokens fixture
  tokens_df <- data.frame(
    uuid    = c("uuid-soldier", "uuid-goblin-token"),
    name    = c("Soldier", "Goblin"),
    setCode = c("MH3", "ZEN"),
    number  = c("T1", "T2"),
    type    = c("Token Creature - Soldier", "Token Creature - Goblin"),
    artist  = c("Greg Staples", "Warren Mahy"),
    power   = c("1", "1"),
    toughness = c("1", "1"),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(conn$raw(), "DROP TABLE IF EXISTS tokens")
  DBI::dbWriteTable(conn$raw(), "tokens", tokens_df)
  conn$registered_views <- c(conn$registered_views, "tokens")

  # Sets fixture
  sets_df <- data.frame(
    code         = c("A25", "M11", "ZEN", "MH3"),
    name         = c("Masters 25", "Magic 2011", "Zendikar", "Modern Horizons 3"),
    type         = c("masters", "core", "expansion", "expansion"),
    releaseDate  = c("2018-03-16", "2010-07-16", "2009-10-02", "2024-06-14"),
    baseSetSize  = c(249L, 249L, 249L, 303L),
    totalSetSize = c(249L, 275L, 269L, 303L),
    block        = c(NA, "core", "Zendikar", NA),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(conn$raw(), "DROP TABLE IF EXISTS sets")
  DBI::dbWriteTable(conn$raw(), "sets", sets_df)
  conn$registered_views <- c(conn$registered_views, "sets")

  # Card identifiers fixture
  ids_df <- data.frame(
    uuid        = c("uuid-bolt-1", "uuid-bolt-2", "uuid-counterspell"),
    scryfallId  = c("scry-bolt-a25", "scry-bolt-m11", "scry-counter"),
    tcgplayerProductId = c("111", "222", "333"),
    mtgoId      = c("1001", "1002", "1003"),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(conn$raw(), "DROP TABLE IF EXISTS card_identifiers")
  DBI::dbWriteTable(conn$raw(), "card_identifiers", ids_df)
  conn$registered_views <- c(conn$registered_views, "card_identifiers")

  # Card legalities fixture (already unpivoted long format for tests)
  legalities_df <- data.frame(
    uuid   = c("uuid-bolt-1", "uuid-bolt-1", "uuid-bolt-1",
               "uuid-counterspell", "uuid-counterspell",
               "uuid-creature", "uuid-creature"),
    format = c("modern", "legacy", "standard",
               "modern", "legacy",
               "modern", "legacy"),
    status = c("Legal", "Legal", "Not Legal",
               "Legal", "Legal",
               "Legal", "Banned"),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(conn$raw(), "DROP TABLE IF EXISTS card_legalities")
  DBI::dbWriteTable(conn$raw(), "card_legalities", legalities_df)
  conn$registered_views <- c(conn$registered_views, "card_legalities")

  # Prices fixture
  prices_df <- data.frame(
    uuid     = c("uuid-bolt-1", "uuid-bolt-1", "uuid-bolt-2",
                 "uuid-counterspell", "uuid-creature"),
    source   = rep("paper", 5),
    provider = rep("tcgplayer", 5),
    currency = rep("USD", 5),
    price_type = rep("retail", 5),
    finish   = rep("normal", 5),
    date     = c("2024-01-15", "2024-01-14", "2024-01-15",
                 "2024-01-15", "2024-01-15"),
    price    = c(2.50, 2.40, 1.75, 0.50, 15.00),
    stringsAsFactors = FALSE
  )
  DBI::dbExecute(conn$raw(), "DROP TABLE IF EXISTS all_prices_today")
  DBI::dbWriteTable(conn$raw(), "all_prices_today", prices_df)
  conn$registered_views <- c(conn$registered_views, "all_prices_today")

  # all_prices (history) fixture — same data as all_prices_today for tests
  DBI::dbExecute(conn$raw(), "DROP TABLE IF EXISTS all_prices")
  DBI::dbWriteTable(conn$raw(), "all_prices", prices_df)
  conn$registered_views <- c(conn$registered_views, "all_prices")

  conn
}

#' Create a test CacheManager pointing to a temp dir with fixture JSON files.
#' @noRd
create_test_cache <- function() {
  cache_dir <- tempfile("mtgjson_test_cache_")
  dir.create(cache_dir, recursive = TRUE)

  # Meta.json
  jsonlite::write_json(
    list(data = list(version = "5.2.2+20240101", date = "2024-01-01")),
    file.path(cache_dir, "Meta.json"), auto_unbox = TRUE)

  # Keywords.json
  jsonlite::write_json(
    list(data = list(
      abilityWords = list("Addendum", "Enrage"),
      keywordAbilities = list("Flying", "Trample"),
      keywordActions = list("Destroy", "Exile")
    )),
    file.path(cache_dir, "Keywords.json"), auto_unbox = TRUE)

  # CardTypes.json
  jsonlite::write_json(
    list(data = list(
      creature = list(subTypes = list("Human", "Elf"), superTypes = list("Legendary")),
      instant  = list(subTypes = list(), superTypes = list())
    )),
    file.path(cache_dir, "CardTypes.json"), auto_unbox = TRUE)

  # EnumValues.json
  jsonlite::write_json(
    list(data = list(colors = list("B", "G", "R", "U", "W"))),
    file.path(cache_dir, "EnumValues.json"), auto_unbox = TRUE)

  # DeckList.json
  jsonlite::write_json(
    list(data = list(
      list(code = "MH3", name = "Eldrazi Incursion", fileName = "eldrazi.json",
           type = "Commander Deck", releaseDate = "2024-06-14"),
      list(code = "MH3", name = "Graveyard Overdrive", fileName = "graveyard.json",
           type = "Commander Deck", releaseDate = "2024-06-14"),
      list(code = "ZEN", name = "Rise of the Vampires", fileName = "vampires.json",
           type = "Intro Pack", releaseDate = "2009-10-02")
    )),
    file.path(cache_dir, "DeckList.json"), auto_unbox = TRUE)

  CacheManager$new(cache_dir = cache_dir, offline = TRUE)
}
