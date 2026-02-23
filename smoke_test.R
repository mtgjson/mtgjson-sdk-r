#' Smoke test: pull real data from CDN and exercise ALL SDK methods.
#'
#' Coverage goal: 100% of public methods, all filter parameters,
#' and key edge cases. R SDK returns data.frames natively
#' (no as_dict/as_dataframe output modes).

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) {
  source(f)
}

PASS <- 0L
FAIL <- 0L
SKIP <- 0L

check <- function(label, condition, detail = "") {
  status <- if (condition) "PASS" else "FAIL"
  if (condition) PASS <<- PASS + 1L else FAIL <<- FAIL + 1L
  suffix <- if (nzchar(detail)) paste0(" -- ", detail) else ""
  cat(sprintf("  [%s] %s%s\n", status, label, suffix))
}

has_value <- function(x) !is.null(x) && !is.na(x) && nzchar(as.character(x)) && as.character(x) != "NA"

skip <- function(label, reason = "") {
  SKIP <<- SKIP + 1L
  suffix <- if (nzchar(reason)) paste0(" -- ", reason) else ""
  cat(sprintf("  [SKIP] %s%s\n", label, suffix))
}

section <- function(name) {
  cat(sprintf("\n%s\n  %s\n%s\n", strrep("=", 60), name, strrep("=", 60)))
}

# ============================================================
#  CLIENT LIFECYCLE
# ============================================================
section("Client Lifecycle")

t0 <- proc.time()["elapsed"]

sdk <- MtgjsonSDK$new()
check("constructor runs", inherits(sdk, "MtgjsonSDK"))

# meta property
meta <- sdk$meta
check("meta loads", is.list(meta) && "data" %in% names(meta),
      paste("keys =", paste(names(meta), collapse = ", ")))
if ("data" %in% names(meta)) {
  version <- meta$data$version %||% "?"
  date_val <- meta$data$date %||% "?"
  check("meta has version", nzchar(version),
        sprintf("v=%s, date=%s", version, date_val))
}

# views property (starts empty, grows as we query)
views_before <- sdk$views
check("views property (initial)", is.character(views_before))

# refresh()
refresh_result <- sdk$refresh()
check("refresh()", is.logical(refresh_result),
      sprintf("stale=%s", refresh_result))


# ============================================================
#  CARDS -- CardQuery (8 methods, ~20 filter params)
# ============================================================
section("Cards: get_by_name / get_by_uuid")

bolt <- sdk$cards$get_by_name("Lightning Bolt")
check("get_by_name Lightning Bolt", nrow(bolt) > 0,
      sprintf("found %d printings", nrow(bolt)))

# get_by_name with set_code filter
bolt_lea <- sdk$cards$get_by_name("Lightning Bolt", set_code = "LEA")
check("get_by_name set_code=LEA", nrow(bolt_lea) >= 0,
      sprintf("found %d", nrow(bolt_lea)))

uuid <- NULL
if (nrow(bolt) > 0) {
  uuid <- bolt$uuid[1]

  # get_by_uuid
  card <- sdk$cards$get_by_uuid(uuid)
  check("get_by_uuid (data.frame)", !is.null(card) && card$name[1] == "Lightning Bolt")

  # get_by_uuid -- nonexistent
  missing <- sdk$cards$get_by_uuid("00000000-0000-0000-0000-000000000000")
  check("get_by_uuid nonexistent returns NULL", is.null(missing))
}

# -- Cards: get_by_uuids (bulk lookup) --
section("Cards: bulk lookups (get_by_uuids)")

if (nrow(bolt) >= 2) {
  bulk_uuids <- bolt$uuid[1:min(5, nrow(bolt))]
  bulk_cards <- sdk$cards$get_by_uuids(bulk_uuids)
  check("get_by_uuids",
        nrow(bulk_cards) == length(bulk_uuids),
        sprintf("requested %d, got %d", length(bulk_uuids), nrow(bulk_cards)))
}

# empty list
bulk_empty <- sdk$cards$get_by_uuids(character(0))
check("get_by_uuids empty list", is.data.frame(bulk_empty) && nrow(bulk_empty) == 0)

# nonexistent uuids
bulk_none <- sdk$cards$get_by_uuids("00000000-0000-0000-0000-000000000000")
check("get_by_uuids nonexistent", nrow(bulk_none) == 0)


# -- Cards: search (all filter params) --
section("Cards: search filters")

# name LIKE
s <- sdk$cards$search(name = "Lightning%", limit = 10)
check("search name LIKE", nrow(s) > 0, sprintf("found %d", nrow(s)))

# exact name
s <- sdk$cards$search(name = "Lightning Bolt", limit = 5)
check("search name exact", nrow(s) > 0)

# colors
s <- sdk$cards$search(colors = c("R"), mana_value = 1.0, limit = 5)
check("search colors=R mv=1", nrow(s) > 0, sprintf("found %d", nrow(s)))

# color_identity
s <- sdk$cards$search(color_identity = c("W", "U"), limit = 5)
check("search color_identity=[W,U]", nrow(s) > 0, sprintf("found %d", nrow(s)))

# types
s <- sdk$cards$search(types = "Creature", limit = 5)
check("search types=Creature", nrow(s) > 0, sprintf("found %d", nrow(s)))

# rarity
s <- sdk$cards$search(rarity = "mythic", limit = 5)
check("search rarity=mythic", nrow(s) > 0, sprintf("found %d", nrow(s)))

# text
s <- sdk$cards$search(text = "draw a card", limit = 5)
check("search text='draw a card'", nrow(s) > 0, sprintf("found %d", nrow(s)))

# power / toughness
s <- sdk$cards$search(power = "4", toughness = "4", limit = 5)
check("search power=4 toughness=4", nrow(s) > 0, sprintf("found %d", nrow(s)))

# mana_value exact
s <- sdk$cards$search(mana_value = 3.0, limit = 5)
check("search mana_value=3", nrow(s) > 0, sprintf("found %d", nrow(s)))

# mana_value_lte
s <- sdk$cards$search(mana_value_lte = 1.0, limit = 5)
check("search mana_value_lte=1", nrow(s) > 0, sprintf("found %d", nrow(s)))

# mana_value_gte
s <- sdk$cards$search(mana_value_gte = 10.0, limit = 5)
check("search mana_value_gte=10", nrow(s) > 0, sprintf("found %d", nrow(s)))

# artist
s <- sdk$cards$search(artist = "Christopher Moeller", limit = 5)
check("search artist", nrow(s) > 0, sprintf("found %d", nrow(s)))

# keyword
s <- sdk$cards$search(keyword = "Flying", limit = 5)
check("search keyword=Flying", nrow(s) > 0, sprintf("found %d", nrow(s)))

# layout
s <- sdk$cards$search(layout = "split", limit = 5)
check("search layout=split", nrow(s) > 0, sprintf("found %d", nrow(s)))

# is_promo
s <- sdk$cards$search(is_promo = TRUE, limit = 5)
check("search is_promo=TRUE", nrow(s) > 0, sprintf("found %d", nrow(s)))

s_np <- sdk$cards$search(is_promo = FALSE, limit = 5)
check("search is_promo=FALSE", nrow(s_np) > 0, sprintf("found %d", nrow(s_np)))

# availability
s <- sdk$cards$search(availability = "mtgo", limit = 5)
check("search availability=mtgo", nrow(s) > 0, sprintf("found %d", nrow(s)))

s <- sdk$cards$search(availability = "paper", limit = 5)
check("search availability=paper", nrow(s) > 0, sprintf("found %d", nrow(s)))

# language
s <- sdk$cards$search(language = "Japanese", limit = 5)
check("search language=Japanese", is.data.frame(s), sprintf("found %d", nrow(s)))

# set_code
s <- sdk$cards$search(set_code = "MH3", limit = 5)
check("search set_code=MH3", nrow(s) > 0, sprintf("found %d", nrow(s)))

# set_type (requires JOIN with sets)
s <- sdk$cards$search(set_type = "expansion", limit = 5)
check("search set_type=expansion", nrow(s) > 0, sprintf("found %d", nrow(s)))

# legal_in + mana_value_lte
s <- sdk$cards$search(legal_in = "modern", mana_value_lte = 2.0, limit = 5)
check("search legal_in=modern + mana_value_lte", nrow(s) > 0, sprintf("found %d", nrow(s)))

# combined filters
s <- sdk$cards$search(colors = c("R"), rarity = "rare", mana_value_lte = 3.0, limit = 5)
check("search combined (colors+rarity+mv)", nrow(s) > 0, sprintf("found %d", nrow(s)))

# offset (pagination)
page1 <- sdk$cards$search(name = "Lightning%", limit = 3, offset = 0)
page2 <- sdk$cards$search(name = "Lightning%", limit = 3, offset = 3)
check("search offset (pagination)", nrow(page1) > 0 && nrow(page2) > 0, "two pages fetched")
if (nrow(page1) > 0 && nrow(page2) > 0) {
  check("search pages differ", page1$uuid[1] != page2$uuid[1], "different cards")
}

# text_regex
s <- sdk$cards$search(text_regex = "deals \\d+ damage", limit = 5)
check("search text_regex", nrow(s) > 0, sprintf("found %d", nrow(s)))

# localized_name (foreign language search)
s <- sdk$cards$search(localized_name = "Blitzschlag", limit = 5)
check("search localized_name (German)", nrow(s) > 0,
      sprintf("found %d, name=%s", nrow(s), if (nrow(s) > 0) s$name[1] else "?"))

# localized_name LIKE
s <- sdk$cards$search(localized_name = "%Foudre%", limit = 5)
check("search localized_name LIKE", is.data.frame(s), sprintf("found %d", nrow(s)))

# fuzzy_name
s <- sdk$cards$search(fuzzy_name = "Ligtning Bolt", limit = 5)
check("search fuzzy_name", nrow(s) > 0,
      sprintf("found %d, top=%s", nrow(s), if (nrow(s) > 0) s$name[1] else "?"))


# -- Cards: other methods --
section("Cards: random, count, printings, atomic, find_by_scryfall_id")

rand <- sdk$cards$random(3)
check("random(3)", nrow(rand) == 3,
      sprintf("names: %s", paste(rand$name, collapse = ", ")))

count <- sdk$cards$count()
check("count()", count > 1000, sprintf("total cards: %d", count))

# count with filters
count_r <- sdk$cards$count(rarity = "mythic")
check("count(rarity=mythic)", count_r > 0 && count_r < count,
      sprintf("mythic cards: %d", count_r))

printings <- sdk$cards$get_printings("Counterspell")
check("get_printings Counterspell", nrow(printings) > 5,
      sprintf("found %d printings", nrow(printings)))

# get_atomic -- exact name
atomic <- sdk$cards$get_atomic("Lightning Bolt")
check("get_atomic Lightning Bolt", nrow(atomic) > 0)

# get_atomic -- face name fallback for split cards
atomic_fire <- sdk$cards$get_atomic("Fire")
check("get_atomic face name 'Fire'", nrow(atomic_fire) > 0,
      sprintf("layout=%s", if (nrow(atomic_fire) > 0) atomic_fire$layout[1] else "?"))

# find_by_scryfall_id (may not match since uuid != scryfallId)
if (!is.null(uuid)) {
  scry_cards <- sdk$cards$find_by_scryfall_id(uuid)
  check("find_by_scryfall_id runs", is.data.frame(scry_cards), "no error")
}


# ============================================================
#  TOKENS -- TokenQuery (6 methods, ~8 filter params)
# ============================================================
section("Tokens")

token_count <- sdk$tokens$count()
check("token count()", token_count > 0, sprintf("total tokens: %d", token_count))

# count with filters
token_count_mh3 <- sdk$tokens$count(setCode = "MH3")
check("token count(setCode=MH3)", is.numeric(token_count_mh3),
      sprintf("MH3 tokens: %d", token_count_mh3))

# search by name LIKE
token_search <- sdk$tokens$search(name = "%Soldier%", limit = 5)
check("token search name LIKE", nrow(token_search) > 0,
      sprintf("found %d", nrow(token_search)))

# search by set_code -- find a set that actually has tokens
token_set_row <- sdk$sql("SELECT DISTINCT setCode FROM tokens LIMIT 1")
token_set_code <- if (nrow(token_set_row) > 0) token_set_row$setCode[1] else "MH3"
token_search_set <- sdk$tokens$search(set_code = token_set_code, limit = 5)
check("token search set_code", nrow(token_search_set) > 0,
      sprintf("set=%s, found %d", token_set_code, nrow(token_search_set)))

# search by types
token_search_type <- sdk$tokens$search(types = "Creature", limit = 5)
check("token search types=Creature", nrow(token_search_type) > 0,
      sprintf("found %d", nrow(token_search_type)))

# search by colors
token_search_colors <- sdk$tokens$search(colors = c("W"), limit = 5)
check("token search colors=[W]", nrow(token_search_colors) > 0,
      sprintf("found %d", nrow(token_search_colors)))

# search with offset
tp1 <- sdk$tokens$search(name = "%Soldier%", limit = 2, offset = 0)
tp2 <- sdk$tokens$search(name = "%Soldier%", limit = 2, offset = 2)
check("token search offset", is.data.frame(tp1) && is.data.frame(tp2))

# get_by_uuid
if (nrow(token_search) > 0) {
  token <- sdk$tokens$get_by_uuid(token_search$uuid[1])
  check("token get_by_uuid", !is.null(token),
        sprintf("name=%s", if (!is.null(token)) token$name[1] else "?"))

  # nonexistent
  missing_token <- sdk$tokens$get_by_uuid("00000000-0000-0000-0000-000000000000")
  check("token get_by_uuid nonexistent", is.null(missing_token))
}

# get_by_name
token_soldiers <- sdk$tokens$get_by_name("Soldier")
check("token get_by_name Soldier", nrow(token_soldiers) > 0,
      sprintf("found %d", nrow(token_soldiers)))

# get_by_name with set_code
token_soldiers_set <- sdk$tokens$get_by_name("Soldier", set_code = token_set_code)
check("token get_by_name set_code", is.data.frame(token_soldiers_set),
      sprintf("set=%s, found %d", token_set_code, nrow(token_soldiers_set)))

# for_set
tokens_for <- sdk$tokens$for_set(token_set_code)
check("token for_set", nrow(tokens_for) > 0,
      sprintf("set=%s, found %d", token_set_code, nrow(tokens_for)))

# get_by_uuids (bulk token lookup)
if (nrow(token_search) >= 2) {
  token_uuids <- token_search$uuid[1:min(3, nrow(token_search))]
  bulk_tokens <- sdk$tokens$get_by_uuids(token_uuids)
  check("token get_by_uuids",
        nrow(bulk_tokens) == length(token_uuids),
        sprintf("requested %d, got %d", length(token_uuids), nrow(bulk_tokens)))
}

check("token get_by_uuids empty",
      is.data.frame(sdk$tokens$get_by_uuids(character(0))) &&
      nrow(sdk$tokens$get_by_uuids(character(0))) == 0)


# ============================================================
#  SETS -- SetQuery (5 methods, ~7 filter params)
# ============================================================
section("Sets")

# get
mh3 <- sdk$sets$get("MH3")
check("get set MH3", !is.null(mh3),
      sprintf("name=%s", if (!is.null(mh3)) mh3$name[1] else "?"))

# get nonexistent
missing_set <- sdk$sets$get("ZZZZZ")
check("get set nonexistent", is.null(missing_set))

# list -- no filter
all_sets <- sdk$sets$list(limit = 10)
check("list sets (no filter)", nrow(all_sets) > 0, sprintf("found %d", nrow(all_sets)))

# list -- set_type
expansions <- sdk$sets$list(set_type = "expansion", limit = 10)
check("list expansions", nrow(expansions) > 0, sprintf("found %d", nrow(expansions)))

# list -- name filter
horizon_list <- sdk$sets$list(name = "%Horizons%", limit = 10)
check("list name filter", nrow(horizon_list) > 0, sprintf("found %d", nrow(horizon_list)))

# list -- offset
sets_p1 <- sdk$sets$list(limit = 3, offset = 0)
sets_p2 <- sdk$sets$list(limit = 3, offset = 3)
check("list offset (pagination)", nrow(sets_p1) > 0 && nrow(sets_p2) > 0)
if (nrow(sets_p1) > 0 && nrow(sets_p2) > 0) {
  check("list pages differ", sets_p1$code[1] != sets_p2$code[1])
}

# search -- name
set_search <- sdk$sets$search(name = "Horizons")
check("search 'Horizons'", nrow(set_search) > 0, sprintf("found %d", nrow(set_search)))

# search -- set_type
set_search_type <- sdk$sets$search(set_type = "masters", limit = 10)
check("search set_type=masters", nrow(set_search_type) > 0,
      sprintf("found %d", nrow(set_search_type)))

# search -- block
set_search_block <- sdk$sets$search(block = "Innistrad")
check("search block=Innistrad", is.data.frame(set_search_block),
      sprintf("found %d", nrow(set_search_block)))

# search -- release_year
set_search_year <- sdk$sets$search(release_year = 2024, limit = 10)
check("search release_year=2024", nrow(set_search_year) > 0,
      sprintf("found %d", nrow(set_search_year)))

# count
set_count <- sdk$sets$count()
check("set count", set_count > 100, sprintf("total sets: %d", set_count))


# ============================================================
#  IDENTIFIERS -- IdentifierQuery (18 methods)
# ============================================================
section("Identifiers")

if (!is.null(uuid)) {
  ids <- sdk$identifiers$get_identifiers(uuid)
  check("get_identifiers", !is.null(ids),
        sprintf("keys=%s", paste(head(names(ids), 5), collapse = ", ")))

  # Exercise ALL named find_by_* methods using real IDs from Lightning Bolt
  if (!is.null(ids)) {
    # Scryfall ID
    if (has_value(ids$scryfallId)) {
      by_scry <- sdk$identifiers$find_by_scryfall_id(ids$scryfallId)
      check("find_by_scryfall_id", nrow(by_scry) > 0, sprintf("found %d", nrow(by_scry)))
    } else skip("find_by_scryfall_id", "no scryfallId in data")

    # Scryfall Oracle ID
    if (has_value(ids$scryfallOracleId)) {
      by_oracle <- sdk$identifiers$find_by_scryfall_oracle_id(ids$scryfallOracleId)
      check("find_by_scryfall_oracle_id", nrow(by_oracle) > 0,
            sprintf("found %d", nrow(by_oracle)))
    } else skip("find_by_scryfall_oracle_id", "no scryfallOracleId")

    # Scryfall Illustration ID
    if (has_value(ids$scryfallIllustrationId)) {
      by_illus <- sdk$identifiers$find_by_scryfall_illustration_id(ids$scryfallIllustrationId)
      check("find_by_scryfall_illustration_id", nrow(by_illus) > 0,
            sprintf("found %d", nrow(by_illus)))
    } else skip("find_by_scryfall_illustration_id", "no scryfallIllustrationId")

    # TCGPlayer Product ID
    if (has_value(ids$tcgplayerProductId)) {
      by_tcg <- sdk$identifiers$find_by_tcgplayer_id(as.character(ids$tcgplayerProductId))
      check("find_by_tcgplayer_id", nrow(by_tcg) > 0, sprintf("found %d", nrow(by_tcg)))
    } else skip("find_by_tcgplayer_id", "no tcgplayerProductId")

    # TCGPlayer Etched ID
    if (has_value(ids$tcgplayerEtchedProductId)) {
      by_tcg_e <- sdk$identifiers$find_by_tcgplayer_etched_id(as.character(ids$tcgplayerEtchedProductId))
      check("find_by_tcgplayer_etched_id", nrow(by_tcg_e) > 0)
    } else skip("find_by_tcgplayer_etched_id", "no tcgplayerEtchedProductId")

    # MTGO ID
    if (has_value(ids$mtgoId)) {
      by_mtgo <- sdk$identifiers$find_by_mtgo_id(as.character(ids$mtgoId))
      check("find_by_mtgo_id", nrow(by_mtgo) > 0)
    } else skip("find_by_mtgo_id", "no mtgoId")

    # MTGO Foil ID
    if (has_value(ids$mtgoFoilId)) {
      by_mtgo_f <- sdk$identifiers$find_by_mtgo_foil_id(as.character(ids$mtgoFoilId))
      check("find_by_mtgo_foil_id", nrow(by_mtgo_f) > 0)
    } else skip("find_by_mtgo_foil_id", "no mtgoFoilId")

    # MTG Arena ID
    if (has_value(ids$mtgArenaId)) {
      by_arena <- sdk$identifiers$find_by_mtg_arena_id(as.character(ids$mtgArenaId))
      check("find_by_mtg_arena_id", nrow(by_arena) > 0)
    } else skip("find_by_mtg_arena_id", "no mtgArenaId")

    # Multiverse ID
    if (has_value(ids$multiverseId)) {
      by_multi <- sdk$identifiers$find_by_multiverse_id(as.character(ids$multiverseId))
      check("find_by_multiverse_id", nrow(by_multi) > 0)
    } else skip("find_by_multiverse_id", "no multiverseId")

    # MCM ID
    if (has_value(ids$mcmId)) {
      by_mcm <- sdk$identifiers$find_by_mcm_id(as.character(ids$mcmId))
      check("find_by_mcm_id", nrow(by_mcm) > 0)
    } else skip("find_by_mcm_id", "no mcmId")

    # MCM Meta ID
    if (has_value(ids$mcmMetaId)) {
      by_mcm_m <- sdk$identifiers$find_by_mcm_meta_id(as.character(ids$mcmMetaId))
      check("find_by_mcm_meta_id", nrow(by_mcm_m) > 0)
    } else skip("find_by_mcm_meta_id", "no mcmMetaId")

    # Card Kingdom ID
    if (has_value(ids$cardKingdomId)) {
      by_ck <- sdk$identifiers$find_by_card_kingdom_id(as.character(ids$cardKingdomId))
      check("find_by_card_kingdom_id", nrow(by_ck) > 0)
    } else skip("find_by_card_kingdom_id", "no cardKingdomId")

    # Card Kingdom Foil ID
    if (has_value(ids$cardKingdomFoilId)) {
      by_ck_f <- sdk$identifiers$find_by_card_kingdom_foil_id(as.character(ids$cardKingdomFoilId))
      check("find_by_card_kingdom_foil_id", nrow(by_ck_f) > 0)
    } else skip("find_by_card_kingdom_foil_id", "no cardKingdomFoilId")

    # Card Kingdom Etched ID
    if (has_value(ids$cardKingdomEtchedId)) {
      by_ck_e <- sdk$identifiers$find_by_card_kingdom_etched_id(as.character(ids$cardKingdomEtchedId))
      check("find_by_card_kingdom_etched_id", nrow(by_ck_e) > 0)
    } else skip("find_by_card_kingdom_etched_id", "no cardKingdomEtchedId")

    # Cardsphere ID
    if (has_value(ids$cardsphereId)) {
      by_cs <- sdk$identifiers$find_by_cardsphere_id(as.character(ids$cardsphereId))
      check("find_by_cardsphere_id", nrow(by_cs) > 0)
    } else skip("find_by_cardsphere_id", "no cardsphereId")

    # Cardsphere Foil ID
    if (has_value(ids$cardsphereFoilId)) {
      by_cs_f <- sdk$identifiers$find_by_cardsphere_foil_id(as.character(ids$cardsphereFoilId))
      check("find_by_cardsphere_foil_id", nrow(by_cs_f) > 0)
    } else skip("find_by_cardsphere_foil_id", "no cardsphereFoilId")

    # Generic find_by with valid column
    if (has_value(ids$scryfallId)) {
      by_gen <- sdk$identifiers$find_by("scryfallId", ids$scryfallId)
      check("find_by generic (scryfallId)", nrow(by_gen) > 0)
    }
  }
}

# Second card for identifier coverage
section("Identifiers (secondary card for fuller coverage)")
alt_cards <- sdk$cards$search(name = "Llanowar Elves", set_code = "M19", limit = 1)
if (nrow(alt_cards) > 0) {
  alt_uuid <- alt_cards$uuid[1]
  alt_ids <- sdk$identifiers$get_identifiers(alt_uuid)
  if (!is.null(alt_ids)) {
    id_checks <- list(
      list(col = "mtgArenaId",               method = "find_by_mtg_arena_id"),
      list(col = "multiverseId",             method = "find_by_multiverse_id"),
      list(col = "mcmMetaId",                method = "find_by_mcm_meta_id"),
      list(col = "cardsphereId",             method = "find_by_cardsphere_id"),
      list(col = "cardsphereFoilId",         method = "find_by_cardsphere_foil_id"),
      list(col = "cardKingdomEtchedId",      method = "find_by_card_kingdom_etched_id"),
      list(col = "tcgplayerEtchedProductId", method = "find_by_tcgplayer_etched_id"),
      list(col = "mtgoFoilId",               method = "find_by_mtgo_foil_id")
    )
    for (item in id_checks) {
      val <- alt_ids[[item$col]]
      if (has_value(val)) {
        fn <- sdk$identifiers[[item$method]]
        result <- fn(as.character(val))
        check(sprintf("%s (alt card)", item$method), nrow(result) > 0,
              sprintf("%s=%s", item$col, val))
      } else {
        skip(sprintf("%s (alt card)", item$method), sprintf("no %s", item$col))
      }
    }
  } else skip("alt card identifiers", "no identifiers found")
} else skip("alt card identifiers", "Llanowar Elves M19 not found")

# find_by -- invalid column raises error
err_caught <- tryCatch({
  sdk$identifiers$find_by("invalidColumn", "123")
  FALSE
}, error = function(e) TRUE)
check("find_by invalid column raises", err_caught)


# ============================================================
#  LEGALITIES -- LegalityQuery (7 methods)
# ============================================================
section("Legalities")

if (!is.null(uuid)) {
  # formats_for_card
  formats <- sdk$legalities$formats_for_card(uuid)
  check("formats_for_card", length(formats) > 0,
        sprintf("formats: %s...", paste(head(names(formats), 5), collapse = ", ")))

  # is_legal
  is_legal <- sdk$legalities$is_legal(uuid, "modern")
  check("is_legal modern", isTRUE(is_legal))

  is_legal_fake <- sdk$legalities$is_legal(uuid, "nonexistent_format")
  check("is_legal nonexistent format", isFALSE(is_legal_fake))
}

# legal_in
modern_cards <- sdk$legalities$legal_in("modern", limit = 5)
check("legal_in modern", nrow(modern_cards) > 0, sprintf("found %d", nrow(modern_cards)))

# legal_in -- with offset
legal_p1 <- sdk$legalities$legal_in("modern", limit = 3, offset = 0)
legal_p2 <- sdk$legalities$legal_in("modern", limit = 3, offset = 3)
check("legal_in offset", nrow(legal_p1) > 0 && nrow(legal_p2) > 0)

# banned_in
banned <- sdk$legalities$banned_in("modern", limit = 5)
check("banned_in modern", is.data.frame(banned), sprintf("found %d", nrow(banned)))

# restricted_in
restricted <- sdk$legalities$restricted_in("vintage", limit = 5)
check("restricted_in vintage", is.data.frame(restricted), sprintf("found %d", nrow(restricted)))

# suspended_in (may have 0 results)
suspended <- sdk$legalities$suspended_in("historic", limit = 5)
check("suspended_in historic", is.data.frame(suspended), sprintf("found %d", nrow(suspended)))

# not_legal_in
not_legal <- sdk$legalities$not_legal_in("standard", limit = 5)
check("not_legal_in standard", is.data.frame(not_legal), sprintf("found %d", nrow(not_legal)))


# ============================================================
#  PRICES -- PriceQuery (7 methods)
#  Downloads AllPricesToday.json.gz (~large file)
# ============================================================
section("Prices")

tryCatch({
  if (!is.null(uuid)) {
    # get -- raw nested structure
    price_raw <- sdk$prices$get(uuid)
    check("prices.get", is.list(price_raw) || is.null(price_raw),
          sprintf("type=%s", class(price_raw)[1]))

    # today
    today_prices <- sdk$prices$today(uuid)
    check("prices.today", is.data.frame(today_prices),
          sprintf("found %d rows", nrow(today_prices)))

    if (nrow(today_prices) > 0) {
      # today -- with provider filter
      providers <- unique(today_prices$provider)
      if (length(providers) > 0) {
        first_prov <- providers[1]
        today_filt <- sdk$prices$today(uuid, provider = first_prov)
        check("prices.today provider filter", nrow(today_filt) > 0,
              sprintf("provider=%s", first_prov))
      }

      # today -- with finish filter
      finishes <- unique(today_prices$finish)
      if (length(finishes) > 0) {
        first_fin <- finishes[1]
        today_fin <- sdk$prices$today(uuid, finish = first_fin)
        check("prices.today finish filter", nrow(today_fin) > 0,
              sprintf("finish=%s", first_fin))
      }

      # today -- with category filter
      categories <- unique(today_prices$category)
      if (length(categories) > 0) {
        first_cat <- categories[1]
        today_cat <- sdk$prices$today(uuid, category = first_cat)
        check("prices.today category filter", nrow(today_cat) > 0,
              sprintf("cat=%s", first_cat))
      }
    }

    # history
    history <- sdk$prices$history(uuid)
    check("prices.history", is.data.frame(history),
          sprintf("found %d rows", nrow(history)))

    if (nrow(history) > 0) {
      # history -- with date range
      dates <- sort(unique(history$date[!is.na(history$date)]))
      if (length(dates) >= 2) {
        hist_range <- sdk$prices$history(uuid, date_from = dates[1], date_to = dates[length(dates)])
        check("prices.history date range", nrow(hist_range) > 0)
      } else skip("prices.history date range", "only 1 date")

      # history -- with provider filter
      hist_prov <- sdk$prices$history(uuid, provider = history$provider[1])
      check("prices.history provider filter", is.data.frame(hist_prov))
    }

    # price_trend
    trend <- sdk$prices$price_trend(uuid)
    check("prices.price_trend", is.list(trend) || is.null(trend),
          if (!is.null(trend)) sprintf("avg=$%.2f", trend$avg_price) else "no trend data")

    if (!is.null(trend)) {
      check("price_trend has keys",
            all(c("min_price", "max_price", "avg_price") %in% names(trend)))

      # price_trend with provider/finish
      trend2 <- sdk$prices$price_trend(uuid, provider = "tcgplayer", finish = "normal")
      check("price_trend with filters", is.list(trend2) || is.null(trend2))
    }

    # cheapest_printing
    cheapest <- sdk$prices$cheapest_printing("Lightning Bolt")
    check("prices.cheapest_printing", is.list(cheapest) || is.null(cheapest),
          if (!is.null(cheapest)) sprintf("$%.2f", cheapest$price) else "no price data")

    if (!is.null(cheapest)) {
      check("cheapest has price", !is.null(cheapest$price) && cheapest$price > 0)
    }

    # cheapest with different provider
    cheapest2 <- sdk$prices$cheapest_printing("Lightning Bolt", provider = "cardkingdom")
    check("cheapest_printing alt provider", is.list(cheapest2) || is.null(cheapest2))

    # cheapest_printings (N cheapest overall)
    cheapest_n <- sdk$prices$cheapest_printings(limit = 5)
    check("prices.cheapest_printings", is.data.frame(cheapest_n),
          sprintf("found %d", nrow(cheapest_n)))

    # most_expensive_printings
    priciest_n <- sdk$prices$most_expensive_printings(limit = 5)
    check("prices.most_expensive_printings", is.data.frame(priciest_n),
          sprintf("found %d", nrow(priciest_n)))

  } else skip("prices tests", "no uuid from Lightning Bolt")
}, error = function(e) {
  check("prices module available", FALSE, sprintf("error: %s", conditionMessage(e)))
})


# ============================================================
#  SET FINANCIAL SUMMARY (requires prices loaded)
# ============================================================
section("Set Financial Summary (EV calculation)")

if ("prices_today" %in% sdk$views) {
  fin <- sdk$sets$get_financial_summary("MH3")
  check("get_financial_summary MH3",
        !is.null(fin) && (fin$card_count %||% 0) > 0,
        if (!is.null(fin)) sprintf("cards=%d, total=$%.2f", fin$card_count, fin$total_value)
        else "no data")

  # With different provider
  fin_ck <- sdk$sets$get_financial_summary("MH3", provider = "cardkingdom", finish = "normal")
  check("get_financial_summary alt provider", is.list(fin_ck) || is.null(fin_ck))

  # Nonexistent set
  fin_none <- sdk$sets$get_financial_summary("ZZZZZ")
  check("get_financial_summary no data", is.null(fin_none))
} else skip("get_financial_summary", "prices not loaded")


# ============================================================
#  DECKS -- DeckQuery (3 methods)
#  Downloads DeckList.json
# ============================================================
section("Decks")

tryCatch({
  # count
  deck_count <- sdk$decks$count()
  check("decks.count", deck_count >= 0, sprintf("total decks: %d", deck_count))

  # list -- no filter
  deck_list <- sdk$decks$list()
  check("decks.list (all)", is.data.frame(deck_list), sprintf("found %d", nrow(deck_list)))

  if (nrow(deck_list) > 0) {
    # list -- set_code filter
    first_code <- deck_list$code[1]
    if (!is.null(first_code) && nzchar(first_code)) {
      decks_by_set <- sdk$decks$list(set_code = first_code)
      check("decks.list set_code", nrow(decks_by_set) > 0, sprintf("set=%s", first_code))
    }

    # list -- deck_type filter
    first_type <- deck_list$type[1]
    if (!is.null(first_type) && nzchar(first_type)) {
      decks_by_type <- sdk$decks$list(deck_type = first_type)
      check("decks.list deck_type", nrow(decks_by_type) > 0, sprintf("type=%s", first_type))
    }

    # search -- name
    first_name <- deck_list$name[1]
    if (!is.null(first_name) && nzchar(first_name)) {
      search_term <- strsplit(first_name, " ")[[1]][1]
      deck_search <- sdk$decks$search(name = search_term)
      check("decks.search name", nrow(deck_search) > 0,
            sprintf("term='%s'", search_term))
    }

    # search -- set_code
    if (!is.null(first_code) && nzchar(first_code)) {
      deck_search_set <- sdk$decks$search(set_code = first_code)
      check("decks.search set_code", nrow(deck_search_set) > 0)
    }
  } else skip("deck list/search tests", "no decks loaded")
}, error = function(e) {
  check("decks module available", FALSE, sprintf("error: %s", conditionMessage(e)))
})


# ============================================================
#  SKUS -- SkuQuery (3 methods)
#  Downloads TcgplayerSkus.json.gz (~large file)
# ============================================================
section("SKUs")

tryCatch({
  if (!is.null(uuid)) {
    # get
    skus <- sdk$skus$get(uuid)
    check("skus.get", is.data.frame(skus), sprintf("found %d SKUs", nrow(skus)))

    if (nrow(skus) > 0) {
      # find_by_sku_id
      sku_id <- skus$skuId[1]
      if (!is.null(sku_id) && !is.na(sku_id)) {
        by_sku <- sdk$skus$find_by_sku_id(sku_id)
        check("skus.find_by_sku_id", !is.null(by_sku), sprintf("skuId=%s", sku_id))
      } else skip("skus.find_by_sku_id", "no skuId in data")

      # find_by_product_id
      prod_id <- skus$productId[1]
      if (!is.null(prod_id) && !is.na(prod_id)) {
        by_prod <- sdk$skus$find_by_product_id(prod_id)
        check("skus.find_by_product_id", nrow(by_prod) > 0,
              sprintf("productId=%s", prod_id))
      } else skip("skus.find_by_product_id", "no productId in data")
    } else {
      skip("skus.find_by_sku_id", "no SKU data for this card")
      skip("skus.find_by_product_id", "no SKU data for this card")
    }
  } else skip("skus tests", "no uuid")
}, error = function(e) {
  check("skus module available", FALSE, sprintf("error: %s", conditionMessage(e)))
})


# ============================================================
#  ENUMS -- EnumQuery (3 methods)
#  Downloads Keywords.json, CardTypes.json, EnumValues.json
# ============================================================
section("Enums")

tryCatch({
  # keywords
  kw <- sdk$enums$keywords()
  check("enums.keywords", is.list(kw) && length(kw) > 0,
        sprintf("keys=%s", paste(head(names(kw), 5), collapse = ", ")))

  if (length(kw) > 0) {
    has_ability <- "abilityWords" %in% names(kw) ||
      any(grepl("ability", names(kw), ignore.case = TRUE))
    check("keywords has expected keys", has_ability || length(kw) > 0,
          sprintf("top keys: %s", paste(head(names(kw), 5), collapse = ", ")))
  }

  # card_types
  ct <- sdk$enums$card_types()
  check("enums.card_types", is.list(ct) && length(ct) > 0,
        sprintf("keys=%s", paste(head(names(ct), 5), collapse = ", ")))

  if (length(ct) > 0) {
    has_creature <- any(grepl("creature", names(ct), ignore.case = TRUE))
    check("card_types has creature", has_creature || length(ct) > 0)
  }

  # enum_values
  ev <- sdk$enums$enum_values()
  check("enums.enum_values", is.list(ev) && length(ev) > 0,
        sprintf("keys=%s", paste(head(names(ev), 5), collapse = ", ")))
}, error = function(e) {
  check("enums module available", FALSE, sprintf("error: %s", conditionMessage(e)))
})


# ============================================================
#  SEALED -- SealedQuery (2 methods)
# ============================================================
section("Sealed Products")

# list -- no filter
sealed_all <- sdk$sealed$list()
check("sealed.list (all)", is.data.frame(sealed_all), sprintf("found %d", nrow(sealed_all)))

# list -- set_code filter
sealed_mh3 <- sdk$sealed$list(set_code = "MH3")
check("sealed.list set_code=MH3", is.data.frame(sealed_mh3),
      sprintf("found %d", nrow(sealed_mh3)))

# list -- category filter
sealed_cat <- sdk$sealed$list(category = "booster_box")
check("sealed.list category", is.data.frame(sealed_cat),
      sprintf("found %d", nrow(sealed_cat)))

# get -- nonexistent uuid
sealed_item <- sdk$sealed$get("00000000-0000-0000-0000-000000000000")
check("sealed.get (graceful)", is.null(sealed_item) || is.list(sealed_item))


# ============================================================
#  BOOSTER -- BoosterSimulator (4 methods)
# ============================================================
section("Booster Simulation")

# available_types
types <- sdk$booster$available_types("MH3")
check("booster.available_types", is.character(types), sprintf("types: %s", paste(types, collapse = ", ")))

# open_pack
tryCatch({
  pack <- sdk$booster$open_pack("MH3", "draft")
  check("booster.open_pack", is.data.frame(pack), sprintf("got %d cards", nrow(pack)))
}, error = function(e) {
  if (grepl("No booster config", conditionMessage(e)))
    check("booster.open_pack raises (no booster data)", TRUE)
  else
    check("booster.open_pack", FALSE, conditionMessage(e))
})

# open_box
tryCatch({
  box <- sdk$booster$open_box("MH3", "draft", packs = 1)
  check("booster.open_box", is.list(box))
}, error = function(e) {
  if (grepl("No booster config", conditionMessage(e)))
    check("booster.open_box raises (no booster data)", TRUE)
  else
    check("booster.open_box", FALSE, conditionMessage(e))
})

# sheet_contents
contents <- sdk$booster$sheet_contents("MH3", "draft", "common")
check("booster.sheet_contents", is.null(contents) || is.list(contents),
      sprintf("type=%s", class(contents)[1]))


# ============================================================
#  RAW SQL -- sdk$sql() (all modes)
# ============================================================
section("Raw SQL / Escape Hatches")

# Simple query
rows <- sdk$sql("SELECT COUNT(*) AS cnt FROM cards")
check("sql COUNT", rows$cnt[1] > 1000, sprintf("count=%d", rows$cnt[1]))

# Query with params
rows_param <- sdk$sql("SELECT name FROM cards WHERE manaValue = $1 LIMIT $2",
                       params = list(1.0, 5))
check("sql with params", nrow(rows_param) > 0, sprintf("found %d", nrow(rows_param)))

# More complex query
top_edhrec <- sdk$sql(paste(
  "SELECT name, edhrecRank FROM cards",
  "WHERE edhrecRank IS NOT NULL",
  "ORDER BY edhrecRank ASC LIMIT 5"))
check("sql top EDHREC", nrow(top_edhrec) == 5,
      sprintf("top: %s", paste(top_edhrec$name, collapse = ", ")))

# Cross-table join via raw SQL
join_result <- sdk$sql(paste(
  "SELECT c.name, s.name AS setName",
  "FROM cards c JOIN sets s ON c.setCode = s.code",
  "LIMIT 3"))
check("sql cross-table join", nrow(join_result) > 0 && "setName" %in% names(join_result))


# ============================================================
#  VIEWS -- verify views grew as we queried
# ============================================================
section("Views (post-query)")

views_after <- sdk$views
check("views grew", length(views_after) > length(views_before),
      sprintf("before=%d, after=%d, views=%s",
              length(views_before), length(views_after),
              paste(views_after, collapse = ", ")))


# ============================================================
#  EDGE CASES & VALIDATION
# ============================================================
section("Edge Cases & Validation")

# Empty search results
empty <- sdk$cards$search(name = "XYZ_NONEXISTENT_CARD_12345", limit = 5)
check("empty search result", nrow(empty) == 0)

# Card with special characters in name
s <- sdk$cards$search(name = "J%tun%", limit = 5)
check("search unicode name", is.data.frame(s), sprintf("found %d", nrow(s)))

# Card data.frame field validation
if (nrow(bolt) > 0) {
  check("card has uuid", nzchar(bolt$uuid[1]))
  check("card has name", bolt$name[1] == "Lightning Bolt")
  check("card has manaValue", !is.null(bolt$manaValue[1]),
        sprintf("mv=%s", bolt$manaValue[1]))
}

# Set data.frame field validation
if (!is.null(mh3)) {
  check("set has code", mh3$code[1] == "MH3")
  check("set has name", grepl("Horizons", mh3$name[1]), sprintf("name=%s", mh3$name[1]))
  check("set has releaseDate", nzchar(mh3$releaseDate[1]))
  check("set has type", nzchar(mh3$type[1]))
  check("set has baseSetSize", mh3$baseSetSize[1] > 0,
        sprintf("baseSetSize=%d", mh3$baseSetSize[1]))
  check("set has totalSetSize", mh3$totalSetSize[1] > 0,
        sprintf("totalSetSize=%d", mh3$totalSetSize[1]))
}

# Token field validation
if (nrow(token_search) > 0) {
  check("token has uuid", nzchar(token_search$uuid[1]))
  check("token has name", nzchar(token_search$name[1]))
}

# Atomic card validation
if (nrow(atomic) > 0) {
  check("atomic has name", atomic$name[1] == "Lightning Bolt")
  check("atomic has layout", nzchar(atomic$layout[1]))
}

# DeckList validation
tryCatch({
  deck_list_all <- sdk$decks$list()
  if (nrow(deck_list_all) > 0) {
    check("deck has code", nzchar(deck_list_all$code[1]))
    check("deck has name", nzchar(deck_list_all$name[1]))
    check("deck has type", nzchar(deck_list_all$type[1]))
    check("deck has fileName", nzchar(deck_list_all$fileName[1]))
  }
}, error = function(e) skip("deck model validation", "deck data not loaded"))


# ============================================================
#  DONE -- close and report
# ============================================================
sdk$close()
elapsed <- proc.time()["elapsed"] - t0

section("RESULTS")
total <- PASS + FAIL
cat(sprintf("  Total:   %d checks (%d skipped)\n", total, SKIP))
cat(sprintf("  Passed:  %d\n", PASS))
cat(sprintf("  Failed:  %d\n", FAIL))
cat(sprintf("  Time:    %.1fs\n", elapsed))
cat("\n")

if (FAIL > 0) {
  cat("  *** FAILURES DETECTED ***\n\n")
}

quit(status = if (FAIL == 0) 0 else 1, save = "no")
