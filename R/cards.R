#' Card query interface.
#'
#' @noRd
CardQuery <- R6::R6Class("CardQuery",
  public = list(
    initialize = function(conn) {
      private$.conn <- conn
    },

    #' @description Get a single card by UUID.
    #' @param uuid MTGJSON UUID string.
    #' @return A one-row data.frame or NULL.
    get_by_uuid = function(uuid) {
      private$ensure()
      df <- private$.conn$execute("SELECT * FROM cards WHERE uuid = $1",
                                  params = list(uuid))
      if (nrow(df) == 0) return(NULL)
      df
    },

    #' @description Get multiple cards by UUID.
    #' @param uuids Character vector of UUIDs.
    #' @return A data.frame.
    get_by_uuids = function(uuids) {
      if (length(uuids) == 0) return(data.frame())
      private$ensure()
      q <- SqlBuilder$new("cards")$where_in("uuid", as.list(uuids))
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Get all printings of a card by exact name.
    #' @param name Exact card name.
    #' @param set_code Optional set code filter.
    #' @return A data.frame.
    get_by_name = function(name, set_code = NULL) {
      private$ensure()
      q <- SqlBuilder$new("cards")$where_eq("name", name)
      if (!is.null(set_code)) q$where_eq("setCode", set_code)
      q$order_by("setCode DESC", "number ASC")
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Search cards with flexible filters.
    #' @param name Name pattern (use % for LIKE wildcard).
    #' @param fuzzy_name Typo-tolerant name search.
    #' @param localized_name Foreign-language name search.
    #' @param set_code Set code filter.
    #' @param colors Character vector of colors.
    #' @param color_identity Character vector of color identity.
    #' @param types Type line search.
    #' @param rarity Rarity filter.
    #' @param legal_in Format legality filter.
    #' @param mana_value Exact mana value.
    #' @param mana_value_lte Maximum mana value.
    #' @param mana_value_gte Minimum mana value.
    #' @param text Rules text search.
    #' @param text_regex Regex rules text search.
    #' @param power Power filter.
    #' @param toughness Toughness filter.
    #' @param artist Artist name filter.
    #' @param keyword Keyword ability filter.
    #' @param is_promo Promo status filter.
    #' @param availability Availability filter ("paper", "mtgo").
    #' @param language Language filter.
    #' @param layout Card layout filter.
    #' @param set_type Set type filter.
    #' @param limit Maximum results.
    #' @param offset Result offset.
    #' @return A data.frame.
    search = function(name = NULL, fuzzy_name = NULL, localized_name = NULL,
                      set_code = NULL, colors = NULL, color_identity = NULL,
                      types = NULL, rarity = NULL, legal_in = NULL,
                      mana_value = NULL, mana_value_lte = NULL,
                      mana_value_gte = NULL, text = NULL, text_regex = NULL,
                      power = NULL, toughness = NULL, artist = NULL,
                      keyword = NULL, is_promo = NULL, availability = NULL,
                      language = NULL, layout = NULL, set_type = NULL,
                      limit = 100L, offset = 0L) {
      private$ensure()
      q <- SqlBuilder$new("cards")

      if (!is.null(name)) {
        if (grepl("%", name, fixed = TRUE)) q$where_like("name", name)
        else q$where_eq("name", name)
      }
      if (!is.null(fuzzy_name))
        q$where_fuzzy("cards.name", fuzzy_name, threshold = 0.8)
      if (!is.null(set_code))    q$where_eq("setCode", set_code)
      if (!is.null(rarity))      q$where_eq("rarity", rarity)
      if (!is.null(mana_value))  q$where_eq("manaValue", mana_value)
      if (!is.null(mana_value_lte)) q$where_lte("manaValue", mana_value_lte)
      if (!is.null(mana_value_gte)) q$where_gte("manaValue", mana_value_gte)
      if (!is.null(text))        q$where_like("text", paste0("%", text, "%"))
      if (!is.null(text_regex))  q$where_regex("text", text_regex)
      if (!is.null(types))       q$where_like("type", paste0("%", types, "%"))
      if (!is.null(power))       q$where_eq("power", power)
      if (!is.null(toughness))   q$where_eq("toughness", toughness)
      if (!is.null(artist))      q$where_like("artist", paste0("%", artist, "%"))
      if (!is.null(language))    q$where_eq("language", language)
      if (!is.null(layout))      q$where_eq("layout", layout)
      if (!is.null(is_promo))    q$where_eq("isPromo", is_promo)

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
      if (!is.null(color_identity)) {
        for (color in color_identity) {
          idx <- length(q$.__enclos_env__$private$.params) + 1L
          q$.__enclos_env__$private$.where <- c(
            q$.__enclos_env__$private$.where,
            sprintf("list_contains(colorIdentity, $%d)", idx))
          q$.__enclos_env__$private$.params <- c(
            q$.__enclos_env__$private$.params, list(color))
        }
      }
      if (!is.null(keyword)) {
        idx <- length(q$.__enclos_env__$private$.params) + 1L
        q$.__enclos_env__$private$.where <- c(
          q$.__enclos_env__$private$.where,
          sprintf("list_contains(keywords, $%d)", idx))
        q$.__enclos_env__$private$.params <- c(
          q$.__enclos_env__$private$.params, list(keyword))
      }
      if (!is.null(availability)) {
        idx <- length(q$.__enclos_env__$private$.params) + 1L
        q$.__enclos_env__$private$.where <- c(
          q$.__enclos_env__$private$.where,
          sprintf("list_contains(availability, $%d)", idx))
        q$.__enclos_env__$private$.params <- c(
          q$.__enclos_env__$private$.params, list(availability))
      }

      if (!is.null(localized_name)) {
        private$.conn$ensure_views("card_foreign_data")
        q$select("cards.*")
        q$join("JOIN card_foreign_data cfd ON cards.uuid = cfd.uuid")
        if (grepl("%", localized_name, fixed = TRUE))
          q$where_like("cfd.name", localized_name)
        else
          q$where_eq("cfd.name", localized_name)
      }

      if (!is.null(legal_in)) {
        private$.conn$ensure_views("card_legalities")
        q$join("JOIN card_legalities cl ON cards.uuid = cl.uuid")
        q$where_eq("cl.format", legal_in)
        q$where_eq("cl.status", "Legal")
      }

      if (!is.null(set_type)) {
        private$.conn$ensure_views("sets")
        q$select("cards.*")
        q$join("JOIN sets s ON cards.setCode = s.code")
        q$where_eq("s.type", set_type)
      }

      if (!is.null(fuzzy_name)) {
        sim_idx <- length(q$.__enclos_env__$private$.params) + 1L
        q$.__enclos_env__$private$.params <- c(
          q$.__enclos_env__$private$.params, list(fuzzy_name))
        q$order_by(
          sprintf("jaro_winkler_similarity(cards.name, $%d) DESC", sim_idx),
          "cards.number ASC")
      } else {
        q$order_by("cards.name ASC", "cards.number ASC")
      }

      q$limit(limit)$offset(offset)
      built <- q$build()
      private$.conn$execute(built$sql, built$params)
    },

    #' @description Get all printings of a card (alias for get_by_name).
    #' @param name Exact card name.
    #' @return A data.frame.
    get_printings = function(name) {
      self$get_by_name(name)
    },

    #' @description Get atomic (oracle) card data by name.
    #' @param name Exact card name or face name.
    #' @return A data.frame.
    get_atomic = function(name) {
      private$ensure()
      atomic_cols <- c(
        "name", "asciiName", "faceName", "type", "types", "subtypes",
        "supertypes", "colors", "colorIdentity", "colorIndicator",
        "producedMana", "manaCost", "text", "layout", "side", "power",
        "toughness", "loyalty", "keywords", "isFunny", "edhrecSaltiness",
        "subsets", "manaValue", "faceConvertedManaCost", "faceManaValue",
        "defense", "hand", "life", "edhrecRank", "hasAlternativeDeckLimit",
        "isReserved", "isGameChanger", "printings", "leadershipSkills",
        "relatedCards"
      )
      q <- SqlBuilder$new("cards")
      q$select(atomic_cols)
      q$where_eq("name", name)
      q$order_by("isFunny ASC NULLS FIRST", "isOnlineOnly ASC NULLS FIRST",
                  "side ASC NULLS FIRST")
      built <- q$build()
      df <- private$.conn$execute(built$sql, built$params)

      # Fallback: search by faceName
      if (nrow(df) == 0) {
        q2 <- SqlBuilder$new("cards")
        q2$select(atomic_cols)
        q2$where("CAST(faceName AS VARCHAR) = $1", name)
        q2$order_by("isFunny ASC NULLS FIRST", "isOnlineOnly ASC NULLS FIRST",
                    "side ASC NULLS FIRST")
        built2 <- q2$build()
        df <- private$.conn$execute(built2$sql, built2$params)
      }

      if (nrow(df) == 0) return(df)

      # De-duplicate by name + faceName
      key <- paste(df$name, df$faceName, sep = "|||")
      df[!duplicated(key), , drop = FALSE]
    },

    #' @description Find cards by Scryfall ID.
    #' @param scryfall_id Scryfall UUID.
    #' @return A data.frame.
    find_by_scryfall_id = function(scryfall_id) {
      private$.conn$ensure_views("cards", "card_identifiers")
      private$.conn$execute(
        "SELECT c.* FROM cards c JOIN card_identifiers ci ON c.uuid = ci.uuid WHERE ci.scryfallId = $1",
        params = list(scryfall_id))
    },

    #' @description Get random cards.
    #' @param count Number of random cards (default 1).
    #' @return A data.frame.
    random = function(count = 1L) {
      private$ensure()
      private$.conn$execute(sprintf("SELECT * FROM cards USING SAMPLE %d", count))
    },

    #' @description Count cards matching optional filters.
    #' @param ... Column name = value pairs.
    #' @return Integer count.
    count = function(...) {
      private$ensure()
      filters <- list(...)
      if (length(filters) == 0) {
        return(private$.conn$execute_scalar("SELECT COUNT(*) FROM cards") %||% 0L)
      }
      q <- SqlBuilder$new("cards")$select("COUNT(*)")
      for (nm in names(filters)) {
        q$where_eq(nm, filters[[nm]])
      }
      built <- q$build()
      private$.conn$execute_scalar(built$sql, built$params) %||% 0L
    }
  ),

  private = list(
    .conn = NULL,
    ensure = function() {
      private$.conn$ensure_views("cards")
    }
  )
)

#' Null-coalescing operator.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
