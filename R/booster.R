#' Weighted random booster pack simulation.
#'
#' @noRd
BoosterSimulator <- R6::R6Class("BoosterSimulator",
  public = list(
    initialize = function(conn) {
      private$.conn <- conn
    },

    #' @description List available booster types for a set.
    #' @return Character vector of type names.
    available_types = function(set_code) {
      config <- private$get_booster_config(set_code)
      if (is.null(config)) return(character(0))
      names(config)
    },

    #' @description Simulate opening a single booster pack.
    #' @return A data.frame of cards in the pack.
    open_pack = function(set_code, booster_type = "draft") {
      configs <- private$get_booster_config(set_code)
      if (is.null(configs) || !(booster_type %in% names(configs))) {
        avail <- if (!is.null(configs)) names(configs) else character(0)
        stop(sprintf(
          "No booster config for set '%s' type '%s'. Available: %s",
          set_code, booster_type, paste(avail, collapse = ", ")))
      }

      config <- configs[[booster_type]]
      pack_template <- pick_pack(config[["boosters"]])
      sheets <- config[["sheets"]]

      card_uuids <- character(0)
      for (sheet_name in names(pack_template[["contents"]])) {
        count <- pack_template[["contents"]][[sheet_name]]
        if (!(sheet_name %in% names(sheets))) next
        sheet <- sheets[[sheet_name]]
        picked <- pick_from_sheet(sheet, count)
        card_uuids <- c(card_uuids, picked)
      }

      if (length(card_uuids) == 0) return(data.frame())

      private$.conn$ensure_views("cards")
      placeholders <- paste(sprintf("$%d", seq_along(card_uuids)), collapse = ", ")
      sql <- sprintf("SELECT * FROM cards WHERE uuid IN (%s)", placeholders)
      df <- private$.conn$execute(sql, as.list(card_uuids))

      # Preserve pack order
      if (nrow(df) > 0) {
        ord <- match(card_uuids, df$uuid)
        ord <- ord[!is.na(ord)]
        df <- df[ord, , drop = FALSE]
      }
      df
    },

    #' @description Simulate opening a booster box.
    #' @return A list of data.frames (one per pack).
    open_box = function(set_code, booster_type = "draft", packs = 36L) {
      lapply(seq_len(packs), function(i) self$open_pack(set_code, booster_type))
    },

    #' @description Get card UUIDs and weights for a booster sheet.
    #' @return A named integer vector (uuid = weight) or NULL.
    sheet_contents = function(set_code, booster_type, sheet_name) {
      configs <- private$get_booster_config(set_code)
      if (is.null(configs) || !(booster_type %in% names(configs))) return(NULL)
      sheets <- configs[[booster_type]][["sheets"]]
      sheet <- sheets[[sheet_name]]
      if (is.null(sheet)) return(NULL)
      sheet[["cards"]]
    }
  ),

  private = list(
    .conn = NULL,

    get_booster_config = function(set_code) {
      private$.conn$ensure_views("sets", "cards")
      tryCatch({
        df <- private$.conn$execute(
          "SELECT booster FROM sets WHERE code = $1",
          params = list(toupper(set_code)))
        if (nrow(df) == 0 || is.null(df$booster[[1]])) return(NULL)
        df$booster[[1]]
      }, error = function(e) NULL)
    }
  )
)

#' Weighted random pick of a pack template.
#' @noRd
pick_pack <- function(boosters) {
  weights <- vapply(boosters, function(b) b[["weight"]], numeric(1))
  idx <- sample(seq_along(boosters), 1, prob = weights)
  boosters[[idx]]
}

#' Weighted random pick of cards from a sheet.
#' @noRd
pick_from_sheet <- function(sheet, count) {
  cards <- sheet[["cards"]]
  uuids <- names(cards)
  weights <- as.numeric(unlist(cards))
  allow_duplicates <- isTRUE(sheet[["allowDuplicates"]])

  if (allow_duplicates) {
    return(sample(uuids, count, replace = TRUE, prob = weights))
  }

  if (count >= length(uuids)) {
    return(sample(uuids))
  }

  # Weighted sampling without replacement
  picked <- character(count)
  remaining_uuids   <- uuids
  remaining_weights  <- weights

  for (i in seq_len(count)) {
    choice <- sample(remaining_uuids, 1, prob = remaining_weights)
    picked[i] <- choice
    idx <- which(remaining_uuids == choice)
    remaining_uuids   <- remaining_uuids[-idx]
    remaining_weights  <- remaining_weights[-idx]
  }
  picked
}
