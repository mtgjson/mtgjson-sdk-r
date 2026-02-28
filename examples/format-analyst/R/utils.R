# Shared constants and helpers for the Format Metagame Analyst.

# MTG color palette for consistent chart theming
mtg_colors <- c(
  W = "#F9FAF4", U = "#0E68AB", B = "#150B00",
  R = "#D3202A", G = "#00733E",
  Colorless = "#CDC5BF", Multicolor = "#E6C34D"
)

# Outline colors for light fills (White)
mtg_outline <- c(
  W = "#B8A860", U = "#0E68AB", B = "#150B00",
  R = "#D3202A", G = "#00733E",
  Colorless = "#9E9893", Multicolor = "#C9A830"
)

# Readable labels for color codes
color_labels <- c(
  W = "White", U = "Blue", B = "Black",
  R = "Red", G = "Green",
  Colorless = "Colorless", Multicolor = "Multicolor"
)

# Supported competitive formats
format_choices <- c(
  "standard", "pioneer", "modern", "legacy", "vintage",
  "pauper", "commander", "historic", "alchemy", "brawl",
  "explorer", "timeless", "oathbreaker", "predh",
  "duel", "penny", "oldschool", "premodern"
)

# Main card supertypes for type breakdown
main_types <- c(
  "Creature", "Instant", "Sorcery", "Enchantment",
  "Artifact", "Planeswalker", "Land", "Battle"
)

# Type palette
type_colors <- c(
  Creature = "#00733E", Instant = "#0E68AB", Sorcery = "#D3202A",
  Enchantment = "#E6C34D", Artifact = "#CDC5BF", Planeswalker = "#8B5CF6",
  Land = "#92702E", Battle = "#D97706"
)

#' Classify a card row into a single color bucket.
#' @param colors A list-column element (character vector of colors).
#' @return One of "W","U","B","R","G","Multicolor","Colorless".
classify_color <- function(colors) {
  if (is.null(colors) || length(colors) == 0 || all(is.na(colors))) {
    return("Colorless")
  }
  cols <- unlist(colors)
  cols <- cols[!is.na(cols)]
  if (length(cols) == 0) return("Colorless")
  if (length(cols) > 1) return("Multicolor")
  cols[1]
}

#' Cap mana value at 7+ for histogram buckets.
#' @param mv Numeric mana value.
#' @return Character label like "0","1",...,"7+".
mana_bucket <- function(mv) {
  ifelse(is.na(mv), NA_character_,
         ifelse(mv >= 7, "7+", as.character(as.integer(mv))))
}

#' Safely convert power/toughness strings to numeric.
#' Returns NA for "*", "X", etc.
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}
