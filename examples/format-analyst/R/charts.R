# ggplot2 chart-building functions for the Format Metagame Analyst.

library(ggplot2)
library(scales)

# Shared minimal theme
theme_mtg <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

#' Horizontal bar chart comparing card pool sizes across formats.
chart_format_comparison <- function(df, current_format = NULL) {
  df$format <- factor(df$format, levels = df$format[order(df$count)])
  df$highlight <- ifelse(df$format == current_format, "Current", "Other")

  ggplot(df, aes(x = format, y = count, fill = highlight)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = c(Current = "#0E68AB", Other = "#CDC5BF")) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Legal cards", title = "Card Pool by Format") +
    theme_mtg()
}

#' Bar chart of color distribution.
chart_color_dist <- function(df) {
  order <- c("W", "U", "B", "R", "G", "Multicolor", "Colorless")
  df <- df[df$color_bucket %in% order, ]
  df$color_bucket <- factor(df$color_bucket, levels = order)
  df$label <- color_labels[as.character(df$color_bucket)]

  fills <- mtg_colors[as.character(df$color_bucket)]
  outlines <- mtg_outline[as.character(df$color_bucket)]

  ggplot(df, aes(x = color_bucket, y = count)) +
    geom_col(fill = fills, color = outlines, linewidth = 0.5) +
    scale_x_discrete(labels = color_labels) +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Cards", title = "Color Distribution") +
    theme_mtg()
}

#' Mana curve histogram.
chart_mana_curve <- function(df) {
  bucket_order <- c("0", "1", "2", "3", "4", "5", "6", "7+")
  df <- df[df$bucket %in% bucket_order, ]
  df$bucket <- factor(df$bucket, levels = bucket_order)

  ggplot(df, aes(x = bucket, y = cnt)) +
    geom_col(fill = "#0E68AB", color = "#094d7e", linewidth = 0.3) +
    scale_y_continuous(labels = comma) +
    labs(x = "Mana Value", y = "Cards", title = "Mana Curve") +
    theme_mtg()
}

#' Average mana value by color grouped bar chart.
chart_avg_mv <- function(df) {
  order <- c("W", "U", "B", "R", "G", "Multicolor", "Colorless")
  df <- df[df$color %in% order, ]
  df$color <- factor(df$color, levels = order)

  fills <- mtg_colors[as.character(df$color)]
  outlines <- mtg_outline[as.character(df$color)]

  ggplot(df, aes(x = color, y = avg_mv)) +
    geom_col(fill = fills, color = outlines, linewidth = 0.5) +
    scale_x_discrete(labels = color_labels) +
    labs(x = NULL, y = "Average Mana Value",
         title = "Average Mana Value by Color") +
    theme_mtg()
}

#' Horizontal bar chart of top keywords.
chart_keywords <- function(df) {
  df$keyword <- factor(df$keyword, levels = rev(df$keyword))

  ggplot(df, aes(x = keyword, y = count)) +
    geom_col(fill = "#00733E", color = "#005a30", linewidth = 0.3) +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Cards", title = "Top Keywords") +
    theme_mtg()
}

#' Donut chart of card type breakdown.
chart_type_donut <- function(df) {
  df <- df[df$count > 0, ]
  df$pct <- df$count / sum(df$count)
  df$label <- paste0(df$type, "\n", percent(df$pct, accuracy = 0.1))
  df$type <- factor(df$type, levels = df$type)

  fills <- type_colors[as.character(df$type)]

  ggplot(df, aes(x = 2, y = count, fill = type)) +
    geom_col(width = 1, color = "white", linewidth = 0.5) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    scale_fill_manual(values = fills, labels = df$type) +
    labs(fill = "Type", title = "Card Type Breakdown") +
    theme_void(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      legend.position = "right"
    )
}

#' Horizontal bar chart of creature subtype frequency.
chart_subtypes <- function(df) {
  df$subtype <- factor(df$subtype, levels = rev(df$subtype))

  ggplot(df, aes(x = subtype, y = count)) +
    geom_col(fill = "#D3202A", color = "#a81922", linewidth = 0.3) +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Creatures", title = "Top Creature Subtypes") +
    theme_mtg()
}

#' P/T heatmap for creatures.
chart_pt_heatmap <- function(creatures) {
  df <- creatures[!is.na(creatures$p) & !is.na(creatures$t), ]
  df$p <- pmin(df$p, 13)
  df$t <- pmin(df$t, 13)

  ggplot(df, aes(x = factor(p), y = factor(t))) +
    geom_bin2d(drop = FALSE) +
    scale_fill_gradient(low = "#f0f0f0", high = "#D3202A",
                        name = "Cards") +
    labs(x = "Power", y = "Toughness",
         title = "Power / Toughness Heatmap") +
    theme_mtg() +
    theme(legend.position = "right")
}

#' Average P/T by mana cost line chart.
chart_avg_pt_by_mv <- function(creatures) {
  df <- creatures[!is.na(creatures$mv) & creatures$mv <= 10, ]
  if (nrow(df) == 0) return(ggplot() + theme_void())

  agg <- stats::aggregate(cbind(p, t) ~ mv, data = df, FUN = mean)

  ggplot(agg) +
    geom_line(aes(x = mv, y = p, color = "Power"), linewidth = 1) +
    geom_point(aes(x = mv, y = p, color = "Power"), size = 2.5) +
    geom_line(aes(x = mv, y = t, color = "Toughness"), linewidth = 1) +
    geom_point(aes(x = mv, y = t, color = "Toughness"), size = 2.5) +
    scale_color_manual(values = c(Power = "#D3202A", Toughness = "#0E68AB")) +
    scale_x_continuous(breaks = 0:10) +
    labs(x = "Mana Value", y = "Average", color = NULL,
         title = "Average Power & Toughness by Mana Cost") +
    theme_mtg()
}

#' Token count by set bar chart (top N sets).
chart_tokens_by_set <- function(df, top_n = 25L) {
  df <- head(df, top_n)
  df$setCode <- factor(df$setCode, levels = rev(df$setCode))

  ggplot(df, aes(x = setCode, y = token_count)) +
    geom_col(fill = "#E6C34D", color = "#c9a830", linewidth = 0.3) +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(x = NULL, y = "Tokens", title = "Tokens by Set") +
    theme_mtg()
}

#' Token color distribution pie chart.
chart_token_colors <- function(all_tokens) {
  if (nrow(all_tokens) == 0) return(ggplot() + theme_void())

  buckets <- vapply(seq_len(nrow(all_tokens)), function(i) {
    classify_color(all_tokens$colors[[i]])
  }, character(1))

  df <- as.data.frame(table(color = buckets), stringsAsFactors = FALSE)
  names(df) <- c("color", "count")
  order <- c("W", "U", "B", "R", "G", "Multicolor", "Colorless")
  df <- df[df$color %in% order, ]
  df$color <- factor(df$color, levels = order)

  fills <- mtg_colors[as.character(df$color)]

  ggplot(df, aes(x = "", y = count, fill = color)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    scale_fill_manual(values = fills, labels = color_labels[as.character(df$color)]) +
    labs(fill = "Color", title = "Token Color Distribution") +
    theme_void(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
      legend.position = "right"
    )
}
