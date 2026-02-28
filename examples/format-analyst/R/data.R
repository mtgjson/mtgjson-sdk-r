# SDK data-fetching functions for the Format Metagame Analyst.
# All functions take the initialized sdk object and format name.

#' Register DuckDB views that SqlBuilder queries depend on.
#' sdk$sql() bypasses the SDK's lazy view registration, so we trigger
#' it once by calling domain methods that internally call ensure_views().
warmup_views <- function(sdk) {
  sdk$cards$count()                            # registers "cards" view
  sdk$legalities$banned_in("standard", limit = 1L) # registers "card_legalities" + "cards"
  sdk$tokens$count()                           # registers "tokens" view
  invisible(NULL)
}

#' Count legal cards in a format via SqlBuilder aggregation.
count_legal <- function(sdk, fmt) {
  q <- SqlBuilder$new("card_legalities")$
    select("COUNT(DISTINCT uuid) AS cnt")$
    where_eq("format", fmt)$
    where_eq("status", "Legal")$
    build()
  result <- sdk$sql(q$sql, q$params)
  if (nrow(result) > 0) as.numeric(result$cnt[1]) else 0
}

#' Get banned cards for a format.
fetch_banned <- function(sdk, fmt) {
  sdk$legalities$banned_in(fmt, limit = 1000L)
}

#' Get restricted cards for a format.
fetch_restricted <- function(sdk, fmt) {
  sdk$legalities$restricted_in(fmt, limit = 1000L)
}

#' Get suspended cards for a format.
fetch_suspended <- function(sdk, fmt) {
  sdk$legalities$suspended_in(fmt, limit = 1000L)
}

#' Count legal cards per format for comparison chart.
count_all_formats <- function(sdk, formats) {
  counts <- vapply(formats, function(f) count_legal(sdk, f), numeric(1))
  data.frame(format = formats, count = counts, stringsAsFactors = FALSE)
}

#' Get color distribution via SqlBuilder.
#' Returns a data.frame with columns: color_bucket, count.
fetch_color_distribution <- function(sdk, fmt) {
  q <- SqlBuilder$new("cards")$
    select("cards.colors")$
    join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
    where_eq("cl.format", fmt)$
    where_eq("cl.status", "Legal")$
    build()
  df <- sdk$sql(q$sql, q$params)
  if (nrow(df) == 0) return(data.frame(color_bucket = character(), count = integer()))

  buckets <- vapply(seq_len(nrow(df)), function(i) {
    classify_color(df$colors[[i]])
  }, character(1))

  as.data.frame(table(color_bucket = buckets), stringsAsFactors = FALSE) |>
    stats::setNames(c("color_bucket", "count"))
}

#' Get mana curve data via SqlBuilder aggregation.
fetch_mana_curve <- function(sdk, fmt) {
  q <- SqlBuilder$new("cards")$
    select("cards.manaValue", "COUNT(*) AS cnt")$
    join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
    where_eq("cl.format", fmt)$
    where_eq("cl.status", "Legal")$
    where("cards.manaValue IS NOT NULL")$
    group_by("cards.manaValue")$
    order_by("cards.manaValue ASC")$
    build()
  df <- sdk$sql(q$sql, q$params)
  if (nrow(df) == 0) return(data.frame(bucket = character(), count = integer()))
  df$bucket <- mana_bucket(df$manaValue)
  stats::aggregate(cnt ~ bucket, data = df, FUN = sum)
}

#' Get average mana value by color via SqlBuilder.
fetch_avg_mv_by_color <- function(sdk, fmt) {
  q <- SqlBuilder$new("cards")$
    select("cards.colors", "cards.manaValue")$
    join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
    where_eq("cl.format", fmt)$
    where_eq("cl.status", "Legal")$
    where("cards.manaValue IS NOT NULL")$
    build()
  df <- sdk$sql(q$sql, q$params)
  if (nrow(df) == 0) return(data.frame(color = character(), avg_mv = numeric()))

  df$color_bucket <- vapply(seq_len(nrow(df)), function(i) {
    classify_color(df$colors[[i]])
  }, character(1))

  stats::aggregate(manaValue ~ color_bucket, data = df, FUN = mean) |>
    stats::setNames(c("color", "avg_mv"))
}

#' Get type breakdown via SqlBuilder for each main type.
fetch_type_breakdown <- function(sdk, fmt) {
  counts <- vapply(main_types, function(tp) {
    q <- SqlBuilder$new("cards")$
      select("COUNT(*) AS cnt")$
      join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
      where_eq("cl.format", fmt)$
      where_eq("cl.status", "Legal")$
      where_like("cards.type", paste0("%", tp, "%"))$
      build()
    result <- sdk$sql(q$sql, q$params)
    if (nrow(result) > 0) as.numeric(result$cnt[1]) else 0
  }, numeric(1))

  data.frame(type = main_types, count = counts, stringsAsFactors = FALSE)
}

#' Count cards for each keyword in the format.
#' Checks top keywords by searching with the SDK keyword filter.
fetch_keyword_counts <- function(sdk, fmt, top_n = 20L) {
  kw_data <- sdk$enums$keywords()
  all_kws <- unique(unlist(kw_data, use.names = FALSE))
  if (length(all_kws) == 0) return(data.frame(keyword = character(), count = integer()))

  # Use SqlBuilder to count keywords efficiently
  q <- SqlBuilder$new("cards")$
    select("cards.keywords")$
    join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
    where_eq("cl.format", fmt)$
    where_eq("cl.status", "Legal")$
    where("cards.keywords IS NOT NULL")$
    where("len(cards.keywords) > 0")$
    build()
  df <- sdk$sql(q$sql, q$params)
  if (nrow(df) == 0) return(data.frame(keyword = character(), count = integer()))

  # Tally keywords from the list column
  kw_vec <- unlist(df$keywords)
  kw_vec <- kw_vec[!is.na(kw_vec)]
  tbl <- sort(table(kw_vec), decreasing = TRUE)
  n <- min(top_n, length(tbl))
  data.frame(
    keyword = names(tbl)[seq_len(n)],
    count = as.integer(tbl[seq_len(n)]),
    stringsAsFactors = FALSE
  )
}

#' Fetch creature subtype frequency for the format.
fetch_subtype_counts <- function(sdk, fmt, top_n = 20L) {
  q <- SqlBuilder$new("cards")$
    select("cards.subtypes")$
    join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
    where_eq("cl.format", fmt)$
    where_eq("cl.status", "Legal")$
    where_like("cards.type", "%Creature%")$
    where("cards.subtypes IS NOT NULL")$
    where("len(cards.subtypes) > 0")$
    build()
  df <- sdk$sql(q$sql, q$params)
  if (nrow(df) == 0) return(data.frame(subtype = character(), count = integer()))

  st_vec <- unlist(df$subtypes)
  st_vec <- st_vec[!is.na(st_vec)]
  tbl <- sort(table(st_vec), decreasing = TRUE)
  n <- min(top_n, length(tbl))
  data.frame(
    subtype = names(tbl)[seq_len(n)],
    count = as.integer(tbl[seq_len(n)]),
    stringsAsFactors = FALSE
  )
}

#' Fetch all creatures in the format for P/T analysis.
fetch_creatures <- function(sdk, fmt) {
  q <- SqlBuilder$new("cards")$
    select("cards.name", "cards.power", "cards.toughness",
           "cards.manaValue", "cards.type")$
    join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
    where_eq("cl.format", fmt)$
    where_eq("cl.status", "Legal")$
    where_like("cards.type", "%Creature%")$
    where("cards.power IS NOT NULL")$
    where("cards.toughness IS NOT NULL")$
    build()
  df <- sdk$sql(q$sql, q$params)
  if (nrow(df) == 0) return(df)
  df$p <- safe_numeric(df$power)
  df$t <- safe_numeric(df$toughness)
  df$mv <- safe_numeric(df$manaValue)
  df[!is.na(df$p) & !is.na(df$t), ]
}

#' Fetch token data for sets in the format.
#' Returns a list: tokens_by_set (data.frame) and all_tokens (data.frame).
fetch_token_data <- function(sdk, fmt) {
  # Get set codes that have legal cards in this format
  q <- SqlBuilder$new("cards")$
    select("DISTINCT cards.setCode")$
    join("JOIN card_legalities cl ON cards.uuid = cl.uuid")$
    where_eq("cl.format", fmt)$
    where_eq("cl.status", "Legal")$
    build()
  sets_df <- sdk$sql(q$sql, q$params)
  if (nrow(sets_df) == 0) {
    return(list(
      by_set = data.frame(setCode = character(), token_count = integer()),
      all_tokens = data.frame()
    ))
  }

  set_codes <- sets_df$setCode
  all_tokens <- list()
  by_set <- data.frame(setCode = character(), token_count = integer(),
                       stringsAsFactors = FALSE)

  for (code in set_codes) {
    toks <- tryCatch(sdk$tokens$for_set(code), error = function(e) data.frame())
    if (nrow(toks) > 0) {
      by_set <- rbind(by_set, data.frame(
        setCode = code, token_count = nrow(toks), stringsAsFactors = FALSE
      ))
      all_tokens[[length(all_tokens) + 1]] <- toks
    }
  }

  combined <- if (length(all_tokens) > 0) do.call(rbind, all_tokens) else data.frame()
  by_set <- by_set[order(by_set$token_count, decreasing = TRUE), ]

  list(by_set = by_set, all_tokens = combined)
}
