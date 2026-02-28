# Format Metagame Analyst — Shiny Dashboard
#
# An interactive dashboard for analyzing MTG format metagames using the
# mtgjsonsdk R package. Showcases Enums/Keywords, Tokens, SqlBuilder,
# and Legalities — SDK domains untouched by the TypeScript, Python, and
# Rust example apps.
#
# Usage:
#   shiny::runApp(".")
#
# Dependencies: shiny, bslib, ggplot2, dplyr, DT, scales, mtgjsonsdk

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)
library(mtgjsonsdk)

source("R/utils.R")
source("R/data.R")
source("R/charts.R")

# ── UI ──────────────────────────────────────────────────────────────────

ui <- page_sidebar(
  title = "Format Metagame Analyst",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0E68AB"
  ),

  sidebar = sidebar(
    width = 260,
    selectInput("format", "Format",
                choices = format_choices,
                selected = "modern"),
    actionButton("analyze", "Analyze", class = "btn-primary w-100"),
    hr(),
    uiOutput("meta_label")
  ),

  navset_card_tab(
    id = "main_tabs",

    # ── Tab 1: Format Overview ──────────────────────────────────────────
    nav_panel("Overview",
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        value_box("Legal Cards", textOutput("legal_count"),
                  showcase = icon("layer-group"), theme = "primary"),
        value_box("Banned", textOutput("banned_count"),
                  showcase = icon("ban"), theme = "danger"),
        value_box("Restricted", textOutput("restricted_count"),
                  showcase = icon("lock"), theme = "warning"),
        value_box("Suspended", textOutput("suspended_count"),
                  showcase = icon("pause"), theme = "secondary")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Format Comparison"),
          plotOutput("format_comparison", height = "450px")
        ),
        card(
          card_header("Ban / Restrict List"),
          DTOutput("ban_table")
        )
      )
    ),

    # ── Tab 2: Mana & Colors ───────────────────────────────────────────
    nav_panel("Mana & Colors",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Color Distribution"),
          plotOutput("color_dist", height = "380px")
        ),
        card(
          card_header("Mana Curve"),
          plotOutput("mana_curve", height = "380px")
        )
      ),
      layout_columns(
        col_widths = 12,
        card(
          card_header("Average Mana Value by Color"),
          plotOutput("avg_mv", height = "350px")
        )
      )
    ),

    # ── Tab 3: Keywords & Types ────────────────────────────────────────
    nav_panel("Keywords & Types",
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Top 20 Keywords"),
          plotOutput("keywords_chart", height = "500px")
        ),
        card(
          card_header("Card Type Breakdown"),
          plotOutput("type_donut", height = "500px")
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          card_header("Top 20 Creature Subtypes"),
          plotOutput("subtypes_chart", height = "500px")
        ),
        card(
          card_header("Keyword Reference"),
          uiOutput("keyword_reference")
        )
      )
    ),

    # ── Tab 4: Power & Toughness ───────────────────────────────────────
    nav_panel("Power & Toughness",
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Power / Toughness Heatmap"),
          plotOutput("pt_heatmap", height = "500px")
        ),
        card(
          card_header("Average P/T by Mana Cost"),
          plotOutput("avg_pt", height = "500px")
        )
      ),
      card(
        card_header("Stat Monsters — Best P/T Efficiency"),
        DTOutput("stat_monsters")
      )
    ),

    # ── Tab 5: Token Census ────────────────────────────────────────────
    nav_panel("Token Census",
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Tokens by Set"),
          plotOutput("tokens_by_set", height = "500px")
        ),
        card(
          card_header("Token Color Distribution"),
          plotOutput("token_colors", height = "500px")
        )
      ),
      card(
        card_header("Most Common Tokens"),
        DTOutput("common_tokens")
      )
    )
  )
)

# ── Server ──────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  # Initialize SDK once per session
  sdk <- MtgjsonSDK$new()
  warmup_views(sdk)
  onSessionEnded(function() sdk$close())

  # Show dataset version
  output$meta_label <- renderUI({
    m <- sdk$meta
    version <- if (!is.null(m$version)) m$version else "unknown"
    date <- if (!is.null(m$date)) m$date else ""
    tags$small(class = "text-muted",
               paste0("MTGJSON v", version),
               if (nzchar(date)) paste0(" (", date, ")") else "")
  })

  # Cache per format
  cache <- reactiveValues()

  # Main data loading triggered by Analyze button
  format_data <- eventReactive(input$analyze, {
    fmt <- input$format

    # Return cache hit
    if (!is.null(cache[[fmt]])) return(cache[[fmt]])

    withProgress(message = paste("Loading", fmt, "data..."), value = 0, {
      incProgress(0.1, detail = "Counting legal cards")
      legal_n <- count_legal(sdk, fmt)

      incProgress(0.1, detail = "Fetching ban list")
      banned <- fetch_banned(sdk, fmt)
      restricted <- fetch_restricted(sdk, fmt)
      suspended <- fetch_suspended(sdk, fmt)

      incProgress(0.1, detail = "Comparing formats")
      fmt_comparison <- count_all_formats(sdk, format_choices)

      incProgress(0.1, detail = "Analyzing colors")
      color_dist <- fetch_color_distribution(sdk, fmt)
      mana_curve <- fetch_mana_curve(sdk, fmt)
      avg_mv <- fetch_avg_mv_by_color(sdk, fmt)

      incProgress(0.1, detail = "Counting keywords")
      kw_counts <- fetch_keyword_counts(sdk, fmt)

      incProgress(0.1, detail = "Analyzing types")
      type_breakdown <- fetch_type_breakdown(sdk, fmt)
      subtype_counts <- fetch_subtype_counts(sdk, fmt)

      incProgress(0.1, detail = "Fetching creatures")
      creatures <- fetch_creatures(sdk, fmt)

      incProgress(0.1, detail = "Surveying tokens")
      token_data <- fetch_token_data(sdk, fmt)

      incProgress(0.1, detail = "Done")
    })

    result <- list(
      legal_n = legal_n,
      banned = banned,
      restricted = restricted,
      suspended = suspended,
      fmt_comparison = fmt_comparison,
      color_dist = color_dist,
      mana_curve = mana_curve,
      avg_mv = avg_mv,
      kw_counts = kw_counts,
      type_breakdown = type_breakdown,
      subtype_counts = subtype_counts,
      creatures = creatures,
      token_data = token_data
    )

    cache[[fmt]] <- result
    result
  }, ignoreNULL = FALSE)

  # ── Tab 1: Overview outputs ───────────────────────────────────────────
  output$legal_count <- renderText({
    comma(format_data()$legal_n)
  })

  output$banned_count <- renderText({
    nrow(format_data()$banned)
  })

  output$restricted_count <- renderText({
    nrow(format_data()$restricted)
  })

  output$suspended_count <- renderText({
    nrow(format_data()$suspended)
  })

  output$format_comparison <- renderPlot({
    chart_format_comparison(format_data()$fmt_comparison, input$format)
  })

  output$ban_table <- renderDT({
    banned <- format_data()$banned
    restricted <- format_data()$restricted
    suspended <- format_data()$suspended

    parts <- list()
    if (nrow(banned) > 0) {
      banned$status <- "Banned"
      parts[[length(parts) + 1]] <- banned
    }
    if (nrow(restricted) > 0) {
      restricted$status <- "Restricted"
      parts[[length(parts) + 1]] <- restricted
    }
    if (nrow(suspended) > 0) {
      suspended$status <- "Suspended"
      parts[[length(parts) + 1]] <- suspended
    }

    if (length(parts) == 0) {
      return(datatable(data.frame(
        Name = character(), Status = character()
      ), options = list(pageLength = 10)))
    }

    df <- do.call(rbind, parts)
    cols <- intersect(c("name", "status"), names(df))
    datatable(df[, cols, drop = FALSE],
              colnames = c("Card Name" = "name", "Status" = "status"),
              options = list(pageLength = 15, dom = "ftp"),
              rownames = FALSE)
  })

  # ── Tab 2: Mana & Colors outputs ─────────────────────────────────────
  output$color_dist <- renderPlot({
    chart_color_dist(format_data()$color_dist)
  })

  output$mana_curve <- renderPlot({
    chart_mana_curve(format_data()$mana_curve)
  })

  output$avg_mv <- renderPlot({
    chart_avg_mv(format_data()$avg_mv)
  })

  # ── Tab 3: Keywords & Types outputs ──────────────────────────────────
  output$keywords_chart <- renderPlot({
    chart_keywords(format_data()$kw_counts)
  })

  output$type_donut <- renderPlot({
    chart_type_donut(format_data()$type_breakdown)
  })

  output$subtypes_chart <- renderPlot({
    chart_subtypes(format_data()$subtype_counts)
  })

  output$keyword_reference <- renderUI({
    kw_data <- sdk$enums$keywords()
    if (length(kw_data) == 0) return(tags$p("No keyword data available."))

    panels <- lapply(names(kw_data), function(category) {
      words <- sort(unlist(kw_data[[category]]))
      tags$details(
        tags$summary(tags$strong(category),
                     tags$span(class = "badge bg-secondary ms-2",
                               length(words))),
        tags$p(class = "mt-2 text-muted small",
               paste(words, collapse = ", "))
      )
    })
    do.call(tagList, panels)
  })

  # ── Tab 4: Power & Toughness outputs ─────────────────────────────────
  output$pt_heatmap <- renderPlot({
    creatures <- format_data()$creatures
    validate(need(nrow(creatures) > 0, "No creature data for this format."))
    chart_pt_heatmap(creatures)
  })

  output$avg_pt <- renderPlot({
    creatures <- format_data()$creatures
    validate(need(nrow(creatures) > 0, "No creature data for this format."))
    chart_avg_pt_by_mv(creatures)
  })

  output$stat_monsters <- renderDT({
    creatures <- format_data()$creatures
    validate(need(nrow(creatures) > 0, "No creature data for this format."))

    df <- creatures |>
      filter(!is.na(mv) & mv > 0) |>
      mutate(efficiency = (p + t) / mv) |>
      arrange(desc(efficiency)) |>
      select(name, power, toughness, manaValue, efficiency) |>
      head(50)

    df$efficiency <- round(df$efficiency, 2)

    datatable(df,
              colnames = c("Name" = "name", "Power" = "power",
                           "Toughness" = "toughness", "MV" = "manaValue",
                           "Efficiency" = "efficiency"),
              options = list(pageLength = 15, dom = "ftp"),
              rownames = FALSE)
  })

  # ── Tab 5: Token Census outputs ──────────────────────────────────────
  output$tokens_by_set <- renderPlot({
    td <- format_data()$token_data
    validate(need(nrow(td$by_set) > 0, "No token data for this format."))
    chart_tokens_by_set(td$by_set)
  })

  output$token_colors <- renderPlot({
    td <- format_data()$token_data
    validate(need(nrow(td$all_tokens) > 0, "No token data for this format."))
    chart_token_colors(td$all_tokens)
  })

  output$common_tokens <- renderDT({
    td <- format_data()$token_data
    validate(need(nrow(td$all_tokens) > 0, "No token data for this format."))

    tokens <- td$all_tokens
    cols <- intersect(c("name", "power", "toughness", "type"), names(tokens))
    if (length(cols) == 0) return(datatable(data.frame()))

    # Count how many sets each token appears in
    token_sets <- tokens |>
      group_by(name) |>
      summarise(sets = n_distinct(setCode), .groups = "drop") |>
      arrange(desc(sets))

    # Get representative info per token
    token_info <- tokens |>
      group_by(name) |>
      slice(1) |>
      ungroup()

    result <- token_sets |>
      left_join(token_info |> select(any_of(c("name", "power", "toughness", "type"))),
                by = "name") |>
      head(50)

    datatable(result,
              options = list(pageLength = 15, dom = "ftp"),
              rownames = FALSE)
  })
}

shinyApp(ui, server)
