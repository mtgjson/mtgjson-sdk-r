# mtgjsonsdk

A high-performance, DuckDB-backed R query client for [MTGJSON](https://mtgjson.com).

Unlike traditional SDKs that rely on rate-limited REST APIs, `mtgjsonsdk` implements a local data warehouse architecture. It synchronizes optimized Parquet data from the MTGJSON CDN to your local machine, utilizing DuckDB to execute complex analytics, fuzzy searches, and booster simulations with sub-millisecond latency.

## Key Features

*   **Vectorized Execution**: Powered by DuckDB for high-speed OLAP queries on the full MTG dataset.
*   **Offline-First**: Data is cached locally, allowing for full functionality without an active internet connection.
*   **Fuzzy Search**: Built-in Jaro-Winkler similarity matching to handle typos and approximate name lookups.
*   **Native Data Frames**: All queries return standard R data.frames — ready for dplyr, ggplot2, or any tidyverse workflow.
*   **R6 Class System**: Clean, object-oriented API with lazy-loaded query interfaces and method chaining.
*   **Booster Simulation**: Accurate pack opening logic using official MTGJSON weights and sheet configurations.

## Install

```r
# install.packages("devtools")
devtools::install_local("path/to/mtgjson-sdk-r")
```

## Quick Start

```r
library(mtgjsonsdk)

sdk <- MtgjsonSDK$new()

# Search for cards (returns data.frames)
bolts <- sdk$cards$get_by_name("Lightning Bolt")
cat(sprintf("Found %d printings of Lightning Bolt\n", nrow(bolts)))

# Get set metadata
mh3 <- sdk$sets$get("MH3")
cat(sprintf("%s -- %s cards\n", mh3$name, mh3$totalSetSize))

# Check format legality
if (nrow(bolts) > 0) {
  cat(sprintf("Modern legal: %s\n", sdk$legalities$is_legal(bolts$uuid[1], "modern")))
}

# Find the cheapest printing
cheapest <- sdk$prices$cheapest_printing("Lightning Bolt")
if (!is.null(cheapest)) {
  cat(sprintf("Cheapest: $%s (%s)\n", cheapest$price, cheapest$setCode))
}

# Execute raw SQL with parameter binding
rows <- sdk$sql("SELECT name FROM cards WHERE manaValue = $1 LIMIT 5", list(0))

sdk$close()
```

## Architecture

By using DuckDB, the SDK leverages columnar storage and vectorized execution, making it significantly faster than SQLite or standard JSON parsing for MTG's relational dataset.

1.  **Synchronization**: On first use, the SDK lazily downloads Parquet and JSON files from the MTGJSON CDN to a platform-specific cache directory (`~/.cache/mtgjson-sdk` on Linux, `~/Library/Caches/mtgjson-sdk` on macOS, `AppData/Local/mtgjson-sdk` on Windows).
2.  **Virtual Schema**: DuckDB views are registered on-demand. Accessing `sdk$cards` registers the card view; accessing `sdk$prices` registers price data. You only pay the memory cost for the data you query.
3.  **Dynamic Adaptation**: The SDK introspects Parquet metadata to automatically handle schema changes, plural-column array conversion, and format legality unpivoting.
4.  **Materialization**: Queries return standard R data.frames, ready for use with dplyr, ggplot2, or base R.

## Use Cases

### Price Analytics

```r
sdk <- MtgjsonSDK$new()

# Find the cheapest printing of a card by name
cheapest <- sdk$prices$cheapest_printing("Ragavan, Nimble Pilferer")

# Aggregate statistics (min, max, avg) for a specific card
trend <- sdk$prices$price_trend(
  cheapest$uuid, provider = "tcgplayer", finish = "normal"
)
cat(sprintf("Range: $%s - $%s\n", trend$min_price, trend$max_price))
cat(sprintf("Average: $%s over %d data points\n", trend$avg_price, trend$data_points))

# Historical price lookup with date filtering
history <- sdk$prices$history(
  cheapest$uuid,
  provider = "tcgplayer",
  date_from = "2024-01-01",
  date_to = "2024-12-31"
)

# Top 10 most expensive printings across the entire dataset
priciest <- sdk$prices$most_expensive_printings(limit = 10)

sdk$close()
```

### Advanced Card Search

The `search()` method supports ~20 composable filters that can be combined freely:

```r
sdk <- MtgjsonSDK$new()

# Complex filters: Modern-legal red creatures with CMC <= 2
aggro <- sdk$cards$search(
  colors = "R",
  types = "Creature",
  mana_value_lte = 2,
  legal_in = "modern",
  limit = 50
)

# Typo-tolerant fuzzy search (Jaro-Winkler similarity)
results <- sdk$cards$search(fuzzy_name = "Ligtning Bolt")  # still finds it

# Rules text search using regular expressions
burn <- sdk$cards$search(text_regex = "deals? \\d+ damage to any target")

# Search by keyword ability across formats
flyers <- sdk$cards$search(keyword = "Flying", colors = c("W", "U"), legal_in = "standard")

# Find cards by foreign-language name
blitz <- sdk$cards$search(localized_name = "Blitzschlag")  # German for Lightning Bolt

sdk$close()
```

<details>
<summary>All <code>search()</code> parameters</summary>

| Parameter | Type | Description |
|---|---|---|
| `name` | `character` | Name pattern (`%` = wildcard) |
| `fuzzy_name` | `character` | Typo-tolerant Jaro-Winkler match |
| `localized_name` | `character` | Foreign-language name search |
| `colors` | `character` | Cards containing these colors |
| `color_identity` | `character` | Color identity filter |
| `legal_in` | `character` | Format legality |
| `rarity` | `character` | Rarity filter |
| `mana_value` | `numeric` | Exact mana value |
| `mana_value_lte` | `numeric` | Mana value upper bound |
| `mana_value_gte` | `numeric` | Mana value lower bound |
| `text` | `character` | Rules text substring |
| `text_regex` | `character` | Rules text regex |
| `types` | `character` | Type line search |
| `artist` | `character` | Artist name |
| `keyword` | `character` | Keyword ability |
| `is_promo` | `logical` | Promo status |
| `availability` | `character` | `"paper"` or `"mtgo"` |
| `language` | `character` | Language filter |
| `layout` | `character` | Card layout |
| `set_code` | `character` | Set code |
| `set_type` | `character` | Set type (joins sets table) |
| `power` | `character` | Power filter |
| `toughness` | `character` | Toughness filter |
| `limit` / `offset` | `integer` | Pagination |

</details>

### Collection & Cross-Reference

```r
sdk <- MtgjsonSDK$new()

# Cross-reference by any external ID system
cards <- sdk$identifiers$find_by_scryfall_id("f7a21fe4-...")
cards <- sdk$identifiers$find_by_tcgplayer_id("12345")
cards <- sdk$identifiers$find_by_mtgo_id("67890")

# Get all external identifiers for a card
all_ids <- sdk$identifiers$get_identifiers("card-uuid-here")
# -> Scryfall, TCGPlayer, MTGO, Arena, Cardmarket, Card Kingdom, Cardsphere, ...

# TCGPlayer SKU variants (foil, etched, etc.)
skus <- sdk$skus$get("card-uuid-here")

# Export to a standalone DuckDB file for offline analysis
sdk$export_db("my_collection.duckdb")
# Now query with: duckdb my_collection.duckdb "SELECT * FROM cards LIMIT 5"

sdk$close()
```

### Booster Simulation

```r
sdk <- MtgjsonSDK$new()

# See available booster types for a set
types <- sdk$booster$available_types("MH3")  # c("draft", "collector", ...)

# Open a single draft pack using official set weights
pack <- sdk$booster$open_pack("MH3", "draft")
for (i in seq_len(nrow(pack))) {
  cat(sprintf("  %s (%s)\n", pack$name[i], pack$rarity[i]))
}

# Simulate opening a full box (36 packs)
box <- sdk$booster$open_box("MH3", "draft", packs = 36L)
cat(sprintf("Opened %d packs, %d total cards\n",
    length(box), sum(vapply(box, nrow, integer(1)))))

sdk$close()
```

## API Reference

### Core Data

```r
# Cards
sdk$cards$get_by_uuid("uuid")                # single card lookup
sdk$cards$get_by_uuids(c("uuid1", "uuid2"))  # batch lookup
sdk$cards$get_by_name("Lightning Bolt")       # all printings of a name
sdk$cards$search(...)                         # composable filters (see above)
sdk$cards$get_printings("Lightning Bolt")     # all printings across sets
sdk$cards$get_atomic("Lightning Bolt")        # oracle data (no printing info)
sdk$cards$find_by_scryfall_id("...")          # cross-reference shortcut
sdk$cards$random(5)                           # random cards
sdk$cards$count()                             # total (or filtered with kwargs)

# Tokens
sdk$tokens$get_by_uuid("uuid")
sdk$tokens$get_by_name("Soldier Token")
sdk$tokens$search(name = "%Token", set_code = "MH3", colors = "W")
sdk$tokens$for_set("MH3")

# Sets
sdk$sets$get("MH3")
sdk$sets$list(set_type = "expansion")
sdk$sets$search(name = "Horizons", release_year = 2024)
```

### Playability

```r
# Legalities
sdk$legalities$formats_for_card("uuid")     # -> c(modern = "Legal", ...)
sdk$legalities$legal_in("modern")           # all modern-legal cards
sdk$legalities$is_legal("uuid", "modern")   # -> TRUE/FALSE
sdk$legalities$banned_in("modern")          # also: restricted_in, suspended_in

# Decks & Sealed Products
sdk$decks$list(set_code = "MH3")
sdk$decks$search(name = "Eldrazi")
sdk$sealed$list(set_code = "MH3")
sdk$sealed$get("uuid")
```

### Market & Identifiers

```r
# Prices
sdk$prices$get("uuid")                      # full nested price data
sdk$prices$today("uuid", provider = "tcgplayer", finish = "foil")
sdk$prices$history("uuid", provider = "tcgplayer", date_from = "2024-01-01")
sdk$prices$price_trend("uuid", provider = "tcgplayer", finish = "normal")
sdk$prices$cheapest_printing("Lightning Bolt")
sdk$prices$most_expensive_printings(limit = 10)

# Identifiers (supports all major external ID systems)
sdk$identifiers$find_by_scryfall_id("...")
sdk$identifiers$find_by_tcgplayer_id("...")
sdk$identifiers$find_by_mtgo_id("...")
sdk$identifiers$find_by_mtg_arena_id("...")
sdk$identifiers$find_by_multiverse_id("...")
sdk$identifiers$find_by_mcm_id("...")
sdk$identifiers$find_by_card_kingdom_id("...")
sdk$identifiers$find_by("scryfallId", "...")  # generic lookup
sdk$identifiers$get_identifiers("uuid")       # all IDs for a card

# SKUs
sdk$skus$get("uuid")
sdk$skus$find_by_sku_id(123456)
sdk$skus$find_by_product_id(789)
```

### Booster & Enums

```r
sdk$booster$available_types("MH3")
sdk$booster$open_pack("MH3", "draft")
sdk$booster$open_box("MH3", packs = 36L)
sdk$booster$sheet_contents("MH3", "draft", "common")

sdk$enums$keywords()
sdk$enums$card_types()
sdk$enums$enum_values()
```

### System

```r
sdk$meta                                    # version and build date
sdk$views                                   # registered view names
sdk$refresh()                               # check CDN for new data -> logical
sdk$export_db("output.duckdb")              # export to persistent DuckDB file
sdk$sql(query, params)                      # raw parameterized SQL
sdk$close()                                 # release resources
```

## Advanced Usage

### SqlBuilder

`SqlBuilder` is exported for constructing parameterized DuckDB queries with method chaining. All user values go through `$N` parameter binding — never string interpolation.

```r
library(mtgjsonsdk)

sdk <- MtgjsonSDK$new()

# Ensure views are registered before querying
sdk$cards$count()

q <- SqlBuilder$new("cards")$
  select("name", "setCode", "manaValue")$
  where_eq("rarity", "mythic")$
  where_gte("manaValue", "5")$
  where_like("name", "%Dragon%")$
  where_in("setCode", list("MH3", "LTR", "WOE"))$
  order_by("manaValue DESC", "name ASC")$
  limit(25)$
  build()

result <- sdk$sql(q$sql, q$params)

sdk$close()
```

### Auto-Refresh for Long-Running Services

```r
# In a scheduled task or health check:
if (sdk$refresh()) {
  message("New MTGJSON data detected -- cache refreshed")
}
```

### Custom Cache Directory & Progress

```r
on_progress <- function(filename, downloaded, total) {
  pct <- if (total > 0) downloaded / total * 100 else 0
  cat(sprintf("\r%s: %.1f%%", filename, pct))
}

sdk <- MtgjsonSDK$new(
  cache_dir = "/data/mtgjson-cache",
  timeout = 300,
  on_progress = on_progress
)
```

### Raw SQL

All user input goes through DuckDB parameter binding (`$1`, `$2`, ...):

```r
sdk <- MtgjsonSDK$new()

# Ensure views are registered before querying
sdk$cards$count()

# Parameterized queries
rows <- sdk$sql(
  "SELECT name, setCode, rarity FROM cards WHERE manaValue <= $1 AND rarity = $2",
  list(2, "mythic")
)

sdk$close()
```

## Examples

### Format Metagame Analyst (Shiny Dashboard)

An interactive Shiny dashboard for analyzing MTG format metagames — card pool sizes, ban lists, mana curves, color distributions, keyword frequency, creature P/T heatmaps, and token census across all major formats.

**SDK features showcased:** Legalities (`legal_in`, `banned_in`, `restricted_in`, `suspended_in`), SqlBuilder (custom aggregation queries), Enums (`keywords`, `card_types`), Tokens (`for_set`, `search`), Cards (`search` with keyword/type/mana value filters).

```bash
cd examples/format-analyst
Rscript install.R                    # install Shiny + ggplot2 + other deps
Rscript -e "shiny::runApp('.')"      # launch dashboard
```

| Tab | What it shows |
|-----|---------------|
| Overview | Legal card count, banned/restricted/suspended counts, format comparison bar chart, ban list table |
| Mana & Colors | Color distribution, mana curve histogram, average mana value by color |
| Keywords & Types | Top 20 keywords, card type donut chart, top creature subtypes, keyword reference panel |
| Power & Toughness | P/T heatmap, average P/T by mana cost, "stat monsters" efficiency table |
| Token Census | Tokens by set, most common tokens, token color distribution |

## Development

```bash
git clone https://github.com/mtgjson/mtgjson-sdk-r.git
cd mtgjson-sdk-r
Rscript -e "devtools::install_deps(dependencies = TRUE)"
Rscript -e "devtools::test()"
Rscript -e "devtools::check()"
```

## License

MIT
