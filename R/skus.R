#' TCGPlayer SKU query interface.
#'
#' @noRd
SkuQuery <- R6::R6Class("SkuQuery",
  public = list(
    initialize = function(conn, cache) {
      private$.conn  <- conn
      private$.cache <- cache
      private$.loaded <- FALSE
    },

    #' @description Get all TCGPlayer SKUs for a card UUID.
    get = function(uuid) {
      private$ensure()
      if (!("tcgplayer_skus" %in% private$.conn$registered_views))
        return(data.frame())
      private$.conn$execute(
        "SELECT * FROM tcgplayer_skus WHERE uuid = $1",
        params = list(uuid))
    },

    #' @description Find a SKU by its TCGPlayer SKU ID.
    find_by_sku_id = function(sku_id) {
      private$ensure()
      if (!("tcgplayer_skus" %in% private$.conn$registered_views)) return(NULL)
      df <- private$.conn$execute(
        "SELECT * FROM tcgplayer_skus WHERE skuId = $1",
        params = list(sku_id))
      if (nrow(df) == 0) return(NULL)
      as.list(df[1, ])
    },

    #' @description Find all SKUs for a TCGPlayer product ID.
    find_by_product_id = function(product_id) {
      private$ensure()
      if (!("tcgplayer_skus" %in% private$.conn$registered_views))
        return(data.frame())
      private$.conn$execute(
        "SELECT * FROM tcgplayer_skus WHERE productId = $1",
        params = list(product_id))
    }
  ),

  private = list(
    .conn   = NULL,
    .cache  = NULL,
    .loaded = FALSE,

    ensure = function() {
      if (private$.loaded) return(invisible(NULL))
      if ("tcgplayer_skus" %in% private$.conn$registered_views) {
        private$.loaded <- TRUE
        return(invisible(NULL))
      }
      path <- tryCatch(
        private$.cache$ensure_json("tcgplayer_skus"),
        error = function(e) {
          warning("SKU data not available: ", conditionMessage(e))
          NULL
        })
      if (!is.null(path)) load_skus_to_duckdb(path, private$.conn)
      private$.loaded <- TRUE
    }
  )
)

#' Parse TcgplayerSkus JSON, stream-flatten to NDJSON, load into DuckDB.
#' @noRd
load_skus_to_duckdb <- function(path, conn) {
  if (grepl("\\.gz$", path)) {
    tmp_json <- tempfile(fileext = ".json")
    gz_in <- gzfile(path, "rb")
    out_f <- file(tmp_json, "wb")
    repeat {
      chunk <- readBin(gz_in, "raw", n = 1048576L)
      if (length(chunk) == 0L) break
      writeBin(chunk, out_f)
    }
    close(gz_in)
    close(out_f)
    raw <- jsonlite::fromJSON(tmp_json, simplifyVector = FALSE, simplifyDataFrame = FALSE)
    unlink(tmp_json)
  } else {
    raw <- jsonlite::fromJSON(path, simplifyVector = FALSE, simplifyDataFrame = FALSE)
  }

  data <- raw[["data"]]
  if (is.null(data)) return(invisible(NULL))
  rm(raw)

  tmp <- tempfile(fileext = ".ndjson")
  on.exit(unlink(tmp), add = TRUE)

  tmp_con <- file(tmp, "w")
  count <- 0L

  for (uuid in names(data)) {
    skus <- data[[uuid]]
    if (!is.list(skus)) next
    for (sku in skus) {
      if (is.list(sku)) {
        row <- sku
        row[["uuid"]] <- uuid
        writeLines(jsonlite::toJSON(row, auto_unbox = TRUE), tmp_con)
        count <- count + 1L
      }
    }
  }
  close(tmp_con)

  if (count > 0) {
    conn$register_table_from_ndjson("tcgplayer_skus", tmp)
  }
  unlink(tmp)
}
