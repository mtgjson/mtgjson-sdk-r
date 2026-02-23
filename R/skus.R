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

#' Decompress .gz to a temp file and return the path; returns input path if not gz.
#' Caller must unlink the temp file when done.
#' @noRd
decompress_gz <- function(path) {
  if (!grepl("\\.gz$", path)) return(list(path = path, tmp = NULL))
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
  list(path = tmp_json, tmp = tmp_json)
}

#' Load TcgplayerSkus JSON into DuckDB using DuckDB's native JSON reader.
#' @noRd
load_skus_to_duckdb <- function(path, conn) {
  dec <- decompress_gz(path)
  on.exit(if (!is.null(dec$tmp)) unlink(dec$tmp), add = TRUE)
  path_fwd <- forward_slashes(dec$path)

  raw_conn <- conn$raw()
  tryCatch({
    DBI::dbExecute(raw_conn, "DROP TABLE IF EXISTS tcgplayer_skus")
    DBI::dbExecute(raw_conn, sprintf("
      CREATE TABLE tcgplayer_skus AS
      WITH entries AS (
        SELECT unnest(map_entries(data)) AS entry
        FROM read_json('%s', auto_detect=true, maximum_object_size=1073741824)
      ),
      flattened AS (
        SELECT entry.key AS uuid, unnest(entry.value) AS sku FROM entries
      )
      SELECT uuid, sku.* FROM flattened
    ", path_fwd))
    conn$registered_views <- c(conn$registered_views, "tcgplayer_skus")
  }, error = function(e) {
    warning("Failed to load SKU data: ", conditionMessage(e))
  })
}
