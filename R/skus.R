#' TCGPlayer SKU query interface.
#'
#' @noRd
SkuQuery <- R6::R6Class("SkuQuery",
  public = list(
    initialize = function(conn) {
      private$.conn  <- conn
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
    .conn = NULL,

    ensure = function() {
      tryCatch(
        private$.conn$ensure_views("tcgplayer_skus"),
        error = function(e) {
          warning("SKU data not available: ", conditionMessage(e))
        })
    }
  )
)
