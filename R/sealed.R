#' Sealed product query interface.
#'
#' @noRd
SealedQuery <- R6::R6Class("SealedQuery",
  public = list(
    initialize = function(conn) {
      private$.conn <- conn
    },

    #' @description List sealed products.
    list = function(set_code = NULL, category = NULL, limit = 100L) {
      private$ensure()
      df <- tryCatch({
        q <- SqlBuilder$new("sets")
        q$select("code", "name AS setName", "sealedProduct")
        if (!is.null(set_code)) q$where_eq("code", toupper(set_code))
        q$limit(limit)
        built <- q$build()
        private$.conn$execute(built$sql, built$params)
      }, error = function(e) {
        data.frame()
      })

      if (nrow(df) == 0) return(data.frame())

      products <- list()
      for (i in seq_len(nrow(df))) {
        sealed <- df$sealedProduct[[i]]
        if (is.list(sealed)) {
          for (sp in sealed) {
            if (is.list(sp)) {
              if (!is.null(category) && (sp[["category"]] %||% "") != category) next
              sp[["setCode"]] <- df$code[i]
              products <- c(products, list(sp))
            }
          }
        }
      }

      if (length(products) == 0) return(data.frame())
      # Convert to data.frame best-effort
      tryCatch(
        do.call(rbind, lapply(products, function(p) {
          as.data.frame(p[intersect(names(p),
            c("uuid", "name", "category", "subtype", "setCode"))],
            stringsAsFactors = FALSE)
        })),
        error = function(e) data.frame()
      )
    },

    #' @description Get a sealed product by UUID.
    get = function(uuid) {
      private$ensure()
      tryCatch({
        sql <- paste(
          "SELECT sub.code AS setCode, sub.sp",
          "FROM (SELECT code, UNNEST(sealedProduct) AS sp",
          "FROM sets WHERE sealedProduct IS NOT NULL) sub",
          "WHERE sub.sp.uuid = $1 LIMIT 1")
        df <- private$.conn$execute(sql, params = list(uuid))
        if (nrow(df) == 0) return(NULL)
        row <- as.list(df[1, ])
        product <- row$sp
        if (is.list(product)) {
          product[["setCode"]] <- row$setCode
          return(product)
        }
        NULL
      }, error = function(e) NULL)
    }
  ),

  private = list(
    .conn = NULL,
    ensure = function() {
      private$.conn$ensure_views("sets")
    }
  )
)
