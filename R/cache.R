#' Version-aware CDN download and local file cache manager.
#'
#' @noRd
CacheManager <- R6::R6Class("CacheManager",
  public = list(
    #' @field cache_dir Path to cache directory.
    cache_dir = NULL,

    #' @description Create a cache manager.
    #' @param cache_dir Directory for cached data files. NULL for default.
    #' @param offline If TRUE, never download from CDN.
    #' @param timeout HTTP request timeout in seconds.
    #' @param on_progress Optional callback function(filename, downloaded, total).
    initialize = function(cache_dir = NULL, offline = FALSE, timeout = 120,
                          on_progress = NULL) {
      self$cache_dir <- if (is.null(cache_dir)) default_cache_dir() else cache_dir
      dir.create(self$cache_dir, recursive = TRUE, showWarnings = FALSE)
      private$.offline     <- offline
      private$.timeout     <- timeout
      private$.on_progress <- on_progress
      private$.remote_ver  <- NULL
    },

    #' @description Close the cache manager (no-op, httr2 is stateless).
    close = function() {
      invisible(NULL)
    },

    #' @description Fetch the current MTGJSON version from Meta.json.
    #' @return Version string or NULL.
    remote_version = function() {
      if (!is.null(private$.remote_ver)) return(private$.remote_ver)
      if (private$.offline) return(NULL)
      tryCatch({
        resp <- httr2::request(META_URL) |>
          httr2::req_timeout(private$.timeout) |>
          httr2::req_perform()
        data <- httr2::resp_body_json(resp)
        ver <- data[["data"]][["version"]]
        if (is.null(ver)) ver <- data[["meta"]][["version"]]
        private$.remote_ver <- ver
        ver
      }, error = function(e) {
        warning("Failed to fetch MTGJSON version from CDN: ", conditionMessage(e))
        NULL
      })
    },

    #' @description Check if local cache is out of date.
    #' @return TRUE if stale, FALSE if up to date.
    is_stale = function() {
      local_ver <- private$local_version()
      if (is.null(local_ver)) return(TRUE)
      remote_ver <- self$remote_version()
      if (is.null(remote_ver)) return(FALSE)
      local_ver != remote_ver
    },

    #' @description Ensure a parquet file is cached locally.
    #' @param view_name Logical view name (e.g. "cards").
    #' @return Local filesystem path to the cached parquet file.
    ensure_parquet = function(view_name) {
      filename   <- PARQUET_FILES[[view_name]]
      if (is.null(filename)) stop(sprintf("Unknown parquet view: '%s'", view_name))
      local_path <- file.path(self$cache_dir, filename)
      if (!file.exists(local_path) || self$is_stale()) {
        if (private$.offline) {
          if (file.exists(local_path)) return(local_path)
          stop(sprintf(
            "Parquet file '%s' not cached and offline mode is enabled", filename))
        }
        private$download_file(filename, local_path)
        ver <- self$remote_version()
        if (!is.null(ver)) private$save_version(ver)
      }
      local_path
    },

    #' @description Ensure a JSON file is cached locally.
    #' @param name Logical file name (e.g. "meta").
    #' @return Local filesystem path.
    ensure_json = function(name) {
      filename   <- JSON_FILES[[name]]
      if (is.null(filename)) stop(sprintf("Unknown JSON file: '%s'", name))
      local_path <- file.path(self$cache_dir, filename)
      if (!file.exists(local_path) || self$is_stale()) {
        if (private$.offline) {
          if (file.exists(local_path)) return(local_path)
          stop(sprintf(
            "JSON file '%s' not cached and offline mode is enabled", filename))
        }
        private$download_file(filename, local_path)
        ver <- self$remote_version()
        if (!is.null(ver)) private$save_version(ver)
      }
      local_path
    },

    #' @description Load and parse a JSON file (handles .gz transparently).
    #' @param name Logical file name.
    #' @return Parsed JSON as a list.
    load_json = function(name) {
      path <- self$ensure_json(name)
      tryCatch({
        if (grepl("\\.gz$", path)) {
          con <- gzfile(path, "rt", encoding = "UTF-8")
          on.exit(close(con))
          jsonlite::fromJSON(con, simplifyVector = TRUE, simplifyDataFrame = FALSE)
        } else {
          jsonlite::fromJSON(path, simplifyVector = TRUE, simplifyDataFrame = FALSE)
        }
      }, error = function(e) {
        warning(sprintf("Corrupt cache file %s: %s -- removing", basename(path),
                        conditionMessage(e)))
        unlink(path)
        stop(sprintf(
          "Cache file '%s' was corrupt and has been removed. Retry to re-download.",
          basename(path)))
      })
    },

    #' @description Remove all cached files and recreate the cache directory.
    clear = function() {
      if (dir.exists(self$cache_dir)) {
        unlink(self$cache_dir, recursive = TRUE)
        dir.create(self$cache_dir, recursive = TRUE, showWarnings = FALSE)
      }
    }
  ),

  private = list(
    .offline     = FALSE,
    .timeout     = 120,
    .on_progress = NULL,
    .remote_ver  = NULL,

    local_version = function() {
      vf <- file.path(self$cache_dir, "version.txt")
      if (file.exists(vf)) trimws(readLines(vf, n = 1, warn = FALSE))
      else NULL
    },

    save_version = function(version) {
      writeLines(version, file.path(self$cache_dir, "version.txt"))
    },

    download_file = function(filename, dest) {
      url <- paste0(CDN_BASE, "/", filename)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      tmp_dest <- paste0(dest, ".tmp")
      tryCatch({
        req <- httr2::request(url) |>
          httr2::req_timeout(private$.timeout)

        if (!is.null(private$.on_progress)) {
          req <- req |> httr2::req_progress()
        }

        httr2::req_perform(req, path = tmp_dest)
        file.rename(tmp_dest, dest)
      }, error = function(e) {
        unlink(tmp_dest)
        stop(e)
      })
    }
  )
)
