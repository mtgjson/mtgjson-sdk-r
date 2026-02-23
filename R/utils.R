# Shared utility functions.

#' Normalize file path to forward slashes for DuckDB SQL.
#' @param path A file path string.
#' @return Path with forward slashes.
#' @noRd
forward_slashes <- function(path) {
  gsub("\\\\", "/", path)
}

#' Stop with a structured MTGJSON error.
#' @param message Error message.
#' @param class Optional error subclass.
#' @noRd
mtgjson_error <- function(message, class = NULL) {
  classes <- c(class, "mtgjson_error", "error", "condition")
  stop(structure(
    class = classes,
    list(message = message)
  ))
}
