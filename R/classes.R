#' Class for amino acid sequences
#'
#' Class for storing the sequence
#' @description Stores the sequence
#' @param x sequence
#' @param name name of a sequence
#' @param ... additional arguments
#' @export
#'
new_seq <- function(x, name, ...) {
  x <- toupper(x)
  x <- unlist(strsplit(x, ""))
  lev <- c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L",
            "M", "N", "P", "Q", "R", "S", "T", "W", "V", "Y", "U")
  if (!all(x %in% lev)) {
    stop("Sequence contains non-standard character!")
  }
  if (length(x) < 2) {
    stop("Sequence too short!")
  }
  y <- list(sequence = x, name = name)
  class(y) <- "seq"
  y
}


