#' Print function for seq class
#'
#' Prints the sequence
#' @description Prints the sequence
#' @param x sequence
#' @param ... additional arguments
#' @export

print.nuc <- function(x, ...) {
  print(paste(x, collapse = ""))
}


#' Summary function for seq class
#'
#' Prints the composition of a sequence
#' @description Prints the composition of a sequence
#' @param object sequence
#' @param ... additional arguments
#' @export
summary.seq <- function(object, ...) {
    factor(object[["sequence"]], levels = c("A", "C", "D", "E", "F", "G", "H", "I", "K", "L",
                         "M", "N", "P", "Q", "R", "S", "T", "W", "V", "Y", "U"))
    comp <- table(object[["sequence"]], dnn = paste(object[["name"]], "composition [%]:"))
    comp/sum(comp)*100
}


#' Summary function for list of sequences of seq class
#'
#' Prints the composition of sequences
#' @description Prints the composition of a sequences
#' @param object sequences
#' @param ... additional arguments
#' @export
summary.seq_list <- function(object, ...) {
  lapply(object, function(i) summary(i))}


#' Summary function for list of kmers
#'
#' Prints the composition of sequences
#' @description Prints the composition of a sequences
#' @param object sequences
#' @param ... additional arguments
#' @export
summary.kmers_list <- function(object, ...) {
  lapply(object, function(i) {
    table(i, dnn = "Kmer counts")
  })
}
