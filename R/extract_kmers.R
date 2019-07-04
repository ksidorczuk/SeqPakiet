#' Extract continuous k-mers from a sequence
#'
#' Extracts continuous k-mers of a given length from a single sequence
#' @description Extracts continuous k-mers of a given length from a single sequence in a FASTA format
#' @param seq Sequence from which k-mers will be extracted
#' @param n Lenght of k-mers
#' @export



extract_kmers <- function(seq, n) {

    ret <- lapply(seq, function(i) extract_kmers_single(i, n))
    class(ret) <- "kmers_list"
    ret
}

extract_kmers_single <- function(seq, n) {
  indices <- lapply(0:(length(seq[["sequence"]])-n), function (i) 1L:n + i)
  res <- lapply(indices, function(i) {
        subs <- seq[["sequence"]][i]
        paste(subs, collapse = "")
  })
  res <- unlist(res)
  class(res) <- "kmers"
  res
}

