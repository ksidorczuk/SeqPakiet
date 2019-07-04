#' Read fasta file
#'
#' Function that reads fasta files and stores sequences in a named list
#' @description Reads in fasta files and stores sequences
#' @param file File containing sequences in a FASTA format
#' @export
#' @examples
#' more_seq <- read_fasta_file("/home/kasia/RProjects/SeqPakiet/inst/more_seq.fa")
#' seq <- read_fasta_file("/home/kasia/RProjects/SeqPakiet/inst/seq.fa")

read_fasta_file <- function(file) {
  fasta <- readLines(file)
  names_ind <- grep("^>", fasta)
  names_ind <- c(names_ind, length(fasta))

  names <- grep("^>", fasta, value = TRUE)
  names <- gsub(">", "", names)

  seqs <- lapply(2:(length(names_ind)), function(i){
    paste(fasta[(names_ind[i-1]+1):(names_ind[i]-1)], collapse = "")
  })

 ret <- lapply(1:length(names), function(i) {
   new_seq(seqs[i], names[i])
   })

 class(ret) <- "seq_list"
 ret
}

