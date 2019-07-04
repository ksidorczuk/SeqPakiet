library(testthat)
library(SeqPakiet)

read_test <- read_fasta_file(system.file(package = "SeqPakiet", "test.fa"))

test_that("Files are loaded correctly", {
  expect_equal(read_test,
               structure(list(structure(list(sequence = c("A", "A", "G", "C", "A", "H", "A", "G", "Y", "T", "M"),
                                name = "Test"), class = "seq")), class = "seq_list"))
  expect_s3_class(read_test, "seq_list")
})

test_that("K-mers are extracted properly", {
  expect_equal(extract_kmers(read_test, 3),
               structure(list(structure(c("AAG", "AGC", "GCA", "CAH", "AHA",
                                          "HAG", "AGY", "GYT", "YTM"), class = "kmers")), class = "kmers_list"))
  expect_equal(extract_kmers(read_test, 8),
               structure(list(structure(c("AAGCAHAG", "AGCAHAGY", "GCAHAGYT",
                                          "CAHAGYTM"), class = "kmers")), class = "kmers_list"))
  expect_s3_class(extract_kmers(read_test, 3), "kmers_list")
})
