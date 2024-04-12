library(testthat)
cand_corpus <- list(c(1,2,3), c(1,2))
ref_corpus <- list(list(c(1,2,3), c(2,3,4)), list(c(1,2,6), c(781, 21, 9), c(7, 3)))


test_that("Expect errors for wrong arguments", {
  testthat::expect_error(bleu_sentence_ids('as', 'as', 'as'))
  testthat::expect_error(bleu_sentence_ids(-a, TRUE, 0.5))
  testthat::expect_error(bleu_sentence_ids(list(list(1,2,3), list(2,3,4)), list(1,2,3), weights=list(0.33, 0.33, 0.33)))
  testthat::expect_error(bleu_sentence_ids(list(c(1,2,3), c(2,3,4)), c(1,2,3), weights=list(0.33, 0.33, 0.33)))
  testthat::expect_error(bleu_sentence_ids(list(list(1,2,3), list(2,3,4)), list(1,2,3), weights=c(0.33, 0.33, 0.33)))
  testthat::expect_error(bleu_sentence_ids(NULL, NULL, weights=NULL))
  testthat::expect_error(bleu_sentence_ids(list(c(1,2,3), c(2,3,4)), c(1,2,3), n=-1))
  testthat::expect_error(bleu_sentence_ids(list(c(1,2,3), c(2,3,4)), c(1,2,3), n=0.5))

  testthat::expect_error(bleu_corpus_ids('as', 'as', 'as'))
  testthat::expect_error(bleu_corpus_ids(-a, TRUE, 0.5))
  testthat::expect_error(bleu_corpus_ids(list(list(list(1,2,3), list(2,3,4))), list(list(1,2,3)), weights=list(0.33, 0.33, 0.33)))
  testthat::expect_error(bleu_corpus_ids(list(list(c(1,2,3), c(2,3,4))), list(c(1,2,3)), weights=list(0.33, 0.33, 0.33)))
  testthat::expect_error(bleu_corpus_ids(list(list(list(1,2,3), list(2,3,4))), list(list(1,2,3)), weights=c(0.33, 0.33, 0.33)))
  testthat::expect_error(bleu_corpus_ids(NULL, NULL, weights=NULL))
  testthat::expect_error(bleu_corpus_ids(list(list(c(1,2,3), c(2,3,4))), list(c(1,2,3)), n=-1))
  testthat::expect_error(bleu_corpus_ids(list(list(c(1,2,3), c(2,3,4))), list(c(1,2,3)), n=0.5))
})

test_that("Expect number for `bleu_sentence_ids`", {
  testthat::expect_vector(bleu_sentence_ids(list(c(1,2,3), c(2,3,4)), c(1,2,3), n=2))
  ref_corpus <- list(c(1,2,3,4))
  cand_corpus <- c(1,2,3,5)
  testthat::expect_vector(bleu_sentence_ids(list(c(1,2,3), c(2,3,4)), c(1,2,3), weights=c(0.33, 0.33, 0.33)))
  testthat::expect_vector(bleu_sentence_ids(ref_corpus, cand_corpus, weights=c(0.25, 0.25, 0.25, 0.25)))
  testthat::expect_vector(bleu_sentence_ids(ref_corpus, cand_corpus, n=4, smoothing="floor", epsilon=0.01))
  testthat::expect_vector(bleu_sentence_ids(ref_corpus, cand_corpus, n=4, smoothing="add-k", k=1))
})

test_that("Expect number for `bleu_corpus_ids`", {
  testthat::expect_vector(bleu_corpus_ids(list(list(c(1,2,3), c(2,3,4))), list(c(1,2,3)), n=2))
  ref_corpus <- list(list(c(1,2,3,4)))
  cand_corpus <- list(c(1,2,3,5))
  testthat::expect_vector(bleu_corpus_ids(list(list(c(1,2,3), c(2,3,4))), list(c(1,2,3)), weights=c(0.33, 0.33, 0.33)))
  testthat::expect_vector(bleu_corpus_ids(ref_corpus, cand_corpus, weights=c(0.25, 0.25, 0.25, 0.25)))
  testthat::expect_vector(bleu_corpus_ids(ref_corpus, cand_corpus, n=4, smoothing="floor", epsilon=0.01))
  testthat::expect_vector(bleu_corpus_ids(ref_corpus, cand_corpus, n=4, smoothing="add-k", k=1))
})

test_that("NLTK & SacreBLEU Example", {
  result_nltk <- 0.7938839309316524
  result_nlreval <- bleu_corpus_ids(ref_corpus, cand_corpus, n=3)
  testthat::expect_equal(result_nlreval, result_nltk, tolerance=0.001)

  result_nlreval <- bleu_corpus_ids(ref_corpus, cand_corpus, weights=c(0.3333, 0.3333, 0.3333))
  testthat::expect_equal(result_nlreval, result_nltk, tolerance=0.001)

  result_nltk_k_smoothing <- 0.6865890479690392
  result_nlreval <- bleu_corpus_ids(ref_corpus, cand_corpus, n=4, smoothing="add-k")
  testthat::expect_equal(result_nlreval, result_nltk_k_smoothing, tolerance=0.001)

  result_sacrebleu <- 0.223606797749979
  ref_corpus <- list(list(c(1,2,3,4)))
  cand_corpus <- list(c(1,2,3,5))
  result_nlreval <- bleu_corpus_ids(ref_corpus, cand_corpus, n=4, smoothing="floor", epsilon=0.01)
  testthat::expect_equal(result_nlreval, result_sacrebleu, tolerance=0.001)

  result_sacrebleu <- 0.223606797749979
  ref_corpus <- list(c(1,2,3,4))
  cand_corpus <- c(1,2,3,5)
  result_nlreval <- bleu_sentence_ids(ref_corpus, cand_corpus, n=4, smoothing="floor", epsilon=0.01)
  testthat::expect_equal(result_nlreval, result_sacrebleu, tolerance=0.001)
})

test_that("Tests Special cases", {
  result_nlreval <- bleu_corpus_ids(list(), list())
  testthat::expect_equal(result_nlreval, 0.0)
})


test_that("Random Input", {
  generate_random_vectors <- function(length, min, max, m=7) {
    lapply(1:m, function(x) sample(min:max, length, replace = TRUE))
  }
  n <- sample(1:100, 1)
  ref <- lapply(sample(5:20), generate_random_vectors, min=0, max=50000)
  cand <- generate_random_vectors(10, min=0, max=50000, m=16)
  testthat::expect_vector(bleu_corpus_ids(ref, cand))
  testthat::expect_vector(bleu_corpus_ids(ref, cand, n=4, smoothing="floor", epsilon=0.01))
})