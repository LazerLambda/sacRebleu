library(checkmate)

#' Validate Arguemnts
#'
#' @param weights Weight vector for `bleu_corpus` and `bleu_sentence` functions
#' @param smoothing Smoothing method for `bleu_corpus` and `bleu_sentence` functions
#' @param n N-gram for `bleu_corpus` and `bleu_sentence` functions
#' @returns A list with the validated arguments (weights and smoothing)
validate_arguments <- function(weights, smoothing, n) {
  if (!is.null(weights)) {
    checkmate::assert_vector(weights)
    checkmate::assert_numeric(weights)
  } else {
    weights <- rep(1 / n, n)
  }
  if (!is.null(smoothing)) {
    checkmate::assert_string(smoothing)
    checkmate::assert_true(smoothing %in% c("floor", "add-k"))
  } else {
    smoothing <- "standard"
  }
  return(list(weights = weights, smoothing = smoothing))
}

#' Computes BLEU-Score (Papineni et al., 2002).
#'
#' `bleu_sentence` computes the BLEU score for a single candidate sentence and a list of reference sentences.
#' The sentences must be tokenized before so they are represented as integer vectors.
#' Akin to sacreBLEU, the function allows the application of different smoothing methods.
#' Epsilon- and add-k-smoothing are available. Epsilon-smoothing is equivalent to 'floor'
#' smoothing in the sacreBLEU implementation.
#' The different smoothing techniques are described in Chen et al., 2014
#' (https://aclanthology.org/W14-3346/).
#'
#' @param references A list of reference sentences.
#' @param candidate A candidate sentence.
#' @param n N-gram for BLEU score (default is set to 4).
#' @param weights Weights for the n-grams (default is set to 1/n for each entry).
#' @param smoothing Smoothing method for BLEU score (default is set to 'standard', 'floor', 'add-k' available)
#' @param epsilon Epsilon value for epsilon-smoothing (default is set to 0.1).
#' @param k K value for add-k-smoothing (default is set to 1).
#'
#' @returns The BLEU score for the candidate sentence.
#' @export
#' @examples
#' ref_corpus <- list(c(1,2,3,4))
#' cand_corpus <- c(1,2,3,5)
#' bleu_standard <- bleu_sentence(ref_corpus, cand_corpus)
#' bleu_floor <- bleu_sentence(ref_corpus, cand_corpus, smoothing="floor", epsilon=0.01)
#' bleu_add_k <- bleu_sentence(ref_corpus, cand_corpus, smoothing="add-k", k=1)
bleu_sentence <- function(references, candidate, n = 4, weights = NULL, smoothing = NULL, epsilon = 0.1, k = 1) {
  checkmate::assert_list(references)
  checkmate::assert_vector(references[[1]])
  checkmate::assert_numeric(unlist(references))
  checkmate::assert_numeric(candidate)
  checkmate::assert_numeric(n)
  checkmate::assert_true(n > 0 && n %% 1 == 0)
  checkmate::assert_number(epsilon)
  checkmate::assert_number(k)

  args <- validate_arguments(weights, smoothing, n)
  .cpp_bleu_corpus(list(references), list(candidate), args$weights, args$smoothing, epsilon, k)
}

#' Computes BLEU score (Papineni et al., 2002).
#'
#' `bleu_sentence` computes the BLEU score for a corpus and its respective reference sentences.
#' The sentences must be tokenized before so they are represented as integer vectors.
#' Akin to sacreBLEU, the function allows the application of different smoothing methods.
#' Epsilon- and add-k-smoothing are available. Epsilon-smoothing is equivalent to 'floor'
#' smoothing in the sacreBLEU implementation.
#' The different smoothing techniques are described in Chen et al., 2014
#' (https://aclanthology.org/W14-3346/).
#'
#' @param references A list of a list of reference sentences (`list(list(c(1,2,...)), list(c(3,5,...)))`).
#' @param candidates A list of candidate sentences (`list(c(1,2,...), c(3,5,...))`).
#' @param n N-gram for BLEU score (default is set to 4).
#' @param weights Weights for the n-grams (default is set to 1/n for each entry).
#' @param smoothing Smoothing method for BLEU score (default is set to 'standard', 'floor', 'add-k' available)
#' @param epsilon Epsilon value for epsilon-smoothing (default is set to 0.1).
#' @param k K value for add-k-smoothing (default is set to 1).
#'
#' @returns The BLEU score for the candidate sentence.
#' @export
#' @examples
#' cand_corpus <- list(c(1,2,3), c(1,2))
#' ref_corpus <- list(list(c(1,2,3), c(2,3,4)), list(c(1,2,6), c(781, 21, 9), c(7, 3)))
#' bleu_corpus_standard <- bleu_corpus(ref_corpus, cand_corpus, weights=c(0.3333, 0.3333, 0.3333))
#' bleu_corpus_floor <- bleu_corpus(ref_corpus, cand_corpus, smoothing="floor", epsilon=0.01)
#' bleu_corpus_add_k <- bleu_corpus(ref_corpus, cand_corpus, smoothing="add-k", k=1)
bleu_corpus <- function(references, candidates, n = 4, weights = NULL, smoothing = NULL, epsilon = 0.1, k = 1) {
  checkmate::assert_list(references)
  checkmate::assert_true(Reduce(function(acc, e) (class(e) == "list") && acc, references, TRUE))
  checkmate::assert_true(Reduce(function(acc, e) (class(e) %in% c("numeric", "integer")) && acc, candidates, TRUE))
  checkmate::assert_true(length(references) == length(candidates))
  checkmate::assert_numeric(n)
  checkmate::assert_true(n > 0 && n %% 1 == 0)
  checkmate::assert_number(epsilon)
  checkmate::assert_number(k)

  args <- validate_arguments(weights, smoothing, n)
  .cpp_bleu_corpus(references, candidates, args$weights, args$smoothing, epsilon, k)
}
