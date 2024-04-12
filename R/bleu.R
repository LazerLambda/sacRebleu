library(checkmate)



#' Validate Arguemnts
#'
#' @param weights Weight vector for `bleu_corpus_ids` and `bleu_sentence_ids` functions
#' @param smoothing Smoothing method for `bleu_corpus_ids` and `bleu_sentence_ids` functions
#' @param n N-gram for `bleu_corpus_ids` and `bleu_sentence_ids` functions
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
#' `bleu_sentence_ids` computes the BLEU score for a single candidate sentence and a list of reference sentences.
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
#' bleu_standard <- bleu_sentence_ids(ref_corpus, cand_corpus)
#' bleu_floor <- bleu_sentence_ids(ref_corpus, cand_corpus, smoothing="floor", epsilon=0.01)
#' bleu_add_k <- bleu_sentence_ids(ref_corpus, cand_corpus, smoothing="add-k", k=1)
bleu_sentence_ids <- function(references, candidate, n = 4, weights = NULL, smoothing = NULL, epsilon = 0.1, k = 1) {
  checkmate::assert_list(references)
  checkmate::assert_vector(references[[1]])
  checkmate::assert_numeric(unlist(references))
  checkmate::assert_numeric(candidate)
  checkmate::assert_numeric(n)
  checkmate::assert_true(n > 0 && n %% 1 == 0)
  checkmate::assert_number(epsilon)
  checkmate::assert_number(k)

  args <- validate_arguments(weights, smoothing, n)
  .cpp_bleu_corpus_ids(list(references), list(candidate), args$weights, args$smoothing, epsilon, k)
}



#' Computes BLEU score (Papineni et al., 2002).
#'
#' `bleu_sentence_ids` computes the BLEU score for a corpus and its respective reference sentences.
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
#' bleu_corpus_ids_standard <- bleu_corpus_ids(ref_corpus, cand_corpus, weights=c(0.3333, 0.3333, 0.3333))
#' bleu_corpus_ids_floor <- bleu_corpus_ids(ref_corpus, cand_corpus, smoothing="floor", epsilon=0.01)
#' bleu_corpus_ids_add_k <- bleu_corpus_ids(ref_corpus, cand_corpus, smoothing="add-k", k=1)
bleu_corpus_ids <- function(references, candidates, n = 4, weights = NULL, smoothing = NULL, epsilon = 0.1, k = 1) {
  checkmate::assert_list(references)
  checkmate::assert_true(Reduce(function(acc, e) (class(e) == "list") && acc, references, TRUE))
  checkmate::assert_true(Reduce(function(acc, e) (class(e) %in% c("numeric", "integer")) && acc, candidates, TRUE))
  checkmate::assert_true(length(references) == length(candidates))
  checkmate::assert_numeric(n)
  checkmate::assert_true(n > 0 && n %% 1 == 0)
  checkmate::assert_number(epsilon)
  checkmate::assert_number(k)

  args <- validate_arguments(weights, smoothing, n)
  .cpp_bleu_corpus_ids(references, candidates, args$weights, args$smoothing, epsilon, k)
}



# Compute BLEU for a Corpus with Tokenization
#
#' This function applies tokenization based on the 'tok' library and computes the BLEU score.
#' An already initializied tokenizer can be provided using the `tokenizer` argument or a valid huggingface identifier (string) can
#' be passed. If only the identifier is used, the tokenizer is newly initialized on every call.
#' @param references A list of a list of reference sentences (`list(list(c(1,2,...)), list(c(3,5,...)))`).
#' @param candidates A list of candidate sentences (`list(c(1,2,...), c(3,5,...))`).
#' @param tokenizer Either an already initialized 'tok' tokenizer object or a huggingface identifier (default is 'bert-base-uncased')
#' @param n N-gram for BLEU score (default is set to 4).
#' @param weights Weights for the n-grams (default is set to 1/n for each entry).
#' @param smoothing Smoothing method for BLEU score (default is set to 'standard', 'floor', 'add-k' available)
#' @param epsilon Epsilon value for epsilon-smoothing (default is set to 0.1).
#' @param k K value for add-k-smoothing (default is set to 1).
#'
#' @returns The BLEU score for the candidate sentence.
#' @export
#' @examples
#' 
bleu_corpus <- function(references, candidates, tokenizer="bert-base-uncased", n = 4, weights = NULL, smoothing = NULL, epsilon = 0.1, k = 1) {
  if (class(tokenizer)[[1]] != "tok_tokenizer" && class(tokenizer)[[1]] != "character") {
    stop("ERROR: `tokenizer` argument must be either an identifier string for the `tok` package or a 'tok_tokenizer' object!")
  }
  if (class(tokenizer)[[1]] == "character") {
    tokenizer <- tok::tokenizer$from_pretrained(tokenizer)
  }
  cand_ids <- lapply(
    candidates,
    function(cand) tokenizer$encode(cand)$ids)
  ref_ids <- lapply(
    references,
    function(references_local) lapply(
      references_local,
      function(e){tokenizer$encode(e)$ids}))
  bleu_corpus_ids(ref_ids, cand_ids, n, weights, smoothing, epsilon, k)
}



#' Compute 
#' 
bleu_sentence <- function(references, candidate, tokenizer="bert-base-uncased", n = 4, weights = NULL, smoothing = NULL, epsilon = 0.1, k = 1) {
  if (class(tokenizer)[[1]] != "tok_tokenizer" && class(tokenizer)[[1]] != "character") {
    stop("ERROR: `tokenizer` argument must be either an identifier string for the `tok` package or a 'tok_tokenizer' object!")
  }
  if (class(tokenizer)[[1]] == "character") {
    tokenizer <- tok::tokenizer$from_pretrained(tokenizer)
  }
  cand_ids <- tokenizer$encode(candidate)$ids
  ref_ids <- lapply(
    references,
    function(reference) tokenizer$encode(reference)$ids)
  bleu_sentence_ids(ref_ids, cand_ids, n, weights, smoothing, epsilon, k)
}