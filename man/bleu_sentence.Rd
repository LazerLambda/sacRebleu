% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bleu.R
\name{bleu_sentence}
\alias{bleu_sentence}
\title{Compute BLEU for a Sentence with Tokenization}
\usage{
bleu_sentence(
  references,
  candidate,
  tokenizer = "bert-base-cased",
  n = 4,
  weights = NULL,
  smoothing = NULL,
  epsilon = 0.1,
  k = 1
)
}
\arguments{
\item{references}{A list of reference sentences.}

\item{candidate}{A candidate sentence.}

\item{tokenizer}{Either an already initialized 'tok' tokenizer object or a
huggingface identifier (default is 'bert-base-cased')}

\item{n}{N-gram for BLEU score (default is set to 4).}

\item{weights}{Weights for the n-grams (default is set to 1/n for each entry).}

\item{smoothing}{Smoothing method for BLEU score (default is set to 'standard', 'floor', 'add-k' available)}

\item{epsilon}{Epsilon value for epsilon-smoothing (default is set to 0.1).}

\item{k}{K value for add-k-smoothing (default is set to 1).}
}
\value{
The BLEU score for the candidate sentence.
}
\description{
This function applies tokenization based on the 'tok' library and computes the BLEU score.
An already initializied tokenizer can be provided using the 'tokenizer' argument or
a valid huggingface identifier (string) can be passed. If the identifier is used only,
the tokenizer is newly initialized on every call.
}
\examples{
cand <- "Hello World!"
ref <- list("Hello everyone.", "Hello Planet", "Hello World")
\donttest{
tok <- tok::tokenizer$from_pretrained("bert-base-uncased")
bleu_standard <- bleu_sentence(ref, cand, tok)}
}
