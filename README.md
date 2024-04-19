
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sacReBLEU

<!-- badges: start -->
<!-- badges: end -->

The goal of sacReBLEU is to provide a simple interface to the BLEU
score, a metric for evaluating the quality of generated text. This
package is inspired by the NLTK and sacrebleu implementations of the
BLEU score, and is implemented in C++ for the R programming language.

## Installation

You can install the development version of sacReBLEU from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LazerLambda/sacReBLEU")
```

## Example

``` r
library(sacReBLEU)
cand_corpus <- list("This is good", "This is not good")
ref_corpus <- list(list("Perfect outcome!", "Excellent!"), list("Not sufficient.", "Horrible."))
bleu_corpus <- bleu_corpus(ref_corpus, cand_corpus)
```
