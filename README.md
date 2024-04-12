
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sac :registered: eBLEU

<!-- badges: start -->
<!-- badges: end -->

The goal of sacReBLEU is to provide a simple interface to the BLEU
score, a metric for evaluating the quality of machine-translated text.
This package is inspired by the NLTK and sacrebleu implementations of
the BLEU score, and is based on a high-performance C++ implementation
for the R programming language.

## Installation

You can install the development version of sacReBLEU from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LazerLambda/sacReBLEU")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(sacReBLEU)
ref_corpus <- list(c(1,2,3,4))
cand_corpus <- c(1,2,3,5)
bleu_standard <- bleu_sentence_ids(ref_corpus, cand_corpus)
```
