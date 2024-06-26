// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cpp_bleu_corpus_ids
NumericVector cpp_bleu_corpus_ids(List references, List candidate, NumericVector weights, CharacterVector smoothing, double epsilon, int k);
RcppExport SEXP _sacRebleu_cpp_bleu_corpus_ids(SEXP referencesSEXP, SEXP candidateSEXP, SEXP weightsSEXP, SEXP smoothingSEXP, SEXP epsilonSEXP, SEXP kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type references(referencesSEXP);
    Rcpp::traits::input_parameter< List >::type candidate(candidateSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type weights(weightsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type smoothing(smoothingSEXP);
    Rcpp::traits::input_parameter< double >::type epsilon(epsilonSEXP);
    Rcpp::traits::input_parameter< int >::type k(kSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_bleu_corpus_ids(references, candidate, weights, smoothing, epsilon, k));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sacRebleu_cpp_bleu_corpus_ids", (DL_FUNC) &_sacRebleu_cpp_bleu_corpus_ids, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_sacRebleu(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
