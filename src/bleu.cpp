
#include <iostream>
// #include <math.h> 
#include <cmath>
#include <set>
#include <tuple>
#include <vector>
#include <unordered_map>
#include <stdexcept>
#include <string>
#include <Rcpp.h>
#include "Fraction.h"

using namespace std;
using namespace Rcpp;


/**
 * @brief Concatenates the elements of a vector into a string.
 *
 * This function takes a vector of integers as input and concatenates
 * each element into a string. Each integer is converted to a string
 * and then appended to the result string with an underscore ("_") 
 * added after each number.
 *
 * @param vec The vector of integers to concatenate.
 * @return A string that is the concatenation of the string representations
 * of each element in the input vector, separated by underscores.
 */
string concatenate(vector<int> vec) {
    string result = "";
    for (int i = 0; i < vec.size(); i++)
        result += to_string(vec[i]) + "_";
    return result;
}


/**
 * @brief Collects n-grams from a sentence.
 * 
 * This function takes a vector of integers representing a sentence and
 * an integer n as input, and returns a map of n-grams to their counts.
 * The function iterates over the sentence and collects all n-grams of
 * length n, storing them in a map where the key is the n-gram and the
 * value is the count of that n-gram in the sentence.
 * 
 * @param sentence The vector of integers representing the sentence.
 * @param n The length of the n-grams to collect.
 * @return A map of n-grams to their counts in the input sentence.
 */
static unordered_map<string, int> collect_n_grams(vector<int> sentence, int n) {
    unordered_map<string, int> counter;
    int sentence_size = sentence.size();
    for (int i = 0; i < sentence_size- n + 1; i++) {
        vector<int> n_gram = vector<int>(sentence.begin() + i, sentence.end() - sentence_size + i + n);
        string slice = concatenate(n_gram);
        counter[slice] += 1;
    }
    return counter;
}


/**
 * @brief Iterates over a list of references and collects n-grams.
 * 
 * This function takes a list of references, where each reference is
 * a vector of integers representing a sentence, and an integer n as
 * input. The function iterates over each reference and collects all
 * n-grams of length n, storing them in a map where the key is the
 * n-gram and the value is the maximum count of that n-gram in any
 * of the references.
 * 
 * @param references The list of references to iterate over.
 * @param n The length of the n-grams to collect.
 * @return A map of n-grams to their maximum counts in the references.
 * @throws invalid_argument if n is less than 1.
 */
static unordered_map<string, int> iterate_references_n_gram(vector<vector<int>> references, int n) {
    if (n < 1) {
        throw invalid_argument( "`n` must be larger than 0!" );
    }
    unordered_map<string, int> counter;
    for (int i = 0; i < references.size(); i++) {
        unordered_map <string, int> counter_inner = collect_n_grams(references[i], n);
        for (const auto &pair : counter_inner) {
            if (counter.find(pair.first) != counter_inner.end()) {
                if (counter[pair.first] < pair.second ){
                    counter[pair.first] = pair.second;
                }
            } else {
                counter[pair.first] = pair.second;
            }
        }
    }
    return counter;
}

/**
 * @brief Combines n-gram dictionaries from candidate sentences.
 * 
 * This function takes a list of candidate sentences, where each candidate
 * is a vector of integers representing a sentence, and an integer n as
 * input. The function iterates over each candidate and collects all
 * n-grams of length n, storing them in a map where the key is the
 * n-gram and the value is the maximum count of that n-gram in any
 * of the candidates.
 * 
 * @param candidate The list of candidate sentences to iterate over.
 * @param n The length of the n-grams to collect.
 * @return A map of n-grams to their maximum counts in the candidates.
 * @throws invalid_argument if n is less than 1.
 */
unordered_map<string, int> combine_u_maps_cand(vector<int> candidate, int n) {
    unordered_map<string, int> n_gram_dict_ref;
    for (int i = 0; i < n; i++) {
        unordered_map<string, int> n_gram_dict = collect_n_grams(candidate, i + 1);
        for (const auto &pair : n_gram_dict) {
            if (n_gram_dict_ref.find(pair.first) != n_gram_dict_ref.end()) {
                if (n_gram_dict_ref[pair.first] < pair.second ){
                    n_gram_dict_ref[pair.first] = pair.second;
                }
            } else {
                n_gram_dict_ref[pair.first] = pair.second;
            }
        }
    }
    return n_gram_dict_ref;
} 


/**
 * @brief Combines n-gram dictionaries from reference sentences.
 * 
 * This function takes a list of reference sentences, where each reference
 * is a vector of integers representing a sentence, and an integer n as
 * input. The function iterates over each reference and collects all
 * n-grams of length n, storing them in a map where the key is the
 * n-gram and the value is the maximum count of that n-gram in any
 * of the references.
 * 
 * @param references The list of reference sentences to iterate over.
 * @param n The length of the n-grams to collect.
 * @return A map of n-grams to their maximum counts in the references.
 */
unordered_map<string, int> combine_u_maps(vector<vector<int>> references, int n) {
    unordered_map<string, int> n_gram_dict_ref;
    for (int i = 0; i < n; i++) {
        unordered_map<string, int> n_gram_dict = iterate_references_n_gram(references, i + 1);
        for (const auto &pair : n_gram_dict) {
            if (n_gram_dict_ref.find(pair.first) != n_gram_dict_ref.end()) {
                if (n_gram_dict_ref[pair.first] < pair.second ){
                    n_gram_dict_ref[pair.first] = pair.second;
                }
            } else {
                n_gram_dict_ref[pair.first] = pair.second;
            }
        }
    }
    return n_gram_dict_ref;
}

/**
 * @brief Calculates the precision of n-grams in a candidate sentence.
 * 
 * This function takes a list of reference sentences, where each reference
 * is a vector of integers representing a sentence, a candidate sentence
 * as a vector of integers, and an integer n as input. The function calculates
 * the precision of n-grams in the candidate sentence by comparing them to
 * the n-grams in the reference sentences. The precision is calculated as
 * the number of n-grams in the candidate that appear in the references,
 * divided by the total number of n-grams in the candidate. Implementation based
 * on Papineni et al. (2002) https://aclanthology.org/P02-1040/.
 * 
 * @param references The list of reference sentences to compare against.
 * @param candidate The candidate sentence to calculate precision for.
 * @param n The length of the n-grams to compare.
 * @return The precision of the n-grams in the candidate sentence.
 */
static Fraction n_gram_precision(vector<vector<int>> references, vector<int> candidate, int n) {
    unordered_map<string, int> test_ref = iterate_references_n_gram(references, n);
    unordered_map<string, int> test_cand = collect_n_grams(candidate, n);
    int collector_num = 0;
    int collector_denom = 0;
    for (const auto &cand : test_cand) {
        // Count
        collector_denom += cand.second;
        if (test_ref.find(cand.first) != test_ref.end()) {
            // Count_clip
            if (test_ref[cand.first]< cand.second) {
                collector_num += test_ref[cand.first];
            } else {
                collector_num += cand.second;
            }
        } else {
            continue;
        }
    }
    // From NLTK:
    // Ensures that denominator is minimum 1 to avoid ZeroDivisionError.
    // Usually this happens when the ngram order is > len(reference).
    if (collector_denom == 0) {
        collector_denom = 1;
    }
    Fraction f = Fraction(collector_num, collector_denom);
    return f;
}

/**
 * @brief Finds the closest reference length to the candidate length.
 * 
 * This function takes an integer representing the length of a candidate
 * sentence and a vector of integers representing the lengths of reference
 * sentences as input. The function finds the reference sentence length
 * that is closest to the candidate sentence length and returns it.
 * 
 * @param cand_len The length of the candidate sentence.
 * @param ref_len The vector of reference sentence lengths.
 * @return The reference sentence length closest to the candidate length.
 */
static int closest_ref_len(int cand_len, vector<int> ref_len) {
    int min_diff = abs(cand_len - ref_len[0]);
    int closest = ref_len[0];
    for (int i = 1; i < ref_len.size(); i++) {
        int diff = abs(cand_len - ref_len[i]);
        if (diff < min_diff) {
            min_diff = diff;
            closest = ref_len[i];
        }
    }
    return closest;

}

/**
 * @brief Calculates the brevity penalty for a candidate sentence.
 * 
 * This function takes an integer representing the length of a reference
 * sentence and an integer representing the length of a candidate sentence
 * as input. The function calculates the brevity penalty for the candidate
 * sentence based on the reference sentence length. The brevity penalty is
 * calculated as the exponential of 1 minus the ratio of the reference length
 * to the candidate length. Implementation based on Papineni et al. (2002)
 * https://aclanthology.org/P02-1040/.
 * 
 * @param ref_len The length of the reference sentence.
 * @param cand_len The length of the candidate sentence.
 * @return The brevity penalty for the candidate sentence.
*/
static long double brevity_penalty(int ref_len, int cand_len) {
    if (cand_len > ref_len) {
        return 1.0;
    } else {
        return exp(1 - (long double) ref_len / (long double) cand_len);
    }
}

/**
 * @brief Calculates the brevity penalty for a candidate sentence.
 * 
 * This function takes a vector of vectors of integers representing reference
 * sentences and a vector of integers representing a candidate sentence as input.
 * The function calculates the brevity penalty for the candidate sentence based on
 * the reference sentences. The brevity penalty is calculated as the exponential of
 * 1 minus the ratio of the reference length to the candidate length. Implementation
 * based on Papineni et al. (2002) https://aclanthology.org/P02-1040/.
 * 
 * @param references The list of reference sentences to compare against.
 * @param candidate The candidate sentence to calculate brevity penalty for.
 * @return The brevity penalty for the candidate sentence.
 */
static long double brevity_penalty_single(vector<vector<int>> references, vector<int> candidate){
    int cand_len = candidate.size();
    vector<int> lengths;
    transform(references.begin(), references.end(), back_inserter(lengths), [](const vector<int>& v) { return v.size(); });
    int ref_len = closest_ref_len(cand_len, lengths);
    return brevity_penalty(ref_len, cand_len);
}

/**
 * @brief Calculates the BLEU score for a candidate sentence.
 * 
 * This function takes a vector of vectors of integers representing reference
 * sentences, a vector of integers representing a candidate sentence, a vector
 * of floats representing the weights for each n-gram, and a string representing
 * the smoothing method to use as input. The function calculates the BLEU score
 * for the candidate sentence based on the reference sentences, weights, and smoothing
 * method. The BLEU score is calculated as the brevity penalty multiplied by the
 * exponential of the sum of the weighted log precisions for each n-gram. Implementation
 * based on Papineni et al. (2002) https://aclanthology.org/P02-1040/.
 * 
 * @param references The list of reference sentences to compare against.
 * @param candidate The candidate sentence to calculate BLEU score for.
 * @param weights The list of weights for each n-gram.
 * @param smoothing The smoothing method to use for precision calculation.
 * @return The BLEU score for the candidate sentence.
 */
static long double bleu(vector<vector<int>> references, vector<int> candidate, vector<float> weights, string smoothing) {
    long double collector = 0.0;
    int n = weights.size();
    for (int i = 0; i < n; i++) {
        Fraction precision_frac = n_gram_precision(references, candidate, i + 1);
        long double precision  = (long double) precision_frac.numerator / (long double) precision_frac.denominator;
        collector += weights[i] * log(precision);
    }
    long double bp = brevity_penalty_single(references, candidate);
    return bp * exp(collector);
}

/**
 * @brief Calculates the Precision over a list of fractions.
 * 
 * This function takes a vector of Fractions, a string representing the smoothing method,
 * a long double representing the epsilon value, and a long double representing the k value
 * as input. The function calculates the precision over the list of Fractions based on the
 * smoothing method, epsilon, and k value. The precision is calculated as the sum of the
 * numerators divided by the sum of the denominators. Different smoothing techniques besides
 * 'standard' are based on Chen et al., 2014 (https://aclanthology.org/W14-3346/).
 * 
 * @param fractions The list of Fractions to calculate precision for.
 * @param smoothing The smoothing method to use for precision calculation.
 * @param epsilon The epsilon value for 'floor' smoothing.
 * @param k The k value for 'add-k' smoothing.
 * @return The precision over the list of Fractions.
*/
static long double get_precision(vector<Fraction> fractions, string smoothing, long double epsilon, long double k) {
    long double collector_num = 0.0;
    long double collector_denom = 0.0;
    long double precision = 0.0;
    for (int i = 0; i < fractions.size(); i++) {
        if (smoothing == "standard") {
            collector_num += (long double) fractions[i].numerator;
            collector_denom += (long double) fractions[i].denominator;
        }
        if (smoothing == "floor") {
            collector_num += (long double) fractions[i].numerator;
            if (fractions[i].numerator == 0) {
                collector_num += epsilon;
            }
            collector_denom += (long double) fractions[i].denominator;
        }
        if (smoothing == "add-k") {
            collector_num += (long double) fractions[i].numerator;
            collector_denom += (long double) fractions[i].denominator;
            if (i == 0) {
                collector_num += k;
                collector_denom += k;
            }
        }
        if(collector_denom != 0.0)
            precision  = (long double) collector_num / (long double) collector_denom;
    }
    return precision;
}

/**
 * @brief Calculates the BLEU score for a list of a corpus.
 * 
 * This function takes a list of lists of reference sentences, a list of candidate sentences,
 * a vector of floats representing the weights for each n-gram, a string representing the smoothing
 * method to use, a long double representing the epsilon value, and an integer representing the k
 * value as input. The function calculates the BLEU score for the corpus based on the reference
 * sentences, weights, smoothing method, epsilon, and k value. The BLEU score is calculated as the
 * brevity penalty multiplied by the exponential of the sum of the weighted log precisions for each n-gram.
 * Implementation based on Papineni et al. (2002) https://aclanthology.org/P02-1040/ and inspired on the implementation
 * of the natural language toolkit for Python (https://www.nltk.org).
 * 
 * @param references The list of reference sentences to compare against.
 * @param candidate The candidate sentence to calculate BLEU score for.
 * @param weights The list of weights for each n-gram.
 * @param smoothing The smoothing method to use for precision calculation.
 * @param epsilon The epsilon value for 'floor' smoothing.
 * @param k The k value for 'add-k' smoothing.
 * @return The BLEU score for the candidate sentence.
*/
static long double bleu_corpus_ids(vector<vector<vector<int>>> references, vector<vector<int>> candidate, vector<float> weights, string smoothing, long double epsilon, int k) {
    long double collector = 0.0;
    int corpus_clos_ref_len_coll = 0;
    int corpus_cand_len_coll = 0;
    int n = weights.size();
    for (int i = 0; i < n; i++) {
        vector<Fraction> fractions; 
        for (int j = 0; j < candidate.size(); j++) {
            Fraction precision_frac = n_gram_precision(references[j], candidate[j], i + 1);
            fractions.push_back(precision_frac);
            if(i == 0){
                int cand_len = candidate[j].size();
                vector<int> lengths;
                transform(references[j].begin(), references[j].end(), back_inserter(lengths), [](const vector<int>& v) { return v.size(); });
                int ref_len = closest_ref_len(cand_len, lengths);
                corpus_clos_ref_len_coll += ref_len;
                corpus_cand_len_coll += cand_len;
            }
        }
        long double precision = get_precision(fractions, smoothing, epsilon, (long double) k);
        collector += weights[i] * log(precision);
    }
    long double bp = brevity_penalty(corpus_clos_ref_len_coll, corpus_cand_len_coll);
    return bp * exp(collector);
}

/**
 * @brief Calculates the BLEU score for a list of a corpus.
 * 
 * Wrapper for the `bleu_corpus_ids` function to cast R datatypes to C++ datatypes.
 * 
 * @param references The list of reference sentences to compare against.
 * @param candidate The candidate sentence to calculate BLEU score for.
 * @param weights The list of weights for each n-gram.
 * @param smoothing The smoothing method to use for precision calculation.
 * @param epsilon The epsilon value for 'floor' smoothing.
 * @param k The k value for 'add-k' smoothing.
 * @return The BLEU score for the candidate sentence.
*/
// [[Rcpp::export(".cpp_bleu_corpus_ids")]]
NumericVector cpp_bleu_corpus_ids(List references, List candidate, NumericVector weights, CharacterVector smoothing, double epsilon, int k){
    if (!references.length() || !candidate.length()) {
        return wrap(0.0);
    }
    if (candidate.length() != references.length()) {
        throw invalid_argument( "C++:The length of candidate and references must be the same!" );
    }
    vector<vector<int>> cand = as<vector<vector<int>>>(candidate);
    vector<vector<vector<int>>> ref = as<vector<vector<vector<int>>>>(references);
    vector<float> weights_vec = as<vector<float>>(weights);
    string smoothing_str = as<string>(smoothing);

    return(wrap(bleu_corpus_ids(ref, cand, weights_vec, smoothing_str, (long double) epsilon, k)));
}
