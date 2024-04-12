#ifndef NGRAMMETRIC_H
#define NGRAMMETRIC_H

#include <vector>

using namespace std;

struct Fraction {
    int numerator;
    int denominator;
};

class NGramMetric {
    public:
        vector<Fraction> fractions;
        // TODO: Use hashable instead of int
        NGramMetric(vector<int> candidate, vector<vector<int>> references, int n);
        __float128 compute_metric();
    };

#endif