#ifndef FRACTION_H
#define FRACTION_H

class Fraction {
    public:
        int numerator;
        int denominator;
        Fraction(int numerator, int denominator);
        long double get_value();
    };

#endif