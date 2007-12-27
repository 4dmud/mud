// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_MATRIx_UTILITIES_ABSTRACT_
#ifdef DLIB_MATRIx_UTILITIES_ABSTRACT_

#include "matrix_abstract.h"
#include <complex>

namespace dlib
{

// ----------------------------------------------------------------------------------------

    const matrix<matrix_exp::type, matrix_exp::NR, matrix_exp::NC> inv (
        const matrix_exp& m
    );
    /*!
        requires
            - m is a square matrix
        ensures
            - returns the inverse of m 
              (Note that if m is singular or so close to being singular that there
              is a lot of numerical error then the returned matrix will be bogus.  
              You can check by seeing if m*inv(m) is an identity matrix)
    !*/

// ----------------------------------------------------------------------------------------

    const matrix<matrix_exp::type, matrix_exp::NR, matrix_exp::NC> tmp (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns a temporary matrix object that is a copy of m. 
              (This is useful because it allows you to easily force a matrix_exp to 
              fully evaluate before giving it to some other function that queries
              the elements of the matrix more than once each, such as the matrix
              multiplication operator.)
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp::type min (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns the value of the smallest element of m
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp::type max (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns the value of the biggest element of m
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp::type sum (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns the sum of all elements in m
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp::type prod (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns the results of multiplying all elements of m together. 
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp::type det (
        const matrix_exp& m
    );
    /*!
        requires
            - m is a square matrix
        ensures
            - returns the determinant of m
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp diag (
        const matrix_exp& m
    );
    /*!
        requires
            - m is a square matrix
        ensures
            - returns a column vector R that contains the elements from the diagonal 
              of m in the order R(0)==m(0,0), R(1)==m(1,1), R(2)==m(2,2) and so on.
    !*/

// ----------------------------------------------------------------------------------------

    template <
        long R,
        long C
        >
    const matrix_exp removerc (
        const matrix_exp& m
    );
    /*!
        requires
            - m.nr() > 1
            - m.nc() > 1
        ensures
            - returns a matrix R such that:
                - R.nr() == m.nr() - 1
                - R.nc() == m.nc() - 1
                - R == m with its R row and C column removed
    !*/

// ----------------------------------------------------------------------------------------
    const matrix_exp trans (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns the transpose of the matrix m
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp conj (
        const matrix_exp& m
    );
    /*!
        requires
            - matrix_exp::type == std::complex<T>
        ensures
            - returns a matrix R such that:
                - R::type == std::complex<T>
                - R has the same dimensions as m
                - for all valid r and c:
                  R(r,c) == std::conj(m(r,c))
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp imag (
        const matrix_exp& m
    );
    /*!
        requires
            - matrix_exp::type == std::complex<T>
        ensures
            - returns a matrix R such that:
                - R::type == T
                - R has the same dimensions as m
                - for all valid r and c:
                  R(r,c) == std::imag(m(r,c))
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp real (
        const matrix_exp& m
    );
    /*!
        requires
            - matrix_exp::type == std::complex<T>
        ensures
            - returns a matrix R such that:
                - R::type == T
                - R has the same dimensions as m
                - for all valid r and c:
                  R(r,c) == std::real(m(r,c))
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp complex_matrix (
        const matrix_exp& real_part,
        const matrix_exp& imag_part
    );
    /*!
        requires
            - real_part.nr() == imag_part.nr()
            - real_part.nc() == imag_part.nc()
            - real_part and imag_part both contain the same type of element
        ensures
            - returns a matrix R such that:
                - R::type == std::complex<T> where T is whatever type real_part and imag_part used.
                - R has the same dimensions as real_part and imag_part
                - for all valid r and c:
                  R(r,c) == std::complex(real_part(r,c),imag_part(r,c))
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp abs (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns a matrix R such that:
                - R::type == the same type that was in m
                - R has the same dimensions as m
                - for all valid r and c:
                  R(r,c) == std::abs(m(r,c)) 
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp round (
        const matrix_exp& m
    );
    /*!
        requires
            - matrix_exp::type == float, double, or long double 
        ensures
            - returns a matrix R such that:
                - R::type == the same type that was in m
                - R has the same dimensions as m
                - for all valid r and c:
                  R(r,c) == m(r,c) rounded to the nearest integral value
    !*/

// ----------------------------------------------------------------------------------------

    template <
       typename target_type
       >
    const matrix_exp matrix_cast (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns a matrix R where for all valid r and c:
              R(r,c) == static_cast<target_type>(m(r,c))
              also, R has the same dimensions as m.
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename T, 
        long NR, 
        long NC, 
        T val
        >
    const matrix_exp uniform_matrix (
    );
    /*!
        ensures
            - returns an NR by NC matrix with elements of type T and all set to val.
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename T, 
        long N
        >
    const matrix_exp identity_matrix (
    );
    /*!
        ensures
            - returns an N by N identity matrix with elements of type T.
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long NR,
        long NC,
        typename U
        >
    void set_all_elements (
        matrix<T,NR,NC>& m,
        U value
    );
    /*!
        ensures
            - for all valid r and c:
              m(r,c) == value
    !*/

// ----------------------------------------------------------------------------------------

    template <
        long R,
        long C
        >
    const matrix_exp rotate (
        const matrix_exp& m
    );
    /*!
        requires
            - R < m.nr()
            - C < m.nc()
        ensures
            - returns a matrix R such that:
                - R::type == the same type that was in m
                - R has the same dimensions as m
                - for all valid r and c:
                  R( (r+R)%m.nr() , (c+C)%m.nc() ) == m(r,c)
    !*/

// ----------------------------------------------------------------------------------------

    const matrix_exp pointwise_multiply (
        const matrix_exp& a,
        const matrix_exp& b 
    );
    /*!
        requires
            - a.nr() == b.nr()
            - a.nc() == b.nc()
            - a and b both contain the same type of element
        ensures
            - returns a matrix R such that:
                - R::type == the same type that was in a and b.
                - R has the same dimensions as a and b. 
                - for all valid r and c:
                  R(r,c) == a(r,c) * b(r,c)
    !*/

// ----------------------------------------------------------------------------------------
}

#endif // DLIB_MATRIx_UTILITIES_ABSTRACT_

