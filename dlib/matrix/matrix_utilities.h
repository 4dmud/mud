// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_MATRIx_UTILITIES_
#define DLIB_MATRIx_UTILITIES_

#include "matrix_utilities_abstract.h"
#include "matrix.h"
#include <cmath>
#include <complex>


namespace dlib
{

// ----------------------------------------------------------------------------------------

    template <
        typename OP
        >
    class matrix_zeroary_exp;  

    template <
        typename M,
        typename OP
        >
    class matrix_unary_exp;  

    template <
        typename M1,
        typename M2,
        typename OP
        >
    class matrix_binary_exp;

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_abs
    {
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;
        typedef typename EXP::type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return static_cast<type>(std::abs(m(r,c))); }
    };

    template <
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_abs<EXP> > > abs (
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_unary_exp<matrix_exp<EXP>,op_abs<EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP
        >
    const typename matrix_exp<EXP>::type max (
        const matrix_exp<EXP>& m
    )
    {
        typedef typename matrix_exp<EXP>::type type;
        const long NR = matrix_exp<EXP>::NR;
        const long NC = matrix_exp<EXP>::NC;

        type val = m(0,0);
        for (long r = 0; r < NR; ++r)
        {
            for (long c = 0; c < NC; ++c)
            {
                type temp = m(r,c);
                if (temp > val)
                    val = temp;
            }
        }
        return val;
    }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP
        >
    const typename matrix_exp<EXP>::type min (
        const matrix_exp<EXP>& m
    )
    {
        typedef typename matrix_exp<EXP>::type type;
        const long NR = matrix_exp<EXP>::NR;
        const long NC = matrix_exp<EXP>::NC;

        type val = m(0,0);
        for (long r = 0; r < NR; ++r)
        {
            for (long c = 0; c < NC; ++c)
            {
                type temp = m(r,c);
                if (temp < val)
                    val = temp;
            }
        }
        return val;
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    namespace nric
    {
        // This namespace contains stuff from Numerical Recipes in C

        template <
            typename T,
            long n
            >
        bool ludcmp (
            matrix<T,n,n>& a,
            matrix<long,n,1>& indx,
            T& d
        )
        /*!
            ( this function is derived from the one in numerical recipes in C chapter 2.3)
            ensures
                - #a == both the L and U matrices
                - #indx == the permutation vector (see numerical recipes in C)
                - #d == some other thing (see numerical recipes in C)
                - if (the matrix is singular and we can't do anything) then
                    - returns false
                - else
                    - returns true
        !*/
        {
            long i, imax = 0, j, k;
            T big, dum, sum, temp;
            matrix<T,n,1> vv;

            d = 1.0;
            for (i = 0; i < n; ++i)
            {
                big = 0;
                for (j = 0; j < n; ++j)
                {
                    if ((temp=std::abs(a(i,j))) > big)
                        big = temp;
                }
                if (big == 0.0)
                {
                    return false;
                }
                vv(i) = 1/big;
            }

            for (j = 0; j < n; ++j)
            {
                for (i = 0; i < j; ++i)
                {
                    sum = a(i,j);
                    for (k = 0; k < i; ++k)
                        sum -= a(i,k)*a(k,j);
                    a(i,j) = sum;
                }
                big = 0;
                for (i = j; i < n; ++i)
                {
                    sum = a(i,j);
                    for (k = 0; k < j; ++k)
                        sum -= a(i,k)*a(k,j);
                    a(i,j) = sum;
                    if ( (dum=vv(i)*std::abs(sum)) >= big)
                    {
                        big = dum;
                        imax = i;
                    }
                }
                if (j != imax)
                {
                    for (k = 0; k < n; ++k)
                    {
                        dum = a(imax,k);
                        a(imax,k) = a(j,k);
                        a(j,k) = dum;
                    }
                    d = -d;
                    vv(imax) = vv(j);
                }
                indx(j) = imax;

                if (j != n)
                {
                    dum = 1/a(j,j);
                    for (i = j+1; i < n; ++i)
                        a(i,j) *= dum;
                }
            }
            return true;
        }

// ----------------------------------------------------------------------------------------

        template <
            typename T,
            long n
            >
        void lubksb (
            const matrix<T,n,n>& a,
            const matrix<long,n,1>& indx,
            matrix<T,n,1>& b
        )
        /*!
            ( this function is derived from the one in numerical recipes in C chapter 2.3)
            requires
                - a == the LU decomposition you get from ludcmp()
                - indx == the indx term you get out of ludcmp()
                - b == the right hand side vector from the expression a*x = b
            ensures
                - #b == the soltuion vector x from the expression a*x = b
                  (basically, this function solves for x given b and a)
        !*/
        {
            long i, ii = -1, ip, j;
            T sum;

            for (i = 0; i < n; ++i)
            {
                ip = indx(i);
                sum=b(ip);
                b(ip) = b(i);
                if (ii != -1)
                {
                    for (j = ii; j < i; ++j)
                        sum -= a(i,j)*b(j);
                }
                else if (sum)
                {
                    ii = i;
                }
                b(i) = sum;
            }
            for (i = n-1; i >= 0; --i)
            {
                sum = b(i);
                for (j = i+1; j < n; ++j)
                    sum -= a(i,j)*b(j);
                b(i) = sum/a(i,i);
            }
        }
    }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename OP
        >
    class matrix_zeroary_exp  
    {
    public:
        typedef typename OP::type type;
        typedef matrix_zeroary_exp ref_type;
        const static long NR = OP::NR;
        const static long NC = OP::NC;

        const typename OP::type operator() (
            long r, 
            long c
        ) const { return OP::apply(r,c); }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return false; }

        const ref_type& ref(
        ) const { return *this; }

    };

// ----------------------------------------------------------------------------------------

    template <
        typename M,
        typename OP
        >
    class matrix_unary_exp  
    {
        /*!
            REQUIREMENTS ON M 
                - must be a matrix_exp or matrix_ref object (or
                  an object with a compatible interface).
        !*/
    public:
        typedef typename OP::type type;
        typedef matrix_unary_exp ref_type;
        const static long NR = OP::NR;
        const static long NC = OP::NC;

        matrix_unary_exp (
            const M& m_
        ) :
            m(m_)
        {}

        const typename OP::type operator() (
            long r, 
            long c
        ) const { return OP::apply(m,r,c); }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return m.aliases(item); }

        const ref_type& ref(
        ) const { return *this; }

    private:

        const M m;
    };

// ----------------------------------------------------------------------------------------

    template <
        typename M1,
        typename M2,
        typename OP
        >
    class matrix_binary_exp  
    {
        /*!
            REQUIREMENTS ON M 
                - must be a matrix_exp or matrix_ref object (or
                  an object with a compatible interface).
        !*/
    public:
        typedef typename OP::type type;
        typedef matrix_binary_exp ref_type;
        const static long NR = OP::NR;
        const static long NC = OP::NC;

        matrix_binary_exp (
            const M1& m1_,
            const M2& m2_
        ) :
            m1(m1_),
            m2(m2_)
        {}

        const typename OP::type operator() (
            long r, 
            long c
        ) const { return OP::apply(m1,m2,r,c); }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return m1.aliases(item) || m2.aliases(item); }

        const ref_type& ref(
        ) const { return *this; }

    private:

        const M1 m1;
        const M2 m2;
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_trans
    {
        const static long NR = EXP::NC;
        const static long NC = EXP::NR;
        typedef typename EXP::type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return m(c,r); }
    };

    template <
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_trans<EXP> > > trans (
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_unary_exp<matrix_exp<EXP>,op_trans<EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP, long R, long C>
    struct op_removerc
    {
        const static long NR = EXP::NC - 1;
        const static long NC = EXP::NR - 1;
        typedef typename EXP::type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { 
            if (r < R)
            {
                if (c < C)
                    return m(r,c); 
                else
                    return m(r,c+1); 
            }
            else
            {
                if (c < C)
                    return m(r+1,c); 
                else
                    return m(r+1,c+1); 
            }
        }
    };

    template <
        long R,
        long C,
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_removerc<EXP,R,C> > > removerc (
        const matrix_exp<EXP>& m
    )
    {
        COMPILE_TIME_ASSERT(EXP::NR > 1);
        COMPILE_TIME_ASSERT(EXP::NC > 1);
        typedef matrix_unary_exp<matrix_exp<EXP>,op_removerc<EXP,R,C> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_diag
    {
        const static long NR = EXP::NC;
        const static long NC = 1;
        typedef typename EXP::type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return m(r,r); }
    };

    template <
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_diag<EXP> > > diag (
        const matrix_exp<EXP>& m
    )
    {
        // You can only get the diagonal for square matrices.
        COMPILE_TIME_ASSERT(EXP::NR == EXP::NC);
        typedef matrix_unary_exp<matrix_exp<EXP>,op_diag<EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP, typename target_type>
    struct op_cast
    {
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;
        typedef target_type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return static_cast<target_type>(m(r,c)); }
    };

    template <
        typename target_type,
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_cast<EXP,target_type> > > matrix_cast (
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_unary_exp<matrix_exp<EXP>,op_cast<EXP,target_type> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_round
    {
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;
        typedef typename EXP::type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return static_cast<type>(std::floor(m(r,c)+0.5)); }
    };

    template <
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_round<EXP> > > round (
        const matrix_exp<EXP>& m
    )
    {
        // you can only round matrices that contain floats, doubles or long doubles.
        COMPILE_TIME_ASSERT((
                is_same_type<typename EXP::type,float>::value == true || 
                is_same_type<typename EXP::type,double>::value == true || 
                is_same_type<typename EXP::type,long double>::value == true 
        ));
        typedef matrix_unary_exp<matrix_exp<EXP>,op_round<EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_conj
    {
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;
        typedef typename EXP::type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return std::conj(m(r,c)); }
    };

    template <
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_conj<EXP> > > conj (
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_unary_exp<matrix_exp<EXP>,op_conj<EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_imag
    {
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;
        typedef typename EXP::type::value_type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return std::imag(m(r,c)); }
    };

    template <
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_imag<EXP> > > imag (
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_unary_exp<matrix_exp<EXP>,op_imag<EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_real
    {
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;
        typedef typename EXP::type::value_type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return std::real(m(r,c)); }
    };

    template <
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_real<EXP> > > real (
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_unary_exp<matrix_exp<EXP>,op_real<EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_complex_matrix
    {
        typedef std::complex<typename EXP::type> type;
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;

        template <typename M1, typename M2>
        static type apply ( const M1& m1, const M2& m2 , long r, long c)
        { return type(m1(r,c),m2(r,c)); }
    };

    template <
        typename EXP1,
        typename EXP2
        >
    const matrix_exp<matrix_binary_exp<matrix_exp<EXP1>,matrix_exp<EXP2>,op_complex_matrix<EXP1> > > complex_matrix (
        const matrix_exp<EXP1>& real_part,
        const matrix_exp<EXP2>& imag_part 
    )
    {
        COMPILE_TIME_ASSERT((is_same_type<typename EXP1::type,typename EXP2::type>::value == true));
        COMPILE_TIME_ASSERT(EXP1::NR == EXP2::NR);
        COMPILE_TIME_ASSERT(EXP1::NC == EXP2::NC);
        typedef matrix_binary_exp<matrix_exp<EXP1>,matrix_exp<EXP2>,op_complex_matrix<EXP1> > exp;
        return matrix_exp<exp>(exp(real_part,imag_part));
    }

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
    )
    {
        for (long r = 0; r < NR; ++r)
        {
            for (long c = 0; c < NC; ++c)
            {
                m(r,c) = static_cast<T>(value);
            }
        }
    }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP,
        long N
        >
    struct inv_helper
    {
        static const matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> inv (
            const matrix_exp<EXP>& m
        )
        {
            using namespace nric;
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            matrix<type, N, N> a(m), y;
            matrix<long,N,1> indx;
            matrix<type,N,1> col;
            type d;
            long i, j;
            ludcmp(a,indx,d);

            for (j = 0; j < N; ++j)
            {
                for (i = 0; i < N; ++i)
                    col(i) = 0;
                col(j) = 1;
                lubksb(a,indx,col);
                for (i = 0; i < N; ++i)
                    y(i,j) = col(i);
            }
            return y;
        }
    };

    template <
        typename EXP
        >
    struct inv_helper<EXP,1>
    {
        static const matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> inv (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            matrix<type, 1, 1> a;
            a(0) = 1/m(0);
            return a;
        }
    };

    template <
        typename EXP
        >
    struct inv_helper<EXP,2>
    {
        static const matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> inv (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            matrix<type, 2, 2> a;
            type d = static_cast<type>(1.0/det(m));
            a(0,0) = m(1,1)*d;
            a(0,1) = m(0,1)*-d;
            a(1,0) = m(1,0)*-d;
            a(1,1) = m(0,0)*d;
            return a;
        }
    };

    template <
        typename EXP
        >
    struct inv_helper<EXP,3>
    {
        static const matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> inv (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            matrix<type, 3, 3> ret;
            const type de = static_cast<type>(1.0/det(m));
            const type a = m(0,0);
            const type b = m(0,1);
            const type c = m(0,2);
            const type d = m(1,0);
            const type e = m(1,1);
            const type f = m(1,2);
            const type g = m(2,0);
            const type h = m(2,1);
            const type i = m(2,2);

            ret(0,0) = (e*i - f*h)*de;
            ret(1,0) = (f*g - d*i)*de;
            ret(2,0) = (d*h - e*g)*de;

            ret(0,1) = (c*h - b*i)*de;
            ret(1,1) = (a*i - c*g)*de;
            ret(2,1) = (b*g - a*h)*de;

            ret(0,2) = (b*f - c*e)*de;
            ret(1,2) = (c*d - a*f)*de;
            ret(2,2) = (a*e - b*d)*de;

            return ret;
        }
    };

    template <
        typename EXP
        >
    struct inv_helper<EXP,4>
    {
        static const matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> inv (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            matrix<type, 4, 4> ret;
            const type de = static_cast<type>(1.0/det(m));
            ret(0,0) =  det(removerc<0,0>(m));
            ret(0,1) = -det(removerc<0,1>(m));
            ret(0,2) =  det(removerc<0,2>(m));
            ret(0,3) = -det(removerc<0,3>(m));

            ret(1,0) = -det(removerc<1,0>(m));
            ret(1,1) =  det(removerc<1,1>(m));
            ret(1,2) = -det(removerc<1,2>(m));
            ret(1,3) =  det(removerc<1,3>(m));

            ret(2,0) =  det(removerc<2,0>(m));
            ret(2,1) = -det(removerc<2,1>(m));
            ret(2,2) =  det(removerc<2,2>(m));
            ret(2,3) = -det(removerc<2,3>(m));

            ret(3,0) = -det(removerc<3,0>(m));
            ret(3,1) =  det(removerc<3,1>(m));
            ret(3,2) = -det(removerc<3,2>(m));
            ret(3,3) =  det(removerc<3,3>(m));

            return trans(ret)*de;
        }
    };

    template <
        typename EXP
        >
    inline const matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> inv (
        const matrix_exp<EXP>& m
    ) { return inv_helper<EXP,matrix_exp<EXP>::NR>::inv(m); }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP
        >
    inline const matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> tmp (
        const matrix_exp<EXP>& m
    )
    {
        return matrix<typename matrix_exp<EXP>::type, matrix_exp<EXP>::NR, matrix_exp<EXP>::NC> (m);
    }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP
        >
    const typename matrix_exp<EXP>::type sum (
        const matrix_exp<EXP>& m
    )
    {
        typedef typename matrix_exp<EXP>::type type;
        const long NR = matrix_exp<EXP>::NR;
        const long NC = matrix_exp<EXP>::NC;

        type val = 0;
        for (long r = 0; r < NR; ++r)
        {
            for (long c = 0; c < NC; ++c)
            {
                val += m(r,c);
            }
        }
        return val;
    }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP
        >
    const typename matrix_exp<EXP>::type prod (
        const matrix_exp<EXP>& m
    )
    {
        typedef typename matrix_exp<EXP>::type type;
        const long NR = matrix_exp<EXP>::NR;
        const long NC = matrix_exp<EXP>::NC;

        type val = 1;
        for (long r = 0; r < NR; ++r)
        {
            for (long c = 0; c < NC; ++c)
            {
                val *= m(r,c);
            }
        }
        return val;
    }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP,
        long N = EXP::NR
        >
    struct det_helper
    {
        static const typename matrix_exp<EXP>::type det (
            const matrix_exp<EXP>& m
        )
        {
            using namespace nric;
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            matrix<type, N, N> lu(m);
            matrix<long,N,1> indx;
            type d;
            if (ludcmp(lu,indx,d) == false)
            {
                // the matrix is singular so its det is 0
                return 0;
            }

            return prod(diag(lu))*d;
        }
    };

    template <
        typename EXP
        >
    struct det_helper<EXP,1>
    {
        static const typename matrix_exp<EXP>::type det (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            return m(0);
        }
    };

    template <
        typename EXP
        >
    struct det_helper<EXP,2>
    {
        static const typename matrix_exp<EXP>::type det (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            return m(0,0)*m(1,1) - m(0,1)*m(1,0);
        }
    };

    template <
        typename EXP
        >
    struct det_helper<EXP,3>
    {
        static const typename matrix_exp<EXP>::type det (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            type temp = m(0,0)*(m(1,1)*m(2,2) - m(1,2)*m(2,1)) -
                        m(0,1)*(m(1,0)*m(2,2) - m(1,2)*m(2,0)) +
                        m(0,2)*(m(1,0)*m(2,1) - m(1,1)*m(2,0));
            return temp;
        }
    };

    template <
        typename EXP
        >
    inline const typename matrix_exp<EXP>::type det (
        const matrix_exp<EXP>& m
    );

    template <
        typename EXP
        >
    struct det_helper<EXP,4>
    {
        static const typename matrix_exp<EXP>::type det (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT(matrix_exp<EXP>::NR == matrix_exp<EXP>::NC);
            typedef typename matrix_exp<EXP>::type type;

            type temp = m(0,0)*(dlib::det(removerc<0,0>(m))) -
                        m(0,1)*(dlib::det(removerc<0,1>(m))) +
                        m(0,2)*(dlib::det(removerc<0,2>(m))) -
                        m(0,3)*(dlib::det(removerc<0,3>(m)));
            return temp;
        }
    };

    template <
        typename EXP
        >
    inline const typename matrix_exp<EXP>::type det (
        const matrix_exp<EXP>& m
    ) { return det_helper<EXP>::det(m); }

// ----------------------------------------------------------------------------------------

    template <
        typename T, 
        long NR_, 
        long NC_, 
        T val
        >
    struct op_uniform_matrix
    {
        const static long NR = NR_;
        const static long NC = NC_;
        typedef T type;
        static type apply ( long r, long c)
        { return val; }
    };

    template <
        typename T, 
        long NR, 
        long NC, 
        T val
        >
    const matrix_exp<matrix_zeroary_exp<op_uniform_matrix<T,NR,NC,val> > > uniform_matrix (
    )
    {
        typedef matrix_zeroary_exp<op_uniform_matrix<T,NR,NC,val> > exp;
        return matrix_exp<exp>(exp());
    }


// ----------------------------------------------------------------------------------------

    template <
        typename T, 
        long N
        >
    struct op_identity_matrix
    {
        const static long NR = N;
        const static long NC = N;
        typedef T type;
        static type apply ( long r, long c)
        { return static_cast<type>(r == c); }
    };

    template <
        typename T, 
        long N
        >
    const matrix_exp<matrix_zeroary_exp<op_identity_matrix<T,N> > > identity_matrix (
    )
    {
        typedef matrix_zeroary_exp<op_identity_matrix<T,N> > exp;
        return matrix_exp<exp>(exp());
    }

// ----------------------------------------------------------------------------------------

    template <long R, long C, typename EXP>
    struct op_rotate
    {
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;
        typedef typename EXP::type type;
        template <typename M>
        static type apply ( const M& m, long r, long c)
        { return m((r+R)%NR,(c+C)%NC); }
    };

    template <
        long R,
        long C,
        typename EXP
        >
    const matrix_exp<matrix_unary_exp<matrix_exp<EXP>,op_rotate<R,C,EXP> > > rotate (
        const matrix_exp<EXP>& m
    )
    {
        // You can't rotate a matrix by more rows than it has.
        COMPILE_TIME_ASSERT(R < EXP::NR);
        // You can't rotate a matrix by more columns than it has.
        COMPILE_TIME_ASSERT(C < EXP::NC);
        typedef matrix_unary_exp<matrix_exp<EXP>,op_rotate<R,C,EXP> > exp;
        return matrix_exp<exp>(exp(m));
    }

// ----------------------------------------------------------------------------------------

    template <typename EXP>
    struct op_pointwise_multiply
    {
        typedef typename EXP::type type;
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;

        template <typename M1, typename M2>
        static type apply ( const M1& m1, const M2& m2 , long r, long c)
        { return m1(r,c)*m2(r,c); }
    };

    template <
        typename EXP1,
        typename EXP2
        >
    const matrix_exp<matrix_binary_exp<matrix_exp<EXP1>,matrix_exp<EXP2>,op_pointwise_multiply<EXP1> > > pointwise_multiply (
        const matrix_exp<EXP1>& a,
        const matrix_exp<EXP2>& b 
    )
    {
        COMPILE_TIME_ASSERT((is_same_type<typename EXP1::type,typename EXP2::type>::value == true));
        COMPILE_TIME_ASSERT(EXP1::NR == EXP2::NR);
        COMPILE_TIME_ASSERT(EXP1::NC == EXP2::NC);
        typedef matrix_binary_exp<matrix_exp<EXP1>,matrix_exp<EXP2>,op_pointwise_multiply<EXP1> > exp;
        return matrix_exp<exp>(exp(a,b));
    }

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_MATRIx_UTILITIES_

