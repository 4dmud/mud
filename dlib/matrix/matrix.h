// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#ifndef DLIB_MATRIx_
#define DLIB_MATRIx_

#include "matrix_abstract.h"
#include "../algs.h"
#include "../serialize.h"
#include <sstream>
#include <algorithm>

#ifdef _MSC_VER
// Disable the following warnings for Visual Studio

// This warning is:
//    "warning C4355: 'this' : used in base member initializer list"
// Which we get from this code but it is not an error so I'm turning this
// warning off and then turning it back on at the end of the file.
#pragma warning(disable : 4355)

#endif

namespace dlib
{

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long num_rows,
        long num_cols
        >
    class matrix; 

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long num_rows,
        long num_cols
        >
    class matrix_ref
    {
    public:
        typedef T type;
        typedef matrix_ref ref_type;
        const static long NR = num_rows;
        const static long NC = num_cols;

        matrix_ref (
            const matrix<T,num_rows,num_cols>& m_
        ) : m(m_) {}

        matrix_ref (
            const matrix_ref& i_
        ) : m(i_.m) {}

        const T& operator() (
            long r,
            long c
        ) const { return m(r,c); }

        long nr (
        ) const { return NR; }

        long nc (
        ) const { return NC; }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const  { return false; }

        bool aliases (
            const matrix<T,num_rows,num_cols>& item
        ) const { return (&m == &item); }

        const matrix_ref ref(
        ) const { return *this; }

    private:
        // no assignment operator
        matrix_ref& operator=(const matrix_ref&);

        const matrix<T,num_rows,num_cols>& m; // This is the item contained by this expression.
    };

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long num_rows,
        long num_cols,
        bool no_alloc = (num_rows*num_cols*sizeof(T) <= 64)
        >
    class matrix_data;
    /*!
        WHAT THIS OBJECT REPRESENTS
            This object represents the actual allocation of space for a matrix.
            Small matrices allocate all their data on the stack and bigger ones
            use new to get their memory.
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long num_rows,
        long num_cols
        >
    class matrix_data<T,num_rows,num_cols,true>
    {
    public:
        matrix_data() {}

        T& operator() (
            long r, 
            long c
        ) { return data[r][c]; }

        const T& operator() (
            long r, 
            long c
        ) const { return data[r][c]; }

        void swap(
            matrix_data& item
        )
        {
            for (long r = 0; r < num_rows; ++r)
            {
                for (long c = 0; c < num_cols; ++c)
                {
                    std::swap((*this)(r,c),item(r,c));
                }
            }
        }

        void consume(
            matrix_data& item
        )
        /*!
            ensures
                - #*this == item
                - #item is in an untouchable state.  no one should do anything
                  to it other than let it destruct.
        !*/
        {
            for (long r = 0; r < num_rows; ++r)
            {
                for (long c = 0; c < num_cols; ++c)
                {
                    (*this)(r,c) = item(r,c);
                }
            }
        }

    private:

        // no operator= or copy constructor in matrix_data
        matrix_data& operator=(const matrix_data&);
        matrix_data( const matrix_data&);

        T data[num_rows][num_cols];
    };

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long num_rows,
        long num_cols
        >
    class matrix_data<T,num_rows,num_cols,false>
    {
    public:
        matrix_data (
        ) { data = new T[num_rows*num_cols]; }

        ~matrix_data ()
        { if (data) delete [] data; }

        T& operator() (
            long r, 
            long c
        ) { return data[r*num_cols + c]; }

        const T& operator() (
            long r, 
            long c
        ) const { return data[r*num_cols + c]; }

        void swap(
            matrix_data& item
        )
        {
            std::swap(item.data,data);
        }

        void consume(
            matrix_data& item
        )
        /*!
            ensures
                - #*this == item
                - #item is in an untouchable state.  no one should do anything
                  to it other than let it destruct.
        !*/
        {
            delete [] data;
            data = item.data;
            item.data = 0;
        }

    private:

        // no operator= or copy constructor in matrix base
        matrix_data& operator=(const matrix_data&);
        matrix_data( const matrix_data&);


        T* data;
    };

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename EXP
        >
    class matrix_exp
    {
    public:
        typedef typename EXP::type type;
        typedef typename EXP::ref_type ref_type;
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;

        matrix_exp (
            const EXP& exp
        ) : ref_(exp.ref()) {}

        inline const type operator() (
            long r,
            long c
        ) const 
        { 
            ASSERT(r < nr() && c < nc(), 
                "\tconst type matrix_exp::operator(r,c)"
                << "\n\tYou must give a valid row and column"
                << "\n\tr:    " << r 
                << "\n\tc:    " << c
                << "\n\tnr(): " << nr()
                << "\n\tnc(): " << nc() 
                << "\n\tthis: " << this
                );
            return ref_(r,c); 
        }

        const type operator() (
            long i
        ) const 
        {
            COMPILE_TIME_ASSERT(NC == 1);
            ASSERT(i < nr(), 
                "\tconst type matrix::operator(i)"
                << "\n\tYou must give a valid row number"
                << "\n\ti:    " << i
                << "\n\tnr(): " << nr()
                << "\n\tthis: " << this
                );
            return ref_(i,0);
        }

        long nr (
        ) const { return NR; }

        long nc (
        ) const { return NC; }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return ref_.aliases(item); }

        const ref_type& ref (
        ) const { return ref_; }
    private:

        const ref_type ref_;
    };

 // ----------------------------------------------------------------------------------------

    template <
        typename LHS,
        typename RHS,
        unsigned long count = 0
        >
    class matrix_multiply_exp 
    {
        /*!
            REQUIREMENTS ON LHS AND RHS
                - they must be matrix_exp or matrix_ref objects (or
                  objects with a compatible interface).
        !*/
    public:
        typedef typename LHS::type type;
        typedef matrix_multiply_exp ref_type;
        const static long NR = LHS::NR;
        const static long NC = RHS::NC;

        matrix_multiply_exp (
            const matrix_multiply_exp& item
        ) : lhs(item.lhs), rhs(item.rhs) {}

        inline matrix_multiply_exp (
            const LHS& lhs_,
            const RHS& rhs_
        ) :
            lhs(lhs_),
            rhs(rhs_)
        {
            // You are trying to multiply two incompatible matrices together.  The number of columns 
            // in the matrix on the left must match the number of rows in the matrix on the right.
            COMPILE_TIME_ASSERT(LHS::NC == RHS::NR);

            // You can't multiply matrices together if they don't both contain the same type of elements.
            COMPILE_TIME_ASSERT((is_same_type<typename LHS::type, typename RHS::type>::value == true));
        }

        inline const type operator() (
            long r, 
            long c
        ) const 
        { 
            type temp = type();
            for (long i = 0; i < LHS::NC; ++i)
            {
                temp += lhs(r,i)*rhs(i,c);
            }
            return temp;
        }

        long nr (
        ) const { return NR; }

        long nc (
        ) const { return NC; }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return lhs.aliases(item) || rhs.aliases(item); }

        const ref_type& ref(
        ) const { return *this; }

        const LHS lhs;
        const RHS rhs;
    };

    template <
        typename T,
        long NR,
        long NC,
        typename EXP1,
        typename EXP2
        >
    inline const matrix_exp<matrix_multiply_exp<EXP1, matrix_multiply_exp<EXP2,typename matrix<T,NR,NC>::ref_type >,0 > > operator* (
        const matrix_exp<matrix_multiply_exp<EXP1,EXP2,1> >& m1,
        const matrix<T,NR,NC>& m2
    )
    {
        // We are going to reorder the order of evaluation of the terms here.  This way the
        // multiplication will go faster.
        typedef matrix_multiply_exp<EXP2,typename matrix<T,NR,NC>::ref_type > exp_inner;
        typedef matrix_multiply_exp<EXP1, exp_inner,0 >  exp_outer;
        return matrix_exp<exp_outer>(exp_outer(m1.ref().lhs,exp_inner(m1.ref().rhs,m2)));
    }

    template <
        typename EXP1,
        typename EXP2
        >
    inline const matrix_exp<matrix_multiply_exp<EXP1, EXP2 > >  operator* (
        const matrix_exp<EXP1>& m1,
        const matrix_exp<EXP2>& m2
    )
    {
        typedef matrix_multiply_exp<EXP1, EXP2>  exp;
        return matrix_exp<exp>(exp(m1.ref(),m2.ref()));
    }

    template <
        typename T,
        long NR,
        long NC,
        typename EXP
        >
    inline const matrix_exp<matrix_multiply_exp<typename matrix<T,NR,NC>::ref_type, matrix_exp<EXP> > >  operator* (
        const matrix<T,NR,NC>& m1,
        const matrix_exp<EXP>& m2
    )
    {
        typedef matrix_multiply_exp<typename matrix<T,NR,NC>::ref_type, matrix_exp<EXP> >  exp;
        return matrix_exp<exp>(exp(m1,m2));
    }

    template <
        typename T,
        long NR,
        long NC,
        typename EXP
        >
    inline const matrix_exp<matrix_multiply_exp< matrix_exp<EXP>, typename matrix<T,NR,NC>::ref_type, 1> >  operator* (
        const matrix_exp<EXP>& m1,
        const matrix<T,NR,NC>& m2
    )
    {
        typedef matrix_multiply_exp< matrix_exp<EXP>, typename matrix<T,NR,NC>::ref_type, 1 >  exp;
        return matrix_exp<exp>(exp(m1,m2));
    }

    template <
        typename T,
        long NR1,
        long NC1,
        long NR2,
        long NC2
        >
    inline const matrix_exp<matrix_multiply_exp<typename matrix<T,NR1,NC1>::ref_type,typename matrix<T,NR2,NC2>::ref_type > >  operator* (
        const matrix<T,NR1,NC1>& m1,
        const matrix<T,NR2,NC2>& m2
    )
    {
        typedef matrix_multiply_exp<typename matrix<T,NR1,NC1>::ref_type, typename matrix<T,NR2,NC2>::ref_type >  exp;
        return matrix_exp<exp>(exp(m1,m2));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename LHS,
        typename RHS
        >
    class matrix_add_expression 
    {
        /*!
            REQUIREMENTS ON LHS AND RHS
                - they must be matrix_exp or matrix_ref objects (or
                  objects with a compatible interface).
        !*/
    public:
        typedef typename LHS::type type;
        typedef matrix_add_expression ref_type;
        const static long NR = LHS::NR;
        const static long NC = LHS::NC;

        matrix_add_expression (
            const matrix_add_expression& item
        ) : lhs(item.lhs), rhs(item.rhs) {}

        matrix_add_expression (
            const LHS& lhs_,
            const RHS& rhs_
        ) :
            lhs(lhs_),
            rhs(rhs_)
        {
            // You can only add matrices together if they both have the same number of rows and columns.
            COMPILE_TIME_ASSERT(LHS::NR == RHS::NR);
            COMPILE_TIME_ASSERT(LHS::NC == RHS::NC);

            // You can only add matrices together if they both contain the same types of elements.
            COMPILE_TIME_ASSERT((is_same_type<typename LHS::type, typename RHS::type>::value == true));
        }

        const type operator() (
            long r, 
            long c
        ) const { return lhs(r,c) + rhs(r,c); }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return lhs.aliases(item) || rhs.aliases(item); }

        const ref_type& ref(
        ) const { return *this; }

        const LHS lhs;
        const RHS rhs;
    };

    template <
        typename EXP1,
        typename EXP2
        >
    const matrix_exp<matrix_add_expression<EXP1, EXP2 > > operator+ (
        const matrix_exp<EXP1>& m1,
        const matrix_exp<EXP2>& m2
    )
    {
        typedef matrix_add_expression<EXP1, EXP2 >  exp;
        return matrix_exp<exp>(exp(m1.ref(),m2.ref()));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename LHS,
        typename RHS
        >
    class matrix_subtract_exp 
    {
        /*!
            REQUIREMENTS ON LHS AND RHS
                - they must be matrix_exp or matrix_ref objects (or
                  objects with a compatible interface).
        !*/
    public:
        typedef typename LHS::type type;
        typedef matrix_subtract_exp ref_type;
        const static long NR = LHS::NR;
        const static long NC = LHS::NC;

        matrix_subtract_exp (
            const LHS& lhs_,
            const RHS& rhs_
        ) :
            lhs(lhs_),
            rhs(rhs_)
        {
            // You can only subtract one matrix from another if they both have the same number of rows and columns.
            COMPILE_TIME_ASSERT(LHS::NR == RHS::NR);
            COMPILE_TIME_ASSERT(LHS::NC == RHS::NC);

            // You can only subtract one matrix from another if they both contain elements of the same type.
            COMPILE_TIME_ASSERT((is_same_type<typename LHS::type, typename RHS::type>::value == true));
        }

        const type operator() (
            long r, 
            long c
        ) const { return lhs(r,c) - rhs(r,c); }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return lhs.aliases(item) || rhs.aliases(item); }

        const ref_type& ref(
        ) const { return *this; }

        const LHS lhs;
        const RHS rhs;
    };

    template <
        typename EXP1,
        typename EXP2
        >
    const matrix_exp<matrix_subtract_exp<EXP1, EXP2 > > operator- (
        const matrix_exp<EXP1>& m1,
        const matrix_exp<EXP2>& m2
    )
    {
        typedef matrix_subtract_exp<EXP1, EXP2 >  exp;
        return matrix_exp<exp>(exp(m1.ref(),m2.ref()));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename M,
        typename S
        >
    class matrix_divscal_exp  
    {
        /*!
            REQUIREMENTS ON M 
                - must be a matrix_exp or matrix_ref object (or
                  an object with a compatible interface).

            REQUIREMENTS ON S
                - must be some kind of scalar type
        !*/
    public:
        typedef typename M::type type;
        typedef matrix_divscal_exp ref_type;
        const static long NR = M::NR;
        const static long NC = M::NC;

        matrix_divscal_exp (
            const M& m_,
            const S& s_
        ) :
            m(m_),
            s(s_)
        {}

        const type operator() (
            long r, 
            long c
        ) const { return m(r,c)/s; }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return m.aliases(item); }

        const ref_type& ref(
        ) const { return *this; }

        const M m;
        const S s;
    };

    template <
        typename EXP,
        typename S 
        >
    const matrix_exp<matrix_divscal_exp<matrix_exp<EXP>, S> > operator/ (
        const matrix_exp<EXP>& m,
        const S& s
    )
    {
        typedef matrix_divscal_exp<matrix_exp<EXP>,S >  exp;
        return matrix_exp<exp>(exp(m,s));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename M,
        typename S
        >
    class matrix_mulscal_exp  
    {
        /*!
            REQUIREMENTS ON M 
                - must be a matrix_exp or matrix_ref object (or
                  an object with a compatible interface).

            REQUIREMENTS ON S
                - must be some kind of scalar type
        !*/
    public:
        typedef typename M::type type;
        typedef matrix_mulscal_exp ref_type;
        const static long NR = M::NR;
        const static long NC = M::NC;

        matrix_mulscal_exp (
            const M& m_,
            const S& s_
        ) :
            m(m_),
            s(s_)
        {}

        const type operator() (
            long r, 
            long c
        ) const { return m(r,c)*s; }

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const { return m.aliases(item); }

        const ref_type& ref(
        ) const { return *this; }

        const M m;
        const S s;
    };

    template <
        typename EXP,
        typename S 
        >
    const matrix_exp<matrix_mulscal_exp<matrix_exp<EXP>, S> > operator* (
        const matrix_exp<EXP>& m,
        const S& s
    )
    {
        typedef matrix_mulscal_exp<matrix_exp<EXP>,S >  exp;
        return matrix_exp<exp>(exp(m,s));
    }

    template <
        typename EXP,
        typename S 
        >
    const matrix_exp<matrix_mulscal_exp<matrix_exp<EXP>, S> > operator* (
        const S& s,
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_mulscal_exp<matrix_exp<EXP>,S >  exp;
        return matrix_exp<exp>(exp(m,s));
    }

    template <
        typename EXP 
        >
    const matrix_exp<matrix_mulscal_exp<matrix_exp<EXP>, float> > operator/ (
        const matrix_exp<EXP>& m,
        const float& s
    )
    {
        typedef matrix_mulscal_exp<matrix_exp<EXP>,float >  exp;
        return matrix_exp<exp>(exp(m,1.0/s));
    }

    template <
        typename EXP
        >
    const matrix_exp<matrix_mulscal_exp<matrix_exp<EXP>, double> > operator/ (
        const matrix_exp<EXP>& m,
        const double& s
    )
    {
        typedef matrix_mulscal_exp<matrix_exp<EXP>,double >  exp;
        return matrix_exp<exp>(exp(m,1.0/s));
    }

    template <
        typename EXP
        >
    const matrix_exp<matrix_mulscal_exp<matrix_exp<EXP>, long double> > operator/ (
        const matrix_exp<EXP>& m,
        const long double& s
    )
    {
        typedef matrix_mulscal_exp<matrix_exp<EXP>,long double >  exp;
        return matrix_exp<exp>(exp(m,1.0/s));
    }

    template <
        typename EXP
        >
    const matrix_exp<matrix_mulscal_exp<matrix_exp<EXP>, int> > operator- (
        const matrix_exp<EXP>& m
    )
    {
        typedef matrix_mulscal_exp<matrix_exp<EXP>,int >  exp;
        return matrix_exp<exp>(exp(m,-1));
    }

// ----------------------------------------------------------------------------------------

    template <
        typename EXP1,
        typename EXP2
        >
    bool operator== (
        const matrix_exp<EXP1>& m1,
        const matrix_exp<EXP2>& m2
    )
    {
        if (m1.nr() == m2.nr() && m1.nc() == m2.nc())
        {
            for (long r = 0; r < m1.nr(); ++r)
            {
                for (long c = 0; c < m1.nc(); ++c)
                {
                    if (m1(r,c) != m2(r,c))
                        return false;
                }
            }
            return true;
        }
        return false;
    }

    template <
        typename EXP1,
        typename EXP2
        >
    bool operator!= (
        const matrix_exp<EXP1>& m1,
        const matrix_exp<EXP2>& m2
    ) { return !(m1 == m2); }

// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------
// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long num_rows,
        long num_cols
        >
    class matrix : public matrix_exp<matrix_ref<T,num_rows,num_cols> > 
    {
    public:
        typedef T type;
        typedef matrix_ref<T,num_rows,num_cols> ref_type;
        const static long NR = num_rows;
        const static long NC = num_cols;


        matrix () : matrix_exp<matrix_ref<T,num_rows,num_cols> >(ref_type(*this)) 
        {
            COMPILE_TIME_ASSERT(num_rows > 0 && num_cols > 0); 
        }

        template <typename EXP>
        matrix (
            const matrix_exp<EXP>& m
        ): matrix_exp<matrix_ref<T,num_rows,num_cols> >(ref_type(*this)) 
        {
            COMPILE_TIME_ASSERT(num_rows > 0 && num_cols > 0); 
            COMPILE_TIME_ASSERT(EXP::NR == NR);
            COMPILE_TIME_ASSERT(EXP::NC == NC);
            COMPILE_TIME_ASSERT((is_same_type<typename EXP::type,type>::value == true));
            for (long r = 0; r < NR; ++r)
            {
                for (long c = 0; c < NC; ++c)
                {
                    data(r,c) = m(r,c);
                }
            }
        }

        matrix (
            const matrix& m
        ): matrix_exp<matrix_ref<T,num_rows,num_cols> >(ref_type(*this)) 
        {
            COMPILE_TIME_ASSERT(num_rows > 0 && num_cols > 0); 
            for (long r = 0; r < NR; ++r)
            {
                for (long c = 0; c < NC; ++c)
                {
                    data(r,c) = m(r,c);
                }
            }
        }

        template <typename U, size_t len>
        matrix (
            U (&array)[len]
        ): matrix_exp<matrix_ref<T,num_rows,num_cols> >(ref_type(*this)) 
        {
            COMPILE_TIME_ASSERT(num_rows > 0 && num_cols > 0); 
            COMPILE_TIME_ASSERT(NR*NC == len);
            size_t idx = 0;
            for (long r = 0; r < NR; ++r)
            {
                for (long c = 0; c < NC; ++c)
                {
                    data(r,c) = static_cast<T>(array[idx]);
                    ++idx;
                }
            }
        }

        T& operator() (
            long r, 
            long c
        ) 
        { 
            ASSERT(r < nr() && c < nc(), 
                "\tT& matrix::operator(r,c)"
                << "\n\tYou must give a valid row and column"
                << "\n\tr:    " << r 
                << "\n\tc:    " << c
                << "\n\tnr(): " << nr()
                << "\n\tnc(): " << nc() 
                << "\n\tthis: " << this
                );
            return data(r,c); 
        }

        const T& operator() (
            long r, 
            long c
        ) const 
        { 
            ASSERT(r < nr() && c < nc(), 
                "\tconst T& matrix::operator(r,c)"
                << "\n\tYou must give a valid row and column"
                << "\n\tr:    " << r 
                << "\n\tc:    " << c
                << "\n\tnr(): " << nr()
                << "\n\tnc(): " << nc() 
                << "\n\tthis: " << this
                );
            return data(r,c);
        }

        T& operator() (
            long i
        ) 
        {
            // You can only use this operator on column vectors.
            COMPILE_TIME_ASSERT(NC == 1);
            ASSERT(i < nr(), 
                "\tT& matrix::operator(i)"
                << "\n\tYou must give a valid row number"
                << "\n\ti:    " << i
                << "\n\tnr(): " << nr()
                << "\n\tthis: " << this
                );
            return data(i,0);
        }

        const T& operator() (
            long i
        ) const
        {
            // You can only use this operator on column vectors.
            COMPILE_TIME_ASSERT(NC == 1);
            ASSERT(i < nr(), 
                "\tconst T& matrix::operator(i)"
                << "\n\tYou must give a valid row number"
                << "\n\ti:    " << i
                << "\n\tnr(): " << nr()
                << "\n\tthis: " << this
                );
            return data(i,0);
        }

        long nr (
        ) const { return NR; }

        long nc (
        ) const { return NC; }

        template <typename U, size_t len>
        matrix& operator= (
            U (&array)[len]
        )
        {
            COMPILE_TIME_ASSERT(NR*NC == len);
            size_t idx = 0;
            for (long r = 0; r < NR; ++r)
            {
                for (long c = 0; c < NC; ++c)
                {
                    data(r,c) = static_cast<T>(array[idx]);
                    ++idx;
                }
            }
            return *this;
        }

        template <typename EXP>
        matrix& operator= (
            const matrix_exp<EXP>& m
        )
        {
            COMPILE_TIME_ASSERT((EXP::NR == NR && EXP::NC == NC));
            COMPILE_TIME_ASSERT((is_same_type<typename EXP::type,type>::value == true));
            if (m.aliases(*this) == false)
            {
                for (long r = 0; r < NR; ++r)
                {
                    for (long c = 0; c < NC; ++c)
                    {
                        data(r,c) = m(r,c);
                    }
                }
            }
            else
            {
                // we have to use a temporary matrix_data object here because
                // this->data is aliases inside the matrix_exp m somewhere.
                matrix_data<T,NR,NC> temp;
                for (long r = 0; r < NR; ++r)
                {
                    for (long c = 0; c < NC; ++c)
                    {
                        temp(r,c) = m(r,c);
                    }
                }
                data.consume(temp);
            }
            return *this;
        }

        matrix& operator= (
            const matrix& m
        )
        {
            if (this != &m)
            {
                for (long r = 0; r < NR; ++r)
                {
                    for (long c = 0; c < NC; ++c)
                    {
                        data(r,c) = m(r,c);
                    }
                }
            }
            return *this;
        }

        void swap (
            matrix& item
        )
        {
            data.swap(item.data);
        }

    private:
        matrix_data<T,NR,NC> data;
    };

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long NR,
        long NC
        >
    void swap(
        matrix<T,NR,NC>& a,
        matrix<T,NR,NC>& b
    ) { a.swap(b); }

    template <
        typename T,
        long NR,
        long NC
        >
    void serialize (
        const matrix<T,NR,NC>& item, 
        std::ostream& out
    )
    {
        try
        {
            serialize(NR,out);
            serialize(NC,out);
            for (long r = 0; r < NR; ++r)
            {
                for (long c = 0; c < NC; ++c)
                {
                    serialize(item(r,c),out);
                }
            }
        }
        catch (serialization_error& e)
        {
            throw serialization_error(e.info + "\n   while serializing dlib::matrix");
        }
    }

    template <
        typename T,
        long NR,
        long NC
        >
    void deserialize (
        matrix<T,NR,NC>& item, 
        std::istream& in
    )
    {
        long nr, nc;
        try
        {
            deserialize(nr,in); 
            deserialize(nc,in); 
            if (nr == NR && nc == NC)
            {
                for (long r = 0; r < NR; ++r)
                {
                    for (long c = 0; c < NC; ++c)
                    {
                        deserialize(item(r,c),in);
                    }
                }
            }
        }
        catch (serialization_error& e)
        {
            throw serialization_error(e.info + "\n   while deserializing a dlib::matrix");
        }

        if (nr != NR || nc != NC)
            throw serialization_error("Error deserializing a dlib::matrix, the matrix dimensions don't match");
    }

    template <
        typename EXP
        >
    std::ostream& operator<< (
        std::ostream& out,
        const matrix_exp<EXP>& m
    )
    {
        using namespace std;
        const streamsize old = out.width();

        // first figure out how wide we should make each field
        string::size_type w = 0;
        ostringstream sout;
        for (long r = 0; r < m.nr(); ++r)
        {
            for (long c = 0; c < m.nc(); ++c)
            {
                sout << m(r,c); 
                w = std::max(sout.str().size(),w);
                sout.str("");
            }
        }

        // now actually print it
        for (long r = 0; r < m.nr(); ++r)
        {
            for (long c = 0; c < m.nc(); ++c)
            {
                out.width(static_cast<streamsize>(w));
                out << m(r,c) << " ";
            }
            out << "\n";
        }
        out.width(old);
        return out;
    }

// ----------------------------------------------------------------------------------------

}

#ifdef _MSC_VER
// put that warning back to its default setting
#pragma warning(default : 4355)
#endif

#endif // DLIB_MATRIx_

