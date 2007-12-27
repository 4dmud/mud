// Copyright (C) 2006  Davis E. King (davisking@users.sourceforge.net)
// License: Boost Software License   See LICENSE.txt for the full license.
#undef DLIB_MATRIx_ABSTRACT_
#ifdef DLIB_MATRIx_ABSTRACT_

#include "../serialize.h"

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

    template <
        typename T,
        long num_rows,
        long num_cols
        >
    class matrix_ref
    {
        /*!
            WHAT THIS OBJECT REPRESENTS
                This object represents a copyable (via the copy constructor but not
                operator=) reference to a matrix object.  
        !*/
    public:
        typedef T type;
        typedef matrix_ref ref_type;
        const static long NR = num_rows;
        const static long NC = num_cols;

        matrix_ref (
            const matrix<T,num_rows,num_cols>& m
        );
        /*!
            ensures
                - #aliases(m) == true
                  (i.e. #*this references/aliases the matrix m.)
        !*/

        matrix_ref (
            const matrix_ref& r
        );
        /*!
            ensures
                - #*this references/aliases the same matrix as r does.
        !*/

        const T& operator() (
            long r,
            long c
        ) const;
        /*!
            requires
                - r < nr()
                - c < nc()
            ensures
                - returns a const reference to the value at the given row and column in 
                  this matrix.
        !*/

        long nr (
        ) const;
        /*!
            ensures
                - returns the number of rows in the matrix referenced by *this
        !*/

        long nc (
        ) const;
        /*!
            ensures
                - returns the number of columns in the matrix referenced by *this
        !*/

        template <typename U, long iNR, long iNC>
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const;
        /*!
            ensures
                - if (item is the matrix referenced by *this) then
                    - returns true
                - else
                    - returns false
        !*/

        const ref_type& ref(
        ) const { return *this; }
        /*!
            ensures
                - returns *this
        !*/

    private:
        // no assignment operator
        matrix_ref& operator=(const matrix_ref&);
    };

// ----------------------------------------------------------------------------------------

    template <
        typename EXP
        >
    class matrix_exp
    {
        /*!
            REQUIREMENTS ON EXP
                - must be a matrix_exp or matrix_ref object (or an object with 
                  a compatible interface)

            WHAT THIS OBJECT REPRESENTS
                This object represents an expression that evaluates to a matrix 
                of nr() rows and nc() columns.  
                
                The reason for having an object that represents an expression is that it 
                allows us to use the "expression templates" technique to eliminate the 
                temporary matrix objects that would normally be returned from expressions 
                such as M = A*B*C*D;  Normally each invocation of the * operator would
                construct and return a temporary matrix object but using this technique we 
                can avoid creating all of these temporary objects and receive a large 
                speed boost.

                Note that every time you invoke operator() on this object it recomputes 
                its result which may not be what you want to do.  For example, if you 
                are going to be accessing the same element over and over it would probably
                be faster to assign the matrix_exp to a temporary matrix and then 
                use that temporary.
        !*/

    public:
        typedef typename EXP::type type;
        typedef typename EXP::ref_type ref_type;
        const static long NR = EXP::NR;
        const static long NC = EXP::NC;

        matrix_exp (
            const EXP& exp
        ); 
        /*!
            ensures
                - #ref() == exp.ref()
        !*/

        const type operator() (
            long r,
            long c
        ) const
        /*!
            requires
                - r < nr()
                - c < nc()
            ensures
                - returns ref()(r,c)
                  (i.e. returns the value at the given row and column that would be in
                  the matrix represented by this matrix expression)
        !*/

        const type operator() (
            long i
        ) const;
        /*!
            requires
                - nc() == 1 (i.e. this must be a column vector)
                - i < nr()
            ensures
                - returns (*this)(i,0)
        !*/

        long nr (
        ) const;
        /*!
            ensures
                - returns the number of rows in this matrix expression. (i.e. NR)
        !*/

        long nc (
        ) const 
        /*!
            ensures
                - returns the number of columns in this matrix expression. (i.e. NC)
        !*/

        template <typename U, long iNR, long iNC >
        bool aliases (
            const matrix<U,iNR,iNC>& item
        ) const 
        /*!
            ensures
                - if (this matrix expression contains/aliases the given matrix or contains
                  any subexpressions that contain/alias the given matrix) then
                    - returns true
                - else
                    - returns false
        !*/

        const ref_type& ref (
        ) const; 
        /*!
            ensures
                - returns a copyable reference to the subexpression contained in *this.
        !*/

    };

// ----------------------------------------------------------------------------------------

    /*
        Note that these operator prototypes are not correct C++ (the real versions, which 
        you can see in the implementation are really complex and so probably would 
        distract/confuse people if shown here).  Think of this as just a list of the 
        operators available to you and what they do.
    */

    const matrix_exp operator* (
        const matrix_exp& m1,
        const matrix_exp& m2
    );
    /*!
        requires
            - m1.nc() == m2.nr()
            - m1 and m2 both contain elements of the same type
        ensures
            - returns the result of doing the matrix multiplication m1*m2.  The resulting
              matrix will have m1.nr() rows and m2.nc() columns.
    !*/

    const matrix_exp operator+ (
        const matrix_exp& m1,
        const matrix_exp& m2
    );
    /*!
        requires
            - m1.nr() == m2.nr()
            - m1.nc() == m2.nc()
            - m1 and m2 both contain elements of the same type
        ensures
            - returns a matrix R such that for all valid r and c:
              R(r,c) == m1(r,c) + m2(r,c)
              (i.e. returns the result of doing a pairwise addition of the matrices m1 and m2.)
              The resulting matrix will have the same dimensions as the originals.
    !*/

    const matrix_exp operator- (
        const matrix_exp& m1,
        const matrix_exp& m2
    );
    /*!
        requires
            - m1.nr() == m2.nr()
            - m1.nc() == m2.nc()
            - m1 and m2 both contain elements of the same type
        ensures
            - returns a matrix R such that for all valid r and c:
              R(r,c) == m1(r,c) - m2(r,c)
              (i.e. returns the result of doing a pairwise subtraction of the matrices m1 and m2.)
              The resulting matrix will have the same dimensions as the originals.
    !*/

    template <typename T>
    const matrix_exp operator* (
        const matrix_exp& m,
        const T& value
    );
    /*!
        ensures
            - returns the result of multiplying all the elements of matrix m by the given 
              scalar value.  The resulting matrix will have the same dimensions as m.
    !*/

    template <typename T>
    const matrix_exp operator* (
        const T& value,
        const matrix_exp& m
    );
    /*!
        ensures
            - returns the result of multiplying all the elements of matrix m by the given 
              scalar value.  The resulting matrix will have the same dimensions as m.
    !*/

    const matrix_exp operator- (
        const matrix_exp& m
    );
    /*!
        ensures
            - returns -1*m
    !*/

    template <typename T>
    const matrix_exp operator/ (
        const matrix_exp& m,
        const T& value
    );
    /*!
        ensures
            - returns the result of dividing all the elements of matrix m by the given 
              scalar value.  The resulting matrix will have the same dimensions as m.
    !*/

    bool operator== (
        const matrix_exp& m1,
        const matrix_exp& m2
    );
    /*!
        ensures
            - if (m1.nr() == m2.nr() && m1.nc() == m2.nc() &&
              for all valid r and c:  m1(r,c) == m2(r,c) ) then
                - returns true
            - else
                - returns false
    !*/

    bool operator!= (
        const matrix_exp& m1,
        const matrix_exp& m2
    );
    /*!
        ensures
            - returns !(m1 == m2)
    !*/

// ----------------------------------------------------------------------------------------

    template <
        typename T,
        long num_rows,
        long num_cols
        >
    class matrix : public matrix_exp<matrix_ref<T,num_rows,num_cols> > 
    {
        /*!
            REQUIREMENTS ON num_rows
                must be bigger than 0

            REQUIREMENTS ON num_cols
                must be bigger than 0

            INITIAL VALUE
                - nr() == num_rows
                - nc() == num_cols

            WHAT THIS OBJECT REPRESENTS
                This object represents a matrix of nr() rows and nc() columns.  This object
                is also a matrix_exp.  Thus it can be used in all of the above
                global operators.

                Also note that the elements of this matrix are contiguous in memory and 
                stored in row major order. 
        !*/

    public:
        typedef T type;
        typedef matrix_ref<T,num_rows,num_cols> ref_type;
        const static long NR = num_rows;
        const static long NC = num_cols;

        matrix (
        );
        /*!
            ensures
                - #*this is properly initialized
                - #aliases(*this) == true
                - #ref().aliases(*this) == true
        !*/

        matrix (
            const matrix& m
        );
        /*!
            requires
                - NR == m.nr()
                - NC == m.nc()
            ensures
                - #*this == m
                - #aliases(*this) == true
                - #ref().aliases(*this) == true
        !*/

        template <typename EXP>
        matrix (
            const matrix_exp<EXP>& m
        );
        /*!
            requires
                - matrix_exp<EXP>::type == T
                - NR == m.nr()
                - NC == m.nc()
            ensures
                - #*this == m
                - #aliases(*this) == true
                - #ref().aliases(*this) == true
        !*/

        template <typename U, size_t len>
        matrix (
            U (&array)[len]
        );
        /*!
            requires
                - len == nr()*nc()  (i.e. the array you give here must be the right size)
            ensures
                - for all valid r and c:
                  #(*this)(r,c) == array[r*nc() + c]
                  (i.e. initializes this matrix with the contents of the given array)
        !*/

        T& operator() (
            long r, 
            long c
        ); 
        /*!
            requires
                - r < nr()
                - c < nc()
            ensures
                - returns a reference to the value at the given row and column in 
                  this matrix.
        !*/

        const T& operator() (
            long r, 
            long c
        ) const;
        /*!
            requires
                - r < nr()
                - c < nc()
            ensures
                - returns a const reference to the value at the given row and column in 
                  this matrix.
        !*/

        T& operator() (
            long i
        ); 
        /*!
            requires
                - nc() == 1 (i.e. this must be a column vector)
                - i < nr()
            ensures
                - returns a reference to (*this)(i,0)
        !*/

        const T& operator() (
            long i
        ) const;
        /*!
            requires
                - nc() == 1 (i.e. this must be a column vector)
                - i < nr()
            ensures
                - returns a reference to (*this)(i,0)
        !*/

        template <typename U, size_t len>
        matrix& operator= (
            U (&array)[len]
        );
        /*!
            requires
                - len == nr()*nc()  (i.e. the array you give here must be the right size)
            ensures
                - for all valid r and c:
                  #(*this)(r,c) == array[r*nc() + c]
                  (i.e. loads this matrix with the contents of the given array)
                - returns *this
        !*/

        matrix& operator= (
            const matrix& m
        );
        /*!
            requires
                - nr() == m.nr()
                - nc() == m.nc()
            ensures
                - copies the given matrix object m to *this
                - returns *this
        !*/

        template <typename EXP>
        matrix& operator= (
            const matrix_exp<EXP>& m
        );
        /*!
            requires
                - matrix_exp<EXP>::type == T
                - nr() == m.nr()
                - nc() == m.nc()
            ensures
                - copies the given matrix expression m to *this
                - returns *this
        !*/

        void swap (
            matrix& item
        );
        /*!
            ensures
                - swaps *this and item
        !*/
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
    /*!
        provides a global swap function
    !*/

    template <
        typename T,
        long NR,
        long NC
        >
    void serialize (
        const matrix<T,NR,NC>& item, 
        std::ostream& out
    );   
    /*!
        provides serialization support 
    !*/

    template <
        typename T,
        long NR,
        long NC
        >
    void deserialize (
        matrix<T,NR,NC>& item, 
        std::istream& in
    );   
    /*!
        provides deserialization support 
    !*/

    template <
        typename EXP
        >
    std::ostream& operator<< (
        std::ostream& out,
        const matrix_exp<EXP>& m
    );
    /*!
        ensures
            - writes m to the given out stream in a form suitable for human consumption.
            - returns out
    !*/

// ----------------------------------------------------------------------------------------

}

#endif // DLIB_MATRIx_ABSTRACT_

